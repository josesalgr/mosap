// rcpp_prepare_fragmentation_pu.cpp
#include "Package.h"
#include "OptimizationProblem.h"

#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <cstdint>

// helper: undirected key (i<j), i/j 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_fragmentation_pu(
    SEXP x,
    Rcpp::DataFrame relation_data,    // internal_pu1, internal_pu2, weight (may include both directions and diagonal)
    std::string block_name = "fragmentation_pu",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds: check op->_w_offset/op->_n_pu vs number of variables.");
  }

  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  const int m_in = relation_data.nrows();
  if (m_in <= 0) Rcpp::stop("relation_data has 0 rows.");

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"]; // 1..n_pu
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"]; // 1..n_pu
  Rcpp::NumericVector wgt = relation_data["weight"];

  // ------------------------------------------------------------------
  // Canonicalize OFF-DIAGONAL edges for y creation:
  //  - ignore self (i==j): diagonal handled elsewhere (e.g., in objective as self/exposed)
  //  - collapse symmetric duplicates -> unique undirected edges with max(weight)
  //  - enforce deterministic ordering (sorted by key)
  // ------------------------------------------------------------------
  std::unordered_map<std::uint64_t, double> edge_w;
  edge_w.reserve(static_cast<std::size_t>(m_in) * 2);

  int n_self_rows = 0;

  for (int r = 0; r < m_in; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];

    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range (must be 1..n_pu).");
    }

    const double we = (double)wgt[r];
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    if (i1 == j1) {
      ++n_self_rows;
      continue; // diagonal not used for y creation
    }

    int a = i1, b = j1;
    if (a > b) std::swap(a, b);
    const std::uint64_t k = edge_key_undirected(a, b);

    auto it = edge_w.find(k);
    if (it == edge_w.end()) edge_w.emplace(k, we);
    else it->second = std::max(it->second, we);
  }

  const int k_edges = static_cast<int>(edge_w.size());
  if (k_edges <= 0) {
    Rcpp::stop("No usable non-self edges found to prepare fragmentation y variables.");
  }

  // ------------------------------------------------------------------
  // Idempotence: if already prepared, just check k_edges matches
  // ------------------------------------------------------------------
  if (op->_n_y_pu > 0) {
    if (op->_n_y_pu != k_edges) {
      Rcpp::stop(
        "PU-fragmentation auxiliaries already exist, but canonical edge count differs. "
        "Existing _n_y_pu=" + std::to_string(op->_n_y_pu) +
          ", new k_edges=" + std::to_string(k_edges) + "."
      );
    }
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("already_prepared") = true,
      Rcpp::Named("y_offset") = op->_y_pu_offset,
      Rcpp::Named("n_y") = op->_n_y_pu,
      Rcpp::Named("k_edges") = k_edges
    );
  }

  // Sort edges deterministically by key so add() can replicate order exactly
  std::vector<std::uint64_t> keys;
  keys.reserve(edge_w.size());
  for (const auto& kv : edge_w) keys.push_back(kv.first);
  std::sort(keys.begin(), keys.end());

  std::vector<int> e_i; e_i.reserve(k_edges);
  std::vector<int> e_j; e_j.reserve(k_edges);

  for (auto key : keys) {
    const int a = static_cast<int>(key >> 32);
    const int b = static_cast<int>(key & 0xFFFFFFFFu);
    e_i.push_back(a);
    e_j.push_back(b);
  }

  // ------------------------------------------------------------------
  // 1) Create y variables: one per UNIQUE undirected off-diagonal edge
  //    IMPORTANT: y_ij is an AND auxiliary (0..1 continuous is fine)
  // ------------------------------------------------------------------
  const int y0 = (int)op->ncol_used();
  op->_y_pu_offset = y0;
  op->_n_y_pu      = k_edges;

  op->_obj.reserve(op->_obj.size() + (std::size_t)k_edges);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)k_edges);
  op->_lb.reserve(op->_lb.size() + (std::size_t)k_edges);
  op->_ub.reserve(op->_ub.size() + (std::size_t)k_edges);

  for (int e = 0; e < k_edges; ++e) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("C");  // AND auxiliary: continuous [0,1] is enough
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=prepare"
    ";component=fragmentation_pu"
    ";y_semantics=and"
    ";m_in=" + std::to_string(m_in) +
      ";k_edges=" + std::to_string(k_edges) +
      ";n_self_rows=" + std::to_string(n_self_rows) +
      ";y_offset=" + std::to_string(y0) +
      ";vtype=C"
      ";objective_touched=FALSE";

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::y_pu",
    (std::size_t)y0,
    (std::size_t)(y0 + k_edges),
    full_tag
  );

  // ------------------------------------------------------------------
  // 2) Add constraints (3 per edge): AND linearization
  //    y_ij <= w_i
  //    y_ij <= w_j
  //    y_ij >= w_i + w_j - 1
  // ------------------------------------------------------------------
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  int n_constraints_added = 0;

  for (int e = 0; e < k_edges; ++e) {
    const int i1 = e_i[e];
    const int j1 = e_j[e];

    const int i0 = i1 - 1;
    const int j0 = j1 - 1;

    const int y_col = y0 + e;
    const int wi    = op->_w_offset + i0;
    const int wj    = op->_w_offset + j0;

    // y - w_i <= 0
    op->addRow({ y_col, wi }, { 1.0, -1.0 }, "<=", 0.0, "y_le_wi");
    ++n_constraints_added;

    // y - w_j <= 0
    op->addRow({ y_col, wj }, { 1.0, -1.0 }, "<=", 0.0, "y_le_wj");
    ++n_constraints_added;

    // y - w_i - w_j >= -1
    op->addRow({ y_col, wi, wj }, { 1.0, -1.0, -1.0 }, ">=", -1.0, "y_ge_wi_plus_wj_minus1");
    ++n_constraints_added;
  }

  const std::size_t cons_block_id = op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("already_prepared") = false,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("constraints_block_id") = (double)cons_block_id,
    Rcpp::Named("y_offset") = op->_y_pu_offset,
    Rcpp::Named("n_y") = op->_n_y_pu,
    Rcpp::Named("k_edges") = k_edges,
    Rcpp::Named("m_in") = m_in,
    Rcpp::Named("n_constraints_added") = n_constraints_added,
    Rcpp::Named("tag") = full_tag
  );
}
