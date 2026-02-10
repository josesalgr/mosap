// rcpp_prepare_fragmentation_actions_by_action.cpp
#include "Package.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include <cstdint>

// key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// undirected key (i<j), 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_fragmentation_actions_by_action(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // internal_row, internal_pu, internal_action (model-ready)
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight (diag allowed)
    Rcpp::Nullable<Rcpp::IntegerVector> actions_to_use = R_NilValue, // optional internal_action ids (1-based)
    std::string block_name = "fragmentation_actions_by_action",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This prepare requires action variables, but op->_n_x <= 0.");
  if (op->_x_offset < 0) Rcpp::stop("op->_x_offset not initialized.");

  // checks: relation_data
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }
  // checks: dist_actions_data
  for (auto nm : {"internal_row", "internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // relation arrays
  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector  wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"];     // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];      // 1..n_pu
  Rcpp::IntegerVector daa = dist_actions_data["internal_action"];  // 1..n_actions (inferred)
  const int da_n = dist_actions_data.nrows();

  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // infer n_actions
  int n_actions = 0;
  for (int r = 0; r < da_n; ++r) {
    const int a = daa[r];
    if (a == NA_INTEGER) continue;
    if (a > n_actions) n_actions = a;
  }
  if (n_actions <= 0) Rcpp::stop("Could not infer number of actions (max internal_action <= 0).");

  // choose actions list A (sorted unique)
  std::vector<int> A;
  if (actions_to_use.isNotNull()) {
    Rcpp::IntegerVector tmp(actions_to_use);
    if (tmp.size() == 0) Rcpp::stop("actions_to_use provided but empty.");
    A.reserve((std::size_t)tmp.size());
    for (int k = 0; k < tmp.size(); ++k) {
      const int a = tmp[k];
      if (a < 1 || a > n_actions) Rcpp::stop("actions_to_use contains invalid internal_action id.");
      A.push_back(a);
    }
    std::sort(A.begin(), A.end());
    A.erase(std::unique(A.begin(), A.end()), A.end());
  } else {
    A.reserve((std::size_t)n_actions);
    for (int a = 1; a <= n_actions; ++a) A.push_back(a);
  }
  const int nA = (int)A.size();
  if (nA <= 0) Rcpp::stop("No actions selected for preparation.");

  // canonicalize unique undirected off-diagonal edges (sorted)
  std::unordered_map<std::uint64_t, double> edge_w;
  edge_w.reserve((std::size_t)m * 2);

  int n_self_rows = 0;
  for (int r = 0; r < m; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range (1..n_pu).");
    }

    const double we = (double)wgt[r];
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    if (i1 == j1) { ++n_self_rows; continue; }

    int a = i1, b = j1;
    if (a > b) std::swap(a, b);
    const std::uint64_t k = edge_key_undirected(a, b);

    auto it = edge_w.find(k);
    if (it == edge_w.end()) edge_w.emplace(k, we);
    else it->second = std::max(it->second, we);
  }

  const int k_edges = (int)edge_w.size();
  if (k_edges <= 0) Rcpp::stop("No usable off-diagonal edges found for action fragmentation preparation.");

  // deterministic order of edges by key
  std::vector<std::uint64_t> keys;
  keys.reserve(edge_w.size());
  for (const auto& kv : edge_w) keys.push_back(kv.first);
  std::sort(keys.begin(), keys.end());

  std::vector<int> e_i; e_i.reserve(k_edges);
  std::vector<int> e_j; e_j.reserve(k_edges);
  for (auto key : keys) {
    const int i = static_cast<int>(key >> 32);
    const int j = static_cast<int>(key & 0xFFFFFFFFu);
    e_i.push_back(i);
    e_j.push_back(j);
  }

  const int mA = k_edges * nA;

  // idempotence: if already prepared, enforce same expected size
  if (op->_n_y_action > 0) {
    if (op->_n_y_action != mA) {
      Rcpp::stop(
        "Action-fragmentation auxiliaries already exist, but expected size differs. "
        "Existing _n_y_action=" + std::to_string(op->_n_y_action) +
          ", expected mA=" + std::to_string(mA) + "."
      );
    }
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("already_prepared") = true,
      Rcpp::Named("y_offset") = op->_y_action_offset,
      Rcpp::Named("n_y") = op->_n_y_action,
      Rcpp::Named("k_edges") = k_edges,
      Rcpp::Named("n_actions_used") = nA
    );
  }

  // build sparse map (ipu, act) -> x col
  std::unordered_map<long long, int> xcol;
  xcol.reserve((std::size_t)da_n * 2);

  for (int r = 0; r < da_n; ++r) {
    const int row1 = dar[r];
    const int ipu  = dap[r];
    const int act  = daa[r];

    if (row1 == NA_INTEGER || ipu == NA_INTEGER || act == NA_INTEGER) continue;

    if (row1 < 1 || row1 > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (ipu  < 1 || ipu  > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");
    if (act  < 1 || act  > n_actions) Rcpp::stop("dist_actions_data$internal_action out of range.");

    const int col_x = op->_x_offset + (row1 - 1);
    if (col_x < 0 || col_x >= (int)op->_obj.size()) {
      Rcpp::stop("Computed x col out of bounds: check x_offset/internal_row/object size.");
    }

    xcol[key2(ipu, act)] = col_x;
  }

  // create b vars
  const int y0 = (int)op->ncol_used();
  op->_y_action_offset = y0;
  op->_n_y_action      = mA;

  op->_obj.reserve(op->_obj.size() + (std::size_t)mA);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)mA);
  op->_lb.reserve(op->_lb.size() + (std::size_t)mA);
  op->_ub.reserve(op->_ub.size() + (std::size_t)mA);

  for (int k = 0; k < mA; ++k) {
    op->_obj.push_back(0.0);       // prepare never touches objective
    op->_vtype.push_back("C");     // AND auxiliary: continuous [0,1] is enough
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=prepare"
    ";component=fragmentation_actions_by_action"
    ";b_semantics=and"
    ";k_edges=" + std::to_string(k_edges) +
      ";n_actions=" + std::to_string(n_actions) +
      ";n_actions_used=" + std::to_string(nA) +
      ";mA=" + std::to_string(mA) +
      ";n_self_rows=" + std::to_string(n_self_rows) +
      ";y_offset=" + std::to_string(y0) +
      ";vtype=C"
      ";objective_touched=FALSE";

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::b_action",
    (std::size_t)y0,
    (std::size_t)(y0 + mA),
    full_tag
  );

  // add AND constraints (3 per (edge, action)) when both x exist
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  int n_constraints_added = 0;
  int pairs_missing_x = 0;

  // deterministic index: b_index = e*nA + kk
  for (int e = 0; e < k_edges; ++e) {
    const int i = e_i[e];
    const int j = e_j[e];

    for (int kk = 0; kk < nA; ++kk) {
      const int act = A[(std::size_t)kk];

      const int bcol = y0 + (e * nA + kk);

      auto iti = xcol.find(key2(i, act));
      auto itj = xcol.find(key2(j, act));
      if (iti == xcol.end() || itj == xcol.end()) {
        ++pairs_missing_x;
        continue;
      }

      const int xi = iti->second;
      const int xj = itj->second;

      // b <= xi
      op->addRow({ bcol, xi }, { 1.0, -1.0 }, "<=", 0.0, "b_le_xi");
      ++n_constraints_added;

      // b <= xj
      op->addRow({ bcol, xj }, { 1.0, -1.0 }, "<=", 0.0, "b_le_xj");
      ++n_constraints_added;

      // b >= xi + xj - 1
      op->addRow({ bcol, xi, xj }, { 1.0, -1.0, -1.0 }, ">=", -1.0, "b_ge_xi_plus_xj_minus1");
      ++n_constraints_added;
    }
  }

  const std::size_t cons_block_id = op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("already_prepared") = false,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("constraints_block_id") = (double)cons_block_id,
    Rcpp::Named("y_offset") = op->_y_action_offset,
    Rcpp::Named("n_y") = op->_n_y_action,
    Rcpp::Named("k_edges") = k_edges,
    Rcpp::Named("n_actions_used") = nA,
    Rcpp::Named("mA") = mA,
    Rcpp::Named("pairs_missing_x") = pairs_missing_x,
    Rcpp::Named("n_constraints_added") = n_constraints_added,
    Rcpp::Named("tag") = full_tag
  );
}
