// rcpp_set_objective_min_fragmentation.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <algorithm>
#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>

// helper to build a unique key for an undirected edge (i<j), with i,j 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  // assume i < j
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame pu_data,          // not used (kept for compat)
    Rcpp::DataFrame relation_data,    // internal_pu1, internal_pu2, weight
    double weight_multiplier = 1.0,
    std::string block_name = "objective_min_fragmentation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds: check op->_w_offset/op->_n_pu vs number of variables.");
  }

  // avoid double creation
  if (op->_n_y_pu > 0) {
    Rcpp::stop("PU-fragmentation auxiliaries already exist (_n_y_pu > 0).");
  }

  // relation_data checks
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"]; // 1..n_pu
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"]; // 1..n_pu
  Rcpp::NumericVector wgt = relation_data["weight"];

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // Reset objective coefficients on existing variables (prioriactions-style block objective)
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // ------------------------------------------------------------
  // 0) Preprocess relations:
  //    - self edges (i==j): add weight directly to x_i objective (boundary with outside)
  //    - non-self edges: collapse into undirected unique edges using max weight
  // ------------------------------------------------------------
  std::vector<double> self_w(n_pu + 1, 0.0); // 1-based
  std::unordered_map<std::uint64_t, double> edge_w; // key(i<j) -> weight

  edge_w.reserve(static_cast<std::size_t>(m) * 2);

  for (int r = 0; r < m; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];

    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    const double we = (double)wgt[r];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    if (i1 == j1) {
      // self edge: boundary with outside -> contributes when PU selected
      self_w[i1] += we;
    } else {
      int a = i1, b = j1;
      if (a > b) std::swap(a, b); // undirected
      const std::uint64_t k = edge_key_undirected(a, b);
      auto it = edge_w.find(k);
      if (it == edge_w.end()) edge_w.emplace(k, we);
      else it->second = std::max(it->second, we); // avoid doubling if both (i,j) and (j,i) exist
    }
  }

  // apply self weights directly to w variables in the objective
  // (w_i is your PU decision variable at op->_w_offset + (i-1))
  double sum_self = 0.0;
  for (int i = 1; i <= n_pu; ++i) {
    const double coef = weight_multiplier * self_w[i];
    if (coef != 0.0) {
      op->_obj[op->_w_offset + (i - 1)] += coef;
      sum_self += coef;
    }
  }

  // build arrays of unique edges
  const int k_edges = static_cast<int>(edge_w.size());
  if (k_edges == 0 && sum_self == 0.0) {
    Rcpp::stop("No usable relations found (no edges and no self-boundaries).");
  }

  std::vector<int> e_i;
  std::vector<int> e_j;
  std::vector<double> e_w;
  e_i.reserve(k_edges);
  e_j.reserve(k_edges);
  e_w.reserve(k_edges);

  // decode keys
  for (const auto& kv : edge_w) {
    const std::uint64_t key = kv.first;
    const double we = kv.second;
    const int a = static_cast<int>(key >> 32);
    const int b = static_cast<int>(key & 0xFFFFFFFFu);
    e_i.push_back(a);
    e_j.push_back(b);
    e_w.push_back(we);
  }

  // ------------------------------------------------------------
  // 1) Add y_e variables (one per UNIQUE undirected edge)
  //    Objective: min sum (weight_multiplier * boundary_ij) * y_ij
  // ------------------------------------------------------------
  const int y0 = (int)op->ncol_used();
  op->_y_pu_offset = y0;
  op->_n_y_pu      = k_edges;

  // reserve storage for new vars
  op->_obj.reserve(op->_obj.size() + (std::size_t)k_edges);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)k_edges);
  op->_lb.reserve(op->_lb.size() + (std::size_t)k_edges);
  op->_ub.reserve(op->_ub.size() + (std::size_t)k_edges);

  double sum_edge_weights = 0.0;

  for (int e = 0; e < k_edges; ++e) {
    const double coef = weight_multiplier * e_w[e];
    op->_obj.push_back(coef);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
    sum_edge_weights += coef;
  }

  // ------------------------------------------------------------
  // 2) Add linearization constraints (prioritizr-style: only 2)
  //    y_ij >= w_i - w_j
  //    y_ij >= w_j - w_i
  // ------------------------------------------------------------
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective"
    ";modelsense=min"
    ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_edges_unique=" + std::to_string(k_edges) +
      ";sum_edge_weights=" + std::to_string(sum_edge_weights) +
      ";sum_self_weights=" + std::to_string(sum_self) +
      ";objective_reset=TRUE";

  const std::size_t obj_block_id = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(),     // full objective vector (existing + appended)
    full_tag
  );

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::y_pu",
    (std::size_t)y0,
    (std::size_t)(y0 + k_edges),
    full_tag + ";vtype=B"
  );

  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  int n_constraints_added = 0;

  for (int e = 0; e < k_edges; ++e) {
    const int i1 = e_i[e];
    const int j1 = e_j[e];

    // 0-based indices within PU block
    const int i0 = i1 - 1;
    const int j0 = j1 - 1;

    const int y_col = y0 + e;
    const int wi = op->_w_offset + i0;
    const int wj = op->_w_offset + j0;

    // (1) y - w_i + w_j >= 0  => y >= w_i - w_j
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, -1.0, 1.0 },
        ">=", 0.0,
        "y_ge_wi_minus_wj"
    );
    ++n_constraints_added;

    // (2) y + w_i - w_j >= 0  => y >= w_j - w_i
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, 1.0, -1.0 },
        ">=", 0.0,
        "y_ge_wj_minus_wi"
    );
    ++n_constraints_added;
  }

  op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("objective_block_id") = (double)obj_block_id,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("y_offset") = op->_y_pu_offset,
    Rcpp::Named("n_y") = op->_n_y_pu,
    Rcpp::Named("n_constraints_added") = n_constraints_added,
    Rcpp::Named("sum_edge_weights") = sum_edge_weights,
    Rcpp::Named("sum_self_weights") = sum_self
  );
}
