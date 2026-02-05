// rcpp_set_objective_min_fragmentation.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <algorithm>
#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <cstdint>

// helper: undirected key (i<j), i/j 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame relation_data,
    double weight_multiplier = 1.0,   // penalty
    std::string block_name = "objective_min_fragmentation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  if (!R_finite(weight_multiplier)) {
    Rcpp::stop("weight_multiplier (penalty) must be finite.");
  }

  if (op->ncol_used() == 0) Rcpp::stop("Model has zero variables. Build base variables first.");

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds.");
  }

  if (op->_n_y_pu > 0) {
    Rcpp::stop("Boundary auxiliaries already exist (_n_y_pu > 0).");
  }

  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector  wgt = relation_data["weight"];

  // reset objective on existing vars
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // diagonal = self contribution (already includes edge_factor baked in R)
  std::vector<double> self_w(n_pu + 1, 0.0);
  std::unordered_map<std::uint64_t, double> edge_w;
  edge_w.reserve(static_cast<std::size_t>(m) * 2);

  for (int r = 0; r < m; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) Rcpp::stop("pu index out of range.");

    const double we = (double)wgt[r];
    if (!R_finite(we) || we < 0.0) Rcpp::stop("weight must be finite and >=0.");

    if (i1 == j1) {
      self_w[i1] += we; // already scaled in R: EF * exposed
    } else {
      int a = i1, b = j1;
      if (a > b) std::swap(a, b);
      const std::uint64_t k =
        (static_cast<std::uint64_t>(static_cast<std::uint32_t>(a)) << 32) |
        static_cast<std::uint32_t>(b);

      auto it = edge_w.find(k);
      if (it == edge_w.end()) edge_w.emplace(k, we);
      else it->second = std::max(it->second, we);
    }
  }

  // sort edges by key so y ordering is deterministic and reusable in MO-add
  std::vector<std::pair<std::uint64_t, double>> edges;
  edges.reserve(edge_w.size());
  for (const auto& kv : edge_w) edges.push_back(kv);
  std::sort(edges.begin(), edges.end(),
            [](const auto& a, const auto& b){ return a.first < b.first; });

  const int k_edges = static_cast<int>(edges.size());

  // incident shared per PU
  std::vector<double> incident_w(n_pu + 1, 0.0);
  std::vector<int> e_i; e_i.reserve(k_edges);
  std::vector<int> e_j; e_j.reserve(k_edges);
  std::vector<double> e_w; e_w.reserve(k_edges);

  for (const auto& kv : edges) {
    const std::uint64_t key = kv.first;
    const double we = kv.second;
    const int a = static_cast<int>(key >> 32);
    const int b = static_cast<int>(key & 0xFFFFFFFFu);
    e_i.push_back(a); e_j.push_back(b); e_w.push_back(we);
    incident_w[a] += we;
    incident_w[b] += we;
  }

  // linear PU part: penalty * (incident + self_diag)
  double sum_linear_added = 0.0;
  for (int i = 1; i <= n_pu; ++i) {
    const double coef = weight_multiplier * (incident_w[i] + self_w[i]);
    if (coef != 0.0) {
      op->_obj[op->_w_offset + (i - 1)] += coef;
      sum_linear_added += coef;
    }
  }

  // b variables for shared boundary: coef = -2 * penalty * shared_ij
  const int b0 = (int)op->ncol_used();
  op->_y_pu_offset = b0;
  op->_n_y_pu      = k_edges;

  op->_obj.reserve(op->_obj.size() + (std::size_t)k_edges);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)k_edges);
  op->_lb.reserve(op->_lb.size() + (std::size_t)k_edges);
  op->_ub.reserve(op->_ub.size() + (std::size_t)k_edges);

  for (int e = 0; e < k_edges; ++e) {
    const double coef = (-2.0) * weight_multiplier * e_w[e];
    op->_obj.push_back(coef);
    op->_vtype.push_back("C");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // AND constraints (b <= w_i, b <= w_j, + optional third if needed)
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", tag);

  int n_constraints_added = 0;
  for (int e = 0; e < k_edges; ++e) {
    const int wi = op->_w_offset + (e_i[e] - 1);
    const int wj = op->_w_offset + (e_j[e] - 1);
    const int bcol = b0 + e;

    op->addRow({ bcol, wi }, { 1.0, -1.0 }, "<=", 0.0, "b_le_wi"); ++n_constraints_added;
    op->addRow({ bcol, wj }, { 1.0, -1.0 }, "<=", 0.0, "b_le_wj"); ++n_constraints_added;

    const double bcoef = (-2.0) * weight_multiplier * e_w[e];
    //if (bcoef > 0.0 && op->_modelsense == "min") {
      op->addRow({ bcol, wi, wj }, { 1.0, -1.0, -1.0 }, ">=", -1.0, "b_ge_wi_plus_wj_minus1");

      ++n_constraints_added;
    //}
  }

  op->endConstraintBlock(cblock, true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("n_b") = op->_n_y_pu,
    Rcpp::Named("sum_linear_added") = sum_linear_added,
    Rcpp::Named("n_constraints_added") = n_constraints_added
  );
}
