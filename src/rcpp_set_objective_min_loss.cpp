#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>

// hash key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
bool rcpp_set_objective_min_loss(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_effects_data,
    std::string loss_col = "loss") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // modelsense
  op->_modelsense = "min";

  // checks: dist_actions
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_effects
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_effects_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_effects_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_effects_data.containsElementNamed(loss_col.c_str())) {
    Rcpp::stop("dist_effects_data must contain loss column '" + loss_col + "'.");
  }

  // reset objective coefficients to 0
  for (std::size_t j = 0; j < op->_obj.size(); ++j) op->_obj[j] = 0.0;

  // build map (ipu, iact) -> internal_row-1
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow;
  pa_to_xrow.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu  = da_ipu[r];
    const int ia   = da_iact[r];
    const int row0 = da_irow[r] - 1; // internal_row is 1-based
    if (ipu <= 0 || ia <= 0 || row0 < 0) {
      Rcpp::stop("dist_actions_data internal_* must be positive 1-based.");
    }
    const long long k = key2(ipu, ia);
    if (pa_to_xrow.find(k) == pa_to_xrow.end()) pa_to_xrow[k] = row0;
  }

  // iterate losses and accumulate onto x objective coefficient
  Rcpp::IntegerVector de_ipu  = dist_effects_data["internal_pu"];
  Rcpp::IntegerVector de_iact = dist_effects_data["internal_action"];
  Rcpp::NumericVector de_loss = dist_effects_data[loss_col.c_str()];
  const int n_de = dist_effects_data.nrows();

  for (int r = 0; r < n_de; ++r) {
    const double L = de_loss[r];
    if (!R_finite(L) || L == 0.0) continue;

    const long long k = key2(de_ipu[r], de_iact[r]);
    auto it = pa_to_xrow.find(k);
    if (it == pa_to_xrow.end()) {
      // row refers to infeasible/filtered-out action; ignore (R already inner_join-filtered)
      continue;
    }

    const int row0 = it->second;
    // Minimization: add positive coefficients (losses should be >=0 by construction)
    op->_obj[op->_x_offset + row0] += L;
  }

  return true;
}
