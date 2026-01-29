#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>

static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
bool rcpp_set_objective_max_profit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_profit_data,
    std::string profit_col = "profit") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "max";

  // Defensive: reset objective
  for (std::size_t j = 0; j < op->_obj.size(); ++j) op->_obj[j] = 0.0;

  // checks: dist_actions_data
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  // checks: dist_profit_data
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_profit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_profit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_profit_data.containsElementNamed(profit_col.c_str())) {
    Rcpp::stop("dist_profit_data must contain profit column '" + profit_col + "'.");
  }

  // map (ipu, iact) -> x_row0  (0-based in x block)
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow0;
  pa_to_xrow0.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    const int row0 = da_irow[r] - 1; // internal_row is 1-based
    if (ipu <= 0 || ia <= 0 || row0 < 0) {
      Rcpp::stop("dist_actions_data internal_* must be positive 1-based.");
    }
    const long long k = key2(ipu, ia);
    // If duplicates exist, keep first (should not happen if dist_actions_model is clean)
    if (pa_to_xrow0.find(k) == pa_to_xrow0.end()) pa_to_xrow0[k] = row0;
  }

  // apply profit on x variables
  Rcpp::IntegerVector dp_ipu  = dist_profit_data["internal_pu"];
  Rcpp::IntegerVector dp_iact = dist_profit_data["internal_action"];
  Rcpp::NumericVector dp_prof = dist_profit_data[profit_col.c_str()];
  const int n_dp = dist_profit_data.nrows();

  for (int r = 0; r < n_dp; ++r) {
    const double p = (double)dp_prof[r];
    if (!R_finite(p) || p == 0.0) continue;

    const long long k = key2(dp_ipu[r], dp_iact[r]);
    auto it = pa_to_xrow0.find(k);
    if (it == pa_to_xrow0.end()) {
      // profit row not present in model-ready dist_actions_data -> ignore
      continue;
    }

    const int x_row0 = it->second;
    op->_obj[op->_x_offset + x_row0] += p;
  }

  return true;
}
