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
bool rcpp_set_objective_max_benefit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_benefit_data,
    std::string benefit_col = "benefit") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // modelsense
  op->_modelsense = "max";

  // checks: dist_actions
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_benefit
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_benefit_data.containsElementNamed(benefit_col.c_str())) {
    Rcpp::stop("dist_benefit_data must contain benefit column '" + benefit_col + "'.");
  }

  // reset objective coefficients to 0 (safe if objective was set before)
  for (std::size_t j = 0; j < op->_obj.size(); ++j) op->_obj[j] = 0.0;

  // build map (ipu, iact) -> internal_row-1
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow;
  pa_to_xrow.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    const int row0 = da_irow[r] - 1; // internal_row is 1-based
    if (ipu <= 0 || ia <= 0 || row0 < 0) {
      Rcpp::stop("dist_actions_data internal_* must be positive 1-based.");
    }
    const long long k = key2(ipu, ia);
    // keep the first occurrence; duplicates should not exist, but be defensive
    if (pa_to_xrow.find(k) == pa_to_xrow.end()) pa_to_xrow[k] = row0;
  }

  // iterate benefits and accumulate onto x objective coefficient
  Rcpp::IntegerVector db_ipu  = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact = dist_benefit_data["internal_action"];
  Rcpp::NumericVector db_ben  = dist_benefit_data[benefit_col.c_str()];
  const int n_db = dist_benefit_data.nrows();

  int used = 0;

  for (int r = 0; r < n_db; ++r) {
    const double b = db_ben[r];
    if (!R_finite(b) || b == 0.0) continue;

    const long long k = key2(db_ipu[r], db_iact[r]);
    auto it = pa_to_xrow.find(k);
    if (it == pa_to_xrow.end()) {
      // benefit row refers to infeasible/filtered-out action; ignore silently or stop.
      // I recommend ignoring, because you already do an inner_join filter in R.
      continue;
    }

    const int row0 = it->second;
    op->_obj[op->_x_offset + row0] += b;
    ++used;
  }

  return true;
}
