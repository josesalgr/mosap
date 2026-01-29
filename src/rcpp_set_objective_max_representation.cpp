#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_set_objective_max_representation(SEXP x,
                                           Rcpp::DataFrame dist_features_data,
                                           std::string amount_col = "amount") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->_n_z <= 0) {
    Rcpp::stop("maximizeRepresentation requires z variables. Build model with add_z = TRUE and non-empty dist_features.");
  }

  if (!dist_features_data.containsElementNamed(amount_col.c_str())) {
    Rcpp::stop("Column '%s' not found in dist_features_data.", amount_col);
  }

  Rcpp::NumericVector amount = dist_features_data[amount_col];

  if (amount.size() != op->_n_z) {
    Rcpp::stop("dist_features_data nrows (%i) must match number of z vars (%i).",
               amount.size(), op->_n_z);
  }

  // Reset objective coefficients to 0
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // Set z coefficients
  for (int t = 0; t < op->_n_z; ++t) {
    double a = amount[t];
    if (!R_finite(a) || a < 0.0) {
      Rcpp::stop("Non-finite or negative amount detected in dist_features_data[%i].", t + 1);
    }
    op->_obj[op->_z_offset + t] = a;
  }

  // If your OptimizationProblem stores sense, set it here (depends on your class)
  op->_modelsense = "max";

  return true;
}
