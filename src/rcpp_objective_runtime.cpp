#include "Package.h"
#include "OptimizationProblem.h"

#include <algorithm> // std::fill
#include <cmath>     // std::isfinite

// [[Rcpp::export]]
Rcpp::List rcpp_reset_objective(SEXP x, std::string modelsense = "", bool clear_blocks = true) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!modelsense.empty()) {
    if (modelsense != "min" && modelsense != "max") Rcpp::stop("modelsense must be 'min' or 'max'.");
    op->_modelsense = modelsense;
  }

  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  if (clear_blocks) {
    // si tienes algo tipo op->clear_objective_blocks();
    // o deja esto en no-op si aún no lo implementas.
  }

  return Rcpp::List::create(Rcpp::Named("ok") = true);
}


// [[Rcpp::export]]
void rcpp_add_to_objective(SEXP x,
                           Rcpp::IntegerVector ind,   // 1-based indices from R
                           Rcpp::NumericVector val) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  if (ind.size() != val.size()) {
    Rcpp::stop("ind and val must have the same length.");
  }

  const std::size_t n_used = op->ncol_used();

  for (R_xlen_t k = 0; k < ind.size(); ++k) {
    const int j1 = ind[k];
    const double v = val[k];

    if (j1 == NA_INTEGER) Rcpp::stop("ind contains NA.");

    if (!Rcpp::NumericVector::is_na(v) && !std::isfinite(v)) {
      Rcpp::stop("val contains non-finite values.");
    }

    if (j1 < 1 || (std::size_t)j1 > n_used) {
      Rcpp::stop("ind out of range (1..ncol_used).");
    }

    op->_obj[(std::size_t)j1 - 1] += v; // 1-based -> 0-based
  }
}

// [[Rcpp::export]]
void rcpp_add_to_objective_scalar(SEXP x, int ind1, double val) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const std::size_t n_used = op->ncol_used();

  if (ind1 < 1 || (std::size_t)ind1 > n_used) {
    Rcpp::stop("ind out of range (1..ncol_used).");
  }
  if (!std::isfinite(val)) Rcpp::stop("val must be finite.");

  op->_obj[(std::size_t)ind1 - 1] += val;
}
