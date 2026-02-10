// rcpp_add_objective_max_benefit.cpp
#include "Package.h"
#include "OptimizationProblem.h"

#include <string>
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_max_benefit(
    SEXP x,
    Rcpp::NumericVector coef_x,                 // prepared vector (length op->_n_x)
    double weight = 1.0,
    double weight_multiplier = 1.0,
    std::string block_name = "objective_add_max_benefit",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0 || op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  if (!std::isfinite(weight) || weight < 0.0) {
    Rcpp::stop("weight must be finite and >= 0.");
  }
  if (!std::isfinite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }
  if (weight == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "weight==0"
    );
  }

  if (op->_n_x <= 0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "op->_n_x == 0 (no x variables)"
    );
  }

  const int n_x = op->_n_x;
  if (coef_x.size() != n_x) {
    Rcpp::stop(
      "coef_x length mismatch: coef_x.size()=" + std::to_string((int)coef_x.size()) +
        " but op->_n_x=" + std::to_string(n_x) + "."
    );
  }

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x; // exclusive

  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds: check op->_x_offset/op->_n_x.");
  }

  const double scale = weight * weight_multiplier;

  int nnz_added = 0;
  double sum_added = 0.0;

  for (int i = 0; i < n_x; ++i) {
    const double b = (double)coef_x[i];
    if (!std::isfinite(b) || b == 0.0) continue;

    const double coef = scale * b;
    op->_obj[(std::size_t)(x0 + i)] += coef;

    sum_added += coef;
    ++nnz_added;
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_add"
    ";component=max_benefit"
    ";weight=" + std::to_string(weight) +
      ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_x=" + std::to_string(n_x) +
      ";nnz_added=" + std::to_string(nnz_added) +
      ";sum_added=" + std::to_string(sum_added) +
      ";additive=TRUE";

  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)x0,
    (std::size_t)x1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("x_offset") = x0,
    Rcpp::Named("n_x") = n_x,
    Rcpp::Named("nnz_added") = nnz_added,
    Rcpp::Named("sum_added") = sum_added
  );
}
