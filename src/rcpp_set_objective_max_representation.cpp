#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <algorithm>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_max_representation(
    SEXP x,
    Rcpp::DataFrame dist_features_data,
    std::string amount_col = "amount",
    std::string block_name = "objective_max_representation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // must have variables
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // must have z
  if (op->_n_z <= 0) {
    Rcpp::stop("maximize_representation requires z variables. Build with add_z=TRUE and non-empty dist_features_data.");
  }

  if (!dist_features_data.containsElementNamed(amount_col.c_str())) {
    Rcpp::stop("Column '" + amount_col + "' not found in dist_features_data.");
  }

  const int n_df = dist_features_data.nrows();
  if (n_df != op->_n_z) {
    Rcpp::stop("dist_features_data nrows (" + std::to_string(n_df) +
      ") must match number of z vars (" + std::to_string(op->_n_z) + ").");
  }

  // z block bounds check
  const int z0 = op->_z_offset;
  const int z1 = op->_z_offset + op->_n_z; // exclusive
  if (z0 < 0 || z1 > (int)op->_obj.size()) {
    Rcpp::stop("z block out of bounds: check op->_z_offset/op->_n_z and that z variables exist.");
  }

  // modelsense
  op->_modelsense = "max";

  // Reset all objective coefficients to 0
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // Set z coefficients
  Rcpp::NumericVector amount = dist_features_data[amount_col.c_str()];

  double sum_added = 0.0;
  int used = 0;

  for (int t = 0; t < op->_n_z; ++t) {
    const double a = (double)amount[t];
    if (!std::isfinite(a) || a < 0.0) {
      Rcpp::stop("Non-finite or negative amount at dist_features_data row " + std::to_string(t + 1) + ".");
    }
    op->_obj[z0 + t] = a;
    sum_added += a;
    ++used;
  }

  // ---- registry: record objective touched z block
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=max"
    ";amount_col=" + amount_col +
      ";n_z=" + std::to_string(op->_n_z) +
      ";n_used=" + std::to_string(used) +
      ";sum_added=" + std::to_string(sum_added) +
      ";reset_objective=TRUE";

  const std::size_t block_id = op->register_objective_block(
    block_name + "::z",
    (std::size_t)z0,
    (std::size_t)z1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("block_id") = (double)block_id,
    Rcpp::Named("z_range") = Rcpp::NumericVector::create((double)z0 + 1.0, (double)z1),
    Rcpp::Named("n_used") = used,
    Rcpp::Named("sum_added") = sum_added,
    Rcpp::Named("amount_col_used") = amount_col
  );
}
