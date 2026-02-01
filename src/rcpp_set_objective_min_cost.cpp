#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <algorithm> // std::fill
#include <cmath>     // std::isfinite
#include <string>

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_min_cost(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame dist_actions_data,
    bool include_pu_cost = true,
    bool include_action_cost = true,
    std::string block_name = "objective_min_cost",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  // Sense
  op->_modelsense = "min";

  // Reset objective fully
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // Defensive checks for offsets
  if (op->_w_offset < 0 || op->_x_offset < 0) {
    Rcpp::stop("Offsets not initialized (w/x).");
  }

  // -----------------------------
  // Determine w range
  // -----------------------------
  const int n_pu = pu_data.nrows();
  const std::size_t w0 = (std::size_t)op->_w_offset;
  const std::size_t w1 = w0 + (std::size_t)n_pu; // exclusive

  if (include_pu_cost) {
    if (!pu_data.containsElementNamed("cost")) {
      Rcpp::stop("pu_data must contain column 'cost' when include_pu_cost=TRUE.");
    }
    if (w1 > op->_obj.size()) {
      Rcpp::stop("w block out of bounds in objective: w_offset + n_pu exceeds obj size.");
    }

    Rcpp::NumericVector pu_cost = pu_data["cost"];
    for (int i = 0; i < n_pu; ++i) {
      const double c = (double)pu_cost[i];
      if (Rcpp::NumericVector::is_na(c) || !std::isfinite(c)) continue;
      op->_obj[w0 + (std::size_t)i] = c;
    }
  }

  // -----------------------------
  // Determine x range from internal_row
  // -----------------------------
  std::size_t x0 = (std::size_t)op->_x_offset;
  std::size_t x1 = x0; // exclusive, will be updated if we have actions

  int max_row1 = 0;
  if (dist_actions_data.nrows() > 0) {
    if (!dist_actions_data.containsElementNamed("internal_row")) {
      Rcpp::stop("dist_actions_data must contain column 'internal_row'.");
    }
    Rcpp::IntegerVector irow = dist_actions_data["internal_row"];
    for (int k = 0; k < irow.size(); ++k) {
      const int r1 = irow[k];
      if (r1 == NA_INTEGER) continue;
      if (r1 <= 0) Rcpp::stop("internal_row must be positive 1-based.");
      if (r1 > max_row1) max_row1 = r1;
    }
  }

  if (max_row1 > 0) {
    x1 = x0 + (std::size_t)max_row1; // internal_row is 1..max_row1
    if (x1 > op->_obj.size()) {
      Rcpp::stop("x block out of bounds in objective: x_offset + max(internal_row) exceeds obj size.");
    }
  }

  // -----------------------------
  // Action costs on x
  // -----------------------------
  int n_x_cost_used = 0;
  double sum_x_cost = 0.0;

  if (include_action_cost) {
    if (!dist_actions_data.containsElementNamed("cost")) {
      Rcpp::stop("dist_actions_data must contain column 'cost' when include_action_cost=TRUE.");
    }
    if (!dist_actions_data.containsElementNamed("internal_row")) {
      Rcpp::stop("dist_actions_data must contain column 'internal_row'.");
    }

    Rcpp::NumericVector a_cost = dist_actions_data["cost"];
    Rcpp::IntegerVector irow   = dist_actions_data["internal_row"];
    const int n_da = dist_actions_data.nrows();

    for (int k = 0; k < n_da; ++k) {
      const int r1 = irow[k];
      if (r1 == NA_INTEGER) continue;
      if (r1 <= 0) Rcpp::stop("internal_row must be positive 1-based.");
      const int row0 = r1 - 1;

      const std::size_t col_x = (std::size_t)op->_x_offset + (std::size_t)row0;
      if (col_x >= op->_obj.size()) {
        Rcpp::stop("x index out of bounds: x_offset + (internal_row-1) exceeds obj size.");
      }

      const double c = (double)a_cost[k];
      if (Rcpp::NumericVector::is_na(c) || !std::isfinite(c)) continue;

      op->_obj[col_x] = c;
      ++n_x_cost_used;
      sum_x_cost += c;
    }
  }

  // -----------------------------
  // Registry (objective)
  // -----------------------------
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=min"
    ";include_pu_cost=" + std::string(include_pu_cost ? "true" : "false") +
      ";include_action_cost=" + std::string(include_action_cost ? "true" : "false") +
        ";reset_objective=TRUE" +
        ";n_pu=" + std::to_string(n_pu) +
        ";max_internal_row=" + std::to_string(max_row1) +
        ";n_x_cost_used=" + std::to_string(n_x_cost_used) +
        ";sum_x_cost=" + std::to_string(sum_x_cost);

  // Tú mismo comentaste que aquí “define el objetivo completo”.
  // Registrar todo el vector objetivo es totalmente razonable:
  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(),
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("w_range") = Rcpp::NumericVector::create((double)w0 + 1.0, (double)w1),
    Rcpp::Named("x_range") = (max_row1 > 0)
    ? Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1)
      : Rcpp::NumericVector::create(NA_REAL, NA_REAL),
        Rcpp::Named("n_x_cost_used") = n_x_cost_used,
        Rcpp::Named("sum_x_cost") = sum_x_cost
  );
}
