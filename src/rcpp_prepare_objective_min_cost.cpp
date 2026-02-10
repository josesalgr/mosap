#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <algorithm>
#include <cmath>
#include <string>

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_objective_min_cost(
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

  // Defensive checks for offsets
  if (op->_w_offset < 0 || op->_x_offset < 0) {
    Rcpp::stop("Offsets not initialized (w/x).");
  }

  // ---- validate required columns
  const int n_pu = pu_data.nrows();

  if (include_pu_cost) {
    if (!pu_data.containsElementNamed("cost")) {
      Rcpp::stop("pu_data must contain column 'cost' when include_pu_cost=TRUE.");
    }
  }

  // -----------------------------
  // Determine w range (w = one per PU)
  // -----------------------------
  const std::size_t w0 = (std::size_t)op->_w_offset;
  const std::size_t w1 = w0 + (std::size_t)n_pu; // exclusive

  if (w1 > (std::size_t)op->ncol_used()) {
    Rcpp::stop("w block out of bounds: w_offset + n_pu exceeds ncol_used.");
  }

  // -----------------------------
  // Determine x range from internal_row (x = one per dist_actions row)
  // (We use max(internal_row) so you can have sparse/filtered dist_actions)
  // -----------------------------
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

  const std::size_t x0 = (std::size_t)op->_x_offset;
  std::size_t x1 = x0; // exclusive

  if (max_row1 > 0) {
    x1 = x0 + (std::size_t)max_row1; // internal_row is 1..max_row1
    if (x1 > (std::size_t)op->ncol_used()) {
      Rcpp::stop("x block out of bounds: x_offset + max(internal_row) exceeds ncol_used.");
    }
  }

  if (include_action_cost) {
    if (!dist_actions_data.containsElementNamed("cost")) {
      Rcpp::stop("dist_actions_data must contain column 'cost' when include_action_cost=TRUE.");
    }
    if (!dist_actions_data.containsElementNamed("internal_row")) {
      Rcpp::stop("dist_actions_data must contain column 'internal_row' when include_action_cost=TRUE.");
    }
  }

  // -----------------------------
  // Build a stable tag (NO side effects; just metadata)
  // -----------------------------
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=min"
    ";include_pu_cost=" + std::string(include_pu_cost ? "true" : "false") +
      ";include_action_cost=" + std::string(include_action_cost ? "true" : "false") +
        ";n_pu=" + std::to_string(n_pu) +
        ";max_internal_row=" + std::to_string(max_row1);

  // Return a "prep object" for add_* (no mutation)
  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("modelsense") = std::string("min"),
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("include_pu_cost") = include_pu_cost,
    Rcpp::Named("include_action_cost") = include_action_cost,
    Rcpp::Named("n_pu") = n_pu,
    Rcpp::Named("max_internal_row") = max_row1,
    // 1-based ranges for R friendliness (same style you used)
    Rcpp::Named("w_range") = Rcpp::NumericVector::create((double)w0 + 1.0, (double)w1),
    Rcpp::Named("x_range") = (max_row1 > 0)
    ? Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1)
      : Rcpp::NumericVector::create(NA_REAL, NA_REAL),
        // 0-based offsets to be used directly in C++
        Rcpp::Named("w0") = (double)w0,
        Rcpp::Named("w1") = (double)w1,
        Rcpp::Named("x0") = (double)x0,
        Rcpp::Named("x1") = (max_row1 > 0) ? (double)x1 : NA_REAL
  );
}
