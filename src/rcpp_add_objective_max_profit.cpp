#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>
#include <cmath>

// key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_max_profit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_profit_data,
    std::string profit_col = "profit",
    double weight = 1.0,
    std::string block_name = "objective_max_profit",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // NOTE: "add" should NOT force modelsense. Caller decides (e.g., rcpp_reset_objective(op,"min")).
  // If you still want to expose metadata, we only report current op->_modelsense.

  if (!R_finite(weight)) Rcpp::stop("weight must be finite.");
  if (weight == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("modelsense") = op->_modelsense,
      Rcpp::Named("n_used_rows") = 0,
      Rcpp::Named("sum_added") = 0.0,
      Rcpp::Named("note") = "weight == 0: nothing added."
    );
  }

  // must have variables
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // ---- checks: dist_actions_data
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // ---- checks: dist_profit_data
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_profit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_profit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_profit_data.containsElementNamed(profit_col.c_str())) {
    Rcpp::stop("dist_profit_data must contain profit column '" + profit_col + "'.");
  }

  // Determine x range from MODEL (not nrows)
  if (op->_n_x <= 0) {
    Rcpp::stop("Cannot add profit objective: model has op->_n_x <= 0 (no x variables).");
  }

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x; // exclusive

  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds. Check op->_x_offset/op->_n_x and that x variables exist.");
  }

  // map (ipu, iact) -> x_row0 (0-based within x block), using internal_row
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow0;
  pa_to_xrow0.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu  = da_ipu[r];
    const int ia   = da_iact[r];
    const int row1 = da_irow[r]; // 1-based in R

    if (ipu == NA_INTEGER || ia == NA_INTEGER || row1 == NA_INTEGER) continue;
    if (ipu <= 0 || ia <= 0) {
      Rcpp::stop("dist_actions_data internal_pu/internal_action must be positive 1-based.");
    }
    if (row1 <= 0 || row1 > op->_n_x) {
      Rcpp::stop("dist_actions_data internal_row out of range: must be in [1, op->_n_x].");
    }

    const int row0 = row1 - 1;
    const long long k = key2(ipu, ia);

    if (pa_to_xrow0.find(k) == pa_to_xrow0.end()) pa_to_xrow0[k] = row0;
  }

  // apply profit on x variables (ADDITIVE)
  Rcpp::IntegerVector dp_ipu  = dist_profit_data["internal_pu"];
  Rcpp::IntegerVector dp_iact = dist_profit_data["internal_action"];
  Rcpp::NumericVector dp_prof = dist_profit_data[profit_col.c_str()];
  const int n_dp = dist_profit_data.nrows();

  int used = 0;
  int dropped_missing_action = 0;
  int dropped_nonfinite_or_zero = 0;
  double sum_added = 0.0;

  for (int r = 0; r < n_dp; ++r) {
    const double p = (double)dp_prof[r];

    if (!std::isfinite(p) || p == 0.0) {
      ++dropped_nonfinite_or_zero;
      continue;
    }

    const int ipu = dp_ipu[r];
    const int ia  = dp_iact[r];
    if (ipu == NA_INTEGER || ia == NA_INTEGER) continue;

    const long long k = key2(ipu, ia);
    auto it = pa_to_xrow0.find(k);
    if (it == pa_to_xrow0.end()) {
      ++dropped_missing_action;
      continue;
    }

    const int row0  = it->second;
    const int col_x = x0 + row0;

    if (col_x < x0 || col_x >= x1) {
      Rcpp::stop("Computed x column out of bounds. Check internal_row mapping consistency.");
    }

    const double add = weight * p;
    op->_obj[col_x] += add;

    sum_added += add;
    ++used;
  }

  // registry: OBJECTIVE TERM block on x range (note: additive term)
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_term"
    ";term=profit"
    ";profit_col=" + profit_col +
      ";weight=" + std::to_string(weight) +
      ";n_used_rows=" + std::to_string(used) +
      ";dropped_missing_action=" + std::to_string(dropped_missing_action) +
      ";dropped_nonfinite_or_zero=" + std::to_string(dropped_nonfinite_or_zero) +
      ";sum_added=" + std::to_string(sum_added);

  const std::size_t block_id = op->register_objective_block(
    block_name + "::x::add_profit",
    (std::size_t)x0,
    (std::size_t)x1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("block_id") = (double)block_id,
    Rcpp::Named("x_range") = Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1),
    Rcpp::Named("n_used_rows") = used,
    Rcpp::Named("dropped_missing_action") = dropped_missing_action,
    Rcpp::Named("dropped_nonfinite_or_zero") = dropped_nonfinite_or_zero,
    Rcpp::Named("sum_added") = sum_added,
    Rcpp::Named("profit_col_used") = profit_col,
    Rcpp::Named("weight_used") = weight,
    Rcpp::Named("tag") = full_tag
  );
}
