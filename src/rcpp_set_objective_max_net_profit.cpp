#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>
#include <cmath>

static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_max_net_profit(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_profit_data,
    std::string profit_col = "profit",
    bool include_pu_cost = true,
    bool include_action_cost = true,
    std::string block_name = "objective_max_net_profit",
    std::string tag = ""
) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "max";

  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() before setting objective.");
  }

  // ---- checks: pu_data cost
  if (include_pu_cost) {
    if (!pu_data.containsElementNamed("cost")) {
      Rcpp::stop("pu_data must contain column 'cost' when include_pu_cost=TRUE.");
    }
  }

  // ---- checks: dist_actions_data
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  if (include_action_cost) {
    if (!dist_actions_data.containsElementNamed("cost")) {
      Rcpp::stop("dist_actions_data must contain column 'cost' when include_action_cost=TRUE.");
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

  // -----------------------------
  // Reset objective
  // -----------------------------
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // -----------------------------
  // Determine w range from MODEL (not from pu_data)
  // -----------------------------
  if (op->_n_pu <= 0) {
    // fallback: if model didn't store it, use pu_data rows (but prefer model)
    op->_n_pu = pu_data.nrows();
  }
  if (op->_n_pu <= 0) {
    Rcpp::stop("Cannot determine n_pu (op->_n_pu <= 0 and pu_data empty).");
  }

  const int w0 = op->_w_offset;
  const int w1 = op->_w_offset + op->_n_pu; // exclusive

  if (w0 < 0 || w1 > (int)op->_obj.size()) {
    Rcpp::stop("w block out of bounds. Check op->_w_offset/op->_n_pu and that w variables exist.");
  }

  // -----------------------------
  // Determine x range from MODEL (not max(internal_row))
  // -----------------------------
  if (op->_n_x <= 0) {
    // if base builder didn't store it, infer minimal safe value from dist_actions_data
    // but still validate rows below
    op->_n_x = dist_actions_data.nrows();
  }
  if (op->_n_x <= 0) {
    Rcpp::stop("Cannot determine n_x (op->_n_x <= 0 and dist_actions_data empty).");
  }

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x; // exclusive

  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds. Check op->_x_offset/op->_n_x and that x variables exist.");
  }

  // -----------------------------
  // Apply PU cost to w (negative)
  // -----------------------------
  int n_w_cost_nonzero = 0;
  double sum_w = 0.0;

  if (include_pu_cost) {
    Rcpp::NumericVector pu_cost = pu_data["cost"];

    // if pu_data rows differ from op->_n_pu, take min and warn by stop? I prefer stop.
    if (pu_cost.size() != op->_n_pu) {
      Rcpp::stop("pu_data 'cost' length (%d) must match op->_n_pu (%d).",
                 pu_cost.size(), op->_n_pu);
    }

    for (int i = 0; i < op->_n_pu; ++i) {
      const double c = (double)pu_cost[i];
      if (!std::isfinite(c) || c == 0.0) continue;
      op->_obj[w0 + i] -= c;
      ++n_w_cost_nonzero;
      sum_w -= c;
    }
  }

  // -----------------------------
  // Map (ipu, iact) -> x_row0 using internal_row (robust, validated)
  // -----------------------------
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow0;
  pa_to_xrow0.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu  = da_ipu[r];
    const int ia   = da_iact[r];
    const int row1 = da_irow[r];     // 1-based
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

  // -----------------------------
  // Add profit to x
  // -----------------------------
  Rcpp::IntegerVector dp_ipu  = dist_profit_data["internal_pu"];
  Rcpp::IntegerVector dp_iact = dist_profit_data["internal_action"];
  Rcpp::NumericVector dp_prof = dist_profit_data[profit_col.c_str()];
  const int n_dp = dist_profit_data.nrows();

  int n_profit_used = 0;
  int n_profit_missing_action = 0;
  int n_profit_nonfinite_or_zero = 0;
  double sum_profit = 0.0;

  for (int r = 0; r < n_dp; ++r) {
    const double p = (double)dp_prof[r];
    if (!std::isfinite(p) || p == 0.0) {
      ++n_profit_nonfinite_or_zero;
      continue;
    }

    const int ipu = dp_ipu[r];
    const int ia  = dp_iact[r];
    if (ipu == NA_INTEGER || ia == NA_INTEGER) continue;

    const long long k = key2(ipu, ia);
    auto it = pa_to_xrow0.find(k);
    if (it == pa_to_xrow0.end()) {
      ++n_profit_missing_action;
      continue; // ignore safely (filtered action)
    }

    const int row0 = it->second;
    op->_obj[x0 + row0] += p;
    ++n_profit_used;
    sum_profit += p;
  }

  // -----------------------------
  // Subtract action cost from x
  // -----------------------------
  int n_x_cost_nonzero = 0;
  double sum_x_cost = 0.0;

  if (include_action_cost) {
    Rcpp::NumericVector a_cost = dist_actions_data["cost"];

    for (int k = 0; k < n_da; ++k) {
      const double c = (double)a_cost[k];
      if (!std::isfinite(c) || c == 0.0) continue;

      const int row1 = da_irow[k];
      if (row1 == NA_INTEGER) continue;
      if (row1 <= 0 || row1 > op->_n_x) {
        Rcpp::stop("dist_actions_data internal_row out of range while applying action cost.");
      }

      const int row0 = row1 - 1;
      op->_obj[x0 + row0] -= c;
      ++n_x_cost_nonzero;
      sum_x_cost -= c;
    }
  }

  // -----------------------------
  // Registry: OBJECTIVE blocks (w and x)
  // -----------------------------
  std::string base_tag = tag;
  if (!base_tag.empty()) base_tag += ";";
  base_tag +=
    "modelsense=max"
    ";profit_col=" + profit_col +
      ";include_pu_cost=" + std::string(include_pu_cost ? "TRUE" : "FALSE") +
        ";include_action_cost=" + std::string(include_action_cost ? "TRUE" : "FALSE") +
          ";reset_objective=TRUE";

  const std::size_t w_block_id = op->register_objective_block(
    block_name + "::w",
    (std::size_t)w0,
    (std::size_t)w1,
    base_tag +
      ";sum_w=" + std::to_string(sum_w) +
      ";n_w_cost_nonzero=" + std::to_string(n_w_cost_nonzero)
  );

  const std::size_t x_block_id = op->register_objective_block(
    block_name + "::x",
    (std::size_t)x0,
    (std::size_t)x1,
    base_tag +
      ";sum_profit=" + std::to_string(sum_profit) +
      ";sum_action_cost=" + std::to_string(sum_x_cost) +
      ";profit_used=" + std::to_string(n_profit_used) +
      ";profit_missing_action=" + std::to_string(n_profit_missing_action) +
      ";profit_nonfinite_or_zero=" + std::to_string(n_profit_nonfinite_or_zero) +
      ";x_cost_nonzero=" + std::to_string(n_x_cost_nonzero)
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("w_block_id") = (double)w_block_id,
    Rcpp::Named("x_block_id") = (double)x_block_id,
    Rcpp::Named("w_range") = Rcpp::NumericVector::create((double)w0 + 1.0, (double)w1),
    Rcpp::Named("x_range") = Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1),
    Rcpp::Named("sum_w") = sum_w,
    Rcpp::Named("sum_profit") = sum_profit,
    Rcpp::Named("sum_action_cost") = sum_x_cost,
    Rcpp::Named("n_profit_used") = n_profit_used,
    Rcpp::Named("n_profit_missing_action") = n_profit_missing_action,
    Rcpp::Named("note") = "Objective registered with register_objective_block() using model offsets and sizes."
  );
}
