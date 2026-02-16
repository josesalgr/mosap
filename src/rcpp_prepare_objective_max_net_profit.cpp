#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_objective_max_net_profit(
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
  if (Rf_isNull(x)) Rcpp::stop("model_ptr is NULL.");

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
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

  // ---- determine w range from MODEL
  if (op->_n_pu <= 0) op->_n_pu = pu_data.nrows();
  if (op->_n_pu <= 0) Rcpp::stop("Cannot determine n_pu (op->_n_pu <= 0 and pu_data empty).");

  const int w0 = op->_w_offset;
  const int w1 = op->_w_offset + op->_n_pu;

  if (w0 < 0 || w1 > (int)op->_obj.size()) {
    Rcpp::stop("w block out of bounds. Check op->_w_offset/op->_n_pu and that w variables exist.");
  }

  // ---- determine x range from MODEL
  if (op->_n_x <= 0) op->_n_x = dist_actions_data.nrows();
  if (op->_n_x <= 0) Rcpp::stop("Cannot determine n_x (op->_n_x <= 0 and dist_actions_data empty).");

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x;

  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds. Check op->_x_offset/op->_n_x and that x variables exist.");
  }

  // ---- extra validation: cost length matches n_pu
  if (include_pu_cost) {
    Rcpp::NumericVector pu_cost = pu_data["cost"];
    if (pu_cost.size() != op->_n_pu) {
      Rcpp::stop("pu_data 'cost' length (%d) must match op->_n_pu (%d).",
                 pu_cost.size(), op->_n_pu);
    }
  }

  // ---- registry blocks (no objective modification)
  std::string base_tag = tag;
  if (!base_tag.empty()) base_tag += ";";
  base_tag +=
    "profit_col=" + profit_col +
    ";include_pu_cost=" + std::string(include_pu_cost ? "TRUE" : "FALSE") +
      ";include_action_cost=" + std::string(include_action_cost ? "TRUE" : "FALSE") +
        ";prepare_only=TRUE";

  const std::size_t w_block_id = op->register_objective_block(
    block_name + "::w", (std::size_t)w0, (std::size_t)w1, base_tag
  );

  const std::size_t x_block_id = op->register_objective_block(
    block_name + "::x", (std::size_t)x0, (std::size_t)x1, base_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("w_block_id") = (double)w_block_id,
    Rcpp::Named("x_block_id") = (double)x_block_id,
    Rcpp::Named("w_range")    = Rcpp::NumericVector::create((double)w0 + 1.0, (double)w1),
    Rcpp::Named("x_range")    = Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1),
    Rcpp::Named("profit_col") = profit_col,
    Rcpp::Named("note")       = "Prepared objective blocks (no changes to op->_obj)."
  );
}
