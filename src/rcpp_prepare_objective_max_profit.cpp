#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>

// [[Rcpp::export]]
bool rcpp_prepare_objective_max_profit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_profit_data,
    std::string profit_col = "profit"
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // must have variables
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // must have x vars
  if (op->_n_x <= 0) {
    Rcpp::stop("Prepare max_profit: op->_n_x <= 0 (no action variables).");
  }
  if (op->_x_offset < 0) {
    Rcpp::stop("Prepare max_profit: op->_x_offset not initialized.");
  }

  // minimal checks (same as add/set)
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_profit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_profit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_profit_data.containsElementNamed(profit_col.c_str())) {
    Rcpp::stop("dist_profit_data must contain profit column '" + profit_col + "'.");
  }

  // Nothing to prepare structurally (no aux vars, no constraints).
  return true;
}
