#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_set_objective_min_cost(SEXP x,
                                 Rcpp::DataFrame pu_data,
                                 Rcpp::DataFrame dist_actions_data,
                                 bool include_pu_cost = true,
                                 bool include_action_cost = true) {

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  if (include_pu_cost) {
    Rcpp::NumericVector pu_cost = pu_data["cost"];
    for (int i = 0; i < pu_cost.size(); ++i) {
      op->_obj[op->_w_offset + i] = (double)pu_cost[i];
    }
  }

  if (include_action_cost) {
    Rcpp::NumericVector a_cost = dist_actions_data["cost"];
    Rcpp::IntegerVector irow   = dist_actions_data["internal_row"];
    for (int k = 0; k < dist_actions_data.nrows(); ++k) {
      const int row0 = irow[k] - 1;
      op->_obj[op->_x_offset + row0] = (double)a_cost[k];
    }
  }

  return true;
}
