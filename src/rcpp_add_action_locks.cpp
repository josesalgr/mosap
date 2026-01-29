#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_add_action_locks(SEXP x, Rcpp::DataFrame dist_actions_data) {
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  Rcpp::IntegerVector status = dist_actions_data["status"];
  Rcpp::IntegerVector irow   = dist_actions_data["internal_row"];

  const int n = dist_actions_data.nrows();

  for (int k = 0; k < n; ++k) {
    const int st = status[k];
    if (st == 0) continue;

    const int row0 = irow[k] - 1;
    const int col_x = op->_x_offset + row0;

    if (st == 2) op->addRow({col_x}, {1.0}, "==", 1.0);
    if (st == 3) op->addRow({col_x}, {1.0}, "==", 0.0);
  }

  return true;
}
