#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_add_pu_locks(SEXP x, Rcpp::DataFrame pu_data) {
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  Rcpp::LogicalVector locked_in  = pu_data["locked_in"];
  Rcpp::LogicalVector locked_out = pu_data["locked_out"];

  const int n = pu_data.nrows();
  for (int i = 0; i < n; ++i) {
    const int col_w = op->_w_offset + i;
    if (locked_in[i]) {
      op->addRow({col_w}, {1.0}, "==", 1.0);
    } else if (locked_out[i]) {
      op->addRow({col_w}, {1.0}, "==", 0.0);
    }
  }
  return true;
}
