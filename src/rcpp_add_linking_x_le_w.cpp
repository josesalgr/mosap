#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_add_linking_x_le_w(SEXP x,
                             Rcpp::DataFrame dist_actions_data) {

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  Rcpp::IntegerVector ipu = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector irow = dist_actions_data["internal_row"];

  const int n = dist_actions_data.nrows();

  for (int k = 0; k < n; ++k) {
    const int pu0  = ipu[k] - 1;      // 0-based
    const int row0 = irow[k] - 1;     // 0-based row in dist_actions

    const int col_w = op->_w_offset + pu0;
    const int col_x = op->_x_offset + row0;

    // x - w <= 0
    std::vector<int> cols{col_x, col_w};
    std::vector<double> vals{1.0, -1.0};

    op->addRow(cols, vals, "<=", 0.0);
  }

  return true;
}
