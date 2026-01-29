#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// z_is <= w_i   <=>   z_is - w_i <= 0
// Asumimos:
// - w_i está en columnas [op->_w_offset + (i-1)]
// - z_row (una por fila en dist_features) está en [op->_z_offset + row]
// - dist_features_data trae internal_pu (1..n_pu) y está alineado con el orden usado para crear z
//
// [[Rcpp::export]]
Rcpp::List rcpp_add_linking_z_le_w(SEXP x,
                                   Rcpp::DataFrame dist_features_data) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!dist_features_data.containsElementNamed("internal_pu")) {
    Rcpp::stop("dist_features_data must contain column 'internal_pu'.");
  }

  Rcpp::IntegerVector internal_pu = dist_features_data["internal_pu"];
  const int n_z = dist_features_data.nrows();

  std::size_t added = 0;

  for (int r = 0; r < n_z; ++r) {
    const int ipu = internal_pu[r];
    if (ipu <= 0) Rcpp::stop("internal_pu must be 1-based positive integers.");

    const int col_z = op->_z_offset + r;          // 0-based col index
    const int col_w = op->_w_offset + (ipu - 1);  // 0-based col index

    op->addRow(
        std::vector<int>{col_z, col_w},
        std::vector<double>{1.0, -1.0},
        "<=",
        0.0
    );
    ++added;
  }

  return Rcpp::List::create(Rcpp::Named("n_added") = (int)added);
}
