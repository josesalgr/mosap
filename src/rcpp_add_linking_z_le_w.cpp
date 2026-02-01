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
                                   Rcpp::DataFrame dist_features_data,
                                   std::string block_name = "linking_z_le_w",
                                   std::string tag = "") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!dist_features_data.containsElementNamed("internal_pu")) {
    Rcpp::stop("dist_features_data must contain column 'internal_pu'.");
  }

  const int n_z = dist_features_data.nrows();
  if (n_z == 0) {
    return Rcpp::List::create(
      Rcpp::Named("n_constraints_added") = 0,
      Rcpp::Named("block_id") = NA_REAL,
      Rcpp::Named("row_start") = NA_REAL,
      Rcpp::Named("row_end") = NA_REAL,
      Rcpp::Named("block_name") = block_name,
      Rcpp::Named("tag") = tag
    );
  }

  Rcpp::IntegerVector internal_pu = dist_features_data["internal_pu"];

  // ---- registry begin
  const std::size_t row_start = op->nrow_used();

  if (tag.empty()) {
    tag = "n=" + std::to_string(n_z);
  } else {
    tag = tag + ";n=" + std::to_string(n_z);
  }

  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  int added = 0;

  for (int r = 0; r < n_z; ++r) {
    const int ipu = internal_pu[r];

    if (ipu == NA_INTEGER) continue;
    if (ipu <= 0) Rcpp::stop("internal_pu must be positive 1-based integers.");

    const int col_z = op->_z_offset + r;          // 0-based col index
    const int col_w = op->_w_offset + (ipu - 1);  // 0-based col index

    op->addRow(
        std::vector<int>{col_z, col_w},
        std::vector<double>{1.0, -1.0},
        "<=",
        0.0,
        "z_le_w"
    );

    ++added;
  }

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("block_id")  = static_cast<double>(bid),
    Rcpp::Named("row_start") = static_cast<double>(row_start + 1), // 1-based friendly
    Rcpp::Named("row_end")   = static_cast<double>(row_end),       // end (exclusive in 0-based)
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = tag
  );
}
