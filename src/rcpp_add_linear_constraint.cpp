#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_add_linear_constraint(SEXP model_ptr,
                                      Rcpp::IntegerVector j0,
                                      Rcpp::NumericVector x,
                                      std::string sense,
                                      double rhs,
                                      std::string name = "",
                                      std::string block_name = "linear_constraint",
                                      std::string tag = "") {

  // --- basic checks
  if (Rf_isNull(model_ptr)) Rcpp::stop("model_ptr is NULL.");
  if (j0.size() != x.size()) Rcpp::stop("Length mismatch: j0 and x must have same length.");
  if (!std::isfinite(rhs)) Rcpp::stop("rhs must be finite.");

  if (!(sense == "<=" || sense == ">=" || sense == "==" || sense == "=")) {
    Rcpp::stop("sense must be one of '<=', '>=', '==', '='.");
  }
  if (sense == "=") sense = "==";

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(model_ptr);

  const std::size_t n_var = op->ncol_used();
  if (n_var == 0) Rcpp::stop("Model has zero variables.");

  // --- build sparse row (filter invalid/zero)
  std::vector<int> cols;
  std::vector<double> vals;
  cols.reserve(j0.size());
  vals.reserve(x.size());

  for (R_xlen_t k = 0; k < j0.size(); ++k) {
    int col = j0[k];
    if (col == NA_INTEGER) continue;
    if (col < 0 || static_cast<std::size_t>(col) >= n_var) {
      Rcpp::stop("j0 contains an index out of bounds: %d (valid range: 0..%d).",
                 col, static_cast<int>(n_var) - 1);
    }

    double val = x[k];
    if (Rcpp::NumericVector::is_na(val)) continue;
    if (!std::isfinite(val)) Rcpp::stop("x contains a non-finite value.");
    if (val == 0.0) continue;

    cols.push_back(col);
    vals.push_back(val);
  }

  // Si quedó vacía la fila: decide política (yo la haría error; es casi siempre bug)
  if (cols.empty()) {
    Rcpp::stop("Linear constraint has no non-zero coefficients after filtering.");
  }

  // --- registry block (1 row)
  const std::size_t row_start = op->nrow_used();
  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  // añade la constraint usando el método oficial
  op->addRow(cols, vals, sense, rhs, name);

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  return Rcpp::List::create(
    Rcpp::Named("block_id")   = static_cast<double>(bid),
    Rcpp::Named("row_start")  = static_cast<double>(row_start + 1), // 1-based friendly
    Rcpp::Named("row_end")    = static_cast<double>(row_end),       // end exclusive in 0-based, here friendly
    Rcpp::Named("n_added")    = 1.0,
    Rcpp::Named("sense")      = sense,
    Rcpp::Named("rhs")        = rhs,
    Rcpp::Named("name")       = name,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag")        = tag
  );
}
