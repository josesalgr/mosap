#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <cmath>
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::List rcpp_add_linear_constraint(SEXP model_ptr,
                                      Rcpp::IntegerVector j0,
                                      Rcpp::NumericVector x,
                                      std::string sense,
                                      double rhs,
                                      std::string name = "",
                                      std::string block_name = "linear_constraint",
                                      std::string tag = "") {

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

  // build sparse row (filter invalid/zero)
  std::vector<int> cols;
  std::vector<double> vals;
  cols.reserve(j0.size());
  vals.reserve(x.size());

  for (R_xlen_t k = 0; k < j0.size(); ++k) {
    int col = j0[k];
    if (col == NA_INTEGER) continue;
    if (col < 0 || static_cast<std::size_t>(col) >= n_var) {
      Rcpp::stop(
        std::string("j0 contains an index out of bounds: ") + std::to_string(col) +
          " (valid range: 0.." + std::to_string((int)n_var - 1) + ")."
      );
    }

    double val = x[k];
    if (Rcpp::NumericVector::is_na(val)) continue;
    if (!std::isfinite(val)) Rcpp::stop("x contains a non-finite value.");
    if (val == 0.0) continue;

    cols.push_back(col);
    vals.push_back(val);
  }

  if (cols.empty()) {
    Rcpp::stop("Linear constraint has no non-zero coefficients after filtering.");
  }

  const std::size_t row_start0 = op->nrow_used(); // 0-based internal
  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  op->addRow(cols, vals, sense, rhs, name);

  const std::size_t row_end0 = op->nrow_used(); // now row_start0+1

  // if your signature supports it, prefer:
  // op->endConstraintBlock(bid, /*drop_if_empty=*/true);
  op->endConstraintBlock(bid);

  return Rcpp::List::create(
    Rcpp::Named("ok")         = true,
    Rcpp::Named("block_id")   = (double)bid,
    Rcpp::Named("row_start")  = (double)(row_start0 + 1),   // 1-based friendly
    Rcpp::Named("row_end")    = (double)(row_end0),         // 1-based inclusive == row_start
    Rcpp::Named("n_added")    = 1.0,
    Rcpp::Named("sense")      = sense,
    Rcpp::Named("rhs")        = rhs,
    Rcpp::Named("name")       = name,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag")        = tag
  );
}
