#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_add_linear_constraint(SEXP model_ptr,
                                IntegerVector j0,
                                NumericVector x,
                                std::string sense,
                                double rhs,
                                std::string name = "") {

  // --- basic checks
  if (Rf_isNull(model_ptr)) stop("model_ptr is NULL.");
  if (j0.size() != x.size()) stop("Length mismatch: j0 and x must have same length.");
  if (!std::isfinite(rhs)) stop("rhs must be finite.");

  if (!(sense == "<=" || sense == ">=" || sense == "==" || sense == "=")) {
    stop("sense must be one of '<=', '>=', '==', '='.");
  }
  // normalize "=" to "==", so everything is consistent in your model snapshot
  if (sense == "=") sense = "==";

  // --- get pointer
  XPtr<OptimizationProblem> op = as<XPtr<OptimizationProblem>>(model_ptr);

  const int n_var = static_cast<int>(op->_obj.size());
  if (n_var <= 0) stop("Model has zero variables.");

  // row index = current number of constraints (0-based)
  const int row = static_cast<int>(op->_rhs.size());

  // --- append triplets for this new row (skip zeros)
  for (R_xlen_t k = 0; k < j0.size(); ++k) {
    int col = j0[k];

    if (col == NA_INTEGER) continue;
    if (col < 0 || col >= n_var) {
      stop("j0 contains an index out of bounds: %d (valid range: 0..%d).", col, n_var - 1);
    }

    double val = x[k];
    if (NumericVector::is_na(val)) continue;
    if (!std::isfinite(val)) stop("x contains a non-finite value.");
    if (std::abs(val) <= 0.0) continue; // ignore exact zeros

    op->_A_i.push_back(row);
    op->_A_j.push_back(col);
    op->_A_x.push_back(val);
  }

  // --- append rhs/sense/name
  op->_rhs.push_back(rhs);
  op->_sense.push_back(sense);

  // if you keep constraint names aligned to rhs/sense length
  // (your as_list exports op->_name, so this is the right place to maintain it)
  op->_name.push_back(name);

  return true;
}
