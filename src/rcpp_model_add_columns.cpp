#include "Package.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_model_add_columns(
    SEXP x,
    Rcpp::NumericVector obj,
    Rcpp::NumericVector lb,
    Rcpp::NumericVector ub,
    Rcpp::CharacterVector vtype,
    Rcpp::CharacterVector names = R_NilValue,
    std::string block_name = "augmecon_slacks",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op =
    Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  const int k = obj.size();

  if (k <= 0) {
    Rcpp::stop("`obj` must have positive length.");
  }
  if (lb.size() != k) {
    Rcpp::stop("`lb` length must match `obj` length.");
  }
  if (ub.size() != k) {
    Rcpp::stop("`ub` length must match `obj` length.");
  }
  if (vtype.size() != k) {
    Rcpp::stop("`vtype` length must match `obj` length.");
  }

  const bool has_names = (names.size() == k);
  if (names.size() != 0 && names.size() != k) {
    Rcpp::stop("`names` must have length 0 or the same length as `obj`.");
  }

  const std::size_t old_ncol = op->_obj.size();

  // append columns
  for (int i = 0; i < k; ++i) {
    const double oi = obj[i];
    const double lbi = lb[i];
    const double ubi = ub[i];
    const std::string vt = Rcpp::as<std::string>(vtype[i]);

    if (!R_finite(oi)) {
      Rcpp::stop("`obj` contains non-finite values.");
    }
    if (!R_finite(lbi)) {
      Rcpp::stop("`lb` contains non-finite values.");
    }
    if (!R_finite(ubi)) {
      Rcpp::stop("`ub` contains non-finite values.");
    }
    if (lbi > ubi) {
      Rcpp::stop("Each lower bound must be <= upper bound.");
    }
    if (!(vt == "B" || vt == "I" || vt == "C" || vt == "S")) {
      Rcpp::stop("`vtype` entries must be one of 'B', 'I', 'C', 'S'.");
    }

    op->_obj.push_back(oi);
    op->_lb.push_back(lbi);
    op->_ub.push_back(ubi);
    op->_vtype.push_back(vt);
  }

  const std::size_t new_ncol = op->_obj.size();
  if (new_ncol != old_ncol + static_cast<std::size_t>(k)) {
    Rcpp::stop("Internal error: failed to append model columns.");
  }

  // 0-based / 1-based indices of new columns
  Rcpp::IntegerVector col0(k), col1(k);
  for (int i = 0; i < k; ++i) {
    col0[i] = static_cast<int>(old_ncol + i);
    col1[i] = static_cast<int>(old_ncol + i + 1);
  }

  if (has_names) {
    col0.attr("names") = names;
    col1.attr("names") = names;
  }

  // optional registry block
  // start/end are 0-based, half-open [start, end)
  std::size_t block_id = NA_INTEGER;
  try {
    block_id = op->register_variable_block(
      block_name,
      old_ncol,
      new_ncol,
      tag
    );
  } catch (...) {
    // if your class does not have register_variable_block(), ignore safely
    block_id = NA_INTEGER;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_added") = k,
    Rcpp::Named("old_ncol") = static_cast<int>(old_ncol),
    Rcpp::Named("new_ncol") = static_cast<int>(new_ncol),
    Rcpp::Named("col0") = col0,
    Rcpp::Named("col1") = col1,
    Rcpp::Named("block_id") = block_id
  );
}
