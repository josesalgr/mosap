#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <vector>
#include <string>

// [[Rcpp::export]]
Rcpp::List rcpp_add_target_conservation(
    SEXP x,
    Rcpp::DataFrame features_data,
    Rcpp::DataFrame dist_features_data,
    SEXP target_col_sexp = R_NilValue,   // <- clave: NO std::string aquí
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // ---- dist_features_data required cols
  for (auto nm : {"internal_feature", "amount"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }

  // ---- Determine feature id column name in features_data
  std::string id_col;
  if (features_data.containsElementNamed("internal_id")) {
    id_col = "internal_id";
  } else if (features_data.containsElementNamed("internal_feature")) {
    id_col = "internal_feature";
  } else {
    Rcpp::stop("features_data must contain column 'internal_id' (preferred) or 'internal_feature' (legacy).");
  }

  // ---- Determine target column name
  std::string target_col = "target_conservation";

  if (!Rf_isNull(target_col_sexp)) {
    // New expected type: character scalar
    if (TYPEOF(target_col_sexp) == STRSXP && Rf_length(target_col_sexp) >= 1) {
      target_col = Rcpp::as<std::string>(target_col_sexp);

      // Backward compat: some old builds might pass logical here
    } else if (TYPEOF(target_col_sexp) == LGLSXP && Rf_length(target_col_sexp) >= 1) {
      int flag = LOGICAL(target_col_sexp)[0];
      // if FALSE, assume legacy column "target"; if TRUE, keep default "target_conservation"
      if (flag == 0) target_col = "target";

    } else {
      Rcpp::stop("Argument 'target_col' must be a character scalar (preferred) or logical (legacy).");
    }
  }

  // ---- Extract feature ids
  Rcpp::IntegerVector f_id = features_data[id_col];

  // ---- Extract targets: prefer target_col if present, else fallback to "target"
  Rcpp::NumericVector f_t;
  if (features_data.containsElementNamed(target_col.c_str())) {
    f_t = features_data[target_col.c_str()];
  } else if (features_data.containsElementNamed("target")) {
    // legacy fallback
    target_col = "target";
    f_t = features_data["target"];
  } else {
    Rcpp::stop("features_data must contain target column '" + target_col +
      "' (or legacy column 'target').");
  }

  // ---- dist_features vectors
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];
  Rcpp::NumericVector df_amt   = dist_features_data["amount"];
  const int n_df = dist_features_data.nrows();

  int added = 0;

  for (int s = 0; s < f_id.size(); ++s) {

    const int ifeat = f_id[s];      // 1-based feature id
    const double T  = f_t[s];

    if (!(T > tol)) continue;

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(256);
    vals.reserve(256);

    for (int r = 0; r < n_df; ++r) {
      if (df_ifeat[r] != ifeat) continue;

      const double a = df_amt[r];
      if (!(a > tol)) continue;

      cols.push_back(op->_z_offset + r);
      vals.push_back(a);
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Conservation-only target cannot be met: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no baseline amount rows in dist_features."
      );
    }

    op->addRow(cols, vals, ">=", T);
    ++added;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("mode") = "conservation_only",
    Rcpp::Named("target_col_used") = target_col,
    Rcpp::Named("id_col_used") = id_col
  );
}
