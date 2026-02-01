#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <vector>
#include <string>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_add_target_conservation(
    SEXP x,
    Rcpp::DataFrame features_data,
    Rcpp::DataFrame dist_features_data,
    SEXP target_col_sexp = R_NilValue,
    double tol = 1e-12,
    std::string block_name = "targets_conservation",
    std::string tag = "") {

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
    if (TYPEOF(target_col_sexp) == STRSXP && Rf_length(target_col_sexp) >= 1) {
      target_col = Rcpp::as<std::string>(target_col_sexp);
    } else if (TYPEOF(target_col_sexp) == LGLSXP && Rf_length(target_col_sexp) >= 1) {
      int flag = LOGICAL(target_col_sexp)[0];
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
    target_col = "target";
    f_t = features_data["target"];
  } else {
    Rcpp::stop("features_data must contain target column '" + target_col + "' (or legacy column 'target').");
  }

  if (f_id.size() != f_t.size()) {
    Rcpp::stop("features_data: id column and target column must have same length.");
  }

  // ---- dist_features vectors
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];
  Rcpp::NumericVector df_amt   = dist_features_data["amount"];
  const int n_df = dist_features_data.nrows();

  // ---- registry begin (block)
  const std::size_t row_start = op->nrow_used();

  // si quieres tag automático informativo
  if (tag.empty()) {
    tag = "mode=conservation_only;tol=" + std::to_string(tol) +
      ";target_col=" + target_col + ";id_col=" + id_col;
  } else {
    tag = tag + ";mode=conservation_only;tol=" + std::to_string(tol) +
      ";target_col=" + target_col + ";id_col=" + id_col;
  }

  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  int added = 0;

  for (int s = 0; s < f_id.size(); ++s) {

    const int ifeat = f_id[s];   // 1-based feature id
    const double T  = f_t[s];

    if (ifeat <= 0) continue;
    if (!std::isfinite(T)) continue;
    if (!(T > tol)) continue;

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(256);
    vals.reserve(256);

    for (int r = 0; r < n_df; ++r) {
      if (df_ifeat[r] != ifeat) continue;

      const double a = df_amt[r];
      if (!std::isfinite(a)) continue;
      if (!(a > tol)) continue;

      // z row is aligned with dist_features row index r
      cols.push_back(op->_z_offset + r);
      vals.push_back(a);
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Conservation-only target cannot be met: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no baseline amount rows in dist_features."
      );
    }

    // give each constraint a readable name
    const std::string cname = "target_conservation_f" + std::to_string(ifeat);

    op->addRow(cols, vals, ">=", T, cname);
    ++added;
  }

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  // Nota: aquí el bloque puede quedar vacío si no hubo targets > tol.
  // Si NO quieres registrar bloques vacíos, hay que “delay begin” o borrar al final.

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("mode") = "conservation_only",
    Rcpp::Named("target_col_used") = target_col,
    Rcpp::Named("id_col_used") = id_col,

    // registry info
    Rcpp::Named("block_id")  = static_cast<double>(bid),
    Rcpp::Named("row_start") = static_cast<double>(row_start + 1), // 1-based friendly
    Rcpp::Named("row_end")   = static_cast<double>(row_end),       // end (exclusive in 0-based)
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = tag
  );
}
