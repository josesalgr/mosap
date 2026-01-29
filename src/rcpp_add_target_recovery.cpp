#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>

static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
Rcpp::List rcpp_add_target_recovery(
    SEXP x,
    Rcpp::DataFrame features_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_benefit_data,
    SEXP target_col_sexp = R_NilValue,   // <- clave: NO std::string aquí
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // ---- Required columns in dist tables
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_action", "internal_feature", "benefit"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
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
  std::string target_col = "target_recovery";

  if (!Rf_isNull(target_col_sexp)) {
    // New expected type: character scalar
    if (TYPEOF(target_col_sexp) == STRSXP && Rf_length(target_col_sexp) >= 1) {
      target_col = Rcpp::as<std::string>(target_col_sexp);

      // Backward compat: old builds might pass logical here
    } else if (TYPEOF(target_col_sexp) == LGLSXP && Rf_length(target_col_sexp) >= 1) {
      int flag = LOGICAL(target_col_sexp)[0];
      if (flag == 0) target_col = "target";  // legacy
      // if TRUE, keep default "target_recovery"
    } else {
      Rcpp::stop("Argument 'target_col' must be a character scalar (preferred) or logical (legacy).");
    }
  }

  // ---- Extract feature ids and targets (prefer target_col, else fallback to 'target')
  Rcpp::IntegerVector f_id = features_data[id_col];

  Rcpp::NumericVector f_t;
  if (features_data.containsElementNamed(target_col.c_str())) {
    f_t = features_data[target_col.c_str()];
  } else if (features_data.containsElementNamed("target")) {
    target_col = "target";           // legacy fallback
    f_t = features_data["target"];
  } else {
    Rcpp::stop("features_data must contain target column '" + target_col +
      "' (or legacy column 'target').");
  }

  // ---- Build map (internal_pu, internal_action) -> row in dist_actions (x variable index)
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow;
  pa_to_xrow.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    if (ipu <= 0 || ia <= 0) {
      Rcpp::stop("dist_actions internal_* must be positive 1-based.");
    }
    const long long k = key2(ipu, ia);
    if (pa_to_xrow.find(k) != pa_to_xrow.end()) {
      Rcpp::stop("Duplicate (internal_pu, internal_action) in dist_actions_data.");
    }
    pa_to_xrow[k] = r;
  }

  // ---- Benefit table
  Rcpp::IntegerVector db_ipu   = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact  = dist_benefit_data["internal_action"];
  Rcpp::IntegerVector db_ifeat = dist_benefit_data["internal_feature"];
  Rcpp::NumericVector db_ben   = dist_benefit_data["benefit"];
  const int n_db = dist_benefit_data.nrows();

  int added = 0;

  for (int s = 0; s < f_id.size(); ++s) {
    const int ifeat = f_id[s];
    const double T  = f_t[s];
    if (!(T > tol)) continue;

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(512);
    vals.reserve(512);

    for (int r = 0; r < n_db; ++r) {
      if (db_ifeat[r] != ifeat) continue;

      const double b = db_ben[r];
      if (!(b > tol)) continue;

      const long long k = key2(db_ipu[r], db_iact[r]);
      auto it = pa_to_xrow.find(k);
      if (it == pa_to_xrow.end()) {
        Rcpp::stop("dist_benefit has (internal_pu, internal_action) not found in dist_actions_data.");
      }

      const int x_row = it->second;
      cols.push_back(op->_x_offset + x_row);
      vals.push_back(b);
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Recovery-only target cannot be met: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no positive deltas in dist_benefit."
      );
    }

    op->addRow(cols, vals, ">=", T);
    ++added;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("mode") = "recovery_only",
    Rcpp::Named("target_col_used") = target_col,
    Rcpp::Named("id_col_used") = id_col
  );
}
