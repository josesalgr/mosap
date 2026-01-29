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
Rcpp::List rcpp_add_target_mixed_total(
    SEXP x,
    Rcpp::DataFrame features_data,
    Rcpp::DataFrame dist_features_data,
    Rcpp::DataFrame dist_benefit_data,
    Rcpp::DataFrame dist_actions_data,
    SEXP target_col_sexp = R_NilValue,   // robust: can be character (new) or logical (legacy)
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // ---- Required columns in dist tables
  for (auto nm : {"internal_feature", "amount"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }
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

  // ---- Determine feature id column in features_data
  std::string id_col;
  if (features_data.containsElementNamed("internal_id")) {
    id_col = "internal_id";
  } else if (features_data.containsElementNamed("internal_feature")) {
    id_col = "internal_feature";
  } else {
    Rcpp::stop("features_data must contain column 'internal_id' (preferred) or 'internal_feature' (legacy).");
  }

  // ---- Determine target column name
  // Default for mixed_total (you can pass a different one from R)
  std::string target_col = "target_mixed_total";

  if (!Rf_isNull(target_col_sexp)) {
    if (TYPEOF(target_col_sexp) == STRSXP && Rf_length(target_col_sexp) >= 1) {
      target_col = Rcpp::as<std::string>(target_col_sexp);
    } else if (TYPEOF(target_col_sexp) == LGLSXP && Rf_length(target_col_sexp) >= 1) {
      // legacy: FALSE -> "target", TRUE -> keep default
      int flag = LOGICAL(target_col_sexp)[0];
      if (flag == 0) target_col = "target";
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
    target_col = "target"; // legacy fallback
    f_t = features_data["target"];
  } else {
    Rcpp::stop("features_data must contain target column '" + target_col +
      "' (or legacy column 'target').");
  }

  // ---- dist_features vectors (baseline amounts for z)
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];
  Rcpp::NumericVector df_amt   = dist_features_data["amount"];
  const int n_df = dist_features_data.nrows();

  // ---- Map (internal_pu, internal_action) -> row in dist_actions (x variable index)
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

  // ---- dist_benefit vectors (deltas for x)
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
    cols.reserve(1024);
    vals.reserve(1024);

    // ---- Baseline part: sum(amount * z) for this feature
    for (int r = 0; r < n_df; ++r) {
      if (df_ifeat[r] != ifeat) continue;

      const double a = df_amt[r];
      if (!(a > tol)) continue;

      cols.push_back(op->_z_offset + r);
      vals.push_back(a);
    }

    // ---- Recovery part: sum(benefit * x) for this feature
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
        "Mixed-total target cannot be met: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no baseline amounts and no positive deltas."
      );
    }

    op->addRow(cols, vals, ">=", T);
    ++added;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("mode") = "mixed_total_baseline_plus_recovery",
    Rcpp::Named("target_col_used") = target_col,
    Rcpp::Named("id_col_used") = id_col
  );
}
