#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <cmath>

static inline long long key2_int(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// z_is + x_ia <= 1  for all actions a that improve feature s in planning unit i
//
// Requires:
// - dist_features_data: internal_pu, internal_feature (rows aligned with z variables)
// - dist_actions_data: internal_pu, internal_action (rows aligned with x variables)
// - dist_effects_data: internal_pu, internal_action, internal_feature, benefit_col
//
// [[Rcpp::export]]
Rcpp::List rcpp_add_exclude_conservation_when_actions(
    SEXP x,
    Rcpp::DataFrame dist_features_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_effects_data,
    SEXP benefit_col_sexp = R_NilValue,
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // ---- validate required columns
  for (auto nm : {"internal_pu", "internal_feature"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_action", "internal_feature"}) {
    if (!dist_effects_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_effects_data must contain column '") + nm + "'.");
    }
  }

  std::string benefit_col = "benefit";
  if (!Rf_isNull(benefit_col_sexp)) {
    if (TYPEOF(benefit_col_sexp) == STRSXP && Rf_length(benefit_col_sexp) >= 1) {
      benefit_col = Rcpp::as<std::string>(benefit_col_sexp);
    } else {
      Rcpp::stop("benefit_col must be a character scalar.");
    }
  }
  if (!dist_effects_data.containsElementNamed(benefit_col.c_str())) {
    Rcpp::stop(std::string("dist_effects_data must contain column '") + benefit_col + "'.");
  }

  // ---- map (ipu, iact) -> x_col
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xcol;
  pa_to_xcol.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    if (ipu <= 0 || ia <= 0) Rcpp::stop("dist_actions internal_* must be positive 1-based.");

    const long long k = key2_int(ipu, ia);
    const int x_col = op->_x_offset + r; // x variable column (0-based)
    pa_to_xcol[k] = x_col;
  }

  // ---- gather for each (ipu, ifeat): set of x_cols that improve it
  Rcpp::IntegerVector de_ipu   = dist_effects_data["internal_pu"];
  Rcpp::IntegerVector de_iact  = dist_effects_data["internal_action"];
  Rcpp::IntegerVector de_ifeat = dist_effects_data["internal_feature"];
  Rcpp::NumericVector de_ben   = dist_effects_data[benefit_col];
  const int n_de = dist_effects_data.nrows();

  std::unordered_map<long long, std::unordered_set<int>> pf_to_xcols;
  pf_to_xcols.reserve((std::size_t)n_de / 2 + 1);

  int n_effect_rows_used = 0;

  for (int r = 0; r < n_de; ++r) {
    const int ipu   = de_ipu[r];
    const int iact  = de_iact[r];
    const int ifeat = de_ifeat[r];
    const double b  = de_ben[r];

    if (ipu <= 0 || iact <= 0 || ifeat <= 0) continue;
    if (!(b > tol)) continue;

    const long long k_pa = key2_int(ipu, iact);
    auto it = pa_to_xcol.find(k_pa);
    if (it == pa_to_xcol.end()) {
      // This can happen if effects include actions that were filtered out of dist_actions_model
      // Better to skip than to crash.
      continue;
    }

    const int x_col = it->second;
    const long long k_pf = key2_int(ipu, ifeat);

    pf_to_xcols[k_pf].insert(x_col);
    ++n_effect_rows_used;
  }

  // ---- now add constraints z_row + x_col <= 1 for each dist_features row and each improving x_col
  Rcpp::IntegerVector df_ipu   = dist_features_data["internal_pu"];
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];
  const int n_df = dist_features_data.nrows();

  int added = 0;
  int rows_with_any_improving_action = 0;

  for (int r = 0; r < n_df; ++r) {
    const int ipu   = df_ipu[r];
    const int ifeat = df_ifeat[r];
    if (ipu <= 0 || ifeat <= 0) Rcpp::stop("dist_features internal_* must be positive 1-based.");

    const long long k_pf = key2_int(ipu, ifeat);
    auto it = pf_to_xcols.find(k_pf);
    if (it == pf_to_xcols.end() || it->second.empty()) continue;

    ++rows_with_any_improving_action;

    const int z_col = op->_z_offset + r;

    for (int x_col : it->second) {
      op->addRow(
          std::vector<int>{z_col, x_col},
          std::vector<double>{1.0, 1.0},
          "<=",
          1.0
      );
      ++added;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("n_effect_rows_used") = n_effect_rows_used,
    Rcpp::Named("n_df_rows_with_any_improving_action") = rows_with_any_improving_action,
    Rcpp::Named("benefit_col_used") = benefit_col,
    Rcpp::Named("tol") = tol,
    Rcpp::Named("mode") = "exclude_conservation_when_actions_improve_same_feature"
  );
}
