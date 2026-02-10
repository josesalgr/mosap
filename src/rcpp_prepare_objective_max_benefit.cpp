// rcpp_prepare_objective_max_benefit.cpp
#include "Package.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>
#include <cmath>
#include <cstdint>
#include <vector>

// hash key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_objective_max_benefit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_benefit_data,
    std::string benefit_col = "benefit",
    std::string block_name = "prepare_max_benefit",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // Must have base variables
  if (op->ncol_used() == 0 || op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // x block must exist for benefits
  if (op->_n_x < 0 || op->_x_offset < 0) {
    Rcpp::stop("x block not initialized (op->_n_x/op->_x_offset invalid).");
  }

  const int n_x = op->_n_x;
  if (n_x == 0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "op->_n_x == 0 (no action variables)",
      Rcpp::Named("coef_x") = Rcpp::NumericVector(0),
      Rcpp::Named("benefit_col_used") = benefit_col
    );
  }

  // checks: dist_actions_data
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_benefit_data
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_benefit_data.containsElementNamed(benefit_col.c_str())) {
    Rcpp::stop("dist_benefit_data must contain benefit column '" + benefit_col + "'.");
  }

  // build map (ipu, iact) -> row0 from internal_row-1
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_row0;
  pa_to_row0.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu  = da_ipu[r];
    const int ia   = da_iact[r];
    const int row0 = da_irow[r] - 1;

    if (ipu <= 0 || ia <= 0) {
      Rcpp::stop("dist_actions_data internal_pu/internal_action must be positive 1-based.");
    }
    if (row0 < 0 || row0 >= n_x) {
      Rcpp::stop("dist_actions_data internal_row out of range: must be in [1, op->_n_x].");
    }

    const long long k = key2(ipu, ia);
    // keep first occurrence (dist_actions should be unique anyway)
    if (pa_to_row0.find(k) == pa_to_row0.end()) pa_to_row0.emplace(k, row0);
  }

  // accumulate benefits into coef_x
  Rcpp::IntegerVector db_ipu  = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact = dist_benefit_data["internal_action"];
  Rcpp::NumericVector db_ben  = dist_benefit_data[benefit_col.c_str()];
  const int n_db = dist_benefit_data.nrows();

  std::vector<double> coef_x((std::size_t)n_x, 0.0);

  int used_rows = 0;
  int dropped_missing_action = 0;
  int dropped_nonfinite_or_zero = 0;

  double sum_added = 0.0;

  for (int r = 0; r < n_db; ++r) {
    const double b = (double)db_ben[r];
    if (!std::isfinite(b) || b == 0.0) {
      ++dropped_nonfinite_or_zero;
      continue;
    }

    const int ipu = db_ipu[r];
    const int ia  = db_iact[r];
    if (ipu <= 0 || ia <= 0) {
      ++dropped_missing_action;
      continue;
    }

    const long long k = key2(ipu, ia);
    auto it = pa_to_row0.find(k);
    if (it == pa_to_row0.end()) {
      ++dropped_missing_action;
      continue;
    }

    const int row0 = it->second;
    coef_x[(std::size_t)row0] += b;

    sum_added += b;
    ++used_rows;
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=prepare"
    ";component=max_benefit"
    ";benefit_col=" + benefit_col +
      ";n_x=" + std::to_string(n_x) +
      ";n_used_rows=" + std::to_string(used_rows) +
      ";dropped_missing_action=" + std::to_string(dropped_missing_action) +
      ";dropped_nonfinite_or_zero=" + std::to_string(dropped_nonfinite_or_zero) +
      ";sum_added=" + std::to_string(sum_added);

  // No variable/constraint blocks here: just a "prepared artifact"
  // (If you later add op->register_artifact_block(), you can hook it here.)

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("skipped") = false,
    Rcpp::Named("coef_x") = Rcpp::wrap(coef_x),
    Rcpp::Named("benefit_col_used") = benefit_col,
    Rcpp::Named("n_used_rows") = used_rows,
    Rcpp::Named("dropped_missing_action") = dropped_missing_action,
    Rcpp::Named("dropped_nonfinite_or_zero") = dropped_nonfinite_or_zero,
    Rcpp::Named("sum_added") = sum_added,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("block_name") = block_name
  );
}
