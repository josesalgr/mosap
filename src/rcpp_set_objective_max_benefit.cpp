#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <string>
#include <cmath>

// hash key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_max_benefit(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_benefit_data,
    std::string benefit_col = "benefit",
    std::string block_name = "objective_max_benefit",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // modelsense
  op->_modelsense = "max";

  // checks: dist_actions
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_benefit
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_benefit_data.containsElementNamed(benefit_col.c_str())) {
    Rcpp::stop("dist_benefit_data must contain benefit column '" + benefit_col + "'.");
  }

  // Must have variables
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // x block boundaries should come from the model, not from dist_actions_data rows
  if (op->_n_x <= 0) {
    // reset objective anyway
    std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

    return Rcpp::List::create(
      Rcpp::Named("modelsense") = op->_modelsense,
      Rcpp::Named("n_used_rows") = 0,
      Rcpp::Named("x_range") = Rcpp::NumericVector::create(NA_REAL, NA_REAL),
      Rcpp::Named("block_id") = NA_REAL,
      Rcpp::Named("benefit_col_used") = benefit_col,
      Rcpp::Named("note") = "Model has op->_n_x == 0; objective reset to 0."
    );
  }

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x; // exclusive

  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds: check op->_x_offset/op->_n_x and that base variables exist.");
  }

  // reset objective coefficients to 0
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

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
    const int row0 = da_irow[r] - 1; // 0-based in x block
    if (ipu <= 0 || ia <= 0) {
      Rcpp::stop("dist_actions_data internal_pu/internal_action must be positive 1-based.");
    }
    if (row0 < 0 || row0 >= op->_n_x) {
      Rcpp::stop("dist_actions_data internal_row out of range: must be in [1, op->_n_x].");
    }
    const long long k = key2(ipu, ia);
    if (pa_to_row0.find(k) == pa_to_row0.end()) pa_to_row0[k] = row0;
  }

  // accumulate benefits onto x objective coefficients
  Rcpp::IntegerVector db_ipu  = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact = dist_benefit_data["internal_action"];
  Rcpp::NumericVector db_ben  = dist_benefit_data[benefit_col.c_str()];
  const int n_db = dist_benefit_data.nrows();

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

    const long long k = key2(db_ipu[r], db_iact[r]);
    auto it = pa_to_row0.find(k);
    if (it == pa_to_row0.end()) {
      ++dropped_missing_action;
      continue;
    }

    const int row0 = it->second;
    const int col_x = op->_x_offset + row0; // absolute column index in model
    op->_obj[col_x] += b;

    sum_added += b;
    ++used_rows;
  }

  // ---- Registry: OBJECTIVE block over the x columns
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=max"
    ";benefit_col=" + benefit_col +
      ";n_used_rows=" + std::to_string(used_rows) +
      ";dropped_missing_action=" + std::to_string(dropped_missing_action) +
      ";dropped_nonfinite_or_zero=" + std::to_string(dropped_nonfinite_or_zero) +
      ";sum_added=" + std::to_string(sum_added);

  const std::size_t block_id = op->register_objective_block(
    block_name,                 // name visible in registry
    (std::size_t)x0,
    (std::size_t)x1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("n_used_rows") = used_rows,
    Rcpp::Named("dropped_missing_action") = dropped_missing_action,
    Rcpp::Named("dropped_nonfinite_or_zero") = dropped_nonfinite_or_zero,
    Rcpp::Named("sum_added") = sum_added,
    Rcpp::Named("x_range") = Rcpp::NumericVector::create((double)x0 + 1.0, (double)x1),
    Rcpp::Named("block_id") = (double)block_id,
    Rcpp::Named("benefit_col_used") = benefit_col,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag
  );
}
