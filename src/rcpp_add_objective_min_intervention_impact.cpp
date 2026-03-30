#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_set>
#include <vector>
#include <string>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_min_intervention_impact(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_features_data,
    Rcpp::String subset_key,
    std::string impact_col,
    Rcpp::IntegerVector features_to_use,
    Rcpp::IntegerVector actions_to_use,
    std::string internal_feature_col,
    double weight = 1.0,
    double weight_multiplier = 1.0,
    std::string block_name = "objective_add_min_intervention_impact",
    std::string tag = ""
) {
  if (Rf_isNull(x)) {
    Rcpp::stop("model_ptr is NULL.");
  }

  Rcpp::XPtr<OptimizationProblem> op =
    Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  if (!std::isfinite(weight) || weight < 0.0) {
    Rcpp::stop("weight must be finite and >= 0.");
  }
  if (!std::isfinite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  if (weight == 0.0 || weight_multiplier == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "weight==0 or weight_multiplier==0"
    );
  }

  // ---- checks: pu_data
  if (!pu_data.containsElementNamed("id")) {
    Rcpp::stop("pu_data must contain column 'id'.");
  }

  // ---- checks: dist_actions_data
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // ---- checks: dist_features_data
  for (auto nm : {"internal_pu"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }

  if (!dist_features_data.containsElementNamed(internal_feature_col.c_str())) {
    Rcpp::stop(
      "dist_features_data must contain internal feature column '" +
        internal_feature_col + "'."
    );
  }

  if (!dist_features_data.containsElementNamed(impact_col.c_str())) {
    Rcpp::stop(
      "dist_features_data must contain impact column '" +
        impact_col + "'."
    );
  }

  // ---- actions_to_use is required
  if (actions_to_use.size() <= 0) {
    Rcpp::stop("actions_to_use must contain at least one internal action id.");
  }

  // ---- determine n_pu
  if (op->_n_pu <= 0) op->_n_pu = pu_data.nrows();
  if (op->_n_pu <= 0) {
    Rcpp::stop("Cannot determine n_pu (op->_n_pu <= 0 and pu_data empty).");
  }
  const int n_pu = op->_n_pu;

  // ---- determine u block from MODEL
  // prepare() must have created one u_i per PU
  if (op->_n_u_intervention <= 0) {
    Rcpp::stop(
      "No u_intervention auxiliary variables found. "
      "Call rcpp_prepare_objective_min_intervention_impact() first."
    );
  }

  if (op->_n_u_intervention != n_pu) {
    Rcpp::stop(
      "u_intervention block size mismatch: expected n_pu auxiliaries."
    );
  }

  const int u0 = op->_u_intervention_offset;
  const int u1 = op->_u_intervention_offset + op->_n_u_intervention; // exclusive

  if (u0 < 0 || u1 > (int)op->_obj.size()) {
    Rcpp::stop(
      "u_intervention block out of bounds. "
      "Check prepare step and offsets."
    );
  }

  // ---- optional feature subset
  std::unordered_set<int> feat_subset;
  const bool use_feat_subset = (features_to_use.size() > 0);

  if (use_feat_subset) {
    feat_subset.reserve((std::size_t)features_to_use.size() * 2);
    for (int i = 0; i < features_to_use.size(); ++i) {
      const int f = features_to_use[i];
      if (f == NA_INTEGER || f <= 0) {
        Rcpp::stop("features_to_use must contain positive internal feature ids.");
      }
      feat_subset.insert(f);
    }
  }

  // ---- validate action subset
  std::unordered_set<int> act_subset;
  act_subset.reserve((std::size_t)actions_to_use.size() * 2);
  for (int i = 0; i < actions_to_use.size(); ++i) {
    const int a = actions_to_use[i];
    if (a == NA_INTEGER || a <= 0) {
      Rcpp::stop("actions_to_use must contain positive internal action ids.");
    }
    act_subset.insert(a);
  }

  // ---- read feature columns
  Rcpp::IntegerVector internal_pu = dist_features_data["internal_pu"];
  Rcpp::IntegerVector internal_feature = dist_features_data[internal_feature_col];
  Rcpp::NumericVector impact = dist_features_data[impact_col];

  const int n = dist_features_data.nrows();
  if (internal_pu.size() != n || internal_feature.size() != n || impact.size() != n) {
    Rcpp::stop("Column length mismatch in dist_features_data.");
  }

  // ---- aggregate impact by PU
  // this coefficient multiplies u_i
  std::vector<double> coef_u((std::size_t)n_pu, 0.0);

  for (int k = 0; k < n; ++k) {
    const int ipu = internal_pu[k];
    const int ifeat = internal_feature[k];
    const double val = impact[k];

    if (ipu == NA_INTEGER || ifeat == NA_INTEGER) continue;
    if (ipu <= 0 || ipu > n_pu) {
      Rcpp::stop("dist_features_data has internal_pu out of range.");
    }
    if (!std::isfinite(val) || val == 0.0) continue;

    if (use_feat_subset && feat_subset.find(ifeat) == feat_subset.end()) {
      continue;
    }

    coef_u[(std::size_t)(ipu - 1)] += val;
  }

  // ---- add to objective on u_i block
  const double scale = weight * weight_multiplier;
  int nnz_added = 0;
  double sum_added = 0.0;

  for (int i = 0; i < n_pu; ++i) {
    const double c = coef_u[(std::size_t)i];
    if (!std::isfinite(c) || c == 0.0) continue;

    const double coef = scale * c;
    op->_obj[(std::size_t)(u0 + i)] += coef;

    ++nnz_added;
    sum_added += coef;
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_add"
    ";component=min_intervention_impact"
    ";subset_key=" + std::string(subset_key) +
      ";impact_col=" + impact_col +
      ";internal_feature_col=" + internal_feature_col +
      ";weight=" + std::to_string(weight) +
      ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_pu=" + std::to_string(n_pu) +
      ";n_features_to_use=" + std::to_string((int)features_to_use.size()) +
      ";n_actions_to_use=" + std::to_string((int)actions_to_use.size()) +
      ";nnz_added=" + std::to_string(nnz_added) +
      ";sum_added=" + std::to_string(sum_added) +
      ";additive=TRUE";

  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)u0,
    (std::size_t)u1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("u_offset") = u0,
    Rcpp::Named("n_u") = op->_n_u_intervention,
    Rcpp::Named("nnz_added") = nnz_added,
    Rcpp::Named("sum_added") = sum_added
  );
}
