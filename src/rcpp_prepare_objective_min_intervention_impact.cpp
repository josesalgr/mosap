#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_objective_min_intervention_impact(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_features_data,
    std::string subset_key,
    std::string impact_col,
    Rcpp::IntegerVector features_to_use,
    Rcpp::IntegerVector actions_to_use,
    std::string internal_feature_col = "internal_feature",
    std::string block_name = "objective_min_intervention_impact",
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

  // ---- validate actions_to_use
  if (actions_to_use.size() <= 0) {
    Rcpp::stop("actions_to_use must contain at least one positive internal action id.");
  }

  std::unordered_set<int> act_subset;
  act_subset.reserve((std::size_t)actions_to_use.size() * 2);

  for (int i = 0; i < actions_to_use.size(); ++i) {
    const int a = actions_to_use[i];
    if (a == NA_INTEGER || a <= 0) {
      Rcpp::stop("actions_to_use must contain positive internal action ids.");
    }
    act_subset.insert(a);
  }

  // ---- validate features_to_use if provided
  if (features_to_use.size() > 0) {
    for (int i = 0; i < features_to_use.size(); ++i) {
      const int v = features_to_use[i];
      if (v == NA_INTEGER || v <= 0) {
        Rcpp::stop("features_to_use must contain positive internal feature ids.");
      }
    }
  }

  // ---- determine n_pu
  if (op->_n_pu <= 0) op->_n_pu = pu_data.nrows();
  if (op->_n_pu <= 0) {
    Rcpp::stop("Cannot determine n_pu (op->_n_pu <= 0 and pu_data empty).");
  }
  const int n_pu = op->_n_pu;

  // ------------------------------------------------------------------
  // CREATE u_intervention block if not yet created
  // one binary u_i per PU
  // ------------------------------------------------------------------
  bool created_u_block = false;

  if (op->_n_u_intervention <= 0) {
    op->_u_intervention_offset = (int)op->_obj.size();
    op->_n_u_intervention = n_pu;

    for (int i = 0; i < n_pu; ++i) {
      op->_obj.push_back(0.0);
      op->_vtype.push_back("B");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);
      op->_id_variables.push_back((double)(op->_u_intervention_offset + i + 1));
      op->_id_pow_variables.push_back(1.0);
    }

    created_u_block = true;
  }

  const int u0 = op->_u_intervention_offset;
  const int u1 = op->_u_intervention_offset + op->_n_u_intervention;

  if (op->_n_u_intervention != n_pu) {
    Rcpp::stop("u_intervention block exists but its size does not match n_pu.");
  }

  if (u0 < 0 || u1 > (int)op->_obj.size()) {
    Rcpp::stop(
      "u_intervention block out of bounds. Check _u_intervention_offset/_n_u_intervention."
    );
  }

  // ------------------------------------------------------------------
  // Build PU -> x indices for selected actions
  // ------------------------------------------------------------------
  Rcpp::IntegerVector da_pu = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_action = dist_actions_data["internal_action"];

  const int n_da = dist_actions_data.nrows();
  if (da_pu.size() != n_da || da_action.size() != n_da) {
    Rcpp::stop("Column length mismatch in dist_actions_data.");
  }

  if (op->_n_x <= 0) {
    op->_n_x = n_da;
  }
  if (op->_n_x != n_da) {
    Rcpp::stop("dist_actions_data row count must match op->_n_x.");
  }

  const int x0 = op->_x_offset;
  const int x1 = op->_x_offset + op->_n_x;
  if (x0 < 0 || x1 > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds. Check _x_offset/_n_x.");
  }

  std::vector< std::vector<int> > pu_to_x((std::size_t)n_pu);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_pu[r];
    const int ia  = da_action[r];

    if (ipu == NA_INTEGER || ia == NA_INTEGER) continue;
    if (ipu <= 0 || ipu > n_pu) {
      Rcpp::stop("dist_actions_data has internal_pu out of range.");
    }

    if (act_subset.find(ia) == act_subset.end()) continue;

    pu_to_x[(std::size_t)(ipu - 1)].push_back(x0 + r);
  }

  // ------------------------------------------------------------------
  // Add linking constraints:
  //   x_ia <= u_i            for all selected actions
  //   u_i <= sum_a x_ia      for PUs having at least one selected action
  // This makes u_i = 1 iff some action in subset is selected.
  // ------------------------------------------------------------------
  const std::size_t cons_block_id = op->beginConstraintBlock(
    block_name + "::link_u",
    "subset_key=" + subset_key
  );

  int n_link_rows = 0;

  for (int i = 0; i < n_pu; ++i) {
    const int ui = u0 + i;
    const std::vector<int>& xs = pu_to_x[(std::size_t)i];

    if (xs.empty()) continue;

    // x_ia - u_i <= 0
    for (std::size_t k = 0; k < xs.size(); ++k) {
      std::vector<int> cols;
      std::vector<double> vals;

      cols.push_back(xs[k]);
      vals.push_back(1.0);

      cols.push_back(ui);
      vals.push_back(-1.0);

      op->addRow(cols, vals, "<=", 0.0, block_name + "::x_le_u");
      ++n_link_rows;
    }

    // u_i - sum_a x_ia <= 0
    {
      std::vector<int> cols;
      std::vector<double> vals;

      cols.push_back(ui);
      vals.push_back(1.0);

      for (std::size_t k = 0; k < xs.size(); ++k) {
        cols.push_back(xs[k]);
        vals.push_back(-1.0);
      }

      op->addRow(cols, vals, "<=", 0.0, block_name + "::u_le_sumx");
      ++n_link_rows;
    }
  }

  op->endConstraintBlock(cons_block_id, true);

  // ------------------------------------------------------------------
  // registry block (no objective modification)
  // ------------------------------------------------------------------
  std::string base_tag = tag;
  if (!base_tag.empty()) base_tag += ";";
  base_tag +=
    "subset_key=" + subset_key +
    ";impact_col=" + impact_col +
    ";internal_feature_col=" + internal_feature_col +
    ";n_features_to_use=" + std::to_string((int)features_to_use.size()) +
    ";n_actions_to_use=" + std::to_string((int)actions_to_use.size()) +
    ";created_u_block=" + std::string(created_u_block ? "TRUE" : "FALSE") +
      ";prepare_only=TRUE";

  const std::size_t u_block_id = op->register_objective_block(
    block_name + "::u",
    (std::size_t)u0,
    (std::size_t)u1,
    base_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("u_block_id") = (double)u_block_id,
    Rcpp::Named("u_range") = Rcpp::NumericVector::create((double)u0 + 1.0, (double)u1),
    Rcpp::Named("constraint_block_id") = (double)cons_block_id,
    Rcpp::Named("impact_col") = impact_col,
    Rcpp::Named("internal_feature_col") = internal_feature_col,
    Rcpp::Named("n_features_to_use") = features_to_use.size(),
    Rcpp::Named("n_actions_to_use") = actions_to_use.size(),
    Rcpp::Named("n_link_rows") = n_link_rows,
    Rcpp::Named("created_u_block") = created_u_block,
    Rcpp::Named("note") = "Prepared u_intervention block and linking constraints (no changes to op->_obj)."
  );
}
