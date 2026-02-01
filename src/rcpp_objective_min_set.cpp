#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <vector>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_objective_min_set(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame threats_data,
    Rcpp::DataFrame dist_threats_data,
    Rcpp::DataFrame boundary_data,
    double blm,
    int curve, // (lo dejamos por compat; aquí no se usa si no hay b/b' vars)
    std::string block_name = "objective_min_set",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // ------------------------------------------------------------
  // Checks básicos (pipeline nuevo: variables ya existen)
  // ------------------------------------------------------------
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }
  if (op->_n_pu <= 0) {
    // fallback: infer from pu_data
    op->_n_pu = pu_data.nrows();
  }
  if (op->_n_x <= 0) {
    // fallback: infer from dist_threats_data rows (si x representa acciones)
    op->_n_x = dist_threats_data.nrows();
  }

  const int n_pu = pu_data.nrows();
  const int n_actions = dist_threats_data.nrows();
  const int boundary_size = boundary_data.nrows();

  if (n_pu != op->_n_pu) {
    Rcpp::stop("pu_data.nrows() != op->_n_pu. Ensure consistent build order / inputs.");
  }
  if (n_actions > 0 && (op->_x_offset + n_actions) > (int)op->_obj.size()) {
    Rcpp::stop("x block out of bounds. Ensure x variables were created and offsets are correct.");
  }

  if (!pu_data.containsElementNamed("monitoring_cost")) {
    Rcpp::stop("pu_data must contain column 'monitoring_cost'.");
  }
  if (!threats_data.containsElementNamed("blm_actions")) {
    Rcpp::stop("threats_data must contain column 'blm_actions'.");
  }
  for (auto nm : {"internal_pu", "internal_threat", "action_cost", "amount"}) {
    if (!dist_threats_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_threats_data must contain column '") + nm + "'.");
    }
  }

  // ------------------------------------------------------------
  // Set modelsense + reset objective vector
  // ------------------------------------------------------------
  op->_modelsense = "min";
  for (std::size_t j = 0; j < op->_obj.size(); ++j) op->_obj[j] = 0.0;

  // ------------------------------------------------------------
  // Compute boundary connectivity terms for w and x
  // (reusa tus helpers existentes)
  // ------------------------------------------------------------
  Rcpp::NumericVector unit_costs = pu_data["monitoring_cost"];
  Rcpp::NumericVector blm_actions = threats_data["blm_actions"];

  Rcpp::IntegerVector dist_pu_id = dist_threats_data["internal_pu"];      // 1-based
  Rcpp::IntegerVector dist_thr_id = dist_threats_data["internal_threat"]; // 1-based
  Rcpp::NumericVector action_costs = dist_threats_data["action_cost"];

  // Build boundary matrix extended if available
  arma::sp_mat matrix_boundary_extended;
  if (boundary_size != 0) {
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, n_pu);
  }

  // ---- connectivity for units (w)
  Rcpp::NumericVector connectivity_units(n_pu);
  if (boundary_size != 0 && blm != 0) {
    arma::sp_mat z = matrix_boundary_extended.t();
    for (arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if (it.row() != it.col()) {
        connectivity_units[it.col()] = connectivity_units[it.col()] + (*it);
      }
    }
  }

  // ---- connectivity for actions (x): depends on threat-specific blm_actions
  Rcpp::NumericVector connectivity_actions(n_actions);

  // dist_threats_extended: (pu x threat) sparse amounts (uses your helper)
  const int n_threats = threats_data.nrows();
  arma::sp_mat dist_threats_extended = create_dist_threats_extended(
    dist_threats_data, n_pu, n_threats, dist_threats_data["amount"]
  );

  // Precompute 0-based ids for speed
  Rcpp::IntegerVector pu0 = Rcpp::clone(dist_pu_id);  pu0 = pu0 - 1;
  Rcpp::IntegerVector thr0 = Rcpp::clone(dist_thr_id); thr0 = thr0 - 1;

  for (int a = 0; a < n_actions; ++a) {
    const int ipu = pu0[a];
    const int ith = thr0[a];
    if (ipu < 0 || ith < 0) continue;

    // if threat has blm_actions != 0 and boundary exists
    const double blm_a = (ith < blm_actions.size()) ? (double)blm_actions[ith] : 0.0;
    if (boundary_size != 0 && blm_a != 0.0) {

      for (auto it = dist_threats_extended.begin_col(ith);
           it != dist_threats_extended.end_col(ith); ++it) {

        const int pu2 = it.row();
        if (pu2 == ipu) continue;

        const double bnd = matrix_boundary_extended(ipu, pu2);
        if (bnd != 0.0) {
          connectivity_actions[a] = connectivity_actions[a] + bnd;
        }
      }
    }
  }

  // ------------------------------------------------------------
  // Write objective coefficients to existing w and x vars
  // w_i: unit_cost + blm * connectivity_units
  // x_a: action_cost + blm_actions[threat] * connectivity_actions[a]
  // ------------------------------------------------------------
  int n_w_nonzero = 0;
  int n_x_nonzero = 0;
  double sum_w = 0.0;
  double sum_x = 0.0;

  // w block range
  const std::size_t w0 = (std::size_t)op->_w_offset;
  const std::size_t w1 = w0 + (std::size_t)n_pu;

  for (int i = 0; i < n_pu; ++i) {
    const double c = (double)unit_costs[i];
    const double conn = (double)connectivity_units[i];
    const double coef = c + blm * conn;

    op->_obj[op->_w_offset + i] = coef;

    if (coef != 0.0 && std::isfinite(coef)) {
      ++n_w_nonzero;
      sum_w += coef;
    }
  }

  // x block range
  const std::size_t x0 = (std::size_t)op->_x_offset;
  const std::size_t x1 = x0 + (std::size_t)n_actions;

  for (int a = 0; a < n_actions; ++a) {
    const int ith = thr0[a];
    const double c = (double)action_costs[a];
    const double blm_a = (ith >= 0 && ith < blm_actions.size()) ? (double)blm_actions[ith] : 0.0;
    const double conn = (double)connectivity_actions[a];
    const double coef = c + blm_a * conn;

    op->_obj[op->_x_offset + a] = coef;

    if (coef != 0.0 && std::isfinite(coef)) {
      ++n_x_nonzero;
      sum_x += coef;
    }
  }

  // ------------------------------------------------------------
  // Registry: since OptimizationProblem.h has no objective block,
  // we register as variable blocks with tag kind=objective
  // ------------------------------------------------------------
  std::string base_tag = tag;
  if (!base_tag.empty()) base_tag += ";";
  base_tag += "kind=objective"
  ";modelsense=min"
  ";blm=" + std::to_string(blm) +
    ";curve=" + std::to_string(curve) +
    ";boundary_size=" + std::to_string(boundary_size);

  const std::size_t w_block_id = op->register_variable_block(
    block_name + "::w", w0, w1,
    base_tag +
      ";n_nonzero=" + std::to_string(n_w_nonzero) +
      ";sum=" + std::to_string(sum_w)
  );

  const std::size_t x_block_id = op->register_variable_block(
    block_name + "::x", x0, x1,
    base_tag +
      ";n_nonzero=" + std::to_string(n_x_nonzero) +
      ";sum=" + std::to_string(sum_x)
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("w_block_id") = (double)w_block_id,
    Rcpp::Named("x_block_id") = (double)x_block_id,
    Rcpp::Named("w_range") = Rcpp::NumericVector::create((double)(w0 + 1), (double)w1),
    Rcpp::Named("x_range") = Rcpp::NumericVector::create((double)(x0 + 1), (double)x1),
    Rcpp::Named("n_w_nonzero") = n_w_nonzero,
    Rcpp::Named("n_x_nonzero") = n_x_nonzero,
    Rcpp::Named("sum_w") = sum_w,
    Rcpp::Named("sum_x") = sum_x,
    Rcpp::Named("note") =
      "Objective written into existing w/x variables. "
      "No b/b'/y/p variables are created here (pipeline-new). "
      "Objective registry stored via register_variable_block(kind=objective tag)."
  );
}
