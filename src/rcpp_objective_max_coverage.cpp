#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <vector>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_objective_max_coverage(SEXP x,
                                       Rcpp::DataFrame pu_data,
                                       Rcpp::DataFrame features_data,
                                       Rcpp::DataFrame dist_features_data,
                                       Rcpp::DataFrame threats_data,
                                       Rcpp::DataFrame dist_threats_data,
                                       Rcpp::DataFrame boundary_data,
                                       double blm,
                                       int curve) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);
  op->_modelsense = "max";

  const int number_of_units = pu_data.nrows();
  const int boundary_size   = boundary_data.nrows();

  // --- Sanity: base vars must exist (w at least)
  if ((int)op->_obj.size() < op->_w_offset + number_of_units) {
    Rcpp::stop("rcpp_objective_max_coverage: w variables not built or size mismatch. "
                 "Call rcpp_add_base_variables() first and ensure n_pu matches pu_data.");
  }

  // ------------------------------------------------------------------------------------------
  // 1) Objective coefficients for w (already existing vars)
  // ------------------------------------------------------------------------------------------
  Rcpp::NumericVector connectivity_units(number_of_units);
  arma::sp_mat matrix_boundary_extended;

  if (boundary_size != 0) {
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);
  }

  if (boundary_size != 0 && blm != 0) {
    arma::sp_mat z = matrix_boundary_extended.t();
    for (arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if (it.row() != it.col()) {
        connectivity_units[it.col()] = connectivity_units[it.col()] + (*it);
      }
    }
  }

  // Set objective on existing w cols
  const std::size_t w0 = (std::size_t)op->_w_offset;
  const std::size_t w1 = w0 + (std::size_t)number_of_units;

  for (int i = 0; i < number_of_units; ++i) {
    const std::size_t col_w = (std::size_t)(op->_w_offset + i);
    // En tu código original w tenía coef negativo (penaliza conectividad)
    op->_obj[col_w] = -blm * (double)connectivity_units[i];
  }

  // Registry objective block for w
  {
    std::string tag = "blm=" + std::to_string(blm) + ";boundary_size=" + std::to_string(boundary_size);
    op->register_objective_block("obj_w_connectivity_penalty", w0, w1, tag);
  }

  // ------------------------------------------------------------------------------------------
  // 2) Objective coefficients for x (already existing OR legacy-created)
  // ------------------------------------------------------------------------------------------
  // threats_data: blm_actions
  if (!threats_data.containsElementNamed("blm_actions")) {
    Rcpp::stop("threats_data must contain column 'blm_actions'.");
  }
  Rcpp::NumericVector blm_actions = threats_data["blm_actions"];

  const int number_of_threats = threats_data.nrows();
  const int number_of_actions = dist_threats_data.nrows();

  // We'll compute connectivity_actions per row in dist_threats_data
  Rcpp::NumericVector connectivity_actions(number_of_actions);

  if (!dist_threats_data.containsElementNamed("internal_pu") ||
      !dist_threats_data.containsElementNamed("internal_threat")) {
      Rcpp::stop("dist_threats_data must contain columns 'internal_pu' and 'internal_threat'.");
  }

  Rcpp::IntegerVector dist_pu1 = dist_threats_data["internal_pu"];     // 1-based
  Rcpp::IntegerVector dist_thr = dist_threats_data["internal_threat"]; // 1-based

  // 0-based helpers
  Rcpp::IntegerVector pu1_0 = Rcpp::clone(dist_pu1);
  pu1_0 = pu1_0 - 1;
  Rcpp::IntegerVector thr_0 = Rcpp::clone(dist_thr);
  thr_0 = thr_0 - 1;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(
    dist_threats_data,
    number_of_units,
    number_of_threats,
    dist_threats_data["amount"]
  );

  // For each action-row a: compute connectivity_actions[a]
  for (int a = 0; a < number_of_actions; ++a) {
    if (boundary_size != 0 && blm_actions[thr_0[a]]) {

      int pu_id2_threat = 0;

      for (auto it = dist_threats_extended.begin_col(thr_0[a]);
           it != dist_threats_extended.end_col(thr_0[a]); ++it) {

        pu_id2_threat = it.row();

        if (pu1_0[a] != pu_id2_threat &&
            matrix_boundary_extended(pu1_0[a], pu_id2_threat) != 0) {

          connectivity_actions[a] = connectivity_actions[a] +
            matrix_boundary_extended(pu1_0[a], pu_id2_threat);
        }
      }
    }
  }

  // Map each dist_threats row -> x variable column
  // Prefer internal_row (recommended)
  bool has_internal_row = dist_threats_data.containsElementNamed("internal_row");

  arma::sp_mat actions_extended;
  if (!has_internal_row) {
    // Legacy fallback: uses actions_extended mapping and assumes x columns are:
    // w: 0..n_pu-1, x: n_pu..n_pu+n_x-1 in a very specific order
    actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);
  }

  // We'll touch objective coefficients for x on the fly and record min/max col touched
  std::size_t x_min = (std::size_t)op->_obj.size();
  std::size_t x_max = 0;

  for (int a = 0; a < number_of_actions; ++a) {
    const int thr_idx0 = thr_0[a];
    if (thr_idx0 < 0 || thr_idx0 >= number_of_threats) continue;

    // If blm_actions is 0 => coef 0 (still ok)
    const double coef = - (double)blm_actions[thr_idx0] * (double)connectivity_actions[a];

    int col_x = -1;

    if (has_internal_row) {
      Rcpp::IntegerVector irow = dist_threats_data["internal_row"]; // 1-based row in dist_actions model
      const int row0 = irow[a] - 1;
      if (row0 < 0) continue;
      col_x = op->_x_offset + row0;
    } else {
      // legacy: column = number_of_units + actions_extended(pu, threat) - 1
      const int pu0 = pu1_0[a];
      const int t0  = thr_0[a];
      const int idx = (int)actions_extended(pu0, t0);
      if (idx <= 0) continue;
      col_x = number_of_units + idx - 1; // matches your old build layout
    }

    if (col_x < 0 || col_x >= (int)op->_obj.size()) {
      Rcpp::stop("rcpp_objective_max_coverage: computed x col out of range. "
                   "Check that x vars exist and offsets/layout match.");
    }

    op->_obj[(std::size_t)col_x] = coef;

    x_min = std::min<std::size_t>(x_min, (std::size_t)col_x);
    x_max = std::max<std::size_t>(x_max, (std::size_t)col_x);
  }

  if (x_max >= x_min) {
    std::string tag = "mode=" + std::string(has_internal_row ? "internal_row" : "legacy_actions_extended") +
      ";n_actions=" + std::to_string(number_of_actions);
    op->register_objective_block("obj_x_connectivity_penalty", x_min, x_max + 1, tag);
  }

  // ------------------------------------------------------------------------------------------
  // 3) Create b variables (as in your legacy) and set objective depending on curve
  // ------------------------------------------------------------------------------------------
  const int number_of_features = features_data.nrows();
  arma::sp_mat dist_features_extended = create_dist_features_extended(
    dist_features_data, number_of_units, number_of_features
  );

  const std::size_t b_start = op->ncol_used();

  // For each nonzero (pu,feature) in dist_features_extended, add b var
  for (int s = 0; s < number_of_features; ++s) {
    for (auto it = dist_features_extended.begin_col(s);
         it != dist_features_extended.end_col(s); ++it) {

      const int pu_id = it.row();
      const double feature_intensity = dist_features_extended(pu_id, s);

      // objective: only if curve==1
      const double objcoef = (curve == 1 ? feature_intensity : 0.0);

      op->_obj.push_back(objcoef);
      op->_vtype.push_back("C");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);
    }
  }

  const std::size_t b_end = op->ncol_used();

  if (b_end > b_start) {
    std::string tag = "curve=" + std::to_string(curve) +
      ";n_features=" + std::to_string(number_of_features);
    op->register_variable_block("b_cov", b_start, b_end, tag);
    op->register_objective_block("obj_b_cov", b_start, b_end, tag);
  }

  // ------------------------------------------------------------------------------------------
  // 4) Create b' (pow vars) block (your legacy: only when curve != 1)
  // ------------------------------------------------------------------------------------------
  const std::size_t bpow_start = op->ncol_used();

  if (curve != 1) {
    for (int s = 0; s < number_of_features; ++s) {
      for (auto it = dist_features_extended.begin_col(s);
           it != dist_features_extended.end_col(s); ++it) {

        const double feature_intensity = dist_features_extended(it.row(), s);

        const int col = (int)op->_obj.size();

        op->_obj.push_back(feature_intensity);
        op->_vtype.push_back("C");
        op->_lb.push_back(0.0);
        op->_ub.push_back(1.0);

        op->_id_pow_variables.push_back((double)col);

        // En tu código antiguo metías un triplete con A_x=0 y una fila "row_constraint" que no avanzaba.
        // Eso no está añadiendo una constraint real. Lo omitimos aquí (no aporta).
      }
    }
  }

  const std::size_t bpow_end = op->ncol_used();

  if (bpow_end > bpow_start) {
    std::string tag = "curve=" + std::to_string(curve);
    op->register_variable_block("b_pow", bpow_start, bpow_end, tag);
    op->register_objective_block("obj_b_pow", bpow_start, bpow_end, tag);
  }

  // ------------------------------------------------------------------------------------------
  // 5) Add y variables (boundary aux) + constraints (3 per y)
  // ------------------------------------------------------------------------------------------
  const std::size_t y_start = op->ncol_used();
  int y_added = 0;

  std::size_t y_constr_bid = 0;
  std::size_t y_row_start = op->nrow_used();

  if (boundary_size != 0 && blm != 0) {

    const std::size_t bid = op->beginConstraintBlock(
      "y_boundary_linking",
      "blm=" + std::to_string(blm) + ";boundary_size=" + std::to_string(boundary_size)
    );
    y_constr_bid = bid;

    arma::sp_mat z = matrix_boundary_extended.t();
    for (arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if (it.row() == it.col()) continue;

      const int i = (int)it.row();
      const int j = (int)it.col();
      const double connectivityCoeff = (double)(*it);

      // create y var
      const int col_y = (int)op->_obj.size();
      op->_obj.push_back(blm * connectivityCoeff);  // positive in max
      op->_vtype.push_back("B");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);
      ++y_added;

      // Constraints:
      // (1) y - w_i <= 0
      op->addRow({col_y, op->_w_offset + i}, {1.0, -1.0}, "<=", 0.0, "y_le_wi");

      // (2) y - w_j <= 0
      op->addRow({col_y, op->_w_offset + j}, {1.0, -1.0}, "<=", 0.0, "y_le_wj");

      // (3) y - w_i - w_j >= -1
      op->addRow({col_y, op->_w_offset + i, op->_w_offset + j}, {1.0, -1.0, -1.0}, ">=", -1.0, "y_ge_wi_wj_minus1");
    }

    op->endConstraintBlock(bid);
  }

  const std::size_t y_end = op->ncol_used();
  const std::size_t y_row_end = op->nrow_used();

  if (y_end > y_start) {
    std::string tag = "n_y=" + std::to_string(y_added);
    op->register_variable_block("y_boundary", y_start, y_end, tag);
    op->register_objective_block("obj_y_boundary", y_start, y_end, tag);
  }

  // ------------------------------------------------------------------------------------------
  // 6) Add p variables (action-boundary aux) + constraints (3 per p)
  // ------------------------------------------------------------------------------------------
  const std::size_t p_start = op->ncol_used();
  int p_added = 0;

  if (number_of_actions > 0) {

    // Need actions_extended for legacy mapping in constraints too
    arma::sp_mat actions_ext_for_p;
    if (!has_internal_row) {
      actions_ext_for_p = actions_extended; // already created
    }

    const std::size_t bid = op->beginConstraintBlock(
      "p_action_linking",
      "n_actions=" + std::to_string(number_of_actions) + ";boundary_size=" + std::to_string(boundary_size)
    );

    // We need dist_threats_extended already and matrix_boundary_extended already if boundary_size>0
    for (int a = 0; a < number_of_actions; ++a) {
      const int thr_idx0 = thr_0[a];
      if (thr_idx0 < 0 || thr_idx0 >= number_of_threats) continue;
      if (boundary_size == 0) continue;
      if (blm_actions[thr_idx0] == 0) continue;

      int pu_id2_threat = 0;

      for (auto it = dist_threats_extended.begin_col(thr_idx0);
           it != dist_threats_extended.end_col(thr_idx0); ++it) {

        pu_id2_threat = it.row();

        if (pu1_0[a] == pu_id2_threat) continue;
        if (matrix_boundary_extended(pu1_0[a], pu_id2_threat) == 0) continue;

        const double connectivityCoeff = (double)matrix_boundary_extended(pu1_0[a], pu_id2_threat);

        // create p var
        const int col_p = (int)op->_obj.size();
        op->_obj.push_back((double)blm_actions[thr_idx0] * connectivityCoeff);
        op->_vtype.push_back("B");
        op->_lb.push_back(0.0);
        op->_ub.push_back(1.0);
        ++p_added;

        // Map X[i1,k] and X[i2,k]
        int col_x1 = -1;
        int col_x2 = -1;

        if (has_internal_row) {
          // If dist_threats_data rows correspond to x rows via internal_row,
          // we can use internal_row for row a for the "i1" action selection;
          // but for i2 we need the row of (pu_id2_threat, threat).
          // In that case you'd need a lookup table from (pu,threat)->internal_row.
          // Since your legacy code uses actions_extended to find that, we’ll keep that for i2.
          // Practical recommendation: ALWAYS build and pass internal_row for every (pu,threat) row
          // and create a map in R and pass it, but for now we do a safe legacy lookup.

          // For i1: internal_row on row a
          Rcpp::IntegerVector irow = dist_threats_data["internal_row"];
          const int row0_i1 = irow[a] - 1;
          if (row0_i1 < 0) continue;
          col_x1 = op->_x_offset + row0_i1;

          // For i2: legacy lookup
          arma::sp_mat ae = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);
          const int idx2 = (int)ae(pu_id2_threat, thr_idx0);
          if (idx2 <= 0) continue;
          col_x2 = number_of_units + idx2 - 1;

        } else {
          // legacy full
          const int idx1 = (int)actions_ext_for_p(pu1_0[a], thr_idx0);
          const int idx2 = (int)actions_ext_for_p(pu_id2_threat, thr_idx0);
          if (idx1 <= 0 || idx2 <= 0) continue;

          col_x1 = number_of_units + idx1 - 1;
          col_x2 = number_of_units + idx2 - 1;
        }

        if (col_x1 < 0 || col_x2 < 0 ||
            col_x1 >= (int)op->_obj.size() || col_x2 >= (int)op->_obj.size()) {
          Rcpp::stop("rcpp_objective_max_coverage: computed x cols for p constraints out of range.");
        }

        // Constraints:
        // (1) p - x1 <= 0
        op->addRow({col_p, col_x1}, {1.0, -1.0}, "<=", 0.0, "p_le_x1");
        // (2) p - x2 <= 0
        op->addRow({col_p, col_x2}, {1.0, -1.0}, "<=", 0.0, "p_le_x2");
        // (3) p - x1 - x2 >= -1
        op->addRow({col_p, col_x1, col_x2}, {1.0, -1.0, -1.0}, ">=", -1.0, "p_ge_x1_x2_minus1");
      }
    }

    op->endConstraintBlock(bid);
  }

  const std::size_t p_end = op->ncol_used();
  if (p_end > p_start) {
    std::string tag = "n_p=" + std::to_string(p_added);
    op->register_variable_block("p_action_boundary", p_start, p_end, tag);
    op->register_objective_block("obj_p_action_boundary", p_start, p_end, tag);
  }

  // Final: return a summary + (optional) block sizes
  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("w_obj_range") = Rcpp::NumericVector::create((double)(w0 + 1), (double)w1),
    Rcpp::Named("x_obj_touched") = Rcpp::LogicalVector::create(x_max >= x_min),
    Rcpp::Named("b_added") = (double)(b_end - b_start),
    Rcpp::Named("b_pow_added") = (double)(bpow_end - bpow_start),
    Rcpp::Named("y_added") = y_added,
    Rcpp::Named("p_added") = p_added,
    Rcpp::Named("n_constraints") = (double)op->nrow_used()
  );
}
