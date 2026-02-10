#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_add_base_variables(SEXP x,
                                   Rcpp::DataFrame pu_data,
                                   Rcpp::DataFrame dist_actions_data,
                                   Rcpp::DataFrame dist_features_data,
                                   bool add_z = true) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  const int n_pu = pu_data.nrows();
  const int n_x  = dist_actions_data.nrows();
  const int n_z  = add_z ? dist_features_data.nrows() : 0;

  op->_n_pu = n_pu;
  op->_n_x  = n_x;
  op->_n_z  = n_z;

  op->_w_offset = 0;
  op->_x_offset = n_pu;
  op->_z_offset = n_pu + n_x;

  // FULL RESET: esqueleto base desde cero
  op->_modelsense.clear();

  op->_obj.clear();   op->_vtype.clear();
  op->_lb.clear();    op->_ub.clear();

  op->_A_i.clear(); op->_A_j.clear(); op->_A_x.clear();
  op->_rhs.clear(); op->_sense.clear();
  op->_name.clear();

  // ---- NEW: reset auxiliary blocks (VERY IMPORTANT)
  op->_n_y_pu = 0;
  op->_n_y_action = 0;
  op->_n_y_intervention = 0;
  op->_n_u_intervention = 0;

  op->_y_pu_offset = -1;
  op->_y_action_offset = -1;
  op->_y_intervention_offset = -1;
  op->_u_intervention_offset = -1;

  op->_boundary_size = 0; // optional, if you use it as cache


  // IMPORTANT: en tu OptimizationProblem.h esto también limpia _active_blocks
  op->clear_registry();

  const std::size_t n_total = static_cast<std::size_t>(n_pu + n_x + n_z);

  op->_obj.reserve(n_total);
  op->_vtype.reserve(n_total);
  op->_lb.reserve(n_total);
  op->_ub.reserve(n_total);

  // w
  for (int i = 0; i < n_pu; ++i) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // x
  for (int r = 0; r < n_x; ++r) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // z
  for (int t = 0; t < n_z; ++t) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // Register variable blocks (0-based half-open)
  const std::size_t w0 = static_cast<std::size_t>(op->_w_offset);
  const std::size_t w1 = w0 + static_cast<std::size_t>(n_pu);

  const std::size_t x0 = static_cast<std::size_t>(op->_x_offset);
  const std::size_t x1 = x0 + static_cast<std::size_t>(n_x);

  const std::size_t z0 = static_cast<std::size_t>(op->_z_offset);
  const std::size_t z1 = z0 + static_cast<std::size_t>(n_z);

  // tags útiles para debugging / wrappers MO
  const std::string tag_base =
    "n_pu=" + std::to_string(n_pu) +
    ";n_x="  + std::to_string(n_x) +
    ";n_z="  + std::to_string(n_z);

  const std::size_t w_block_id = op->register_variable_block("w", w0, w1, tag_base);
  const std::size_t x_block_id = op->register_variable_block("x", x0, x1, tag_base);

  std::size_t z_block_id = 0;
  if (n_z > 0) z_block_id = op->register_variable_block("z", z0, z1, tag_base);

  // Return 1-based indices
  Rcpp::IntegerVector w_index(n_pu), x_index(n_x), z_index(n_z);
  for (int i = 0; i < n_pu; ++i) w_index[i] = op->_w_offset + i + 1;
  for (int r = 0; r < n_x;  ++r) x_index[r] = op->_x_offset + r + 1;
  for (int t = 0; t < n_z;  ++t) z_index[t] = op->_z_offset + t + 1;

  // Safer ranges: NumericVector (avoids int overflow)
  auto range_to_R = [](std::size_t start0, std::size_t end0) {
    if (end0 <= start0) return Rcpp::NumericVector::create(NA_REAL, NA_REAL);
    // 1-based, inclusive-friendly: [start0+1, end0]
    return Rcpp::NumericVector::create(
      static_cast<double>(start0 + 1),
      static_cast<double>(end0)
    );
  };

  return Rcpp::List::create(
    Rcpp::Named("w_index") = w_index,
    Rcpp::Named("x_index") = x_index,
    Rcpp::Named("z_index") = z_index,

    Rcpp::Named("w_range") = range_to_R(w0, w1),
    Rcpp::Named("x_range") = range_to_R(x0, x1),
    Rcpp::Named("z_range") = range_to_R(z0, z1),

    Rcpp::Named("w_block_id") = static_cast<double>(w_block_id),
    Rcpp::Named("x_block_id") = static_cast<double>(x_block_id),
    Rcpp::Named("z_block_id") = (z_block_id == 0 ? NA_REAL : static_cast<double>(z_block_id))
  );
}
