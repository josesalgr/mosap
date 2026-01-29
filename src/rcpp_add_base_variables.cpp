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

  // reset containers
  op->_obj.clear();   op->_vtype.clear();
  op->_lb.clear();    op->_ub.clear();

  op->_obj.reserve(n_pu + n_x + n_z);
  op->_vtype.reserve(n_pu + n_x + n_z);
  op->_lb.reserve(n_pu + n_x + n_z);
  op->_ub.reserve(n_pu + n_x + n_z);

  // w_i (PU selected)
  for (int i = 0; i < n_pu; ++i) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // x_row (one var per feasible (pu,action) row)
  for (int r = 0; r < n_x; ++r) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // z_row (one var per (pu,feature) row in dist_features; baseline conservation indicator)
  for (int t = 0; t < n_z; ++t) {
    op->_obj.push_back(0.0);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // Return 1-based indices (debug-friendly in R)
  Rcpp::IntegerVector w_index(n_pu), x_index(n_x), z_index(n_z);
  for (int i = 0; i < n_pu; ++i) w_index[i] = op->_w_offset + i + 1;
  for (int r = 0; r < n_x;  ++r) x_index[r] = op->_x_offset + r + 1;
  for (int t = 0; t < n_z;  ++t) z_index[t] = op->_z_offset + t + 1;

  return Rcpp::List::create(
    Rcpp::Named("w_index") = w_index,
    Rcpp::Named("x_index") = x_index,
    Rcpp::Named("z_index") = z_index
  );
}
