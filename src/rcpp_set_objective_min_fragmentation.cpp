#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

inline void add_row_triplets(
    OptimizationProblem* op,
    const std::string& sense,
    double rhs,
    const std::vector<int>& cols,
    const std::vector<double>& vals
) {
  const int row = static_cast<int>(op->_rhs.size());  // next row index (0-based)
  op->_sense.push_back(sense);
  op->_rhs.push_back(rhs);

  for (size_t k = 0; k < cols.size(); ++k) {
    op->_A_i.push_back(row);
    op->_A_j.push_back(cols[k]);
    op->_A_x.push_back(vals[k]);
  }
}


// [[Rcpp::export]]
bool rcpp_set_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame pu_data,
    Rcpp::DataFrame relation_data,
    double weight_multiplier = 1.0
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (n_pu <= 0).");

  if (!relation_data.containsElementNamed("internal_pu1") ||
      !relation_data.containsElementNamed("internal_pu2") ||
      !relation_data.containsElementNamed("weight")) {
      Rcpp::stop("relation_data must contain columns: internal_pu1, internal_pu2, weight.");
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector wgt = relation_data["weight"];

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // y variables appended AFTER base variables
  // current number of variables = op->_obj.size()
  op->_y_pu_offset = static_cast<int>(op->_obj.size());
  op->_n_y_pu = m;

  op->_obj.reserve(op->_obj.size() + m);
  op->_vtype.reserve(op->_vtype.size() + m);
  op->_lb.reserve(op->_lb.size() + m);
  op->_ub.reserve(op->_ub.size() + m);

  for (int e = 0; e < m; ++e) {

    int i = ip1[e] - 1; // incoming is 1..n in R
    int j = ip2[e] - 1;

    if (i < 0 || i >= n_pu || j < 0 || j >= n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    double we = wgt[e];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    // add y_e variable
    const int y_col = op->_y_pu_offset + e; // 0-based variable index
    op->_obj.push_back(weight_multiplier * we);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);

    // columns for w_i, w_j, y_e
    const int wi = op->_w_offset + i;
    const int wj = op->_w_offset + j;

    // 1) y - w_i + w_j >= 0
    add_row_triplets(
      op.get(),
      ">=",
      0.0,
      { y_col, wi, wj },
      {  1.0, -1.0,  1.0 }
    );

    // 2) y + w_i - w_j >= 0
    add_row_triplets(
      op.get(),
      ">=",
      0.0,
      { y_col, wi, wj },
      {  1.0,  1.0, -1.0 }
    );

    // 3) y - w_i - w_j <= 0
    add_row_triplets(
      op.get(),
      "<=",
      0.0,
      { y_col, wi, wj },
      {  1.0, -1.0, -1.0 }
    );

    // 4) y + w_i + w_j <= 2
    add_row_triplets(
      op.get(),
      "<=",
      2.0,
      { y_col, wi, wj },
      {  1.0,  1.0,  1.0 }
    );
  }

  return true;
}
