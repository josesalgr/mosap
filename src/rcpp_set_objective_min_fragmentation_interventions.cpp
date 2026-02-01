// rcpp_set_objective_min_fragmentation_interventions.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

inline void add_row_triplets(
    OptimizationProblem* op,
    const std::string& sense,
    double rhs,
    const std::vector<int>& cols,     // 0-based var indices
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

/*
 Required fields to add in OptimizationProblem (e.g., OptimizationProblem.h):
 int _u_intervention_offset = 0;
 int _n_u_intervention      = 0;

 int _y_intervention_offset = 0;
 int _n_y_intervention      = 0;     // (this corresponds to your "_n_y_intervetions")

 And ensure they get reset to 0 in any reset/constructor you use.
 */

// [[Rcpp::export]]
bool rcpp_set_objective_min_fragmentation_interventions(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight
    double weight_multiplier = 1.0
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but n_x <= 0.");

  if (op->_n_u_intervention > 0 || op->_n_y_intervention > 0) {
    Rcpp::stop("Intervention-fragmentation auxiliaries already exist in the model.");
  }

  if (!relation_data.containsElementNamed("internal_pu1") ||
      !relation_data.containsElementNamed("internal_pu2") ||
      !relation_data.containsElementNamed("weight")) {
      Rcpp::stop("relation_data must contain columns: internal_pu1, internal_pu2, weight.");
  }
  if (!dist_actions_data.containsElementNamed("internal_row") ||
      !dist_actions_data.containsElementNamed("internal_pu")) {
      Rcpp::stop("dist_actions_data must contain columns: internal_row, internal_pu.");
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  // --- relation
  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // --- dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"]; // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];  // 1..n_pu
  const int da_n = dist_actions_data.nrows();
  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // --- Build list of x columns per PU
  std::vector< std::vector<int> > xcols_by_pu(n_pu + 1); // 1..n_pu

  for (int r = 0; r < da_n; ++r) {
    const int row = dar[r];
    const int i   = dap[r];

    if (row < 1 || row > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (i   < 1 || i   > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");

    const int x_col = op->_x_offset + (row - 1);
    xcols_by_pu[i].push_back(x_col);
  }

  // ------------------------------------------------------------
  // 1) Add u_i vars (intervention indicator per PU)
  // ------------------------------------------------------------
  op->_u_intervention_offset = static_cast<int>(op->_obj.size());
  op->_n_u_intervention      = n_pu;

  op->_obj.reserve(op->_obj.size() + n_pu);
  op->_vtype.reserve(op->_vtype.size() + n_pu);
  op->_lb.reserve(op->_lb.size() + n_pu);
  op->_ub.reserve(op->_ub.size() + n_pu);

  for (int i = 1; i <= n_pu; ++i) {
    op->_obj.push_back(0.0);   // u has no direct objective weight; fragmentation uses y on edges
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // ------------------------------------------------------------
  // 2) Link u_i with x vars:
  //    (a) x_r <= u_i for each action-row r in PU i
  //        u_i - x_r >= 0
  //    (b) u_i <= sum_{r in PU i} x_r  (if PU has at least one feasible action)
  //        -u_i + sum x_r >= 0
  //        If PU has zero actions: force u_i = 0 via u_i <= 0
  // ------------------------------------------------------------
  for (int i = 1; i <= n_pu; ++i) {
    const int u_col = op->_u_intervention_offset + (i - 1); // 0-based var index

    const auto& xs = xcols_by_pu[i];

    if (xs.empty()) {
      // u_i <= 0
      add_row_triplets(op.get(), "<=", 0.0, { u_col }, { 1.0 });
      continue;
    }

    // (a) for each x in PU: u - x >= 0
    for (int xc : xs) {
      add_row_triplets(op.get(), ">=", 0.0, { u_col, xc }, { 1.0, -1.0 });
    }

    // (b) -u + sum x >= 0
    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(1 + xs.size());
    vals.reserve(1 + xs.size());

    cols.push_back(u_col);
    vals.push_back(-1.0);
    for (int xc : xs) {
      cols.push_back(xc);
      vals.push_back(1.0);
    }
    add_row_triplets(op.get(), ">=", 0.0, cols, vals);
  }

  // ------------------------------------------------------------
  // 3) Add y_e vars for edges and link to u_i, u_j:
  //    y >= u_i - u_j
  //    y >= u_j - u_i
  // ------------------------------------------------------------
  op->_y_intervention_offset = static_cast<int>(op->_obj.size());
  op->_n_y_intervention      = m;

  op->_obj.reserve(op->_obj.size() + m);
  op->_vtype.reserve(op->_vtype.size() + m);
  op->_lb.reserve(op->_lb.size() + m);
  op->_ub.reserve(op->_ub.size() + m);

  for (int e = 0; e < m; ++e) {
    int i = ip1[e]; // 1..n_pu
    int j = ip2[e];

    if (i < 1 || i > n_pu || j < 1 || j > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    double we = wgt[e];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    const int y_col = op->_y_intervention_offset + e;

    op->_obj.push_back(weight_multiplier * we);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);

    const int ui = op->_u_intervention_offset + (i - 1);
    const int uj = op->_u_intervention_offset + (j - 1);

    // y - u_i + u_j >= 0
    add_row_triplets(op.get(), ">=", 0.0, { y_col, ui, uj }, { 1.0, -1.0,  1.0 });
    // y + u_i - u_j >= 0
    add_row_triplets(op.get(), ">=", 0.0, { y_col, ui, uj }, { 1.0,  1.0, -1.0 });
  }

  return true;
}
