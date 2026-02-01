// rcpp_set_objective_min_fragmentation_actions.cpp
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
 int _y_action_offset = 0;
 int _n_y_action      = 0;

 And ensure they get reset to 0 in any reset/constructor you use.
 */

// [[Rcpp::export]]
bool rcpp_set_objective_min_fragmentation_actions_by_action(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu, internal_action
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight
    Rcpp::Nullable<Rcpp::IntegerVector> actions_to_use = R_NilValue, // optional internal_action ids (1-based)
    Rcpp::Nullable<Rcpp::NumericVector> action_weights = R_NilValue, // optional weights (len n_actions or len(actions_to_use))
    double weight_multiplier = 1.0
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but n_x <= 0.");

  if (op->_n_y_action > 0) {
    Rcpp::stop("Action-fragmentation auxiliaries already exist in the model (_n_y_action > 0).");
  }

  if (!relation_data.containsElementNamed("internal_pu1") ||
      !relation_data.containsElementNamed("internal_pu2") ||
      !relation_data.containsElementNamed("weight")) {
      Rcpp::stop("relation_data must contain columns: internal_pu1, internal_pu2, weight.");
  }
  if (!dist_actions_data.containsElementNamed("internal_row") ||
      !dist_actions_data.containsElementNamed("internal_pu") ||
      !dist_actions_data.containsElementNamed("internal_action")) {
      Rcpp::stop("dist_actions_data must contain columns: internal_row, internal_pu, internal_action.");
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  // --- read relation
  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // --- read dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"];     // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];      // 1..n_pu
  Rcpp::IntegerVector daa = dist_actions_data["internal_action"];  // 1..n_actions
  const int da_n = dist_actions_data.nrows();
  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // --- detect number of actions (max internal_action)
  int n_actions = 0;
  for (int r = 0; r < da_n; ++r) {
    if (daa[r] > n_actions) n_actions = daa[r];
  }
  if (n_actions <= 0) Rcpp::stop("Could not infer number of actions (max internal_action <= 0).");

  // --- choose actions to use
  std::vector<int> A; // internal_action ids (1-based)
  if (actions_to_use.isNotNull()) {
    Rcpp::IntegerVector tmp(actions_to_use);
    if (tmp.size() == 0) Rcpp::stop("actions_to_use provided but empty.");
    A.reserve(tmp.size());
    for (int k = 0; k < tmp.size(); ++k) {
      int a = tmp[k];
      if (a < 1 || a > n_actions) Rcpp::stop("actions_to_use contains invalid internal_action id.");
      A.push_back(a);
    }
  } else {
    A.reserve(n_actions);
    for (int a = 1; a <= n_actions; ++a) A.push_back(a);
  }

  // --- action weights
  std::vector<double> wA(n_actions + 1, 1.0); // 1-based index
  if (action_weights.isNotNull()) {
    Rcpp::NumericVector aw(action_weights);

    if (aw.size() == n_actions) {
      for (int a = 1; a <= n_actions; ++a) {
        double v = aw[a - 1];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[a] = v;
      }
    } else if ((int)aw.size() == (int)A.size()) {
      for (int k = 0; k < (int)A.size(); ++k) {
        double v = aw[k];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[A[k]] = v;
      }
    } else {
      Rcpp::stop("action_weights length must be either n_actions or length(actions_to_use).");
    }
  }

  // --- map (pu, action) -> x_col (0-based var index), -1 if infeasible/missing
  std::vector<int> xcol_map((n_actions + 1) * (n_pu + 1), -1); // index = a*(n_pu+1) + i

  for (int r = 0; r < da_n; ++r) {
    int row = dar[r];            // 1..n_x
    int i   = dap[r];            // 1..n_pu
    int a   = daa[r];            // 1..n_actions

    if (row < 1 || row > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (i   < 1 || i   > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");
    if (a   < 1 || a   > n_actions) Rcpp::stop("dist_actions_data$internal_action out of range.");

    const int x_col = op->_x_offset + (row - 1); // 0-based var index for x_row
    xcol_map[a * (n_pu + 1) + i] = x_col;
  }

  // --- append y_{e,a} variables
  const int mA = m * (int)A.size();

  op->_y_action_offset = static_cast<int>(op->_obj.size());
  op->_n_y_action      = mA;

  op->_obj.reserve(op->_obj.size() + mA);
  op->_vtype.reserve(op->_vtype.size() + mA);
  op->_lb.reserve(op->_lb.size() + mA);
  op->_ub.reserve(op->_ub.size() + mA);

  // Add vars and constraints
  // Minimal linearization (binary) under min objective:
  // y >= x_i - x_j
  // y >= x_j - x_i
  // If one side missing -> y >= x_existing
  // If both missing -> no constraints (y will go 0 due to objective)
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

    for (int k = 0; k < (int)A.size(); ++k) {
      const int a = A[k]; // 1..n_actions

      const int y_col = op->_y_action_offset + (e * (int)A.size() + k); // 0-based var index
      const double coef = weight_multiplier * we * wA[a];

      op->_obj.push_back(coef);
      op->_vtype.push_back("B");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);

      const int xi = xcol_map[a * (n_pu + 1) + i];
      const int xj = xcol_map[a * (n_pu + 1) + j];

      if (xi >= 0 && xj >= 0) {
        // y - xi + xj >= 0
        add_row_triplets(op.get(), ">=", 0.0, { y_col, xi, xj }, { 1.0, -1.0,  1.0 });
        // y + xi - xj >= 0
        add_row_triplets(op.get(), ">=", 0.0, { y_col, xi, xj }, { 1.0,  1.0, -1.0 });
      } else if (xi >= 0 && xj < 0) {
        // y >= xi
        add_row_triplets(op.get(), ">=", 0.0, { y_col, xi }, { 1.0, -1.0 });
      } else if (xi < 0 && xj >= 0) {
        // y >= xj
        add_row_triplets(op.get(), ">=", 0.0, { y_col, xj }, { 1.0, -1.0 });
      } else {
        // both missing -> no constraints; min objective will set y=0
      }
    }
  }

  return true;
}
