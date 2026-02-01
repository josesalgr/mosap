// rcpp_set_objective_min_fragmentation_interventions.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <vector>
#include <string>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_min_fragmentation_interventions(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight
    double weight_multiplier = 1.0,
    std::string block_name = "objective_min_fragmentation_interventions",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (op->_obj.empty()) Rcpp::stop("Model has zero variables. Build base variables first.");
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but op->_n_x <= 0.");
  if (op->_x_offset < 0) Rcpp::stop("op->_x_offset not initialized.");

  // avoid double-adding auxiliaries
  if (op->_n_u_intervention > 0 || op->_n_y_intervention > 0) {
    Rcpp::stop("Intervention-fragmentation auxiliaries already exist (_n_u_intervention>0 or _n_y_intervention>0).");
  }

  // checks: relation_data
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_actions_data
  for (auto nm : {"internal_row", "internal_pu"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  // relation arrays
  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"]; // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];  // 1..n_pu
  const int da_n = dist_actions_data.nrows();

  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // Reset objective fully: we define THIS objective from scratch
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // Build list of x columns per PU (1..n_pu)
  std::vector< std::vector<int> > xcols_by_pu((std::size_t)n_pu + 1);

  for (int r = 0; r < da_n; ++r) {
    const int row1 = dar[r];
    const int i    = dap[r];

    if (row1 == NA_INTEGER || i == NA_INTEGER) continue;

    if (row1 < 1 || row1 > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (i    < 1 || i    > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");

    const int x_col = op->_x_offset + (row1 - 1);
    if (x_col < 0 || x_col >= (int)op->_obj.size()) {
      Rcpp::stop("Computed x_col out of bounds: check x_offset/internal_row/object size.");
    }

    xcols_by_pu[(std::size_t)i].push_back(x_col);
  }

  // ----------------------------
  // Add u_i vars (intervention indicator per PU)
  // ----------------------------
  const int u0 = (int)op->ncol_used();
  op->_u_intervention_offset = u0;
  op->_n_u_intervention      = n_pu;

  op->_obj.reserve(op->_obj.size() + (std::size_t)n_pu);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)n_pu);
  op->_lb.reserve(op->_lb.size() + (std::size_t)n_pu);
  op->_ub.reserve(op->_ub.size() + (std::size_t)n_pu);

  for (int i = 1; i <= n_pu; ++i) {
    op->_obj.push_back(0.0);     // u has no direct objective weight
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // ----------------------------
  // Add y_e vars for edges (will carry objective weight)
  // ----------------------------
  const int y0 = (int)op->ncol_used();
  op->_y_intervention_offset = y0;
  op->_n_y_intervention      = m;

  op->_obj.reserve(op->_obj.size() + (std::size_t)m);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)m);
  op->_lb.reserve(op->_lb.size() + (std::size_t)m);
  op->_ub.reserve(op->_ub.size() + (std::size_t)m);

  for (int e = 0; e < m; ++e) {
    double we = (double)wgt[e];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }
    op->_obj.push_back(weight_multiplier * we);
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // ----------------------------
  // Registry (objective + variable blocks)
  // ----------------------------
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective"
    ";modelsense=min"
    ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_pu=" + std::to_string(n_pu) +
      ";n_x=" + std::to_string(n_x) +
      ";n_edges=" + std::to_string(m) +
      ";objective_reset=TRUE";

  const std::size_t obj_block_id = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(), // columns BEFORE constraints; objective coefficients set on whole vector anyway
    full_tag
  );

  const std::size_t u_block_id = op->register_variable_block(
    block_name + "::u_intervention",
    (std::size_t)u0,
    (std::size_t)(u0 + n_pu),
    full_tag + ";vtype=B;coef=0"
  );

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::y_intervention",
    (std::size_t)y0,
    (std::size_t)(y0 + m),
    full_tag + ";vtype=B;coef=weight_multiplier*weight"
  );

  // ----------------------------
  // Constraints blocks
  // ----------------------------
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  // (1) Link u_i with x vars:
  //     (a) x <= u  <=>  u - x >= 0
  //     (b) u <= sum x  <=> -u + sum x >= 0   (if xs non-empty)
  //     if xs empty => u <= 0
  for (int i = 1; i <= n_pu; ++i) {
    const int u_col = u0 + (i - 1);
    const auto& xs  = xcols_by_pu[(std::size_t)i];

    if (xs.empty()) {
      // u_i <= 0
      op->addRow(
          std::vector<int>{ u_col },
          std::vector<double>{ 1.0 },
          "<=", 0.0,
          "u_no_actions"
      );
      continue;
    }

    // (a) u - x >= 0 for each x in PU
    for (int xc : xs) {
      op->addRow(
          std::vector<int>{ u_col, xc },
          std::vector<double>{ 1.0, -1.0 },
          ">=", 0.0,
          "u_ge_x"
      );
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

    op->addRow(cols, vals, ">=", 0.0, "u_le_sumx");
  }

  // (2) Link y_e with u_i, u_j:
  //     y >= u_i - u_j  <=> y - u_i + u_j >= 0
  //     y >= u_j - u_i  <=> y + u_i - u_j >= 0
  for (int e = 0; e < m; ++e) {
    const int i = ip1[e];
    const int j = ip2[e];

    if (i == NA_INTEGER || j == NA_INTEGER) continue;
    if (i < 1 || i > n_pu || j < 1 || j > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    const int y_col = y0 + e;
    const int ui    = u0 + (i - 1);
    const int uj    = u0 + (j - 1);

    op->addRow(
        std::vector<int>{ y_col, ui, uj },
        std::vector<double>{ 1.0, -1.0, 1.0 },
        ">=", 0.0,
        "y_ge_ui_minus_uj"
    );

    op->addRow(
        std::vector<int>{ y_col, ui, uj },
        std::vector<double>{ 1.0, 1.0, -1.0 },
        ">=", 0.0,
        "y_ge_uj_minus_ui"
    );
  }

  op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("objective_block_id") = (double)obj_block_id,
    Rcpp::Named("u_block_id") = (double)u_block_id,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("u_offset") = op->_u_intervention_offset,
    Rcpp::Named("n_u") = op->_n_u_intervention,
    Rcpp::Named("y_offset") = op->_y_intervention_offset,
    Rcpp::Named("n_y") = op->_n_y_intervention,
    Rcpp::Named("note") =
      "Objective reset to 0; only y_intervention carries weights. Constraints added via addRow() for consistency."
  );
}
