// rcpp_set_objective_min_fragmentation_actions_by_action.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>

// key (ipu, iact) or (ipu, action)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// [[Rcpp::export]]
bool rcpp_set_objective_min_fragmentation_actions_by_action(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu, internal_action
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight
    Rcpp::Nullable<Rcpp::IntegerVector> actions_to_use = R_NilValue, // optional internal_action ids (1-based)
    Rcpp::Nullable<Rcpp::NumericVector> action_weights = R_NilValue, // optional weights (len n_actions or len(actions_to_use))
    double weight_multiplier = 1.0,
    std::string block_name = "objective_min_fragmentation_actions_by_action",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but op->_n_x <= 0.");
  if (op->_obj.empty()) Rcpp::stop("Model has zero variables. Build base variables first.");
  if (op->_x_offset < 0) Rcpp::stop("op->_x_offset not initialized.");

  // avoid double-adding y_action
  if (op->_n_y_action > 0) {
    Rcpp::stop("Action-fragmentation auxiliaries already exist (_n_y_action > 0).");
  }

  // checks: relation_data
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  // checks: dist_actions_data
  for (auto nm : {"internal_row", "internal_pu", "internal_action"}) {
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
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"];     // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];      // 1..n_pu
  Rcpp::IntegerVector daa = dist_actions_data["internal_action"];  // 1..n_actions
  const int da_n = dist_actions_data.nrows();

  // your assumption: model-ready dist_actions has exactly n_x rows
  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // infer n_actions as max internal_action
  int n_actions = 0;
  for (int r = 0; r < da_n; ++r) {
    const int a = daa[r];
    if (a == NA_INTEGER) continue;
    if (a > n_actions) n_actions = a;
  }
  if (n_actions <= 0) Rcpp::stop("Could not infer number of actions (max internal_action <= 0).");

  // choose actions A (1-based ids)
  std::vector<int> A;
  if (actions_to_use.isNotNull()) {
    Rcpp::IntegerVector tmp(actions_to_use);
    if (tmp.size() == 0) Rcpp::stop("actions_to_use provided but empty.");
    A.reserve((std::size_t)tmp.size());
    for (int k = 0; k < tmp.size(); ++k) {
      const int a = tmp[k];
      if (a < 1 || a > n_actions) Rcpp::stop("actions_to_use contains invalid internal_action id.");
      A.push_back(a);
    }
  } else {
    A.reserve((std::size_t)n_actions);
    for (int a = 1; a <= n_actions; ++a) A.push_back(a);
  }
  const int nA = (int)A.size();

  // action weights vector wA (1-based)
  std::vector<double> wA((std::size_t)n_actions + 1, 1.0);
  if (action_weights.isNotNull()) {
    Rcpp::NumericVector aw(action_weights);

    if (aw.size() == n_actions) {
      for (int a = 1; a <= n_actions; ++a) {
        const double v = aw[a - 1];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[(std::size_t)a] = v;
      }
    } else if ((int)aw.size() == nA) {
      for (int k = 0; k < nA; ++k) {
        const double v = aw[k];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[(std::size_t)A[(std::size_t)k]] = v;
      }
    } else {
      Rcpp::stop("action_weights length must be either n_actions or length(actions_to_use).");
    }
  }

  // reset full objective: we define THIS objective from scratch
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // map (ipu, action) -> x_col (0-based var index), sparse via unordered_map
  // key2(ipu, action) is safe because both are <= 2^31-1 in your setup
  std::unordered_map<long long, int> xcol;
  xcol.reserve((std::size_t)da_n * 2);

  for (int r = 0; r < da_n; ++r) {
    const int row1 = dar[r]; // 1..n_x
    const int ipu  = dap[r]; // 1..n_pu
    const int act  = daa[r]; // 1..n_actions

    if (row1 == NA_INTEGER || ipu == NA_INTEGER || act == NA_INTEGER) continue;

    if (row1 < 1 || row1 > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (ipu  < 1 || ipu  > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");
    if (act  < 1 || act  > n_actions) Rcpp::stop("dist_actions_data$internal_action out of range.");

    const int col_x = op->_x_offset + (row1 - 1);
    if (col_x < 0 || col_x >= (int)op->_obj.size()) {
      Rcpp::stop("Computed x col out of bounds: check x_offset/internal_row/object size.");
    }

    xcol[key2(ipu, act)] = col_x;
  }

  // append y_{e,a} variables
  const int mA = m * nA;

  op->_y_action_offset = (int)op->ncol_used();
  op->_n_y_action      = mA;

  // reserve capacity
  op->_obj.reserve(op->_obj.size() + (std::size_t)mA);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)mA);
  op->_lb.reserve(op->_lb.size() + (std::size_t)mA);
  op->_ub.reserve(op->_ub.size() + (std::size_t)mA);

  // register objective as a block on the FULL obj vector (since we fully define it),
  // and also register y variable block specifically (useful for debugging)
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=min"
    ";kind=objective"
    ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_actions=" + std::to_string(n_actions) +
      ";n_actions_used=" + std::to_string(nA) +
      ";n_edges=" + std::to_string(m);

  const std::size_t obj_block_id = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(), // current columns BEFORE adding y; still OK as "objective scope"
    full_tag + ";note=obj_reset_before_adding_y"
  );
  (void)obj_block_id; // silence unused warning if not used

  // constraints block
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  // add vars + constraints
  for (int e = 0; e < m; ++e) {
    const int i = ip1[e];
    const int j = ip2[e];

    if (i == NA_INTEGER || j == NA_INTEGER) continue;
    if (i < 1 || i > n_pu || j < 1 || j > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    const double we = (double)wgt[e];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    for (int k = 0; k < nA; ++k) {
      const int act = A[(std::size_t)k]; // 1..n_actions

      const int y_col = (int)op->ncol_used(); // next column index
      const double coef = weight_multiplier * we * wA[(std::size_t)act];

      // add y var
      op->_obj.push_back(coef);
      op->_vtype.push_back("B");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);

      // find xi/xj (may be missing if infeasible pair absent)
      auto iti = xcol.find(key2(i, act));
      auto itj = xcol.find(key2(j, act));
      const int xi = (iti == xcol.end() ? -1 : iti->second);
      const int xj = (itj == xcol.end() ? -1 : itj->second);

      if (xi >= 0 && xj >= 0) {
        // y >= xi - xj  -> y - xi + xj >= 0
        op->addRow(
            std::vector<int>{ y_col, xi, xj },
            std::vector<double>{ 1.0, -1.0,  1.0 },
            ">=", 0.0
        );
        // y >= xj - xi  -> y + xi - xj >= 0
        op->addRow(
            std::vector<int>{ y_col, xi, xj },
            std::vector<double>{ 1.0,  1.0, -1.0 },
            ">=", 0.0
        );
      } else if (xi >= 0 && xj < 0) {
        // y >= xi  -> y - xi >= 0
        op->addRow(
            std::vector<int>{ y_col, xi },
            std::vector<double>{ 1.0, -1.0 },
            ">=", 0.0
        );
      } else if (xi < 0 && xj >= 0) {
        // y >= xj  -> y - xj >= 0
        op->addRow(
            std::vector<int>{ y_col, xj },
            std::vector<double>{ 1.0, -1.0 },
            ">=", 0.0
        );
      } else {
        // both missing: no constraints; since coef >= 0, min objective will set y = 0
      }
    }
  }

  // close constraints block
  op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  // register y variable block (now that we know final column range)
  const std::size_t y0 = (std::size_t)op->_y_action_offset;
  const std::size_t y1 = y0 + (std::size_t)mA;

  if (y1 != op->ncol_used()) {
    Rcpp::stop("Internal error: y block size mismatch (y0+mA != ncol_used()).");
  }

  op->register_variable_block(
      block_name + "::y_action",
      y0, y1,
      full_tag + ";vtype=B;linearization=absdiff;constraints=2_per_edge_action_if_both_exist"
  );

  return true;
}
