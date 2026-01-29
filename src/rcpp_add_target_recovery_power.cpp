#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <limits>
#include <algorithm>

static inline long long key2_int(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

struct GroupPF {
  int ipu;                    // 1-based internal_pu
  int ifeat;                  // 1-based internal_feature
  double b0;                  // assumed constant benefit per action within (pu,feature)
  int d;                      // number of actions in group
  double amount;              // amount_is = b0 * d
  std::vector<int> x_cols;    // 0-based column indices of x vars (op->_x_offset + x_row)

  int t_col;                  // 0-based column index of t var  (added here)  in [0,1]
  int y_col;                  // 0-based column index of y var  (added here)  in [0,1]
  std::vector<int> del_cols;  // 0-based column indices of delta vars (added here), binary
};

// [[Rcpp::export]]
Rcpp::List rcpp_add_target_recovery_power(
    SEXP x,
    Rcpp::DataFrame features_data,
    Rcpp::DataFrame dist_actions_data,
    Rcpp::DataFrame dist_benefit_data,
    double exponent = 2.0,
    int segments = 3,
    SEXP target_col_sexp = R_NilValue,
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!(exponent >= 1.0)) {
    Rcpp::stop("rcpp_add_target_recovery_power: 'exponent' must be >= 1.");
  }
  if (segments < 1) {
    Rcpp::stop("rcpp_add_target_recovery_power: 'segments' must be >= 1.");
  }

  // ---- Required columns in dist tables
  for (auto nm : {"internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_action", "internal_feature", "benefit"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
    }
  }

  // ---- Determine feature id column name in features_data
  std::string id_col;
  if (features_data.containsElementNamed("internal_id")) {
    id_col = "internal_id";
  } else if (features_data.containsElementNamed("internal_feature")) {
    id_col = "internal_feature";
  } else {
    Rcpp::stop("features_data must contain column 'internal_id' (preferred) or 'internal_feature' (legacy).");
  }

  // ---- Determine target column name
  std::string target_col = "target_recovery";
  if (!Rf_isNull(target_col_sexp)) {
    if (TYPEOF(target_col_sexp) == STRSXP && Rf_length(target_col_sexp) >= 1) {
      target_col = Rcpp::as<std::string>(target_col_sexp);
    } else if (TYPEOF(target_col_sexp) == LGLSXP && Rf_length(target_col_sexp) >= 1) {
      int flag = LOGICAL(target_col_sexp)[0];
      if (flag == 0) target_col = "target"; // legacy
    } else {
      Rcpp::stop("Argument 'target_col' must be a character scalar (preferred) or logical (legacy).");
    }
  }

  // ---- Extract feature ids and targets
  Rcpp::IntegerVector f_id = features_data[id_col];

  Rcpp::NumericVector f_t;
  if (features_data.containsElementNamed(target_col.c_str())) {
    f_t = features_data[target_col.c_str()];
  } else if (features_data.containsElementNamed("target")) {
    target_col = "target";
    f_t = features_data["target"];
  } else {
    Rcpp::stop("features_data must contain target column '" + target_col + "' (or legacy column 'target').");
  }

  // ---- Map (internal_pu, internal_action) -> x_row in dist_actions
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow;
  pa_to_xrow.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    if (ipu <= 0 || ia <= 0) {
      Rcpp::stop("dist_actions internal_* must be positive 1-based.");
    }
    const long long k = key2_int(ipu, ia);
    if (pa_to_xrow.find(k) != pa_to_xrow.end()) {
      Rcpp::stop("Duplicate (internal_pu, internal_action) in dist_actions_data.");
    }
    pa_to_xrow[k] = r; // 0-based row
  }

  // ---- Benefit table
  Rcpp::IntegerVector db_ipu   = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact  = dist_benefit_data["internal_action"];
  Rcpp::IntegerVector db_ifeat = dist_benefit_data["internal_feature"];
  Rcpp::NumericVector db_ben   = dist_benefit_data["benefit"];
  const int n_db = dist_benefit_data.nrows();

  // ------------------------------------------------------------
  // 1) Group rows by (internal_pu, internal_feature)
  //    (binary legacy assumption: benefit constant within each group)
  // ------------------------------------------------------------
  std::unordered_map<long long, int> group_index;
  group_index.reserve((std::size_t)n_db * 2);

  std::vector<GroupPF> groups;
  groups.reserve((std::size_t)n_db / 2 + 1);

  auto rel_close = [&](double a, double b) -> bool {
    const double scale = std::max(1.0, std::max(std::fabs(a), std::fabs(b)));
    return std::fabs(a - b) <= 1e-9 * scale;
  };

  for (int r = 0; r < n_db; ++r) {
    const int ipu   = db_ipu[r];
    const int iact  = db_iact[r];
    const int ifeat = db_ifeat[r];
    const double b  = db_ben[r];

    if (ipu <= 0 || iact <= 0 || ifeat <= 0) continue;
    if (!(b > tol)) continue;

    const long long k_pa = key2_int(ipu, iact);
    auto itx = pa_to_xrow.find(k_pa);
    if (itx == pa_to_xrow.end()) {
      Rcpp::stop("dist_benefit has (internal_pu, internal_action) not found in dist_actions_data.");
    }
    const int x_row = itx->second;
    const int x_col = op->_x_offset + x_row; // 0-based variable index

    const long long k_pf = key2_int(ipu, ifeat);
    auto itg = group_index.find(k_pf);

    if (itg == group_index.end()) {
      GroupPF g;
      g.ipu = ipu;
      g.ifeat = ifeat;
      g.b0 = b;
      g.d = 0;
      g.amount = 0.0;
      g.x_cols.clear();
      g.x_cols.reserve(16);
      g.t_col = -1;
      g.y_col = -1;
      g.del_cols.clear();

      groups.push_back(std::move(g));
      const int idx = (int)groups.size() - 1;
      group_index[k_pf] = idx;
      itg = group_index.find(k_pf);
    }

    GroupPF &G = groups[itg->second];

    if (!rel_close(b, G.b0)) {
      Rcpp::stop(
        "rcpp_add_target_recovery_power assumes constant 'benefit' across actions for each (pu,feature). "
        "Found non-constant values; likely not binary legacy mode."
      );
    }

    G.x_cols.push_back(x_col);
  }

  for (auto &G : groups) {
    G.d = (int)G.x_cols.size();
    if (G.d > 0) G.amount = G.b0 * (double)G.d;
  }

  const int n_groups = (int)groups.size();
  const int K = segments + 1; // breakpoints u_j, j=0..segments

  // ------------------------------------------------------------
  // 2) Add per-group vars: t_g (C), y_g (C), deltas (B)
  //    and constraints:
  //      t_g = (1/d) * sum x
  //      sum_j delta_gj = 1
  //      y_g <= a_j + b_j t_g + M(1-delta_gj)  (max-of-tangents via binaries)
  // ------------------------------------------------------------
  int n_t_vars_added = 0;
  int n_y_vars_added = 0;
  int n_delta_vars_added = 0;
  int n_link_rows_added = 0;
  int n_delta_rows_added = 0;
  int n_tangent_rows_added = 0;

  const double M = 1.0; // safe since y in [0,1]

  for (int g = 0; g < n_groups; ++g) {
    GroupPF &G = groups[g];
    if (G.d <= 0) continue;

    // t_g
    G.t_col = (int)op->_obj.size();
    op->_obj.push_back(0.0);
    op->_vtype.push_back("C");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
    ++n_t_vars_added;

    // y_g
    G.y_col = (int)op->_obj.size();
    op->_obj.push_back(0.0);
    op->_vtype.push_back("C");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
    ++n_y_vars_added;

    // deltas
    G.del_cols.resize((std::size_t)K);
    for (int j = 0; j < K; ++j) {
      const int col = (int)op->_obj.size();
      G.del_cols[(std::size_t)j] = col;
      op->_obj.push_back(0.0);
      op->_vtype.push_back("B");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);
      ++n_delta_vars_added;
    }

    const double inv_d = 1.0 / (double)G.d;

    // t_g - inv_d * sum x <= 0
    {
      std::vector<int> cols;
      std::vector<double> vals;
      cols.reserve(1 + (std::size_t)G.x_cols.size());
      vals.reserve(1 + (std::size_t)G.x_cols.size());

      cols.push_back(G.t_col);
      vals.push_back(1.0);

      for (std::size_t k = 0; k < G.x_cols.size(); ++k) {
        cols.push_back(G.x_cols[k]);
        vals.push_back(-inv_d);
      }

      op->addRow(cols, vals, "<=", 0.0);
      ++n_link_rows_added;
    }

    // -t_g + inv_d * sum x <= 0
    {
      std::vector<int> cols;
      std::vector<double> vals;
      cols.reserve(1 + (std::size_t)G.x_cols.size());
      vals.reserve(1 + (std::size_t)G.x_cols.size());

      cols.push_back(G.t_col);
      vals.push_back(-1.0);

      for (std::size_t k = 0; k < G.x_cols.size(); ++k) {
        cols.push_back(G.x_cols[k]);
        vals.push_back(inv_d);
      }

      op->addRow(cols, vals, "<=", 0.0);
      ++n_link_rows_added;
    }

    // sum_j delta_gj == 1
    {
      std::vector<int> cols;
      std::vector<double> vals;
      cols.reserve((std::size_t)K);
      vals.reserve((std::size_t)K);

      for (int j = 0; j < K; ++j) {
        cols.push_back(G.del_cols[(std::size_t)j]);
        vals.push_back(1.0);
      }

      op->addRow(cols, vals, "==", 1.0);
      ++n_delta_rows_added;
    }

    // Tangent selection constraints (max of tangents):
    // y <= a_j + b_j t + M(1-delta)
    // => y - b_j t + M*delta <= a_j + M
    for (int j = 0; j < K; ++j) {
      const double u = (double)j / (double)segments;

      const double fu  = std::pow(u, exponent);
      const double fpu = (u > 0.0) ? (exponent * std::pow(u, exponent - 1.0))
        : ((std::fabs(exponent - 1.0) < 1e-15) ? 1.0 : 0.0);

      const double a = fu - fpu * u;
      const double b = fpu;

      std::vector<int> cols;
      std::vector<double> vals;
      cols.reserve(3);
      vals.reserve(3);

      cols.push_back(G.y_col);
      vals.push_back(1.0);

      cols.push_back(G.t_col);
      vals.push_back(-b);

      cols.push_back(G.del_cols[(std::size_t)j]);
      vals.push_back(M);

      const double rhs = a + M;

      op->addRow(cols, vals, "<=", rhs);
      ++n_tangent_rows_added;
    }
  }

  // ------------------------------------------------------------
  // 3) Add recovery target constraints per feature:
  //    sum_{groups with feature} amount_is * y_g >= T
  // ------------------------------------------------------------
  int n_target_rows_added = 0;

  std::unordered_map<int, std::vector<int>> feat_to_groups;
  feat_to_groups.reserve((std::size_t)f_id.size() * 2);

  for (int g = 0; g < n_groups; ++g) {
    const GroupPF &G = groups[g];
    if (G.d <= 0 || G.y_col < 0) continue;
    feat_to_groups[G.ifeat].push_back(g);
  }

  for (int s = 0; s < f_id.size(); ++s) {
    const int ifeat = f_id[s];
    const double T  = f_t[s];
    if (!(T > tol)) continue;

    auto it = feat_to_groups.find(ifeat);
    if (it == feat_to_groups.end() || it->second.empty()) {
      Rcpp::stop(
        "Recovery-only target cannot be met (power): feature internal_id=" +
          std::to_string(ifeat) +
          " has positive target but no (pu,feature) groups / actions in dist_benefit."
      );
    }

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(it->second.size());
    vals.reserve(it->second.size());

    for (int gidx : it->second) {
      const GroupPF &G = groups[gidx];
      if (!(G.amount > tol)) continue;
      cols.push_back(G.y_col);
      vals.push_back(G.amount);
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Recovery-only target cannot be met (power): feature internal_id=" +
          std::to_string(ifeat) +
          " has positive target but all group amounts are zero."
      );
    }

    op->addRow(cols, vals, ">=", T);
    ++n_target_rows_added;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_groups")             = n_groups,
    Rcpp::Named("n_t_vars_added")       = n_t_vars_added,
    Rcpp::Named("n_y_vars_added")       = n_y_vars_added,
    Rcpp::Named("n_delta_vars_added")   = n_delta_vars_added,
    Rcpp::Named("n_link_rows_added")    = n_link_rows_added,
    Rcpp::Named("n_delta_rows_added")   = n_delta_rows_added,
    Rcpp::Named("n_tangent_rows_added") = n_tangent_rows_added,
    Rcpp::Named("n_target_rows_added")  = n_target_rows_added,
    Rcpp::Named("mode")                 = "power_piecewise_max_of_tangents_noSOS2",
    Rcpp::Named("exponent")             = exponent,
    Rcpp::Named("segments")             = segments,
    Rcpp::Named("target_col_used")      = target_col,
    Rcpp::Named("id_col_used")          = id_col
  );
}
