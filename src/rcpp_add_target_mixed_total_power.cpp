#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <limits>

// key for (internal_pu, internal_feature)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// Append a continuous variable at the end of the model
// Returns 0-based column index in op->_obj/_lb/_ub/_vtype
static inline int add_cont_var(Rcpp::XPtr<OptimizationProblem> op,
                               double lb, double ub, double obj = 0.0) {
  op->_obj.push_back(obj);
  op->_vtype.push_back("C");
  op->_lb.push_back(lb);
  op->_ub.push_back(ub);
  return static_cast<int>(op->_obj.size()) - 1;
}

// [[Rcpp::export]]
Rcpp::List rcpp_add_target_mixed_total_power(
    SEXP x,
    Rcpp::DataFrame dist_features_data,   // baseline: one row per (pu,feature) matching z order
    Rcpp::DataFrame dist_actions_data,    // to map (ipu,iact) -> x row index
    Rcpp::DataFrame dist_benefit_data,    // rows (ipu,iact,ifeat,benefit)
    Rcpp::DataFrame targets_df,           // columns: internal_id, target_value (or target)
    double exponent = 2.0,
    int segments = 6,
    SEXP amount_col_sexp = R_NilValue,    // optional: column name for baseline amount in dist_features
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!(std::isfinite(exponent) && exponent > 1.0 + 1e-12)) {
    Rcpp::stop("exponent must be > 1 for rcpp_add_target_mixed_total_power().");
  }
  if (segments < 2) {
    Rcpp::stop("segments must be >= 2 (number of tangent points).");
  }

  // ---- Check required columns
  for (auto nm : {"internal_feature"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }
  if (!dist_actions_data.containsElementNamed("internal_pu") ||
      !dist_actions_data.containsElementNamed("internal_action") ||
      !dist_actions_data.containsElementNamed("internal_row")) {
      Rcpp::stop("dist_actions_data must contain columns: internal_pu, internal_action, internal_row.");
  }
  if (!dist_benefit_data.containsElementNamed("internal_pu") ||
      !dist_benefit_data.containsElementNamed("internal_action") ||
      !dist_benefit_data.containsElementNamed("internal_feature") ||
      !dist_benefit_data.containsElementNamed("benefit")) {
      Rcpp::stop("dist_benefit_data must contain columns: internal_pu, internal_action, internal_feature, benefit.");
  }
  if (!targets_df.containsElementNamed("internal_id")) {
    Rcpp::stop("targets_df must contain column 'internal_id'.");
  }
  if (!targets_df.containsElementNamed("target_value")) {
    // backward compat: allow 'target'
    if (targets_df.containsElementNamed("target")) {
      // ok
    } else {
      Rcpp::stop("targets_df must contain column 'target_value' (or legacy column 'target').");
    }
  }

  // ---- Baseline amount column in dist_features_data
  std::string amount_col = "amount";
  if (!Rf_isNull(amount_col_sexp)) {
    if (TYPEOF(amount_col_sexp) == STRSXP && Rf_length(amount_col_sexp) >= 1) {
      amount_col = Rcpp::as<std::string>(amount_col_sexp);
    } else {
      Rcpp::stop("amount_col must be a character scalar.");
    }
  }
  if (!dist_features_data.containsElementNamed(amount_col.c_str())) {
    Rcpp::stop("dist_features_data must contain baseline amount column '" + amount_col + "'.");
  }

  // ---- Extract baseline tables (z is ordered exactly as dist_features_data rows)
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];
  Rcpp::NumericVector  df_amt  = dist_features_data[amount_col.c_str()];
  const int n_df = dist_features_data.nrows();

  // ---- Build map: (internal_pu, internal_action) -> x_row (0-based) from dist_actions_data
  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  std::unordered_map<long long, int> pa_to_xrow;
  pa_to_xrow.reserve((std::size_t)n_da * 2);

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    const int row0 = da_irow[r] - 1; // internal_row is 1-based in R
    if (ipu <= 0 || ia <= 0 || row0 < 0) {
      Rcpp::stop("dist_actions_data internal_* must be positive and internal_row must be 1-based.");
    }
    const long long k = key2(ipu, ia);
    if (pa_to_xrow.find(k) != pa_to_xrow.end()) {
      Rcpp::stop("Duplicate (internal_pu, internal_action) in dist_actions_data.");
    }
    pa_to_xrow[k] = row0;
  }

  // ---- Benefit table
  Rcpp::IntegerVector db_ipu   = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_iact  = dist_benefit_data["internal_action"];
  Rcpp::IntegerVector db_ifeat = dist_benefit_data["internal_feature"];
  Rcpp::NumericVector db_ben   = dist_benefit_data["benefit"];
  const int n_db = dist_benefit_data.nrows();

  // For each (ipu,ifeat) we need:
  //   B = sum_r benefit_r (positive)
  //   list of (xcol, benefit_r)
  struct PFBlock {
    double B = 0.0;
    std::vector<int> xcols;
    std::vector<double> bvals;
    int t_col = -1; // column index for t in [0,1]
    int y_col = -1; // column index for y in [0,1]
  };

  std::unordered_map<long long, PFBlock> pf_map;
  pf_map.reserve((std::size_t)n_db);

  int dropped_nonpos_benefit = 0;
  int dropped_missing_action = 0;

  for (int r = 0; r < n_db; ++r) {
    const double b = (double)db_ben[r];
    if (!(std::isfinite(b) && b > tol)) {
      ++dropped_nonpos_benefit;
      continue;
    }
    const int ipu = db_ipu[r];
    const int ia  = db_iact[r];
    const int ifeat = db_ifeat[r];

    const long long k_pa = key2(ipu, ia);
    auto it = pa_to_xrow.find(k_pa);
    if (it == pa_to_xrow.end()) {
      ++dropped_missing_action;
      continue; // defensive: benefit row with no action var
    }
    const int x_row0 = it->second;
    const int x_col  = op->_x_offset + x_row0; // 0-based col index

    const long long k_pf = key2(ipu, ifeat);
    PFBlock &blk = pf_map[k_pf];
    blk.B += b;
    blk.xcols.push_back(x_col);
    blk.bvals.push_back(b);
  }

  // ---- Create (t,y) vars and constraints for each (ipu,ifeat) with B>0:
  // 1) Linking: sum(b_r * x_r) - B * t = 0
  // 2) Tangent underestimator for y <= t^p:
  //    y - m_k * t <= c_k  for k=1..segments with u_k = k/segments
  //    (t,y bounds are [0,1])
  int n_pf_blocks = 0;
  int n_link_rows = 0;
  int n_tangent_rows = 0;

  for (auto &kv : pf_map) {
    PFBlock &blk = kv.second;
    if (!(std::isfinite(blk.B) && blk.B > tol)) {
      continue;
    }

    // Add t and y variables at end
    blk.t_col = add_cont_var(op, 0.0, 1.0, 0.0);
    blk.y_col = add_cont_var(op, 0.0, 1.0, 0.0);

    // Linking equality: sum(b*x) - B*t = 0
    {
      std::vector<int> cols;
      std::vector<double> vals;
      cols.reserve(blk.xcols.size() + 1);
      vals.reserve(blk.bvals.size() + 1);

      for (std::size_t j = 0; j < blk.xcols.size(); ++j) {
        cols.push_back(blk.xcols[j]);
        vals.push_back(blk.bvals[j]);
      }
      cols.push_back(blk.t_col);
      vals.push_back(-blk.B);

      op->addRow(cols, vals, "==", 0.0);
      ++n_link_rows;
    }

    // Tangent constraints at u_k = k/segments, k=1..segments
    for (int k = 1; k <= segments; ++k) {
      const double u = (double)k / (double)segments;             // in (0,1]
      const double m = exponent * std::pow(u, exponent - 1.0);   // slope
      const double fu = std::pow(u, exponent);
      const double c  = fu - m * u;                              // intercept

      // y - m t <= c
      std::vector<int> cols(2);
      std::vector<double> vals(2);
      cols[0] = blk.y_col; vals[0] = 1.0;
      cols[1] = blk.t_col; vals[1] = -m;
      op->addRow(cols, vals, "<=", c);
      ++n_tangent_rows;
    }

    ++n_pf_blocks;
  }

  // ---- Targets extraction
  Rcpp::IntegerVector targ_feat = targets_df["internal_id"];
  Rcpp::NumericVector targ_val;
  if (targets_df.containsElementNamed("target_value")) {
    targ_val = targets_df["target_value"];
  } else {
    targ_val = targets_df["target"];
  }

  // ---- For each feature with T>0 add:
  // sum_{rows in dist_features with that feature} amount * z_row
  // + sum_{(ipu,ifeat)=feature} B_{ipu,ifeat} * y_{ipu,ifeat}  >= T
  int n_target_rows = 0;

  for (int s = 0; s < targ_feat.size(); ++s) {
    const int ifeat = targ_feat[s];
    const double T  = (double)targ_val[s];
    if (!(std::isfinite(T) && T > tol)) continue;

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(1024);
    vals.reserve(1024);

    // Baseline part: dist_features rows correspond 1:1 to z vars in order
    for (int r = 0; r < n_df; ++r) {
      if (df_ifeat[r] != ifeat) continue;
      const double a = (double)df_amt[r];
      if (!(std::isfinite(a) && a > tol)) continue;
      const int z_col = op->_z_offset + r; // 0-based, because z vars created in dist_features row order
      cols.push_back(z_col);
      vals.push_back(a);
    }

    // Powered action part: for each (ipu,ifeat) block, add B * y
    double total_B_for_feature = 0.0;
    for (auto &kv : pf_map) {
      const long long k_pf = kv.first;
      const int ifeat_k = (int)(k_pf & 0xFFFFFFFFu); // lower 32 bits
      if (ifeat_k != ifeat) continue;

      PFBlock &blk = kv.second;
      if (blk.y_col < 0) continue;
      if (!(std::isfinite(blk.B) && blk.B > tol)) continue;

      cols.push_back(blk.y_col);
      vals.push_back(blk.B);
      total_B_for_feature += blk.B;
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Mixed_total target cannot be formed: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no baseline amounts and no benefit blocks."
      );
    }

    // Optional: strict feasibility check — if no action blocks and no baseline, already stopped.
    // If no baseline but there are action blocks, that's ok.
    // If baseline exists but action blocks absent, it's ok: reduces to conservation-only.

    op->addRow(cols, vals, ">=", T);
    ++n_target_rows;
  }

  return Rcpp::List::create(
    Rcpp::Named("n_pf_blocks") = n_pf_blocks,
    Rcpp::Named("n_link_rows") = n_link_rows,
    Rcpp::Named("n_tangent_rows") = n_tangent_rows,
    Rcpp::Named("n_target_rows") = n_target_rows,
    Rcpp::Named("exponent") = exponent,
    Rcpp::Named("segments") = segments,
    Rcpp::Named("amount_col_used") = amount_col,
    Rcpp::Named("dropped_nonpos_benefit") = dropped_nonpos_benefit,
    Rcpp::Named("dropped_missing_action") = dropped_missing_action,
    Rcpp::Named("note") =
      "Uses tangent underestimator y <= t^p (convex p>1). No SOS2. Adds continuous (t,y) vars per (pu,feature) block."
  );
}
