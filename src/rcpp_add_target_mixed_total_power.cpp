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
    SEXP amount_col_sexp = R_NilValue,
    double tol = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!(std::isfinite(exponent) && exponent > 1.0 + 1e-12)) {
    Rcpp::stop("exponent must be > 1 for rcpp_add_target_mixed_total_power().");
  }
  if (segments < 2) {
    Rcpp::stop("segments must be >= 2 (number of tangent points).");
  }

  // ---- Check required columns
  if (!dist_features_data.containsElementNamed("internal_feature")) {
    Rcpp::stop("dist_features_data must contain column 'internal_feature'.");
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
    if (!targets_df.containsElementNamed("target")) {
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

  // For each (ipu,ifeat):
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
    const int ipu   = db_ipu[r];
    const int ia    = db_iact[r];
    const int ifeat = db_ifeat[r];

    const long long k_pa = key2(ipu, ia);
    auto it = pa_to_xrow.find(k_pa);
    if (it == pa_to_xrow.end()) {
      ++dropped_missing_action;
      continue;
    }

    const int x_row0 = it->second;
    const int x_col  = op->_x_offset + x_row0; // 0-based col index

    const long long k_pf = key2(ipu, ifeat);
    PFBlock &blk = pf_map[k_pf];
    blk.B += b;
    blk.xcols.push_back(x_col);
    blk.bvals.push_back(b);
  }

  // ------------------------------------------------------------
  // (1) Create all (t,y) vars first, then register ONE var block
  // ------------------------------------------------------------
  const std::size_t col_start = op->ncol_used(); // 0-based start (before adding)

  int n_pf_blocks = 0;
  for (auto &kv : pf_map) {
    PFBlock &blk = kv.second;
    if (!(std::isfinite(blk.B) && blk.B > tol)) continue;

    blk.t_col = add_cont_var(op, 0.0, 1.0, 0.0);
    blk.y_col = add_cont_var(op, 0.0, 1.0, 0.0);
    ++n_pf_blocks;
  }

  const std::size_t col_end = op->ncol_used(); // 0-based end (exclusive)

  std::size_t var_block_id = 0;
  if (col_end > col_start) {
    std::string vtag =
      "type=ty_power"
      ";exponent=" + std::to_string(exponent) +
        ";segments=" + std::to_string(segments) +
        ";n_pf_blocks=" + std::to_string(n_pf_blocks);

    var_block_id = op->register_variable_block("power_ty_vars", col_start, col_end, vtag);
  }

  // ------------------------------------------------------------
  // (2) Linking constraints block: sum(b*x) - B*t = 0
  // ------------------------------------------------------------
  const std::size_t link_row_start = op->nrow_used();
  std::string link_tag =
    "exponent=" + std::to_string(exponent) +
    ";segments=" + std::to_string(segments);

  const std::size_t link_bid = op->beginConstraintBlock("power_linking", link_tag);

  int n_link_rows = 0;

  for (auto &kv : pf_map) {
    PFBlock &blk = kv.second;
    if (!(std::isfinite(blk.B) && blk.B > tol)) continue;
    if (blk.t_col < 0) continue;

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

    op->addRow(cols, vals, "==", 0.0, "power_link");
    ++n_link_rows;
  }

  const std::size_t link_row_end = op->nrow_used();
  op->endConstraintBlock(link_bid);

  // ------------------------------------------------------------
  // (3) Tangent constraints block: y - m t <= c
  // ------------------------------------------------------------
  const std::size_t tan_row_start = op->nrow_used();
  std::string tan_tag =
    "exponent=" + std::to_string(exponent) +
    ";segments=" + std::to_string(segments);

  const std::size_t tan_bid = op->beginConstraintBlock("power_tangents", tan_tag);

  int n_tangent_rows = 0;

  for (auto &kv : pf_map) {
    PFBlock &blk = kv.second;
    if (!(std::isfinite(blk.B) && blk.B > tol)) continue;
    if (blk.t_col < 0 || blk.y_col < 0) continue;

    for (int k = 1; k <= segments; ++k) {
      const double u  = (double)k / (double)segments;            // (0,1]
      const double m  = exponent * std::pow(u, exponent - 1.0);
      const double fu = std::pow(u, exponent);
      const double c  = fu - m * u;

      op->addRow(
          std::vector<int>{blk.y_col, blk.t_col},
          std::vector<double>{1.0, -m},
          "<=",
          c,
          "power_tangent"
      );
      ++n_tangent_rows;
    }
  }

  const std::size_t tan_row_end = op->nrow_used();
  op->endConstraintBlock(tan_bid);

  // ------------------------------------------------------------
  // (4) Target constraints block:
  // baseline(z) + sum(B*y) >= T
  // ------------------------------------------------------------
  Rcpp::IntegerVector targ_feat = targets_df["internal_id"];
  Rcpp::NumericVector targ_val;
  if (targets_df.containsElementNamed("target_value")) targ_val = targets_df["target_value"];
  else targ_val = targets_df["target"];

  const std::size_t targ_row_start = op->nrow_used();
  std::string targ_tag =
    "exponent=" + std::to_string(exponent) +
    ";segments=" + std::to_string(segments) +
    ";amount_col=" + amount_col;

  const std::size_t targ_bid = op->beginConstraintBlock("target_mixed_total_power", targ_tag);

  int n_target_rows = 0;

  for (int s = 0; s < targ_feat.size(); ++s) {
    const int ifeat = targ_feat[s];
    const double T  = (double)targ_val[s];
    if (!(std::isfinite(T) && T > tol)) continue;

    std::vector<int> cols;
    std::vector<double> vals;
    cols.reserve(1024);
    vals.reserve(1024);

    // Baseline: z vars aligned with dist_features rows
    for (int r = 0; r < n_df; ++r) {
      if (df_ifeat[r] != ifeat) continue;
      const double a = (double)df_amt[r];
      if (!(std::isfinite(a) && a > tol)) continue;

      const int z_col = op->_z_offset + r;
      cols.push_back(z_col);
      vals.push_back(a);
    }

    // Powered action part: sum over blocks for this feature: B * y
    for (auto &kv : pf_map) {
      const long long k_pf = kv.first;
      const int ifeat_k = (int)(static_cast<unsigned int>(k_pf)); // low 32 bits
      if (ifeat_k != ifeat) continue;

      PFBlock &blk = kv.second;
      if (blk.y_col < 0) continue;
      if (!(std::isfinite(blk.B) && blk.B > tol)) continue;

      cols.push_back(blk.y_col);
      vals.push_back(blk.B);
    }

    if (cols.empty()) {
      Rcpp::stop(
        "Mixed_total target cannot be formed: feature internal_id=" + std::to_string(ifeat) +
          " has positive target but no baseline amounts and no benefit blocks."
      );
    }

    op->addRow(cols, vals, ">=", T, "target_mixed_power");
    ++n_target_rows;
  }

  const std::size_t targ_row_end = op->nrow_used();
  op->endConstraintBlock(targ_bid);

  // ------------------------------------------------------------
  // Return: include block ids + ranges (R-friendly)
  // ------------------------------------------------------------
  auto range_to_R = [](std::size_t start0, std::size_t end0) {
    if (end0 <= start0) return Rcpp::NumericVector::create(NA_REAL, NA_REAL);
    return Rcpp::NumericVector::create(
      static_cast<double>(start0 + 1), // 1-based start
      static_cast<double>(end0)        // 1-based end inclusive-friendly (end0 is exclusive in 0-based)
    );
  };

  return Rcpp::List::create(
    // counts
    Rcpp::Named("n_pf_blocks") = n_pf_blocks,
    Rcpp::Named("n_link_rows") = n_link_rows,
    Rcpp::Named("n_tangent_rows") = n_tangent_rows,
    Rcpp::Named("n_target_rows") = n_target_rows,

    // inputs
    Rcpp::Named("exponent") = exponent,
    Rcpp::Named("segments") = segments,
    Rcpp::Named("amount_col_used") = amount_col,
    Rcpp::Named("dropped_nonpos_benefit") = dropped_nonpos_benefit,
    Rcpp::Named("dropped_missing_action") = dropped_missing_action,

    // variable block
    Rcpp::Named("var_block_id") = (var_block_id == 0 ? NA_REAL : static_cast<double>(var_block_id)),
    Rcpp::Named("var_col_range") = range_to_R(col_start, col_end),

    // constraint blocks
    Rcpp::Named("link_block_id") = static_cast<double>(link_bid),
    Rcpp::Named("link_row_range") = range_to_R(link_row_start, link_row_end),

    Rcpp::Named("tangent_block_id") = static_cast<double>(tan_bid),
    Rcpp::Named("tangent_row_range") = range_to_R(tan_row_start, tan_row_end),

    Rcpp::Named("target_block_id") = static_cast<double>(targ_bid),
    Rcpp::Named("target_row_range") = range_to_R(targ_row_start, targ_row_end),

    Rcpp::Named("note") =
      "Registers: 1 var block (t,y) + 3 constraint blocks (linking, tangents, targets)."
  );
}
