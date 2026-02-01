#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_set>
#include <string>
#include <cmath>
#include <algorithm>

// clave 64-bit para (ipu, ifeat)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// Fija ub(z_is)=0 (y opcionalmente lb=0) cuando existe alguna acción con delta>0
// para ese par (pu, feature). Es decir: si existe "mejora posible" => z_is no elegible.
//
// dist_features_data: internal_pu, internal_feature   (1-based)
// dist_benefit_data : internal_pu, internal_feature, benefit (delta, puede ser + o -)
//
// [[Rcpp::export]]
Rcpp::List rcpp_fix_z_ineligible_by_positive_delta(SEXP x,
                                                   Rcpp::DataFrame dist_features_data,
                                                   Rcpp::DataFrame dist_benefit_data,
                                                   bool fix_lb_too = true,
                                                   double eps = 1e-12) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  for (auto nm : {"internal_pu", "internal_feature"}) {
    if (!dist_features_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_features_data must contain column '") + nm + "'.");
    }
  }
  for (auto nm : {"internal_pu", "internal_feature", "benefit"}) {
    if (!dist_benefit_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_benefit_data must contain column '") + nm + "'.");
    }
  }

  if (op->_z_offset < 0) Rcpp::stop("op->_z_offset is not initialized.");

  const int n_df = dist_features_data.nrows();
  if (n_df <= 0) {
    return Rcpp::List::create(
      Rcpp::Named("n_pairs_with_improvement") = 0,
      Rcpp::Named("n_z_fixed") = 0,
      Rcpp::Named("n_z_already_zero") = 0,
      Rcpp::Named("eps") = eps,
      Rcpp::Named("z_var_block_id") = NA_REAL,
      Rcpp::Named("z_col_range") = Rcpp::NumericVector::create(NA_REAL, NA_REAL),
      Rcpp::Named("tag") = ""
    );
  }

  const int z0 = op->_z_offset;           // 0-based start col for z
  const int z1 = op->_z_offset + n_df;    // 0-based end (exclusive)
  const int last_z_col = z1 - 1;

  if (last_z_col >= (int)op->_ub.size()) {
    Rcpp::stop("Index out of bounds: op->_ub smaller than z variables range. "
                 "Create base variables (including z) before calling this.");
  }
  if (fix_lb_too && last_z_col >= (int)op->_lb.size()) {
    Rcpp::stop("Index out of bounds: op->_lb smaller than z variables range.");
  }

  // set de (ipu,ifeat) con alguna mejora posible: delta > eps
  Rcpp::IntegerVector db_ipu   = dist_benefit_data["internal_pu"];
  Rcpp::IntegerVector db_ifeat = dist_benefit_data["internal_feature"];
  Rcpp::NumericVector db_ben   = dist_benefit_data["benefit"];
  const int n_db = dist_benefit_data.nrows();

  std::unordered_set<long long> has_improvement;
  has_improvement.reserve((std::size_t)std::min(n_df, n_db));

  for (int r = 0; r < n_db; ++r) {
    const double delta = db_ben[r];
    if (!(delta > eps)) continue;  // solo mejoras

    const int ipu = db_ipu[r];
    const int ife = db_ifeat[r];
    if (ipu <= 0 || ife <= 0) {
      Rcpp::stop("internal_pu/internal_feature in dist_benefit_data must be 1-based positive integers.");
    }
    has_improvement.insert(key2(ipu, ife));
  }

  Rcpp::IntegerVector df_ipu   = dist_features_data["internal_pu"];
  Rcpp::IntegerVector df_ifeat = dist_features_data["internal_feature"];

  int n_fixed = 0;
  int n_already_zero = 0;

  for (int row_df = 0; row_df < n_df; ++row_df) {

    const int ipu = df_ipu[row_df];
    const int ife = df_ifeat[row_df];
    if (ipu <= 0 || ife <= 0) {
      Rcpp::stop("internal_pu/internal_feature in dist_features_data must be 1-based positive integers.");
    }

    if (has_improvement.find(key2(ipu, ife)) == has_improvement.end()) continue;

    const int col_z = op->_z_offset + row_df;

    if (op->_ub[col_z] <= 0.0) {
      ++n_already_zero;
      if (fix_lb_too) op->_lb[col_z] = 0.0;
      continue;
    }

    op->_ub[col_z] = 0.0;
    if (fix_lb_too) op->_lb[col_z] = 0.0;

    ++n_fixed;
  }

  // ---- Registry: variable block describing a bounds update affecting z range
  double z_var_block_id = NA_REAL;
  std::string tag = "";

  if ((n_fixed + n_already_zero) > 0) {
    tag =
      "mode=fix_z_ineligible_by_positive_delta"
      ";fix_lb_too=" + std::string(fix_lb_too ? "true" : "false") +
        ";eps=" + std::to_string(eps) +
        ";n_pairs_with_improvement=" + std::to_string((int)has_improvement.size()) +
        ";n_z_fixed=" + std::to_string(n_fixed) +
        ";n_z_already_zero=" + std::to_string(n_already_zero);

    const std::size_t bid = op->register_variable_block(
      "z_bounds_update_ineligible",
      (std::size_t)z0,
      (std::size_t)z1,
      tag
    );

    z_var_block_id = static_cast<double>(bid);
  }

  // 1-based friendly col range for z
  auto col_range_to_R = [](int start0, int end0) {
    if (end0 <= start0) return Rcpp::NumericVector::create(NA_REAL, NA_REAL);
    return Rcpp::NumericVector::create(
      static_cast<double>(start0 + 1),
      static_cast<double>(end0)       // end0 is exclusive in 0-based; this becomes inclusive in 1-based
    );
  };

  return Rcpp::List::create(
    Rcpp::Named("n_pairs_with_improvement") = (int)has_improvement.size(),
    Rcpp::Named("n_z_fixed") = n_fixed,
    Rcpp::Named("n_z_already_zero") = n_already_zero,
    Rcpp::Named("eps") = eps,

    // registry outputs (variable-side)
    Rcpp::Named("z_var_block_id") = z_var_block_id,
    Rcpp::Named("z_col_range")    = col_range_to_R(z0, z1),
    Rcpp::Named("tag")            = tag
  );
}
