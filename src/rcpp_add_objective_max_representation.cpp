#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <algorithm>
#include <cmath>
#include <unordered_set>

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_max_representation(
    SEXP x,
    Rcpp::DataFrame dist_features_data,
    std::string amount_col = "amount",
    Rcpp::IntegerVector features_to_use = Rcpp::IntegerVector(), // internal_feature ids (1..n_features)
    std::string internal_feature_col = "internal_feature",       // column in dist_features_data
    double weight = 1.0,                                         // NEW: scaling for add()
    std::string block_name = "objective_add_max_representation",
    std::string tag = ""
)
{
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // must have variables
  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Call rcpp_add_base_variables() first.");
  }

  // must have z
  if (op->_n_z <= 0) {
    Rcpp::stop("add_max_representation requires z variables. Build with add_z=TRUE and non-empty dist_features_data.");
  }

  if (!std::isfinite(weight)) {
    Rcpp::stop("weight must be finite.");
  }

  // amount column
  if (!dist_features_data.containsElementNamed(amount_col.c_str())) {
    Rcpp::stop("Column '" + amount_col + "' not found in dist_features_data.");
  }

  // size check (dist_features_data is 1 row per z var)
  const int n_df = dist_features_data.nrows();
  if (n_df != op->_n_z) {
    Rcpp::stop(
      "dist_features_data nrows (" + std::to_string(n_df) +
        ") must match number of z vars (" + std::to_string(op->_n_z) + ")."
    );
  }

  // z block bounds check
  const int z0 = op->_z_offset;
  const int z1 = op->_z_offset + op->_n_z; // exclusive
  if (z0 < 0 || z1 > (int)op->_obj.size()) {
    Rcpp::stop("z block out of bounds: check op->_z_offset/op->_n_z and that z variables exist.");
  }

  // IMPORTANT: do NOT change modelsense here (additive function)
  // IMPORTANT: do NOT reset objective here (additive function)

  // read amount
  Rcpp::NumericVector amount = dist_features_data[amount_col.c_str()];

  // subset support
  const bool use_subset = (features_to_use.size() > 0);

  std::unordered_set<int> keep;
  Rcpp::IntegerVector ifeat; // internal_feature per z-row

  if (use_subset) {
    if (!dist_features_data.containsElementNamed(internal_feature_col.c_str())) {
      Rcpp::stop("Column '" + internal_feature_col + "' not found in dist_features_data.");
    }

    keep.reserve((std::size_t)features_to_use.size());
    for (int i = 0; i < features_to_use.size(); ++i) {
      const int f = features_to_use[i];
      if (f <= 0) {
        Rcpp::stop("features_to_use must contain internal feature ids >= 1.");
      }
      keep.insert(f);
    }

    ifeat = dist_features_data[internal_feature_col.c_str()];
  }

  double sum_added = 0.0;     // sum of coefficients ADDED (after applying weight)
  int used = 0;               // number of z entries touched (kept by subset)
  int skipped = 0;            // subset skipped rows
  double sum_amount_raw = 0.0; // raw sum of amounts used (before weight)

  for (int t = 0; t < op->_n_z; ++t) {
    const double a = (double)amount[t];
    if (!std::isfinite(a) || a < 0.0) {
      Rcpp::stop("Non-finite or negative amount at dist_features_data row " + std::to_string(t + 1) + ".");
    }

    if (use_subset) {
      const int f = ifeat[t];
      if (f <= 0) {
        Rcpp::stop("Non-positive internal_feature at dist_features_data row " + std::to_string(t + 1) + ".");
      }
      if (keep.find(f) == keep.end()) {
        ++skipped;
        continue;
      }
    }

    const double add = weight * a;
    op->_obj[z0 + t] += add;

    sum_amount_raw += a;
    sum_added += add;
    ++used;
  }

  // registry: record objective touched z block
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=" + op->_modelsense +
    ";amount_col=" + amount_col +
    ";n_z=" + std::to_string(op->_n_z) +
    ";n_used=" + std::to_string(used) +
    ";n_skipped=" + std::to_string(skipped) +
    ";sum_amount_raw=" + std::to_string(sum_amount_raw) +
    ";sum_added=" + std::to_string(sum_added) +
    ";weight=" + std::to_string(weight) +
    ";reset_objective=FALSE";

  if (use_subset) {
    full_tag +=
      ";subset=TRUE"
      ";subset_n=" + std::to_string((int)features_to_use.size()) +
        ";internal_feature_col=" + internal_feature_col;
  } else {
    full_tag += ";subset=FALSE";
  }

  const std::size_t block_id = op->register_objective_block(
    block_name + "::z",
    (std::size_t)z0,
    (std::size_t)z1,
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("block_id") = (double)block_id,
    Rcpp::Named("z_range") = Rcpp::NumericVector::create((double)z0 + 1.0, (double)z1),
    Rcpp::Named("n_used") = used,
    Rcpp::Named("n_skipped") = skipped,
    Rcpp::Named("sum_amount_raw_used") = sum_amount_raw,
    Rcpp::Named("sum_added") = sum_added,
    Rcpp::Named("weight_used") = weight,
    Rcpp::Named("amount_col_used") = amount_col,
    Rcpp::Named("subset_used") = use_subset,
    Rcpp::Named("subset_size") = (double)features_to_use.size()
  );
}
