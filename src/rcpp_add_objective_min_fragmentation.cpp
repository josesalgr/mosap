// rcpp_add_objective_min_fragmentation.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <cmath>      // std::isfinite

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame relation_data,     // internal_pu1, internal_pu2, weight (mismo orden que prepare)
    double weight = 1.0,               // <-- NUEVO: peso MO
    double weight_multiplier = 1.0,
    std::string block_name = "objective_add_min_fragmentation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  if (!std::isfinite(weight) || weight < 0.0) {
    Rcpp::stop("weight must be finite and >= 0.");
  }
  if (!std::isfinite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  // Si weight == 0, devuelve rápido (pero aún puede registrar si quieres)
  if (weight == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "weight==0"
    );
  }

  // Debe existir el bloque y_pu creado previamente
  if (op->_n_y_pu <= 0 || op->_y_pu_offset < 0) {
    Rcpp::stop(
      "PU-fragmentation auxiliaries do not exist (missing y_pu block). "
      "Call rcpp_prepare_fragmentation_pu() first."
    );
  }

  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  if (m != op->_n_y_pu) {
    Rcpp::stop(
      "relation_data rows do not match existing y_pu block size. "
      "Existing _n_y_pu=" + std::to_string(op->_n_y_pu) +
        ", new m=" + std::to_string(m) + ". "
        "Pass the exact same relation used in prepare (same filtering/order)."
    );
  }

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector wgt = relation_data["weight"];

  const int y0 = op->_y_pu_offset;

  if ((std::size_t)(y0 + m) > op->_obj.size()) {
    Rcpp::stop("y_pu block out of bounds relative to objective vector size.");
  }

  double sum_added = 0.0;
  int n_edges_used = 0;

  for (int e = 0; e < m; ++e) {
    const int i1 = ip1[e];
    const int j1 = ip2[e];

    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    const double we = (double)wgt[e];
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    const double coef = weight * weight_multiplier * we; // <-- incluye peso MO

    op->_obj[(std::size_t)(y0 + e)] += coef;

    sum_added += coef;
    ++n_edges_used;
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_add"
    ";component=min_fragmentation"
    ";weight=" + std::to_string(weight) +
      ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_edges=" + std::to_string(m) +
      ";n_edges_used=" + std::to_string(n_edges_used) +
      ";sum_added=" + std::to_string(sum_added) +
      ";additive=TRUE";

  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)y0,
    (std::size_t)(y0 + m),
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("y_range") = Rcpp::NumericVector::create((double)y0 + 1.0, (double)(y0 + m)),
    Rcpp::Named("n_edges_used") = n_edges_used,
    Rcpp::Named("sum_added") = sum_added
  );
}
