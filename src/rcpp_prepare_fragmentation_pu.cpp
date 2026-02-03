// rcpp_prepare_fragmentation_pu.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <string>
#include <vector>
#include <cmath>      // std::isfinite

// [[Rcpp::export]]
Rcpp::List rcpp_prepare_fragmentation_pu(
    SEXP x,
    Rcpp::DataFrame relation_data,    // internal_pu1, internal_pu2, weight
    std::string block_name = "fragmentation_pu",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds: check op->_w_offset/op->_n_pu vs number of variables.");
  }

  // relation_data checks
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // ------------------------------------------------------------
  // Idempotencia: si ya existe, valida compatibilidad y retorna
  // ------------------------------------------------------------
  if (op->_n_y_pu > 0) {
    if (op->_n_y_pu != m) {
      Rcpp::stop(
        "PU-fragmentation auxiliaries already exist, but relation_data rows differ. "
        "Existing _n_y_pu=" + std::to_string(op->_n_y_pu) +
          ", new m=" + std::to_string(m) + "."
      );
    }
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("already_prepared") = true,
      Rcpp::Named("y_offset") = op->_y_pu_offset,
      Rcpp::Named("n_y") = op->_n_y_pu
    );
  }

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"]; // 1..n_pu
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"]; // 1..n_pu
  Rcpp::NumericVector wgt = relation_data["weight"];

  // Validación fuerte (índices y pesos), y de paso contar válidas
  int n_edges_valid = 0;
  for (int e = 0; e < m; ++e) {
    const int i1 = ip1[e];
    const int j1 = ip2[e];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;

    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range (must be 1..n_pu).");
    }

    const double we = (double)wgt[e];
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    ++n_edges_valid;
  }

  // ------------------------------------------------------------
  // 1) Crear variables y_e (una por fila en relation_data)
  //    IMPORTANTE: NO tocamos el objetivo existente; solo extendemos con coef=0.
  // ------------------------------------------------------------
  const int y0 = (int)op->ncol_used();
  op->_y_pu_offset = y0;
  op->_n_y_pu      = m;

  op->_obj.reserve(op->_obj.size() + (std::size_t)m);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)m);
  op->_lb.reserve(op->_lb.size() + (std::size_t)m);
  op->_ub.reserve(op->_ub.size() + (std::size_t)m);

  for (int e = 0; e < m; ++e) {
    op->_obj.push_back(0.0);   // coef objetivo = 0
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);
  }

  // Registry: variables
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=prepare"
    ";component=fragmentation_pu"
    ";n_edges=" + std::to_string(m) +
      ";n_edges_valid=" + std::to_string(n_edges_valid) +
      ";y_offset=" + std::to_string(y0) +
      ";vtype=B" +
      ";objective_touched=FALSE";

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::y_pu",
    (std::size_t)y0,
    (std::size_t)(y0 + m),
    full_tag
  );

  // ------------------------------------------------------------
  // 2) Restricciones de linealización para |w_i - w_j|
  // ------------------------------------------------------------
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  int n_constraints_added = 0;

  for (int e = 0; e < m; ++e) {
    const int i1 = ip1[e];
    const int j1 = ip2[e];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;

    const int i0 = i1 - 1;
    const int j0 = j1 - 1;

    const int y_col = y0 + e;
    const int wi    = op->_w_offset + i0;
    const int wj    = op->_w_offset + j0;

    op->addRow({ y_col, wi, wj }, { 1.0, -1.0,  1.0 }, ">=", 0.0, "y_ge_wi_minus_wj"); ++n_constraints_added;
    op->addRow({ y_col, wi, wj }, { 1.0,  1.0, -1.0 }, ">=", 0.0, "y_ge_wj_minus_wi"); ++n_constraints_added;
    op->addRow({ y_col, wi, wj }, { 1.0, -1.0, -1.0 }, "<=", 0.0, "y_le_wi_plus_wj");  ++n_constraints_added;
    op->addRow({ y_col, wi, wj }, { 1.0,  1.0,  1.0 }, "<=", 2.0, "y_le_2_minus_wi_minus_wj"); ++n_constraints_added;
  }

  const std::size_t cons_block_id = op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("already_prepared") = false,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("constraints_block_id") = (double)cons_block_id,
    Rcpp::Named("y_offset") = op->_y_pu_offset,
    Rcpp::Named("n_y") = op->_n_y_pu,
    Rcpp::Named("n_constraints_added") = n_constraints_added,
    Rcpp::Named("tag") = full_tag
  );
}
