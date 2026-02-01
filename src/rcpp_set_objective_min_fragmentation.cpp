// rcpp_set_objective_min_fragmentation.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <algorithm> // std::fill
#include <string>
#include <vector>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List rcpp_set_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame pu_data,          // no se usa aquí, pero lo dejamos por compat
    Rcpp::DataFrame relation_data,    // internal_pu1, internal_pu2, weight
    double weight_multiplier = 1.0,
    std::string block_name = "objective_min_fragmentation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  if (op->_obj.empty()) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds: check op->_w_offset/op->_n_pu vs number of variables.");
  }

  // evita doble creación de y_pu
  if (op->_n_y_pu > 0) {
    Rcpp::stop("PU-fragmentation auxiliaries already exist (_n_y_pu > 0).");
  }

  // relation_data checks
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"]; // 1..n_pu
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"]; // 1..n_pu
  Rcpp::NumericVector wgt = relation_data["weight"];

  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // Definimos el objetivo desde cero
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // ------------------------------------------------------------
  // 1) Agregar y_e variables al final (una por arista en relation_data)
  // ------------------------------------------------------------
  const int y0 = (int)op->ncol_used();
  op->_y_pu_offset = y0;
  op->_n_y_pu      = m;

  op->_obj.reserve(op->_obj.size() + (std::size_t)m);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)m);
  op->_lb.reserve(op->_lb.size() + (std::size_t)m);
  op->_ub.reserve(op->_ub.size() + (std::size_t)m);

  double sum_weights = 0.0;
  int n_edges_used = 0;

  for (int e = 0; e < m; ++e) {
    const int i1 = ip1[e];
    const int j1 = ip2[e];

    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range.");
    }

    const double we = (double)wgt[e];
    if (!R_finite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    const double coef = weight_multiplier * we;

    op->_obj.push_back(coef);   // min sum (coef * y_e)
    op->_vtype.push_back("B");
    op->_lb.push_back(0.0);
    op->_ub.push_back(1.0);

    sum_weights += coef;
    ++n_edges_used;
  }

  // ------------------------------------------------------------
  // 2) Restricciones de linealización para |w_i - w_j|
  //    y >= w_i - w_j
  //    y >= w_j - w_i
  //    y <= w_i + w_j
  //    y <= 2 - w_i - w_j   <=>  y + w_i + w_j <= 2
  // ------------------------------------------------------------
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective"
    ";modelsense=min"
    ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_edges=" + std::to_string(m) +
      ";sum_weights=" + std::to_string(sum_weights) +
      ";objective_reset=TRUE";

  const std::size_t obj_block_id = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(),   // el objetivo está definido sobre el vector completo (coef=0 fuera de y)
    full_tag
  );

  const std::size_t y_block_id = op->register_variable_block(
    block_name + "::y_pu",
    (std::size_t)y0,
    (std::size_t)(y0 + m),
    full_tag + ";vtype=B"
  );

  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  int n_constraints_added = 0;

  for (int e = 0; e < m; ++e) {
    const int i1 = ip1[e];
    const int j1 = ip2[e];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;

    // 0-based indices dentro del bloque w
    const int i0 = i1 - 1;
    const int j0 = j1 - 1;

    const int y_col = y0 + e;
    const int wi = op->_w_offset + i0;
    const int wj = op->_w_offset + j0;

    // (1) y - w_i + w_j >= 0
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, -1.0, 1.0 },
        ">=", 0.0,
        "y_ge_wi_minus_wj"
    );
    ++n_constraints_added;

    // (2) y + w_i - w_j >= 0
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, 1.0, -1.0 },
        ">=", 0.0,
        "y_ge_wj_minus_wi"
    );
    ++n_constraints_added;

    // (3) y - w_i - w_j <= 0  (y <= w_i + w_j)
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, -1.0, -1.0 },
        "<=", 0.0,
        "y_le_wi_plus_wj"
    );
    ++n_constraints_added;

    // (4) y + w_i + w_j <= 2  (y <= 2 - w_i - w_j)
    op->addRow(
        std::vector<int>{ y_col, wi, wj },
        std::vector<double>{ 1.0, 1.0, 1.0 },
        "<=", 2.0,
        "y_le_2_minus_wi_minus_wj"
    );
    ++n_constraints_added;
  }

  op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("objective_block_id") = (double)obj_block_id,
    Rcpp::Named("y_block_id") = (double)y_block_id,
    Rcpp::Named("y_offset") = op->_y_pu_offset,
    Rcpp::Named("n_y") = op->_n_y_pu,
    Rcpp::Named("n_constraints_added") = n_constraints_added,
    Rcpp::Named("sum_weights") = sum_weights
  );
}
