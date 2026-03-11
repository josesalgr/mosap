#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>

// [[Rcpp::export]]
Rcpp::List rcpp_add_linking_w_le_sum_x(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    std::string block_name = "linking_w_le_sum_x",
    std::string tag = ""
) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!dist_actions_data.containsElementNamed("internal_pu"))
    Rcpp::stop("dist_actions_data must contain column 'internal_pu'.");
  if (!dist_actions_data.containsElementNamed("internal_row"))
    Rcpp::stop("dist_actions_data must contain column 'internal_row'.");

  Rcpp::IntegerVector ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector irow = dist_actions_data["internal_row"];

  const int n = dist_actions_data.nrows();
  if (n == 0) {
    return Rcpp::List::create(
      Rcpp::Named("n_constraints_added") = 0,
      Rcpp::Named("block_id") = NA_REAL,
      Rcpp::Named("row_start") = NA_REAL,
      Rcpp::Named("row_end") = NA_REAL,
      Rcpp::Named("block_name") = block_name,
      Rcpp::Named("tag") = tag
    );
  }

  // group x columns by PU
  std::unordered_map<int, std::vector<int>> xcols_by_pu;
  xcols_by_pu.reserve(1024);

  for (int k = 0; k < n; ++k) {

    const int ipu1  = ipu[k];
    const int irow1 = irow[k];

    if (ipu1 == NA_INTEGER || irow1 == NA_INTEGER) continue;
    if (ipu1 <= 0 || irow1 <= 0) {
      Rcpp::stop("internal_pu/internal_row must be positive 1-based.");
    }

    const int pu0  = ipu1 - 1;
    const int row0 = irow1 - 1;

    if (op->_n_pu > 0 && pu0 >= op->_n_pu) {
      Rcpp::stop("internal_pu out of range: %d (n_pu=%d).", ipu1, op->_n_pu);
    }
    if (op->_n_x > 0 && row0 >= op->_n_x) {
      Rcpp::stop("internal_row out of range: %d (n_x=%d).", irow1, op->_n_x);
    }

    const int col_x = op->_x_offset + row0;
    xcols_by_pu[pu0].push_back(col_x);
  }

  const std::size_t row_start = op->nrow_used();

  if (tag.empty()) tag = "n=" + std::to_string(n);
  else tag = tag + ";n=" + std::to_string(n);

  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  int added = 0;

  for (auto &kv : xcols_by_pu) {
    const int pu0 = kv.first;
    std::vector<int> cols = kv.second;

    if (cols.empty()) continue;

    const int col_w = op->_w_offset + pu0;

    // w_i - sum_a x_ia <= 0
    std::vector<int> idx;
    std::vector<double> val;

    idx.reserve(cols.size() + 1);
    val.reserve(cols.size() + 1);

    idx.push_back(col_w);
    val.push_back(1.0);

    for (int col_x : cols) {
      idx.push_back(col_x);
      val.push_back(-1.0);
    }

    op->addRow(idx, val, "<=", 0.0, "w_le_sum_x");
    ++added;
  }

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("block_id")  = static_cast<double>(bid),
    Rcpp::Named("row_start") = static_cast<double>(row_start + 1),
    Rcpp::Named("row_end")   = static_cast<double>(row_end),
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = tag
  );
}
