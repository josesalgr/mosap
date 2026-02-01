#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_add_linking_x_le_w(SEXP x,
                                   Rcpp::DataFrame dist_actions_data,
                                   std::string block_name = "linking_x_le_w",
                                   std::string tag = "") {

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

  // ---- record start (0-based row index)
  const std::size_t row_start = op->nrow_used();

  // enrich tag
  if (tag.empty()) tag = "n=" + std::to_string(n);
  else tag = tag + ";n=" + std::to_string(n);

  // We'll open the block, but only keep it if we actually add constraints
  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  int added = 0;

  for (int k = 0; k < n; ++k) {

    const int ipu1  = ipu[k];   // 1-based expected
    const int irow1 = irow[k];  // 1-based expected

    if (ipu1 == NA_INTEGER || irow1 == NA_INTEGER) continue;
    if (ipu1 <= 0 || irow1 <= 0) Rcpp::stop("internal_pu/internal_row must be positive 1-based.");

    const int pu0  = ipu1 - 1;      // 0-based
    const int row0 = irow1 - 1;     // 0-based row in dist_actions_model

    // optional defensive range checks (only if counters are set)
    if (op->_n_pu > 0 && pu0 >= op->_n_pu) {
      Rcpp::stop("internal_pu out of range: %d (n_pu=%d).", ipu1, op->_n_pu);
    }
    if (op->_n_x > 0 && row0 >= op->_n_x) {
      Rcpp::stop("internal_row out of range: %d (n_x=%d).", irow1, op->_n_x);
    }

    const int col_w = op->_w_offset + pu0;
    const int col_x = op->_x_offset + row0;

    // x - w <= 0
    op->addRow(
        std::vector<int>{col_x, col_w},
        std::vector<double>{1.0, -1.0},
        "<=",
        0.0,
        "x_le_w"
    );

    ++added;
  }

  const std::size_t row_end = op->nrow_used();

  // close block
  op->endConstraintBlock(bid);

  // If you prefer not to register empty blocks, the clean way is:
  // - delay beginConstraintBlock until first added row, OR
  // - add a helper in OptimizationProblem to pop last registry entry.
  //
  // With current OptimizationProblem.h, we already registered.
  // So: if you really want "no empty blocks", do delay-open pattern.
  //
  // For now we just report what happened.

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("block_id")  = static_cast<double>(bid),
    Rcpp::Named("row_start") = static_cast<double>(row_start + 1), // 1-based friendly
    Rcpp::Named("row_end")   = static_cast<double>(row_end),
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = tag
  );
}
