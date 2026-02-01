#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_add_action_locks(SEXP x, Rcpp::DataFrame dist_actions_data) {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (!dist_actions_data.containsElementNamed("status"))
    Rcpp::stop("dist_actions_data must contain column 'status'.");
  if (!dist_actions_data.containsElementNamed("internal_row"))
    Rcpp::stop("dist_actions_data must contain column 'internal_row'.");

  Rcpp::IntegerVector status = dist_actions_data["status"];
  Rcpp::IntegerVector irow   = dist_actions_data["internal_row"];

  const int n = dist_actions_data.nrows();

  int n_lock_in  = 0;
  int n_lock_out = 0;
  int n_added    = 0;

  // ---- registry block (lazy open, so no empty blocks)
  std::size_t block_id = 0;
  bool opened = false;

  // We'll return these for convenience (1-based friendly)
  double row_start_1 = NA_REAL;
  double row_end_1   = NA_REAL;

  for (int k = 0; k < n; ++k) {
    const int st = status[k];
    if (st == NA_INTEGER || st == 0) continue;

    if (st != 2 && st != 3) continue; // ignore other codes defensively

    // open the block at the first actual constraint
    if (!opened) {
      block_id = op->beginConstraintBlock("action_locks", "");
      opened = true;
      // start row (0-based) BEFORE adding anything
      row_start_1 = (double)op->nrow_used() + 1.0;
    }

    const int r1 = irow[k];
    if (r1 == NA_INTEGER) continue;
    if (r1 <= 0) Rcpp::stop("dist_actions_data$internal_row must be positive 1-based.");

    const int row0  = r1 - 1;                // 0-based within x block
    const int col_x = op->_x_offset + row0;  // 0-based absolute col index

    // bounds check (important)
    if (col_x < 0 || (std::size_t)col_x >= op->_obj.size())
      Rcpp::stop("x column out of bounds: x_offset + (internal_row-1) exceeds number of variables.");

    if (st == 2) { // locked-in
      op->addRow({col_x}, {1.0}, "==", 1.0, "action_lock_in");
      ++n_lock_in;
      ++n_added;
    } else {       // st == 3 locked-out
      op->addRow({col_x}, {1.0}, "==", 0.0, "action_lock_out");
      ++n_lock_out;
      ++n_added;
    }
  }

  std::size_t closed_id = 0;

  if (opened) {
    // set tag via the supported API (no direct access to internals)
    const std::string tag =
      "lock_in=" + std::to_string(n_lock_in) +
      ";lock_out=" + std::to_string(n_lock_out) +
      ";n_constraints=" + std::to_string(n_added);

    op->setActiveConstraintBlockTag(block_id, tag);

    // close and register: returns the registered block id (or 0 if dropped)
    closed_id = op->endConstraintBlock(block_id, /*drop_if_empty=*/true);

    // end row (1-based inclusive-friendly): end0 == nrow_used()
    // if block was dropped (closed_id==0), we keep NAs
    if (closed_id != 0) {
      row_end_1 = (double)op->nrow_used();
    } else {
      row_start_1 = NA_REAL;
      row_end_1   = NA_REAL;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = n_added,
    Rcpp::Named("n_lock_in") = n_lock_in,
    Rcpp::Named("n_lock_out") = n_lock_out,
    Rcpp::Named("block_id") = (double)closed_id,
    Rcpp::Named("row_start") = row_start_1,
    Rcpp::Named("row_end")   = row_end_1
  );
}
