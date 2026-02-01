#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_add_pu_locks(SEXP x,
                             Rcpp::DataFrame pu_data,
                             std::string block_name = "pu_locks",
                             std::string tag = "") {

  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // --- required columns
  if (!pu_data.containsElementNamed("locked_in")) {
    Rcpp::stop("pu_data must contain column 'locked_in'.");
  }
  if (!pu_data.containsElementNamed("locked_out")) {
    Rcpp::stop("pu_data must contain column 'locked_out'.");
  }

  const int n = pu_data.nrows();
  if (n == 0) {
    return Rcpp::List::create(
      Rcpp::Named("n_constraints_added") = 0,
      Rcpp::Named("n_lock_in") = 0,
      Rcpp::Named("n_lock_out") = 0,
      Rcpp::Named("block_id") = NA_REAL,
      Rcpp::Named("row_start") = NA_REAL,
      Rcpp::Named("row_end") = NA_REAL,
      Rcpp::Named("block_name") = block_name,
      Rcpp::Named("tag") = tag
    );
  }

  Rcpp::LogicalVector locked_in  = pu_data["locked_in"];
  Rcpp::LogicalVector locked_out = pu_data["locked_out"];

  // --- registry begin
  const std::size_t row_start = op->nrow_used();

  int n_lock_in  = 0;
  int n_lock_out = 0;
  int added      = 0;

  if (tag.empty()) {
    tag = "n_pu=" + std::to_string(n);
  } else {
    tag = tag + ";n_pu=" + std::to_string(n);
  }

  const std::size_t bid = op->beginConstraintBlock(block_name, tag);

  for (int i = 0; i < n; ++i) {
    const int col_w = op->_w_offset + i;

    // NA handling: if either is NA, skip the row
    if (Rcpp::LogicalVector::is_na(locked_in[i]) || Rcpp::LogicalVector::is_na(locked_out[i])) {
      continue;
    }

    if (locked_in[i]) {
      op->addRow({col_w}, {1.0}, "==", 1.0, "pu_lock_in");
      ++n_lock_in;
      ++added;
    } else if (locked_out[i]) {
      op->addRow({col_w}, {1.0}, "==", 0.0, "pu_lock_out");
      ++n_lock_out;
      ++added;
    }
  }

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  // opcional: si no se agregó nada, igual el bloque queda registrado como vacío.
  // si prefieres NO registrar bloques vacíos, dime y lo ajusto con "if (row_end>row_start)".

  // enrich tag (esto no reescribe el tag del bloque ya guardado; si quieres, hay que setear tag antes)
  // Por simplicidad lo dejamos tal cual.

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("n_lock_in") = n_lock_in,
    Rcpp::Named("n_lock_out") = n_lock_out,
    Rcpp::Named("block_id")  = static_cast<double>(bid),
    Rcpp::Named("row_start") = static_cast<double>(row_start + 1), // 1-based friendly
    Rcpp::Named("row_end")   = static_cast<double>(row_end),       // end (exclusive in 0-based)
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = tag
  );
}
