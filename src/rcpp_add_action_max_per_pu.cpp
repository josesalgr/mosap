#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <string>

// [[Rcpp::export]]
Rcpp::List rcpp_add_action_max_per_pu(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,
    int max_per_pu = 1,
    Rcpp::IntegerVector internal_pu_ids = Rcpp::IntegerVector(),
    Rcpp::IntegerVector internal_action_ids = Rcpp::IntegerVector()
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // required columns
  for (auto nm : {"internal_pu", "internal_action", "internal_row"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  if (max_per_pu < 0) Rcpp::stop("max_per_pu must be >= 0.");

  Rcpp::IntegerVector da_ipu  = dist_actions_data["internal_pu"];
  Rcpp::IntegerVector da_iact = dist_actions_data["internal_action"];
  Rcpp::IntegerVector da_irow = dist_actions_data["internal_row"];
  const int n_da = dist_actions_data.nrows();

  // optional filters: internal_pu_ids / internal_action_ids
  std::unordered_set<int> pu_set;
  std::unordered_set<int> act_set;
  bool filter_pu = (internal_pu_ids.size() > 0);
  bool filter_act = (internal_action_ids.size() > 0);

  if (filter_pu) {
    pu_set.reserve((std::size_t)internal_pu_ids.size() * 2);
    for (int i = 0; i < internal_pu_ids.size(); ++i) pu_set.insert((int)internal_pu_ids[i]);
  }
  if (filter_act) {
    act_set.reserve((std::size_t)internal_action_ids.size() * 2);
    for (int i = 0; i < internal_action_ids.size(); ++i) act_set.insert((int)internal_action_ids[i]);
  }

  // group x variable columns by internal_pu
  // x column index assumed: op->_x_offset + (internal_row-1)
  std::unordered_map<int, std::vector<int>> cols_by_pu;
  cols_by_pu.reserve((std::size_t) (filter_pu ? internal_pu_ids.size() : 1024));

  for (int r = 0; r < n_da; ++r) {
    const int ipu = da_ipu[r];
    const int ia  = da_iact[r];
    const int irow = da_irow[r];

    if (ipu <= 0 || ia <= 0 || irow <= 0) {
      Rcpp::stop("dist_actions_data internal_* must be positive 1-based.");
    }

    if (filter_pu && pu_set.find(ipu) == pu_set.end()) continue;
    if (filter_act && act_set.find(ia) == act_set.end()) continue;

    cols_by_pu[ipu].push_back((int)(op->_x_offset + (irow - 1)));
  }

  // registry: begin constraint block
  const std::size_t row_start = op->nrow_used();
  std::string tag =
    "mode=action_max_per_pu"
    ";max=" + std::to_string(max_per_pu) +
      ";n_da=" + std::to_string(n_da) +
      ";filter_pu=" + std::to_string((int)filter_pu) +
      ";filter_act=" + std::to_string((int)filter_act);

  const std::size_t bid = op->beginConstraintBlock("action_max_per_pu", tag);

  int added = 0;

  // Add one row per PU that appears in cols_by_pu
  for (auto &kv : cols_by_pu) {
    const int ipu = kv.first;
    std::vector<int> &cols = kv.second;

    if (cols.empty()) continue; // 0 <= max, skip

    std::vector<double> vals(cols.size(), 1.0);

    // stable name (block name) - could also include ipu if you want
    std::string cname = "action_max_per_pu";

    op->addRow(cols, vals, "<=", (double)max_per_pu, cname);
    ++added;
  }

  const std::size_t row_end = op->nrow_used();
  op->endConstraintBlock(bid);

  auto range_to_R = [](std::size_t start0, std::size_t end0) {
    if (end0 <= start0) return Rcpp::NumericVector::create(NA_REAL, NA_REAL);
    return Rcpp::NumericVector::create(
      static_cast<double>(start0 + 1), // 1-based start
      static_cast<double>(end0)        // 1-based end
    );
  };

  return Rcpp::List::create(
    Rcpp::Named("n_constraints_added") = added,
    Rcpp::Named("block_id") = static_cast<double>(bid),
    Rcpp::Named("row_range") = range_to_R(row_start, row_end),
    Rcpp::Named("tag") = tag
  );
}
