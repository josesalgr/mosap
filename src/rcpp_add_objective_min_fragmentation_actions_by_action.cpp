// rcpp_add_objective_min_fragmentation_actions_by_action.cpp
#include "Package.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include <cstdint>

// key (ipu, iact)
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// undirected key (i<j), 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_min_fragmentation_actions_by_action(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu, internal_action
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight (diag allowed)
    Rcpp::Nullable<Rcpp::IntegerVector> actions_to_use = R_NilValue,
    Rcpp::Nullable<Rcpp::NumericVector> action_weights = R_NilValue,
    double weight = 1.0,
    double weight_multiplier = 1.0,
    std::string block_name = "objective_add_min_fragmentation_actions_by_action",
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

  if (weight == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "weight==0"
    );
  }

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but op->_n_x <= 0.");
  if (op->_x_offset < 0) Rcpp::stop("op->_x_offset not initialized.");

  // must have been prepared (b vars exist)
  if (op->_n_y_action <= 0 || op->_y_action_offset < 0) {
    Rcpp::stop(
      "Action-fragmentation auxiliaries do not exist (missing y_action block). "
      "Call rcpp_prepare_fragmentation_actions_by_action() first."
    );
  }

  // checks: relation_data
  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }
  // checks: dist_actions_data
  for (auto nm : {"internal_row", "internal_pu", "internal_action"}) {
    if (!dist_actions_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("dist_actions_data must contain column '") + nm + "'.");
    }
  }

  // relation arrays
  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector  wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"];     // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];      // 1..n_pu
  Rcpp::IntegerVector daa = dist_actions_data["internal_action"];  // 1..n_actions
  const int da_n = dist_actions_data.nrows();
  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // infer n_actions
  int n_actions = 0;
  for (int r = 0; r < da_n; ++r) {
    const int a = daa[r];
    if (a == NA_INTEGER) continue;
    if (a > n_actions) n_actions = a;
  }
  if (n_actions <= 0) Rcpp::stop("Could not infer number of actions (max internal_action <= 0).");

  // choose actions list A (sorted unique)
  std::vector<int> A;
  if (actions_to_use.isNotNull()) {
    Rcpp::IntegerVector tmp(actions_to_use);
    if (tmp.size() == 0) Rcpp::stop("actions_to_use provided but empty.");
    A.reserve((std::size_t)tmp.size());
    for (int k = 0; k < tmp.size(); ++k) {
      const int a = tmp[k];
      if (a < 1 || a > n_actions) Rcpp::stop("actions_to_use contains invalid internal_action id.");
      A.push_back(a);
    }
    std::sort(A.begin(), A.end());
    A.erase(std::unique(A.begin(), A.end()), A.end());
  } else {
    A.reserve((std::size_t)n_actions);
    for (int a = 1; a <= n_actions; ++a) A.push_back(a);
  }
  const int nA = (int)A.size();
  if (nA <= 0) Rcpp::stop("No actions selected for objective add.");

  // action weights vector wA (1-based), default=1
  std::vector<double> wA((std::size_t)n_actions + 1, 1.0);
  if (action_weights.isNotNull()) {
    Rcpp::NumericVector aw(action_weights);

    if ((int)aw.size() == n_actions) {
      for (int a = 1; a <= n_actions; ++a) {
        const double v = aw[a - 1];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[(std::size_t)a] = v;
      }
    } else if ((int)aw.size() == nA) {
      for (int k = 0; k < nA; ++k) {
        const double v = aw[k];
        if (!R_finite(v) || v < 0.0) Rcpp::stop("action_weights must be finite and >= 0.");
        wA[(std::size_t)A[(std::size_t)k]] = v;
      }
    } else {
      Rcpp::stop("action_weights length must be either n_actions or length(actions_to_use).");
    }
  }

  // canonicalize: self_w (diag) + unique undirected edges (sorted by key)
  std::vector<double> self_w(n_pu + 1, 0.0);
  std::unordered_map<std::uint64_t, double> edge_w;
  edge_w.reserve((std::size_t)m * 2);

  int n_diag_rows = 0;

  for (int r = 0; r < m; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) {
      Rcpp::stop("relation_data internal_pu1/internal_pu2 out of range (1..n_pu).");
    }

    const double we = (double)wgt[r];
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we) || we < 0.0) {
      Rcpp::stop("relation_data weight must be finite and >= 0.");
    }

    if (i1 == j1) {
      self_w[i1] += we;
      ++n_diag_rows;
      continue;
    }

    int a = i1, b = j1;
    if (a > b) std::swap(a, b);
    const std::uint64_t k = edge_key_undirected(a, b);

    auto it = edge_w.find(k);
    if (it == edge_w.end()) edge_w.emplace(k, we);
    else it->second = std::max(it->second, we);
  }

  const int k_edges = (int)edge_w.size();
  if (k_edges <= 0) Rcpp::stop("No usable off-diagonal edges found for action fragmentation objective add.");

  // must match prepared size
  const int mA_expected = k_edges * nA;
  if (mA_expected != op->_n_y_action) {
    Rcpp::stop(
      "Prepared y_action size does not match expected size from canonicalization. "
      "Existing _n_y_action=" + std::to_string(op->_n_y_action) +
        ", expected k_edges*nA=" + std::to_string(mA_expected) +
        " (k_edges=" + std::to_string(k_edges) + ", nA=" + std::to_string(nA) + ")."
    );
  }

  // sorted edges
  std::vector<std::pair<std::uint64_t, double>> edges;
  edges.reserve(edge_w.size());
  for (const auto& kv : edge_w) edges.push_back(kv);
  std::sort(edges.begin(), edges.end(),
            [](const auto& a, const auto& b){ return a.first < b.first; });

  // incident shared per PU
  std::vector<double> incident_w(n_pu + 1, 0.0);
  for (const auto& kv : edges) {
    const std::uint64_t key = kv.first;
    const double we = kv.second;
    const int a = static_cast<int>(key >> 32);
    const int b = static_cast<int>(key & 0xFFFFFFFFu);
    incident_w[a] += we;
    incident_w[b] += we;
  }

  // sparse map (ipu, act) -> x col
  std::unordered_map<long long, int> xcol;
  xcol.reserve((std::size_t)da_n * 2);

  for (int r = 0; r < da_n; ++r) {
    const int row1 = dar[r];
    const int ipu  = dap[r];
    const int act  = daa[r];
    if (row1 == NA_INTEGER || ipu == NA_INTEGER || act == NA_INTEGER) continue;

    const int col_x = op->_x_offset + (row1 - 1);
    xcol[key2(ipu, act)] = col_x;
  }

  // 1) add linear term on x_{i,a}
  double sum_linear_added = 0.0;

  for (int kk = 0; kk < nA; ++kk) {
    const int act = A[(std::size_t)kk];
    const double aw = wA[(std::size_t)act];
    if (aw == 0.0) continue;

    for (int i = 1; i <= n_pu; ++i) {
      auto itx = xcol.find(key2(i, act));
      if (itx == xcol.end()) continue;

      const double coef = weight * weight_multiplier * aw * (incident_w[i] + self_w[i]);
      if (coef != 0.0) {
        op->_obj[(std::size_t)itx->second] += coef;
        sum_linear_added += coef;
      }
    }
  }

  // 2) add edge term on b_{e,a} in EXACT prepare order: b_index = e*nA + kk
  const int b0 = op->_y_action_offset;
  double sum_edge_added = 0.0;

  for (int e = 0; e < k_edges; ++e) {
    const double we = edges[(std::size_t)e].second;

    for (int kk = 0; kk < nA; ++kk) {
      const int act = A[(std::size_t)kk];
      const double aw = wA[(std::size_t)act];
      if (aw == 0.0) continue;

      const int bcol = b0 + (e * nA + kk);
      const double coef = weight * weight_multiplier * (-2.0 * we * aw);

      if (coef != 0.0) {
        op->_obj[(std::size_t)bcol] += coef;
        sum_edge_added += coef;
      }
    }
  }

  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_add"
    ";component=min_fragmentation_actions_by_action"
    ";form=perimeter"
    ";weight=" + std::to_string(weight) +
      ";multiplier=" + std::to_string(weight_multiplier) +
      ";k_edges=" + std::to_string(k_edges) +
      ";n_actions_used=" + std::to_string(nA) +
      ";n_diag_rows=" + std::to_string(n_diag_rows) +
      ";sum_edge_added=" + std::to_string(sum_edge_added) +
      ";sum_linear_added=" + std::to_string(sum_linear_added) +
      ";additive=TRUE";

  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(),
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("y_offset") = op->_y_action_offset,
    Rcpp::Named("n_y") = op->_n_y_action,
    Rcpp::Named("k_edges") = k_edges,
    Rcpp::Named("n_actions_used") = nA,
    Rcpp::Named("sum_edge_added") = sum_edge_added,
    Rcpp::Named("sum_linear_added") = sum_linear_added
  );
}
