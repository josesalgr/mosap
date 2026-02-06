// rcpp_set_objective_min_fragmentation_actions_by_action.cpp
#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

#include <unordered_map>
#include <vector>
#include <string>
#include <cmath>
#include <algorithm>
#include <cstdint>

// key (ipu, iact) -> x col
static inline long long key2(int a, int b) {
  return ( (static_cast<long long>(a) << 32) ^ static_cast<unsigned int>(b) );
}

// helper: undirected key (i<j), i/j 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
bool rcpp_set_objective_min_fragmentation_actions_by_action(
    SEXP x,
    Rcpp::DataFrame dist_actions_data,   // model-ready: internal_row, internal_pu, internal_action
    Rcpp::DataFrame relation_data,       // internal_pu1, internal_pu2, weight (diag allowed)
    Rcpp::Nullable<Rcpp::IntegerVector> actions_to_use = R_NilValue, // optional internal_action ids (1-based)
    Rcpp::Nullable<Rcpp::NumericVector> action_weights = R_NilValue, // optional weights (len n_actions or len(actions_to_use))
    double weight_multiplier = 1.0,
    std::string block_name = "objective_min_fragmentation_actions_by_action",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  op->_modelsense = "min";

  const int n_pu = op->_n_pu;
  const int n_x  = op->_n_x;

  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");
  if (n_x  <= 0) Rcpp::stop("This objective requires action variables, but op->_n_x <= 0.");
  if (op->_obj.empty()) Rcpp::stop("Model has zero variables. Build base variables first.");
  if (op->_x_offset < 0) Rcpp::stop("op->_x_offset not initialized.");

  if (!R_finite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  // avoid double-adding y_action
  if (op->_n_y_action > 0) {
    Rcpp::stop("Action-fragmentation auxiliaries already exist (_n_y_action > 0).");
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
  Rcpp::NumericVector wgt = relation_data["weight"];
  const int m = relation_data.nrows();
  if (m <= 0) Rcpp::stop("relation_data has 0 rows.");

  // dist_actions (model-ready)
  Rcpp::IntegerVector dar = dist_actions_data["internal_row"];     // 1..n_x
  Rcpp::IntegerVector dap = dist_actions_data["internal_pu"];      // 1..n_pu
  Rcpp::IntegerVector daa = dist_actions_data["internal_action"];  // 1..n_actions
  const int da_n = dist_actions_data.nrows();

  // assumption: model-ready dist_actions has exactly n_x rows
  if (da_n != n_x) {
    Rcpp::stop("dist_actions_data.nrows() must equal op->_n_x (model-ready dist_actions expected).");
  }

  // infer n_actions as max internal_action
  int n_actions = 0;
  for (int r = 0; r < da_n; ++r) {
    const int a = daa[r];
    if (a == NA_INTEGER) continue;
    if (a > n_actions) n_actions = a;
  }
  if (n_actions <= 0) Rcpp::stop("Could not infer number of actions (max internal_action <= 0).");

  // choose actions A (1-based ids)
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
  } else {
    A.reserve((std::size_t)n_actions);
    for (int a = 1; a <= n_actions; ++a) A.push_back(a);
  }
  const int nA = (int)A.size();

  // action weights vector wA (1-based). Default = 1.
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

  // map (ipu, action) -> x_col (0-based var index), sparse via unordered_map
  std::unordered_map<long long, int> xcol;
  xcol.reserve((std::size_t)da_n * 2);

  for (int r = 0; r < da_n; ++r) {
    const int row1 = dar[r]; // 1..n_x
    const int ipu  = dap[r]; // 1..n_pu
    const int act  = daa[r]; // 1..n_actions

    if (row1 == NA_INTEGER || ipu == NA_INTEGER || act == NA_INTEGER) continue;

    if (row1 < 1 || row1 > n_x) Rcpp::stop("dist_actions_data$internal_row out of range.");
    if (ipu  < 1 || ipu  > n_pu) Rcpp::stop("dist_actions_data$internal_pu out of range.");
    if (act  < 1 || act  > n_actions) Rcpp::stop("dist_actions_data$internal_action out of range.");

    const int col_x = op->_x_offset + (row1 - 1);
    if (col_x < 0 || col_x >= (int)op->_obj.size()) {
      Rcpp::stop("Computed x col out of bounds: check x_offset/internal_row/object size.");
    }

    xcol[key2(ipu, act)] = col_x;
  }

  // ============================
  // Build self_w + unique undirected edges (same as PU objective)
  // ============================
  std::vector<double> self_w(n_pu + 1, 0.0);
  std::unordered_map<std::uint64_t, double> edge_w;
  edge_w.reserve((std::size_t)m * 2);

  for (int r = 0; r < m; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) Rcpp::stop("pu index out of range in relation_data.");

    const double we = (double)wgt[r];
    if (!R_finite(we) || we < 0.0) Rcpp::stop("relation_data weight must be finite and >= 0.");

    if (i1 == j1) {
      self_w[i1] += we; // diagonal contribution already precomputed in R if needed
    } else {
      int a = i1, b = j1;
      if (a > b) std::swap(a, b);
      const std::uint64_t k = edge_key_undirected(a, b);

      auto it = edge_w.find(k);
      if (it == edge_w.end()) edge_w.emplace(k, we);
      else it->second = std::max(it->second, we);
    }
  }

  // sort edges by key so b ordering is deterministic and reusable
  std::vector<std::pair<std::uint64_t, double>> edges;
  edges.reserve(edge_w.size());
  for (const auto& kv : edge_w) edges.push_back(kv);
  std::sort(edges.begin(), edges.end(),
            [](const auto& a, const auto& b){ return a.first < b.first; });

  const int k_edges = (int)edges.size();

  // incident shared per PU (from unique undirected edges)
  std::vector<double> incident_w(n_pu + 1, 0.0);
  std::vector<int> e_i; e_i.reserve(k_edges);
  std::vector<int> e_j; e_j.reserve(k_edges);
  std::vector<double> e_w; e_w.reserve(k_edges);

  for (const auto& kv : edges) {
    const std::uint64_t key = kv.first;
    const double we = kv.second;
    const int a = static_cast<int>(key >> 32);
    const int b = static_cast<int>(key & 0xFFFFFFFFu);
    e_i.push_back(a); e_j.push_back(b); e_w.push_back(we);
    incident_w[a] += we;
    incident_w[b] += we;
  }

  // ============================
  // Reset objective (this defines objective from scratch)
  // ============================
  std::fill(op->_obj.begin(), op->_obj.end(), 0.0);

  // ============================
  // Linear term on x_{i,a}: penalty * wA[a] * (incident_i + self_i)
  // ============================
  for (int idx = 0; idx < nA; ++idx) {
    const int act = A[(std::size_t)idx];
    const double aw = wA[(std::size_t)act];
    if (aw == 0.0) continue;

    for (int i = 1; i <= n_pu; ++i) {
      auto itx = xcol.find(key2(i, act));
      if (itx == xcol.end()) continue; // missing pair -> no variable -> skip
      const int xi = itx->second;

      const double coef = weight_multiplier * aw * (incident_w[i] + self_w[i]);
      if (coef != 0.0) op->_obj[(std::size_t)xi] += coef;
    }
  }

  // ============================
  // Add b_{e,a} variables and AND constraints (same as PU objective)
  // coef(b) = -2 * penalty * shared_ij * wA[a]
  // ============================
  const int mA = k_edges * nA;

  op->_y_action_offset = (int)op->ncol_used();
  op->_n_y_action      = mA;

  op->_obj.reserve(op->_obj.size() + (std::size_t)mA);
  op->_vtype.reserve(op->_vtype.size() + (std::size_t)mA);
  op->_lb.reserve(op->_lb.size() + (std::size_t)mA);
  op->_ub.reserve(op->_ub.size() + (std::size_t)mA);

  // metadata tag
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "modelsense=min"
    ";kind=objective"
    ";multiplier=" + std::to_string(weight_multiplier) +
      ";n_actions=" + std::to_string(n_actions) +
      ";n_actions_used=" + std::to_string(nA) +
      ";n_edges_unique=" + std::to_string(k_edges);

  // constraints block
  const std::size_t cblock = op->beginConstraintBlock(block_name + "::constraints", full_tag);

  // add b vars in deterministic order: for each edge e, loop actions k
  int b0 = (int)op->ncol_used();

  for (int e = 0; e < k_edges; ++e) {
    const int i = e_i[e];
    const int j = e_j[e];
    const double we = e_w[e];

    for (int kk = 0; kk < nA; ++kk) {
      const int act = A[(std::size_t)kk];
      const double aw = wA[(std::size_t)act];

      const int bcol = (int)op->ncol_used();

      // b var (continuous is enough; x is binary)
      const double bcoef = (-2.0) * weight_multiplier * we * aw;

      op->_obj.push_back(bcoef);
      op->_vtype.push_back("C");
      op->_lb.push_back(0.0);
      op->_ub.push_back(1.0);

      // get x_{i,a}, x_{j,a} (must exist to add AND constraints)
      auto iti = xcol.find(key2(i, act));
      auto itj = xcol.find(key2(j, act));
      if (iti == xcol.end() || itj == xcol.end()) {
        // If one is missing, skip constraints: min objective will set b=0,
        // and linear x terms already account for incident/self on existing x vars.
        continue;
      }

      const int xi = iti->second;
      const int xj = itj->second;

      // b <= xi  -> b - xi <= 0
      op->addRow({ bcol, xi }, { 1.0, -1.0 }, "<=", 0.0, "b_le_xi");
      // b <= xj  -> b - xj <= 0
      op->addRow({ bcol, xj }, { 1.0, -1.0 }, "<=", 0.0, "b_le_xj");
      // b >= xi + xj - 1 -> b - xi - xj >= -1
      op->addRow({ bcol, xi, xj }, { 1.0, -1.0, -1.0 }, ">=", -1.0, "b_ge_xi_plus_xj_minus1");
    }
  }

  op->endConstraintBlock(cblock, /*drop_if_empty=*/true);

  // register y-action block
  const std::size_t y0 = (std::size_t)op->_y_action_offset;
  const std::size_t y1 = y0 + (std::size_t)mA;

  if (y1 != op->ncol_used()) {
    Rcpp::stop("Internal error: y_action block size mismatch (y0+mA != ncol_used()).");
  }

  op->register_variable_block(
      block_name + "::b_action",
      y0, y1,
      full_tag + ";vtype=C;linearization=AND;b=e_by_action"
  );

  return true;
}
