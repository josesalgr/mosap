#pragma once
#ifndef GUARD_OptimizationProblem_h
#define GUARD_OptimizationProblem_h

#include "Package.h"
#include <vector>
#include <string>
#include <cmath>

// CLASS DECLARATION
class OptimizationProblem
{
public:
  // ---------------------------
  // Registry structs
  // ---------------------------
  struct BlockRange {
    std::size_t id;          // stable handle (incremental)
    std::string kind;        // "variable" | "constraint" | "objective"
    std::string name;        // "w", "x", "targets", "budget", ...
    std::size_t start;       // inclusive, 0-based
    std::size_t end;         // exclusive, 0-based
    std::string tag;         // optional metadata
  };

  struct ActiveBlock {
    std::size_t id;
    std::string kind;        // currently only used for "constraint" (stack)
    std::string name;
    std::string tag;
    std::size_t start;       // inclusive start row (0-based)
  };

  // ---------------------------
  // Constructors / destructor
  // ---------------------------
  OptimizationProblem() {}

  OptimizationProblem(std::size_t nrow, std::size_t ncol, std::size_t ncell) {
    _obj.reserve(ncol);
    _A_i.reserve(ncell);
    _A_j.reserve(ncell);
    _A_x.reserve(ncell);

    _rhs.reserve(nrow);
    _sense.reserve(nrow);
    _name.reserve(nrow);

    _vtype.reserve(ncol);
    _lb.reserve(ncol);
    _ub.reserve(ncol);

    _id_pow_variables.reserve(ncol);
    _id_variables.reserve(ncol);
  }

  ~OptimizationProblem() {}

  // ---------------------------
  // Model fields
  // ---------------------------
  std::string _modelsense;

  std::vector<double> _obj;
  std::vector<std::size_t> _A_i;
  std::vector<std::size_t> _A_j;
  std::vector<double> _A_x;

  std::vector<double> _rhs;
  std::vector<std::string> _vtype;
  std::vector<double> _lb;
  std::vector<double> _ub;
  std::vector<std::string> _sense;
  std::vector<std::string> _name;

  std::vector<double> _id_pow_variables;
  std::vector<double> _id_variables;

  int _boundary_size = 0;

  int _n_pu = 0;
  int _n_x  = 0;
  int _n_z  = 0;

  int _n_y_pu = 0;
  int _n_y_action = 0;
  int _n_y_intervention = 0;
  int _n_u_intervention = 0;

  int _w_offset = 0;
  int _x_offset = 0;
  int _z_offset = 0;

  int _y_pu_offset = 0;
  int _y_action_offset = 0;
  int _y_intervention_offset = 0;
  int _u_intervention_offset = 0;

  // ---------------------------
  // Registry state
  // ---------------------------
  std::vector<BlockRange> _registry;
  std::vector<ActiveBlock> _active_constraint_blocks; // LIFO stack
  std::size_t _next_block_id = 1;

  // ---------------------------
  // Basic getters
  // ---------------------------
  inline std::size_t nrow() const { return _rhs.size(); }
  inline std::size_t ncol() const { return _obj.size(); }
  inline std::size_t ncell() const { return _A_x.size(); }

  inline std::size_t nrow_used() const { return _rhs.size(); }
  inline std::size_t ncol_used() const { return _obj.size(); }

  inline Rcpp::List A() const {
    return Rcpp::List::create(
      Rcpp::Named("i") = _A_i,
      Rcpp::Named("j") = _A_j,
      Rcpp::Named("x") = _A_x
    );
  }

  // ---------------------------
  // Registry core
  // ---------------------------
  inline void clear_registry() {
    _registry.clear();
    _active_constraint_blocks.clear();
    _next_block_id = 1;
  }

  inline std::size_t register_block(const std::string& kind,
                                    const std::string& name,
                                    std::size_t start,
                                    std::size_t end,
                                    const std::string& tag = "") {
    BlockRange b;
    b.id    = _next_block_id++;
    b.kind  = kind;
    b.name  = name;
    b.start = start;
    b.end   = end;
    b.tag   = tag;
    _registry.push_back(b);
    return b.id;
  }

  // Convenience wrappers
  inline std::size_t register_variable_block(const std::string& name,
                                             std::size_t col_start,
                                             std::size_t col_end,
                                             const std::string& tag = "") {
    return register_block("variable", name, col_start, col_end, tag);
  }

  inline std::size_t register_objective_block(const std::string& name,
                                              std::size_t col_start,
                                              std::size_t col_end,
                                              const std::string& tag = "") {
    return register_block("objective", name, col_start, col_end, tag);
  }

  inline std::size_t register_constraint_block(const std::string& name,
                                               std::size_t row_start,
                                               std::size_t row_end,
                                               const std::string& tag = "") {
    return register_block("constraint", name, row_start, row_end, tag);
  }

  // ---------------------------
  // Constraint blocks (auto ranges)
  // ---------------------------
  inline std::size_t beginConstraintBlock(const std::string& name,
                                          const std::string& tag = "") {
    ActiveBlock b;
    b.id    = _next_block_id++;     // reserve ID now
    b.kind  = "constraint";
    b.name  = name;
    b.tag   = tag;
    b.start = nrow_used();
    _active_constraint_blocks.push_back(b);
    return b.id;
  }

  inline void setActiveConstraintBlockTag(std::size_t id, const std::string& tag) {
    for (std::size_t i = _active_constraint_blocks.size(); i-- > 0; ) {
      if (_active_constraint_blocks[i].id == id) {
        _active_constraint_blocks[i].tag = tag;
        return;
      }
    }
    Rcpp::stop("setActiveConstraintBlockTag: active block id not found.");
  }

  // drop_if_empty=true => if no rows were added in this block, it won't be registered
  inline std::size_t endConstraintBlock(std::size_t id,
                                        bool drop_if_empty = true,
                                        const std::string& tag_override = "") {

    if (_active_constraint_blocks.empty())
      Rcpp::stop("endConstraintBlock: no active constraint blocks.");

    ActiveBlock b = _active_constraint_blocks.back();
    if (b.id != id)
      Rcpp::stop("endConstraintBlock: id mismatch (must close in LIFO order).");

    _active_constraint_blocks.pop_back();

    const std::size_t row_start = b.start;
    const std::size_t row_end   = nrow_used();

    if (drop_if_empty && row_end <= row_start) return 0;

    const std::string tag_final = (tag_override.empty() ? b.tag : tag_override);

    // IMPORTANT: preserve reserved id
    BlockRange closed;
    closed.id    = b.id;
    closed.kind  = "constraint";
    closed.name  = b.name;
    closed.start = row_start;
    closed.end   = row_end;
    closed.tag   = tag_final;

    _registry.push_back(closed);
    return closed.id;
  }

  // ---------------------------
  // Add one constraint row (0-based variable indices)
  // ---------------------------
  inline void addRow(const std::vector<int>& cols,
                     const std::vector<double>& vals,
                     std::string sense,
                     double rhs,
                     const std::string& name = "") {

    if (cols.size() != vals.size())
      Rcpp::stop("addRow: cols and vals length mismatch.");

    if (!(sense == "<=" || sense == ">=" || sense == "==" || sense == "="))
      Rcpp::stop("addRow: invalid sense.");

    if (sense == "=") sense = "==";

    if (!std::isfinite(rhs))
      Rcpp::stop("addRow: rhs must be finite.");

    const std::size_t row = _rhs.size(); // 0-based row index

    _rhs.push_back(rhs);
    _sense.push_back(sense);
    _name.push_back(name); // aligned 1:1 with rhs/sense

    const std::size_t nvar = _obj.size();
    for (std::size_t t = 0; t < cols.size(); ++t) {
      const int col = cols[t];
      if (col < 0 || (std::size_t)col >= nvar)
        Rcpp::stop("addRow: col out of range.");

      const double v = vals[t];
      if (!std::isfinite(v))
        Rcpp::stop("addRow: non-finite coefficient.");

      if (v == 0.0) continue;

      _A_i.push_back(row);
      _A_j.push_back((std::size_t)col);
      _A_x.push_back(v);
    }
  }

  // ---------------------------
  // Export registry as a data.frame (for R snapshot)
  // ---------------------------
  inline Rcpp::DataFrame registry_as_df() const {
    const int n = (int)_registry.size();
    Rcpp::NumericVector id(n), start(n), end(n);
    Rcpp::CharacterVector kind(n), name(n), tag(n);

    for (int i = 0; i < n; ++i) {
      id[i]    = (double)_registry[i].id;
      kind[i]  = _registry[i].kind;
      name[i]  = _registry[i].name;
      start[i] = (double)_registry[i].start; // 0-based, half-open in C++
      end[i]   = (double)_registry[i].end;
      tag[i]   = _registry[i].tag;
    }

    return Rcpp::DataFrame::create(
      Rcpp::Named("id") = id,
      Rcpp::Named("kind") = kind,
      Rcpp::Named("name") = name,
      Rcpp::Named("start0") = start,
      Rcpp::Named("end0") = end,
      Rcpp::Named("tag") = tag
    );
  }

};

#endif
