#include "OptimizationProblem.h"

// [[Rcpp::export]]
SEXP rcpp_new_optimization_problem(std::size_t nrow = 1000000,
                                   std::size_t ncol = 1000000,
                                   std::size_t ncell= 100000) {
  //
  OptimizationProblem* x = new OptimizationProblem(nrow, ncol, ncell);
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::XPtr<OptimizationProblem>(x,
                                                                        true);
  return(op);
}


static inline Rcpp::DataFrame registry_to_df(const OptimizationProblem* op) {
  const std::size_t n = op->_registry.size();

  Rcpp::NumericVector id(n);
  Rcpp::CharacterVector kind(n), name(n), tag(n);
  Rcpp::NumericVector start1(n), end1(n); // 1-based inclusive-ish friendly

  for (std::size_t i = 0; i < n; ++i) {
    const auto& b = op->_registry[i];
    id[i]    = (double)b.id;
    kind[i]  = b.kind;
    name[i]  = b.name;
    tag[i]   = b.tag;

    // internally [start,end) 0-based
    // expose as [start+1, end] (end is inclusive-friendly)
    start1[i] = (b.end > b.start) ? (double)(b.start + 1) : NA_REAL;
    end1[i]   = (b.end > b.start) ? (double)(b.end)       : NA_REAL;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("id") = id,
    Rcpp::Named("kind") = kind,
    Rcpp::Named("name") = name,
    Rcpp::Named("start") = start1,
    Rcpp::Named("end") = end1,
    Rcpp::Named("tag") = tag,
    Rcpp::_["stringsAsFactors"] = false
  );
}


// [[Rcpp::export]]
Rcpp::List rcpp_optimization_problem_as_list(SEXP x) {

  Rcpp::XPtr<OptimizationProblem> op =
    Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // --- defensive consistency checks
  if (op->_lb.size() != op->_obj.size())
    Rcpp::stop("Inconsistent model: lb.size() != obj.size().");
  if (op->_ub.size() != op->_obj.size())
    Rcpp::stop("Inconsistent model: ub.size() != obj.size().");
  if (op->_vtype.size() != op->_obj.size())
    Rcpp::stop("Inconsistent model: vtype.size() != obj.size().");

  if (op->_sense.size() != op->_rhs.size())
    Rcpp::stop("Inconsistent model: sense.size() != rhs.size().");
  if (op->_name.size() != op->_rhs.size())
    Rcpp::stop("Inconsistent model: name.size() != rhs.size().");

  // bounds (tal cual lo tenías)
  Rcpp::List bounds = Rcpp::List::create(
    Rcpp::Named("lower") = Rcpp::List::create(
      Rcpp::Named("ind") = Rcpp::seq(1, (double)op->_obj.size()),
      Rcpp::Named("val") = op->_lb
    ),
    Rcpp::Named("upper") = Rcpp::List::create(
      Rcpp::Named("ind") = Rcpp::seq(1, (double)op->_obj.size()),
      Rcpp::Named("val") = op->_ub
    )
  );

  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("A_i") = op->_A_i,
    Rcpp::Named("A_j") = op->_A_j,
    Rcpp::Named("A_x") = op->_A_x,
    Rcpp::Named("obj") = op->_obj,
    Rcpp::Named("bounds") = bounds,
    Rcpp::Named("rhs") = op->_rhs,
    Rcpp::Named("sense") = op->_sense,
    Rcpp::Named("vtype") = op->_vtype,
    Rcpp::Named("name") = op->_name,
    Rcpp::Named("yvar") = op->_id_pow_variables,
    Rcpp::Named("xvar") = op->_id_variables,
    Rcpp::Named("boundary_size") = op->_boundary_size,
    Rcpp::Named("n_pu") = op->_n_pu,
    Rcpp::Named("n_x") = op->_n_x,
    Rcpp::Named("n_z") = op->_n_z,
    Rcpp::Named("n_y_pu") = op->_n_y_pu,
    Rcpp::Named("n_y_action") = op->_n_y_action,
    Rcpp::Named("n_y_intervention") = op->_n_y_intervention,
    Rcpp::Named("n_u_intervention") = op->_n_u_intervention,
    Rcpp::Named("w_offset") = op->_w_offset,
    Rcpp::Named("x_offset") = op->_x_offset,
    Rcpp::Named("z_offset") = op->_z_offset,
    Rcpp::Named("y_pu_offset") = op->_y_pu_offset,
    Rcpp::Named("y_action_offset") = op->_y_action_offset,
    Rcpp::Named("u_intervention_offset") = op->_u_intervention_offset,
    Rcpp::Named("y_intervention_offset") = op->_y_intervention_offset,
    Rcpp::Named("registry") = op->registry_as_df()
  );
}


// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncol(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->ncol());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_nrow(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->nrow());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncell(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->ncell());
}

// [[Rcpp::export]]
Rcpp::List rcpp_get_optimization_problem_A(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->A());
}
