#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _prioriactions_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_A(SEXP);
extern SEXP _prioriactions_rcpp_objective_min_set(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_objective_max_coverage(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_activation(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_lock(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_connectivity_units(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_connectivity_actions(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_costs_units(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_costs_actions(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_target(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_budget(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_recovery(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

extern SEXP _prioriactions_rcpp_add_action_locks(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_min_cost(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_pu_locks(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_linking_x_le_w(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_base_variables(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_linking_z_le_w(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_fix_z_ineligible_by_positive_delta(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_target_conservation(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_target_recovery(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_target_mixed_total(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_max_benefit(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_min_loss(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_max_profit(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_max_net_profit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_max_net_profit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_set_objective_max_representation(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_linear_constraint(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_target_mixed_total_power(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_target_recovery_power(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_add_exclude_conservation_when_actions(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
  {"_prioriactions_rcpp_new_optimization_problem", (DL_FUNC) &_prioriactions_rcpp_new_optimization_problem, 3},
  {"_prioriactions_rcpp_optimization_problem_as_list", (DL_FUNC) &_prioriactions_rcpp_optimization_problem_as_list, 1},
  {"_prioriactions_rcpp_get_optimization_problem_ncol", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_ncol, 1},
  {"_prioriactions_rcpp_get_optimization_problem_nrow", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_nrow, 1},
  {"_prioriactions_rcpp_get_optimization_problem_ncell", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_ncell, 1},
  {"_prioriactions_rcpp_get_optimization_problem_A", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_A, 1},
  {"_prioriactions_rcpp_objective_min_set", (DL_FUNC) &_prioriactions_rcpp_objective_min_set, 9},
  {"_prioriactions_rcpp_objective_max_coverage", (DL_FUNC) &_prioriactions_rcpp_objective_max_coverage, 9},
  {"_prioriactions_rcpp_constraint_benefit", (DL_FUNC) &_prioriactions_rcpp_constraint_benefit, 7},
  {"_prioriactions_rcpp_constraint_activation", (DL_FUNC) &_prioriactions_rcpp_constraint_activation, 4},
  {"_prioriactions_rcpp_constraint_lock", (DL_FUNC) &_prioriactions_rcpp_constraint_lock, 3},
  {"_prioriactions_rcpp_stats_connectivity_units", (DL_FUNC) &_prioriactions_rcpp_stats_connectivity_units, 5},
  {"_prioriactions_rcpp_stats_connectivity_actions", (DL_FUNC) &_prioriactions_rcpp_stats_connectivity_actions, 5},
  {"_prioriactions_rcpp_stats_costs_units", (DL_FUNC) &_prioriactions_rcpp_stats_costs_units, 2},
  {"_prioriactions_rcpp_stats_costs_actions", (DL_FUNC) &_prioriactions_rcpp_stats_costs_actions, 4},
  {"_prioriactions_rcpp_constraint_target", (DL_FUNC) &_prioriactions_rcpp_constraint_target, 8},
  {"_prioriactions_rcpp_constraint_budget", (DL_FUNC) &_prioriactions_rcpp_constraint_budget, 4},
  {"_prioriactions_rcpp_stats_benefit", (DL_FUNC) &_prioriactions_rcpp_stats_benefit, 7},
  {"_prioriactions_rcpp_stats_recovery", (DL_FUNC) &_prioriactions_rcpp_stats_recovery, 7},
  {"_prioriactions_rcpp_add_action_locks", (DL_FUNC) &_prioriactions_rcpp_add_action_locks, 2},
  {"_prioriactions_rcpp_set_objective_min_cost", (DL_FUNC) &_prioriactions_rcpp_set_objective_min_cost, 5},
  {"_prioriactions_rcpp_add_pu_locks", (DL_FUNC) &_prioriactions_rcpp_add_pu_locks, 2},
  {"_prioriactions_rcpp_add_linking_x_le_w", (DL_FUNC) &_prioriactions_rcpp_add_linking_x_le_w, 2},
  {"_prioriactions_rcpp_add_base_variables", (DL_FUNC) &_prioriactions_rcpp_add_base_variables, 3},
  {"_prioriactions_rcpp_add_linking_z_le_w", (DL_FUNC) &_prioriactions_rcpp_add_linking_z_le_w, 2},
  {"_prioriactions_rcpp_fix_z_ineligible_by_positive_delta", (DL_FUNC) &_prioriactions_rcpp_fix_z_ineligible_by_positive_delta, 5},
  {"_prioriactions_rcpp_add_target_conservation", (DL_FUNC) &_prioriactions_rcpp_add_target_conservation, 5},
  {"_prioriactions_rcpp_add_target_recovery", (DL_FUNC) &_prioriactions_rcpp_add_target_recovery, 5},
  {"_prioriactions_rcpp_add_target_mixed_total", (DL_FUNC) &_prioriactions_rcpp_add_target_mixed_total, 7},
  {"_prioriactions_rcpp_set_objective_max_benefit", (DL_FUNC) &_prioriactions_rcpp_set_objective_max_benefit, 4},
  {"_prioriactions_rcpp_set_objective_min_loss", (DL_FUNC) &_prioriactions_rcpp_set_objective_min_loss, 4},
  {"_prioriactions_rcpp_set_objective_max_profit", (DL_FUNC) &_prioriactions_rcpp_set_objective_max_profit, 4},
  {"_prioriactions_rcpp_set_objective_max_net_profit", (DL_FUNC) &_prioriactions_rcpp_set_objective_max_net_profit, 7},
  {"_prioriactions_rcpp_set_objective_max_representation", (DL_FUNC) &_prioriactions_rcpp_set_objective_max_representation, 3},
  {"_prioriactions_rcpp_add_linear_constraint", (DL_FUNC) &_prioriactions_rcpp_add_linear_constraint, 6},
  {"_prioriactions_rcpp_add_target_mixed_total_power", (DL_FUNC) &_prioriactions_rcpp_add_target_mixed_total_power, 9},
  {"_prioriactions_rcpp_add_target_recovery_power", (DL_FUNC) &_prioriactions_rcpp_add_target_recovery_power, 8},
  {"_prioriactions_rcpp_add_exclude_conservation_when_actions", (DL_FUNC) &_prioriactions_rcpp_add_exclude_conservation_when_actions, 6},
  {NULL, NULL, 0}

};

void R_init_prioriactions(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
