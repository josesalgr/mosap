#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _mosap_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _mosap_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _mosap_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _mosap_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _mosap_rcpp_get_optimization_problem_A(SEXP);

extern SEXP _mosap_rcpp_objective_min_set(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_objective_max_coverage(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

extern SEXP _mosap_rcpp_add_action_locks(SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_min_cost(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_pu_locks(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_linking_x_le_w(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_base_variables(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_linking_z_le_w(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_fix_z_ineligible_by_positive_delta(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_target_conservation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_target_recovery(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_target_mixed_total(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_max_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_min_loss(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_max_profit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_max_net_profit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_max_representation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_linear_constraint(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_target_mixed_total_power(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_target_recovery_power(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_exclude_conservation_when_actions(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_min_fragmentation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_min_fragmentation_interventions(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_set_objective_min_fragmentation_actions_by_action(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_objective_min_cost(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_reset_objective(SEXP, SEXP);
extern SEXP _mosap_rcpp_add_to_objective(SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_action_max_per_pu(SEXP, SEXP, SEXP, SEXP, SEXP);

extern SEXP _mosap_rcpp_add_to_objective_scalar(SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_prepare_fragmentation_pu(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_objective_min_fragmentation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_objective_max_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_prepare_objective_max_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_objective_min_fragmentation_actions_by_action(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_prepare_fragmentation_actions_by_action(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_prepare_objective_max_profit(SEXP, SEXP, SEXP, SEXP);
extern SEXP _mosap_rcpp_add_objective_max_profit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_mosap_rcpp_new_optimization_problem", (DL_FUNC) &_mosap_rcpp_new_optimization_problem, 3},
  {"_mosap_rcpp_optimization_problem_as_list", (DL_FUNC) &_mosap_rcpp_optimization_problem_as_list, 1},
  {"_mosap_rcpp_get_optimization_problem_ncol", (DL_FUNC) &_mosap_rcpp_get_optimization_problem_ncol, 1},
  {"_mosap_rcpp_get_optimization_problem_nrow", (DL_FUNC) &_mosap_rcpp_get_optimization_problem_nrow, 1},
  {"_mosap_rcpp_get_optimization_problem_ncell", (DL_FUNC) &_mosap_rcpp_get_optimization_problem_ncell, 1},
  {"_mosap_rcpp_get_optimization_problem_A", (DL_FUNC) &_mosap_rcpp_get_optimization_problem_A, 1},
  {"_mosap_rcpp_objective_min_set", (DL_FUNC) &_mosap_rcpp_objective_min_set, 9},
  {"_mosap_rcpp_objective_max_coverage", (DL_FUNC) &_mosap_rcpp_objective_max_coverage, 9},
  {"_mosap_rcpp_add_action_locks", (DL_FUNC) &_mosap_rcpp_add_action_locks, 2},
  {"_mosap_rcpp_set_objective_min_cost", (DL_FUNC) &_mosap_rcpp_set_objective_min_cost, 7},
  {"_mosap_rcpp_add_pu_locks", (DL_FUNC) &_mosap_rcpp_add_pu_locks, 4},
  {"_mosap_rcpp_add_linking_x_le_w", (DL_FUNC) &_mosap_rcpp_add_linking_x_le_w, 4},
  {"_mosap_rcpp_add_base_variables", (DL_FUNC) &_mosap_rcpp_add_base_variables, 5},
  {"_mosap_rcpp_add_linking_z_le_w", (DL_FUNC) &_mosap_rcpp_add_linking_z_le_w, 4},
  {"_mosap_rcpp_fix_z_ineligible_by_positive_delta", (DL_FUNC) &_mosap_rcpp_fix_z_ineligible_by_positive_delta, 5},
  {"_mosap_rcpp_add_target_conservation", (DL_FUNC) &_mosap_rcpp_add_target_conservation, 7},
  {"_mosap_rcpp_add_target_recovery", (DL_FUNC) &_mosap_rcpp_add_target_recovery, 6},
  {"_mosap_rcpp_add_target_mixed_total", (DL_FUNC) &_mosap_rcpp_add_target_mixed_total, 7},
  {"_mosap_rcpp_set_objective_max_benefit", (DL_FUNC) &_mosap_rcpp_set_objective_max_benefit, 6},
  {"_mosap_rcpp_set_objective_min_loss", (DL_FUNC) &_mosap_rcpp_set_objective_min_loss, 4},
  {"_mosap_rcpp_set_objective_max_profit", (DL_FUNC) &_mosap_rcpp_set_objective_max_profit, 6},
  {"_mosap_rcpp_set_objective_max_net_profit", (DL_FUNC) &_mosap_rcpp_set_objective_max_net_profit, 9},
  {"_mosap_rcpp_set_objective_max_representation", (DL_FUNC) &_mosap_rcpp_set_objective_max_representation, 7},
  {"_mosap_rcpp_add_linear_constraint", (DL_FUNC) &_mosap_rcpp_add_linear_constraint, 8},
  {"_mosap_rcpp_add_target_mixed_total_power", (DL_FUNC) &_mosap_rcpp_add_target_mixed_total_power, 9},
  {"_mosap_rcpp_add_target_recovery_power", (DL_FUNC) &_mosap_rcpp_add_target_recovery_power, 8},
  {"_mosap_rcpp_add_exclude_conservation_when_actions", (DL_FUNC) &_mosap_rcpp_add_exclude_conservation_when_actions, 6},
  {"_mosap_rcpp_set_objective_min_fragmentation", (DL_FUNC) &_mosap_rcpp_set_objective_min_fragmentation, 6},
  {"_mosap_rcpp_set_objective_min_fragmentation_interventions", (DL_FUNC) &_mosap_rcpp_set_objective_min_fragmentation_interventions, 6},
  {"_mosap_rcpp_set_objective_min_fragmentation_actions_by_action", (DL_FUNC) &_mosap_rcpp_set_objective_min_fragmentation_actions_by_action, 8},
  {"_mosap_rcpp_add_objective_min_cost", (DL_FUNC) &_mosap_rcpp_add_objective_min_cost, 8},
  {"_mosap_rcpp_reset_objective", (DL_FUNC) &_mosap_rcpp_reset_objective, 2},
  {"_mosap_rcpp_add_to_objective", (DL_FUNC) &_mosap_rcpp_add_to_objective, 3},
  {"_mosap_rcpp_add_action_max_per_pu", (DL_FUNC) &_mosap_rcpp_add_action_max_per_pu, 5},
  {"_mosap_rcpp_add_to_objective_scalar", (DL_FUNC) &_mosap_rcpp_add_to_objective_scalar, 3},
  {"_mosap_rcpp_prepare_fragmentation_pu", (DL_FUNC) &_mosap_rcpp_prepare_fragmentation_pu, 4},
  {"_mosap_rcpp_add_objective_min_fragmentation", (DL_FUNC) &_mosap_rcpp_add_objective_min_fragmentation, 6},
  {"_mosap_rcpp_add_objective_max_benefit", (DL_FUNC) &_mosap_rcpp_add_objective_max_benefit, 6},
  {"_mosap_rcpp_prepare_objective_max_benefit", (DL_FUNC) &_mosap_rcpp_prepare_objective_max_benefit, 6},
  {"_mosap_rcpp_add_objective_min_fragmentation_actions_by_action", (DL_FUNC) &_mosap_rcpp_add_objective_min_fragmentation_actions_by_action, 9},
  {"_mosap_rcpp_prepare_fragmentation_actions_by_action", (DL_FUNC) &_mosap_rcpp_prepare_fragmentation_actions_by_action, 6},
  {"_mosap_rcpp_prepare_objective_max_profit", (DL_FUNC) &_mosap_rcpp_prepare_objective_max_profit, 4},
  {"_mosap_rcpp_add_objective_max_profit", (DL_FUNC) &_mosap_rcpp_add_objective_max_profit, 7},
  {NULL, NULL, 0}

};

void R_init_mosap(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
