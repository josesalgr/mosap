# Package index

## Create and solve problems

Core workflow to build a planning problem and solve it.

- [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  : Create a planning problem input object
- [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md) :
  Solve optimization model
- [`mosap()`](https://josesalgr.github.io/mosap/reference/mosap.md) :
  Create and solve multi-actions planning problems

## Actions, effects, targets and profits

Define actions/effects (benefits), targets, and optional profit layers
used by objectives/constraints.

- [`add_actions()`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  : Add actions to a planning problem
- [`add_effects()`](https://josesalgr.github.io/mosap/reference/add_effects.md)
  : Add effects (benefit/loss) to a planning problem
- [`add_benefits()`](https://josesalgr.github.io/mosap/reference/add_benefits.md)
  : Add benefits (positive effects)
- [`add_profit()`](https://josesalgr.github.io/mosap/reference/add_profit.md)
  : Add profit to a planning problem
- [`add_conservation_targets_absolute()`](https://josesalgr.github.io/mosap/reference/add_conservation_targets_absolute.md)
  : Add conservation targets (absolute)
- [`add_conservation_targets_relative()`](https://josesalgr.github.io/mosap/reference/add_conservation_targets_relative.md)
  : Add conservation targets (relative to baseline)
- [`add_recovery_targets_absolute()`](https://josesalgr.github.io/mosap/reference/add_recovery_targets_absolute.md)
  : Add recovery targets (absolute)
- [`add_recovery_targets_relative()`](https://josesalgr.github.io/mosap/reference/add_recovery_targets_relative.md)
  : Add recovery targets (relative)
- [`add_mixed_targets_total_absolute()`](https://josesalgr.github.io/mosap/reference/add_mixed_targets_total_absolute.md)
  : Add mixed total targets (absolute)
- [`add_mixed_targets_total_relative()`](https://josesalgr.github.io/mosap/reference/add_mixed_targets_total_relative.md)
  : Add mixed total targets (relative to baseline)

## Spatial relations

Register adjacency/boundary relations (from tables, polygons, or
coordinates) used by spatial objectives.

- [`add_spatial_relations()`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md)
  : Add spatial relations (core)
- [`add_spatial_boundary()`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md)
  : Add spatial boundary-length relations from sf polygons or a boundary
  table
- [`add_spatial_rook()`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md)
  : Add rook adjacency from sf polygons
- [`add_spatial_queen()`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md)
  : Add queen adjacency from sf polygons
- [`add_spatial_knn()`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
  : Add k-nearest-neighbours spatial relations from coordinates
- [`add_spatial_distance()`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md)
  : Add distance-threshold spatial relations from coordinates

## Atomic objectives

Register objectives that can be solved alone or combined in
multi-objective methods.

- [`add_objective_max_benefit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_benefit.md)
  : Add objective: maximize benefit
- [`add_objective_max_net_profit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_net_profit.md)
  : Add objective: maximize net profit
- [`add_objective_max_profit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_profit.md)
  : Add objective: maximize profit
- [`add_objective_min_action_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_action_fragmentation.md)
  : Add objective: minimize action fragmentation
- [`add_objective_min_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_fragmentation.md)
  : Add objective: minimize fragmentation (PU cut)
- [`add_objective_min_intervention_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_intervention_fragmentation.md)
  : Add objective: minimize intervention fragmentation

## Solver configuration

Configure solver and runtime options before calling solve().

- [`set_solver()`](https://josesalgr.github.io/mosap/reference/set_solver.md)
  : Configure solver settings
- [`add_actions()`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  : Add actions to a planning problem
- [`add_action_max_per_pu()`](https://josesalgr.github.io/mosap/reference/add_action_max_per_pu.md)
  : Limit the number of actions per planning unit (maximum)
- [`add_area_max_constraint()`](https://josesalgr.github.io/mosap/reference/add_area_max_constraint.md)
  : Add maximum selected area constraint
- [`add_area_min_constraint()`](https://josesalgr.github.io/mosap/reference/add_area_min_constraint.md)
  : Add minimum selected area constraint
- [`add_benefits()`](https://josesalgr.github.io/mosap/reference/add_benefits.md)
  : Add benefits (positive effects)
- [`add_conservation_targets_absolute()`](https://josesalgr.github.io/mosap/reference/add_conservation_targets_absolute.md)
  : Add conservation targets (absolute)
- [`add_conservation_targets_relative()`](https://josesalgr.github.io/mosap/reference/add_conservation_targets_relative.md)
  : Add conservation targets (relative to baseline)
- [`add_effects()`](https://josesalgr.github.io/mosap/reference/add_effects.md)
  : Add effects (benefit/loss) to a planning problem
- [`add_losses()`](https://josesalgr.github.io/mosap/reference/add_losses.md)
  : Add losses (negative effects magnitude)
- [`add_mixed_targets_total_absolute()`](https://josesalgr.github.io/mosap/reference/add_mixed_targets_total_absolute.md)
  : Add mixed total targets (absolute)
- [`add_mixed_targets_total_relative()`](https://josesalgr.github.io/mosap/reference/add_mixed_targets_total_relative.md)
  : Add mixed total targets (relative to baseline)
- [`add_objective_max_benefit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_benefit.md)
  : Add objective: maximize benefit
- [`add_objective_max_net_profit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_net_profit.md)
  : Add objective: maximize net profit
- [`add_objective_max_profit()`](https://josesalgr.github.io/mosap/reference/add_objective_max_profit.md)
  : Add objective: maximize profit
- [`add_objective_min_action_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_action_fragmentation.md)
  : Add objective: minimize action fragmentation
- [`add_objective_min_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_fragmentation.md)
  : Add objective: minimize fragmentation (PU cut)
- [`add_objective_min_intervention_fragmentation()`](https://josesalgr.github.io/mosap/reference/add_objective_min_intervention_fragmentation.md)
  : Add objective: minimize intervention fragmentation
- [`add_profit()`](https://josesalgr.github.io/mosap/reference/add_profit.md)
  : Add profit to a planning problem
- [`add_recovery_targets_absolute()`](https://josesalgr.github.io/mosap/reference/add_recovery_targets_absolute.md)
  : Add recovery targets (absolute)
- [`add_recovery_targets_relative()`](https://josesalgr.github.io/mosap/reference/add_recovery_targets_relative.md)
  : Add recovery targets (relative)
- [`add_spatial_boundary()`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md)
  : Add spatial boundary-length relations from sf polygons or a boundary
  table
- [`add_spatial_distance()`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md)
  : Add distance-threshold spatial relations from coordinates
- [`add_spatial_knn()`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
  : Add k-nearest-neighbours spatial relations from coordinates
- [`add_spatial_queen()`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md)
  : Add queen adjacency from sf polygons
- [`add_spatial_relations()`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md)
  : Add spatial relations (core)
- [`add_spatial_rook()`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md)
  : Add rook adjacency from sf polygons

## Multi-objective methods

Configure and solve multi-objective problems (e.g., weighted sum).

- [`set_method_weighted()`](https://josesalgr.github.io/mosap/reference/set_method_weighted.md)
  : Set multi-objective method: weighted sum
- [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md) :
  Solve optimization model

## Results and solution helpers

Extract tables and diagnostics from a Solution object.

- [`get_pu()`](https://josesalgr.github.io/mosap/reference/get_pu.md) :
  Get planning unit results from a Solution
- [`get_actions()`](https://josesalgr.github.io/mosap/reference/get_actions.md)
  : Get action results from a Solution
- [`get_features()`](https://josesalgr.github.io/mosap/reference/get_features.md)
  : Get feature achievement summary from a Solution
- [`get_targets()`](https://josesalgr.github.io/mosap/reference/get_targets.md)
  : Get target achievement table from a Solution
- [`get_solution_vector()`](https://josesalgr.github.io/mosap/reference/get_solution_vector.md)
  : Get raw solution vector from a Solution

## Classes

Core classes used across the package.

- [`data-class`](https://josesalgr.github.io/mosap/reference/data-class.md)
  [`Data`](https://josesalgr.github.io/mosap/reference/data-class.md) :
  Data class
- [`mo-problem-class`](https://josesalgr.github.io/mosap/reference/mo-problem-class.md)
  [`MOProblem`](https://josesalgr.github.io/mosap/reference/mo-problem-class.md)
  : MOProblem class
- [`solution-class`](https://josesalgr.github.io/mosap/reference/solution-class.md)
  [`Solution`](https://josesalgr.github.io/mosap/reference/solution-class.md)
  : Solution class

## Data

Simulated datasets distributed with the package.

- [`sim_pu_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_features_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_dist_features_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_threats_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_dist_threats_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_sensitivity_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  [`sim_boundary_data`](https://josesalgr.github.io/mosap/reference/simData.md)
  : Simulated multi-action planning data

## Miscellaneous

Utilities and print methods.

- [`print(`*`<Data>`*`)`](https://josesalgr.github.io/mosap/reference/print.md)
  [`print(`*`<OptimizationProblem>`*`)`](https://josesalgr.github.io/mosap/reference/print.md)
  [`print(`*`<Solution>`*`)`](https://josesalgr.github.io/mosap/reference/print.md)
  [`print(`*`<Portfolio>`*`)`](https://josesalgr.github.io/mosap/reference/print.md)
  : Print
- [`show(`*`<Data>`*`)`](https://josesalgr.github.io/mosap/reference/show.md)
  [`show(`*`<OptimizationProblem>`*`)`](https://josesalgr.github.io/mosap/reference/show.md)
  [`show(`*`<Portfolio>`*`)`](https://josesalgr.github.io/mosap/reference/show.md)
  : Show
