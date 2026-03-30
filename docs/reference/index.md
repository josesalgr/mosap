# Package index

## Build and solve problems

Core workflow to create a planning problem, configure it, and solve it.

- [`input_data()`](https://josesalgr.github.io/paretoscape/reference/input_data.md)
  : Create a planning problem input object
- [`solve()`](https://josesalgr.github.io/paretoscape/reference/solve.md)
  : Solve a planning problem

## Actions, effects, profits and locks

Define feasible actions, their effects, optional profits, and fixed
decisions.

- [`add_actions()`](https://josesalgr.github.io/paretoscape/reference/add_actions.md)
  : Add management actions to a planning problem
- [`add_effects()`](https://josesalgr.github.io/paretoscape/reference/add_effects.md)
  : Add action effects to a planning problem
- [`add_benefits()`](https://josesalgr.github.io/paretoscape/reference/add_benefits.md)
  : Add benefits
- [`add_losses()`](https://josesalgr.github.io/paretoscape/reference/add_losses.md)
  : Add losses
- [`add_profit()`](https://josesalgr.github.io/paretoscape/reference/add_profit.md)
  : Add profit to a planning problem
- [`add_locked_actions()`](https://josesalgr.github.io/paretoscape/reference/add_locked_actions.md)
  : Add locked action decisions to a planning problem
- [`add_locked_pu()`](https://josesalgr.github.io/paretoscape/reference/add_locked_pu.md)
  : Add locked planning units to a problem

## Targets and constraints

Define target requirements and area-related constraints.

- [`add_targets_absolute()`](https://josesalgr.github.io/paretoscape/reference/add_targets_absolute.md)
  : Add absolute targets
- [`add_targets_relative()`](https://josesalgr.github.io/paretoscape/reference/add_targets_relative.md)
  : Add relative targets
- [`add_area_min_constraint()`](https://josesalgr.github.io/paretoscape/reference/add_area_min_constraint.md)
  : Add minimum selected area constraint
- [`add_area_max_constraint()`](https://josesalgr.github.io/paretoscape/reference/add_area_max_constraint.md)
  : Add maximum selected area constraint

## Spatial relations

Register boundary and graph-based spatial relations from tables,
polygons, or coordinates.

- [`add_spatial_relations()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_relations.md)
  : Add spatial relations
- [`add_spatial_boundary()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_boundary.md)
  : Add spatial boundary-length relations
- [`add_spatial_rook()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_rook.md)
  : Add rook adjacency from polygons
- [`add_spatial_queen()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_queen.md)
  : Add queen adjacency from polygons
- [`add_spatial_knn()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_knn.md)
  : Add k-nearest-neighbours spatial relations
- [`add_spatial_distance()`](https://josesalgr.github.io/paretoscape/reference/add_spatial_distance.md)
  : Add distance-threshold spatial relations

## Atomic objectives

Register objectives that can be solved individually or combined in
multi-objective workflows.

- [`add_objective_min_cost()`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_cost.md)
  : Add objective: minimize cost
- [`add_objective_max_benefit()`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_benefit.md)
  : Add objective: maximize benefit
- [`add_objective_min_loss()`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_loss.md)
  : Add objective: minimize loss
- [`add_objective_max_profit()`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_profit.md)
  : Add objective: maximize profit
- [`add_objective_max_net_profit()`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_net_profit.md)
  : Add objective: maximize net profit
- [`add_objective_min_fragmentation()`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_fragmentation.md)
  : Add objective: minimize fragmentation
- [`add_objective_min_action_fragmentation()`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_action_fragmentation.md)
  : Add objective: minimize action fragmentation
- [`add_objective_min_intervention_impact()`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_intervention_impact.md)
  : Add objective: minimize intervention impact

## Multi-objective methods

Configure weighted-sum, epsilon-constraint, and AUGMECON workflows.

- [`set_method_weighted()`](https://josesalgr.github.io/paretoscape/reference/set_method_weighted.md)
  : Set the weighted-sum multi-objective method
- [`set_method_epsilon_constraint()`](https://josesalgr.github.io/paretoscape/reference/set_method_epsilon_constraint.md)
  : Set the epsilon-constraint multi-objective method
- [`set_method_augmecon()`](https://josesalgr.github.io/paretoscape/reference/set_method_augmecon.md)
  : Set the AUGMECON multi-objective method

## Solver configuration

Store solver and runtime options before calling solve().

- [`set_solver()`](https://josesalgr.github.io/paretoscape/reference/set_solver.md)
  : Configure solver settings
- [`set_solver_gurobi()`](https://josesalgr.github.io/paretoscape/reference/set_solver_gurobi.md)
  : Configure Gurobi solver settings
- [`set_solver_cplex()`](https://josesalgr.github.io/paretoscape/reference/set_solver_cplex.md)
  : Configure CPLEX solver settings
- [`set_solver_cbc()`](https://josesalgr.github.io/paretoscape/reference/set_solver_cbc.md)
  : Configure CPLEX solver settings
- [`set_solver_symphony()`](https://josesalgr.github.io/paretoscape/reference/set_solver_symphony.md)
  : Configure SYMPHONY solver settings

## Results and extraction

Extract user-facing summaries and raw solution vectors.

- [`compile_model()`](https://josesalgr.github.io/paretoscape/reference/compile_model.md)
  : Compile the optimization model stored in a Problem
- [`get_pu()`](https://josesalgr.github.io/paretoscape/reference/get_pu.md)
  : Get planning-unit results from a solution
- [`get_actions()`](https://josesalgr.github.io/paretoscape/reference/get_actions.md)
  : Get action results from a solution
- [`get_features()`](https://josesalgr.github.io/paretoscape/reference/get_features.md)
  : Get feature summary from a solution
- [`get_targets()`](https://josesalgr.github.io/paretoscape/reference/get_targets.md)
  : Get target achievement summary from a solution
- [`get_solution_vector()`](https://josesalgr.github.io/paretoscape/reference/get_solution_vector.md)
  : Get raw solution vector from a solution

## Plotting

Visualize spatial outputs and multi-objective trade-offs.

- [`plot_spatial()`](https://josesalgr.github.io/paretoscape/reference/plot_spatial.md)
  : Plot spatial outputs from a solution or solution set
- [`plot_spatial_pu()`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_pu.md)
  : Plot selected planning units in space
- [`plot_spatial_actions()`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_actions.md)
  : Plot selected actions in space
- [`plot_spatial_features()`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_features.md)
  : Plot spatial feature values from a solution
- [`plot_tradeoff()`](https://josesalgr.github.io/paretoscape/reference/plot_tradeoff.md)
  : Plot trade-offs from a multi-objective solution set

## Classes

Core classes used across the package.

- [`problem-class`](https://josesalgr.github.io/paretoscape/reference/problem-class.md)
  [`Problem`](https://josesalgr.github.io/paretoscape/reference/problem-class.md)
  : Problem class
- [`solution-class`](https://josesalgr.github.io/paretoscape/reference/solution-class.md)
  [`Solution`](https://josesalgr.github.io/paretoscape/reference/solution-class.md)
  : Solution class
- [`solutionset-class`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md)
  [`SolutionSet`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md)
  : SolutionSet class

## Data

Simulated datasets distributed with the package.

- [`sim_dist_features`](https://josesalgr.github.io/paretoscape/reference/sim_dist_features.md)
  : Simulated feature distribution
- [`sim_features`](https://josesalgr.github.io/paretoscape/reference/sim_features.md)
  : Simulated features
- [`sim_pu_sf`](https://josesalgr.github.io/paretoscape/reference/sim_pu_sf.md)
  : Simulated planning units
