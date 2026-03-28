# Problem class

The `Problem` class is the central container used by mosap to represent
a planning problem before, during, and after model construction.

A `Problem` object stores the full problem specification in a modular
way. This includes the baseline planning data, optional spatial
metadata, action definitions, effects, profit, targets, constraints,
objective registrations, solver settings, and, when available, a built
optimization model or model snapshot.

In other words, `Problem` is the persistent object that connects the
full `mosap` workflow:

    input_data()
    -> add_*() / set_*()
    -> solve()

## Value

No return value. This page documents the `Problem` class.

## Details

**Conceptual role**

The `Problem` class is designed for a data-first and modular workflow.
User-facing functions do not usually modify a solver object directly.
Instead, they enrich the `Problem` object by storing new specifications
in its internal `data` field.

Thus, a `Problem` object should be understood as a structured container
for the mathematical planning problem, not necessarily as a built
optimization model.

Before [`solve`](https://josesalgr.github.io/mosap/reference/solve.md)
is called, the object may contain only input data and user
specifications. During or after solving, it may additionally contain a
built model pointer, model metadata, and solver-related information.

**Core mathematical interpretation**

At a high level, the `Problem` object stores the ingredients required to
define an optimization problem over:

- planning units \\i \in \mathcal{P}\\,

- features \\f \in \mathcal{F}\\,

- actions \\a \in \mathcal{A}\\,

- optional spatial relations over planning units,

- and user-defined objectives and constraints.

The baseline ecological state is typically stored through a planning
unit–feature table of amounts: \$\$ a\_{if} \ge 0, \$\$ where
\\a\_{if}\\ is the baseline amount of feature \\f\\ in planning unit
\\i\\.

Subsequent functions then add action feasibility, effects, profit,
targets, spatial relations, and optimization settings to this baseline
representation.

**How objects are created**

`Problem` objects are usually created by
[`input_data`](https://josesalgr.github.io/mosap/reference/input_data.md).

After creation, downstream functions such as
[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md),
[`add_profit`](https://josesalgr.github.io/mosap/reference/add_profit.md),
[`add_targets_absolute`](https://josesalgr.github.io/mosap/reference/add_targets_absolute.md),
[`add_targets_relative`](https://josesalgr.github.io/mosap/reference/add_targets_relative.md),
spatial relation constructors, objective setters, and solver setters
extend the internal `data` list.

**Internal storage**

The class contains a single field:

- `data`:

  A named `list` storing the full problem specification, metadata, and,
  when available, built-model information.

Common entries of `data` include:

- `pu`:

  Planning-unit table.

- `features`:

  Feature table.

- `actions`:

  Action catalog.

- `dist_features`:

  Planning unit–feature baseline amounts.

- `dist_actions`:

  Feasible planning unit–action pairs.

- `dist_effects`:

  Action effects by planning unit, action, and feature.

- `dist_profit`:

  Profit by planning unit–action pair.

- `pu_sf`:

  Planning-unit geometry when available.

- `pu_coords`:

  Planning-unit coordinates when available.

- `spatial_relations`:

  Registered spatial relations.

- `targets`:

  Stored target specifications.

- `constraints`:

  Stored user-defined constraints.

- `objectives`:

  Registered atomic objectives for single- or multi-objective workflows.

- `method`:

  Stored multi-objective method configuration, when applicable.

- `solve_args`:

  Stored solver settings.

- `model_ptr`:

  Pointer to a built optimization model, when available.

- `model_args`:

  Metadata describing model construction.

- `model_list`:

  Optional exported model snapshot or representation.

- `meta`:

  Auxiliary metadata, including model-dirty flags and other bookkeeping
  fields.

Not every `Problem` object contains all of these entries. The content of
`data` depends on how far the workflow has progressed.

**Lifecycle**

A `Problem` object typically moves through the following stages:

1.  input stage: baseline planning units, features, and feature
    distributions are stored,

2.  specification stage: actions, effects, targets, objectives,
    constraints, and spatial relations are added,

3.  model stage: an optimization model is built from the stored
    specification,

4.  solve stage: the model is solved and results are returned in a
    separate `Solution` or `SolutionSet` object.

The `Problem` object itself is not the solution. It is the structured
problem definition from which a solution can be obtained.

## Methods

- [`print()`](https://josesalgr.github.io/mosap/reference/print.md):

  Print a structured summary of the stored problem specification,
  including data, actions and effects, spatial inputs, targets and
  constraints, and model status. If a model has already been built,
  additional dimensions and auxiliary-variable information are
  displayed.

- [`show()`](https://josesalgr.github.io/mosap/reference/show.md):

  Alias of
  [`print()`](https://josesalgr.github.io/mosap/reference/print.md).

- `repr()`:

  Return a short one-line representation of the object.

- `getData(name)`:

  Return a named entry from `self$data`.

- `getPlanningUnitsAmount()`:

  Return the number of planning units stored in `x$data$pu`.

- `getMonitoringCosts()`:

  Return the planning-unit cost vector, typically taken from
  `x$data$pu$cost`.

- `getFeatureAmount()`:

  Return the number of stored features.

- `getFeatureNames()`:

  Return feature names from `x$data$features$name`, or feature ids if
  names are unavailable.

- `getActionCosts()`:

  Return action-level costs from `x$data$dist_actions$cost` when
  available.

- `getActionsAmount()`:

  Return the number of stored actions.

## Printing and diagnostics

The [`print()`](https://josesalgr.github.io/mosap/reference/print.md)
method is intended as a quick diagnostic summary. It helps users
understand:

- what data have already been loaded,

- whether actions, effects, spatial relations, targets, and constraints
  have been registered,

- whether objectives and methods have been configured,

- whether a model has already been built,

- and whether the object appears ready to be solved.

In particular, the model section of the printed output summarizes
whether the current problem specification is incomplete, ready, or
already materialized as a built optimization model.

## See also

[`input_data`](https://josesalgr.github.io/mosap/reference/input_data.md),
[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md),
[`add_targets_absolute`](https://josesalgr.github.io/mosap/reference/add_targets_absolute.md),
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)
