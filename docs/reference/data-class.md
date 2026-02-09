# Data class

The `Data` class is the core container used by prioriactions to store
the information required to define and solve a multi-action planning
problem. It holds the planning-unit table, feature and threat
catalogues, action definitions and distributions (feasibility, costs,
effects), optional spatial inputs (sf geometry, coordinate tables,
spatial relations), and (optionally) a built optimization model
snapshot.

Objects of this class are typically created by
[`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
or
[`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
Downstream functions (e.g.,
[`add_actions()`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_effects()`](https://josesalgr.github.io/mosap/reference/add_effects.md),
target setters, objectives) enrich the internal `data` list by adding or
updating the relevant tables.

## Value

No return value (class definition).

## Details

The `Data` class is designed to support both legacy single-objective
workflows and newer modular/multi-objective workflows. Many functions
operate in a "data-first" way: they store specifications (e.g., targets,
objectives, constraints) in `x$data` and the optimization model is built
later (typically when calling
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)).

## Storage

The class contains a single field:

- data:

  A named `list` holding all tables and metadata used by the workflow.
  Common elements include `pu`, `features`, `threats`, `actions`,
  `dist_features`, `dist_actions`, `dist_effects`/`dist_benefit`,
  `pu_sf`, `pu_coords`, `spatial_relations`, `targets`, and
  model-related entries such as `model_ptr`, `model_list`, and
  `model_args`.

## Methods

- [`print()`](https://josesalgr.github.io/mosap/reference/print.md):

  Print a concise summary of the stored data and, when present, basic
  information about the built optimization model (dimensions, objective
  type, and auxiliary variables such as fragmentation variables). Uses
  cli when available, otherwise falls back to a plain-text summary.

- [`show()`](https://josesalgr.github.io/mosap/reference/show.md):

  Alias of
  [`print()`](https://josesalgr.github.io/mosap/reference/print.md).

- `repr()`:

  Returns a short string representation.

- `getData(name)`:

  Retrieve an element stored in `data` by its name.

- `getPlanningUnitsAmount()`:

  Return the number of planning units stored in `x$data$pu`.

- `getMonitoringCosts()`:

  Return the planning-unit cost vector. Prefers column `cost` in
  `x$data$pu` and falls back to legacy `monitoring_cost` if present.

- `getFeatureAmount()`:

  Return the number of features stored in `x$data$features`.

- `getFeatureNames()`:

  Return feature names from `x$data$features$name`.

- `getThreatsAmount()`:

  Return the number of threats stored in `x$data$threats`.

- `getThreatNames()`:

  Return threat names from `x$data$threats$name`.

- `getActionCosts()`:

  Return action costs. Prefers the model-ready action table
  `x$data$dist_actions$cost`; otherwise falls back to legacy
  `x$data$dist_threats$action_cost`.

- `getActionsAmount()`:

  Return the number of actions. Prefers `x$data$actions` (new API);
  otherwise falls back to the number of rows in legacy
  `x$data$dist_threats`.
