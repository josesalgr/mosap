# Solution class

The `Solution` class stores the result of solving a single
[`Problem`](https://josesalgr.github.io/paretoscape/reference/problem-class.md)
object in paretoscape.

A `Solution` object contains the original problem definition, the core
optimization output returned by the solver, user-facing summary tables,
diagnostics about the solve process, and metadata describing how the
solution was obtained.

Objects of this class are typically created by
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

## Value

No return value. This page documents the `Solution` class.

## Details

**Conceptual role**

The `Solution` class represents the output of one optimization run.

It should be understood as the single-run counterpart of the modelling
workflow:

    Problem -> solve() -> Solution

Thus, a `Solution` object does not replace the original `Problem`;
instead, it keeps a reference to it in the `problem` field and augments
it with optimization results.

**Single-run semantics**

A `Solution` corresponds to one realized solution of one configured
optimization problem. This may come from:

- a single-objective solve,

- one run of a weighted multi-objective workflow,

- one \\\epsilon\\-constraint subproblem,

- one AUGMECON subproblem,

- or any other workflow that ultimately produces one optimizer output.

When multiple runs are generated, they are typically collected in a
separate `SolutionSet` object rather than a single `Solution`.

**Internal structure**

A `Solution` object separates results into several layers:

- `problem`:

  The original `Problem` object used to generate the solution.

- `solution`:

  A named `list` containing the core optimization output. This may
  include the objective value, raw model variable vector, decoded
  decision vectors, and evaluated objective values by alias.

- `summary`:

  A named `list` of user-facing summary tables, typically derived from
  the original problem and the solved decisions. These tables are
  intended for inspection, plotting, reporting, and downstream analysis.

- `diagnostics`:

  A named `list` containing solver diagnostics such as status, runtime,
  optimality gap, solver name, and runtime settings.

- `method`:

  A named `list` describing the optimization method used to obtain the
  solution.

- `meta`:

  A named `list` containing additional metadata.

- `name`:

  A `character(1)` identifier for the solution object.

**Core optimization output**

The `solution` field stores the solver-facing result. Typical entries
may include:

- `objective`:

  The scalar objective value returned by the solver for this run.

- `vector`:

  The raw internal solution vector in model-variable order.

- `alias_values`:

  Objective values evaluated for registered aliases, when available.

The raw solution vector may contain not only user-facing decision
variables such as planning-unit or action decisions, but also auxiliary
variables introduced internally by the model builder.

**User-facing summaries**

The `summary` field stores derived tables intended for interpretation
rather than solver interaction. Typical entries include:

- `pu`:

  Planning-unit summary table.

- `actions`:

  Planning unit–action allocation summary table.

- `features`:

  Feature-level outcome summary table.

- `targets`:

  Target-achievement summary table, when targets were part of the
  problem.

These tables are the main source used by user-facing accessors such as
[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md),
and
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md).

**Diagnostics**

The `diagnostics` field stores metadata about the optimization process,
including solver status and runtime information. Typical entries may
include:

- solver name,

- runtime in seconds,

- optimality gap,

- number of cores,

- time limit,

- status code or status label.

These values describe how the solution was obtained, not the content of
the solution itself.

**Printing**

The
[`print()`](https://josesalgr.github.io/paretoscape/reference/print.md)
method is intended as a concise diagnostic summary. It reports:

- solver status,

- objective value,

- optimality gap,

- runtime,

- counts of selected planning units and actions,

- target fulfillment summary when available,

- evaluated objective alias values when available,

- and basic solver information.

This summary is intended for quick inspection. More detailed exploration
should use the stored `summary`, `solution`, and `diagnostics` fields
directly, or the dedicated accessor functions.

## Fields

- `problem`:

  The `Problem` object used to generate the solution.

- `solution`:

  A named `list` containing the core optimization result.

- `summary`:

  A named `list` of user-facing summary tables.

- `diagnostics`:

  A named `list` of solver diagnostics.

- `method`:

  A named `list` describing the method used.

- `meta`:

  A named `list` of additional metadata.

- `name`:

  A `character(1)` name for the solution object.

## Methods

- [`print()`](https://josesalgr.github.io/paretoscape/reference/print.md):

  Print a concise summary of the solution, including status, objective
  value, runtime, selection counts, and target fulfillment.

- [`show()`](https://josesalgr.github.io/paretoscape/reference/show.md):

  Alias of
  [`print()`](https://josesalgr.github.io/paretoscape/reference/print.md).

- `repr()`:

  Return a short one-line representation of the solution.

## See also

[`problem-class`](https://josesalgr.github.io/paretoscape/reference/problem-class.md),
[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md),
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md)
