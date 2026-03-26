# SolutionSet class

The `SolutionSet` class stores the result of solving a
[`Problem`](https://josesalgr.github.io/mosap/reference/problem-class.md)
object when multiple runs are produced.

A `SolutionSet` object is the multi-run counterpart of
[`solution-class`](https://josesalgr.github.io/mosap/reference/solution-class.md).
It contains the original problem, run-level design and outcome tables,
the list of individual `Solution` objects, diagnostics summarizing the
full set of runs, method metadata, and additional metadata.

Objects of this class are typically created by
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md) in
workflows that generate more than one optimization run, such as
weighted-sum scans, \\\epsilon\\-constraint designs, or AUGMECON grids.

## Value

No return value. This page documents the `SolutionSet` class.

## Details

**Conceptual role**

The `SolutionSet` class represents the result of a multi-run solving
workflow:

    Problem -> solve() -> SolutionSet

Each run corresponds to one specific optimization subproblem, parameter
setting, or trade-off configuration. The `SolutionSet` object keeps all
these runs together in a structured form.

Thus, a `SolutionSet` is not a single solution with multiple labels, but
a collection of distinct `Solution` objects linked to a shared problem
and a shared multi-run design.

**Typical use cases**

A `SolutionSet` is typically returned when the chosen solution method
generates multiple runs, for example:

- several weighted-sum configurations,

- a grid of \\\epsilon\\-constraint runs,

- an AUGMECON exploration of multiple \\\epsilon\\-combinations,

- or any other workflow producing more than one optimizer call.

**Internal structure**

A `SolutionSet` object separates information into several layers:

- `problem`:

  The original `Problem` object shared by all runs.

- `solution`:

  A named `list` containing the core multi-run optimization outputs.
  This typically includes the run design, the run summary table, and the
  list of individual `Solution` objects.

- `summary`:

  A named `list` containing user-facing summaries aggregated across runs
  when such summaries are available.

- `diagnostics`:

  A named `list` containing diagnostics about the solution set as a
  whole.

- `method`:

  A named `list` describing the multi-run optimization method used.

- `meta`:

  A named `list` containing additional metadata.

- `name`:

  A `character(1)` identifier for the solution set.

**Run-level content**

The `solution` field is the main entry point for run-level information.
Typical entries include:

- `design`:

  A `data.frame` describing the experimental or optimization design, for
  example weights or \\\epsilon\\-levels.

- `runs`:

  A `data.frame` summarizing the outcome of each run. This typically
  includes run identifiers, solver status, runtime, gap, and objective
  values.

- `solutions`:

  A list of individual `Solution` objects, one per run.

In many workflows, the `runs` table is the most important compact
representation of the solution set, while `solutions[[i]]` provides the
full detailed output for run \\i\\.

**Objective values and run tables**

In multi-objective workflows, the run table often stores objective
values in columns named `value_<alias>`, where `<alias>` is the alias of
a registered objective. For example:

- `value_cost`,

- `value_frag`,

- `value_benefit`.

This naming convention is used by downstream functions such as
[`plot_tradeoff`](https://josesalgr.github.io/mosap/reference/plot_tradeoff.md).

Depending on the solving method, the run table may also contain design
columns such as:

- `weight_<alias>` for weighted-sum runs,

- `eps_<alias>` for \\\epsilon\\-constraint or AUGMECON runs.

**Relationship with `Solution`**

A `SolutionSet` is conceptually a collection of `Solution` objects. The
individual runs are typically stored in: \$\$
\code{x\$solution\$solutions\[\[i\]\]} \$\$

where each element is itself a full `Solution` object.

Therefore:

- use `SolutionSet` when working with the full set of runs,

- use an individual `Solution` when inspecting one particular run in
  detail.

**Diagnostics**

The `diagnostics` field stores metadata about the multi-run solve
process. Depending on the implementation, it may summarize:

- number of runs,

- status frequencies,

- runtime ranges,

- gap ranges,

- and other aggregate information about the set of runs.

**Printing**

The [`print()`](https://josesalgr.github.io/mosap/reference/print.md)
method provides a concise summary of the full solution set. It reports:

- the optimization method name,

- the participating objective aliases,

- the number of design rows, runs, and stored solutions,

- run-level status summaries,

- runtime and gap ranges,

- and the names of design and objective-value columns when available.

This printed output is intended as a quick overview. Detailed inspection
should use:

- `x$solution$runs`,

- `x$solution$design`,

- `x$solution$solutions[[i]]`,

- or the accessor methods documented below.

## Fields

- `problem`:

  The `Problem` object used to generate the full solution set.

- `solution`:

  A named `list` containing the core multi-run outputs, typically
  including `design`, `runs`, and `solutions`.

- `summary`:

  A named `list` containing user-facing summaries associated with the
  solution set.

- `diagnostics`:

  A named `list` containing diagnostics about the solution set as a
  whole.

- `method`:

  A named `list` describing the optimization method used.

- `meta`:

  A named `list` of additional metadata.

- `name`:

  A `character(1)` name for the solution set object.

## Methods

- [`print()`](https://josesalgr.github.io/mosap/reference/print.md):

  Print a concise summary of the solution set, including method name,
  number of runs, and run-level diagnostics.

- [`show()`](https://josesalgr.github.io/mosap/reference/show.md):

  Alias of
  [`print()`](https://josesalgr.github.io/mosap/reference/print.md).

- `repr()`:

  Return a short one-line representation of the solution set.

- `getMethod()`:

  Return the method specification stored in `self$method`.

- `getDesign()`:

  Return the design table stored in `self$solution$design`.

- `getRuns()`:

  Return the run summary table stored in `self$solution$runs`.

- `getSolutions()`:

  Return the list of individual `Solution` objects stored in
  `self$solution$solutions`.

## See also

[`solution-class`](https://josesalgr.github.io/mosap/reference/solution-class.md),
[`plot_tradeoff`](https://josesalgr.github.io/mosap/reference/plot_tradeoff.md),
[`get_pu`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/mosap/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/mosap/reference/get_targets.md),
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)
