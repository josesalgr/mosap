# Solve a planning problem

Solve a planning problem stored in a `Problem` object.

This is the main execution step of the paretoscape workflow. It reads
the problem specification stored in `x$data`, builds the corresponding
optimization model when needed, applies the configured solver settings,
and returns either a
[`solution-class`](https://josesalgr.github.io/paretoscape/reference/solution-class.md)
or a
[`solutionset-class`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md)
depending on whether the workflow is single-objective or
multi-objective.

## Usage

``` r
solve(x, ...)

# S3 method for class 'Problem'
solve(x, ...)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md)
  and optionally enriched with actions, effects, targets, constraints,
  objectives, spatial relations, method settings, and solver settings.

- ...:

  Additional arguments reserved for internal or legacy solver handling.
  These are not part of the main recommended user interface.

## Value

Either:

- a
  [`solution-class`](https://josesalgr.github.io/paretoscape/reference/solution-class.md)
  object when solving a single-objective problem, or

- a
  [`solutionset-class`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md)
  object when solving a configured multi-objective problem.

## Details

**Role of `solve()`**

The typical paretoscape workflow is:

    x <- input_data(...)
    x <- add_...(x, ...)
    x <- set_...(x, ...)
    res <- solve(x)

Thus, `solve()` is the stage at which the stored problem specification
is turned into one or more optimization runs.

**What `solve()` reads from the problem object**

The function uses the information stored in `x$data`, including:

- baseline planning data,

- actions, effects, profit, and spatial relations,

- targets and constraints,

- registered objectives,

- an optional multi-objective method configuration in `x$data$method`,

- solver settings in `x$data$solve_args`.

If a model has not yet been built, it is built internally during the
solve process. If a model snapshot or pointer already exists, the
solving layer may reuse or refresh it depending on the internal model
state.

**Single-objective vs multi-objective behaviour**

The behaviour of `solve()` depends on the problem configuration.

**Single-objective case**

If exactly one objective is registered and no multi-objective method is
configured, `solve()` runs a single optimization problem and returns a
[`solution-class`](https://josesalgr.github.io/paretoscape/reference/solution-class.md)
object.

**Multi-objective case**

If a multi-objective method is configured in `x$data$method`, `solve()`
dispatches internally according to the stored method name.

Currently supported method names are:

- `"weighted"`,

- `"epsilon_constraint"`,

- `"augmecon"`.

In these cases, `solve()` runs the corresponding multi-objective solving
workflow and returns a
[`solutionset-class`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md)
object.

**Consistency check**

If multiple objectives are registered but no multi-objective method has
been selected, `solve()` stops with an error. In other words:

- one objective and no MO method \\\Rightarrow\\ single-objective solve,

- multiple objectives and a valid MO method \\\Rightarrow\\
  multi-objective solve,

- multiple objectives and no MO method \\\Rightarrow\\ error.

**Solver settings**

Solver configuration is read from `x$data$solve_args`, typically created
with
[`set_solver`](https://josesalgr.github.io/paretoscape/reference/set_solver.md)
or one of its convenience wrappers such as
[`set_solver_gurobi`](https://josesalgr.github.io/paretoscape/reference/set_solver_gurobi.md).

These settings may include:

- the selected backend,

- time limits,

- optimality-gap settings,

- CPU cores,

- verbosity options,

- backend-specific solver parameters.

**Return value**

The return type depends on the configured workflow:

- `Solution`: for single-objective optimization,

- `SolutionSet`: for multi-objective optimization.

A `Solution` represents one optimization run. A `SolutionSet` represents
multiple runs together with their run table, design information, and
individual `Solution` objects.

**Method dispatch**

`solve()` is an S3 generic. The public method documented here is
`solve.Problem()`, which operates on `Problem` objects. solve() is the
only function that should normally materialize the optimization model.

## See also

[`problem-class`](https://josesalgr.github.io/paretoscape/reference/problem-class.md),
[`solution-class`](https://josesalgr.github.io/paretoscape/reference/solution-class.md),
[`solutionset-class`](https://josesalgr.github.io/paretoscape/reference/solutionset-class.md),
[`set_solver`](https://josesalgr.github.io/paretoscape/reference/set_solver.md),
[`set_method_weighted`](https://josesalgr.github.io/paretoscape/reference/set_method_weighted.md),
[`set_method_epsilon_constraint`](https://josesalgr.github.io/paretoscape/reference/set_method_epsilon_constraint.md),
[`set_method_augmecon`](https://josesalgr.github.io/paretoscape/reference/set_method_augmecon.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ------------------------------------------------------------
# Single-objective solve
# ------------------------------------------------------------
x <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)

x <- x |>
  add_targets_relative(0.3) |>
  add_objective_min_cost(alias = "cost") |>
  set_solver_gurobi(time_limit = 300, gap_limit = 0.01)

sol <- solve(x)

# ------------------------------------------------------------
# Multi-objective solve
# ------------------------------------------------------------
x <- x |>
  add_objective_min_fragmentation(alias = "frag") |>
  set_method_weighted(
    aliases = c("cost", "frag"),
    weights = c(0.5, 0.5),
    normalize_weights = TRUE
  )

solset <- solve(x)
} # }
```
