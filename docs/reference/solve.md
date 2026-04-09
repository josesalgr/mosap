# Solve a planning problem

Solve a planning problem stored in a `Problem` object.

This is the main execution step of the multiscape workflow. It reads the
problem specification stored in a `Problem` object, builds the
corresponding optimization model when needed, applies the configured
solver settings, and returns either a
[`solution-class`](https://josesalgr.github.io/multiscape/reference/solution-class.md)
or a
[`solutionset-class`](https://josesalgr.github.io/multiscape/reference/solutionset-class.md)
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
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)
  and optionally enriched with actions, effects, targets, constraints,
  objectives, spatial relations, method settings, and solver settings.

- ...:

  Additional arguments reserved for internal or legacy solver handling.
  These are not part of the main recommended user interface.

## Value

Either:

- a
  [`solution-class`](https://josesalgr.github.io/multiscape/reference/solution-class.md)
  object when solving a single-objective problem, or

- a
  [`solutionset-class`](https://josesalgr.github.io/multiscape/reference/solutionset-class.md)
  object when solving a configured multi-objective problem.

A `Solution` represents one optimization run. A `SolutionSet` represents
multiple runs together with their run table, design information, and
individual `Solution` objects.

After solving, users will typically inspect or visualize the returned
results through methods associated with `Solution` and `SolutionSet`
objects.

## Details

**Role of `solve()`**

The typical multiscape workflow is:

    x <- create_problem(...)
    x <- add_...(x, ...)
    x <- set_...(x, ...)
    res <- solve(x)

Thus, `solve()` is the stage at which the stored problem specification
is turned into one or more optimization runs.

For most users, `solve()` is the standard execution entry point.
Explicit compilation with
[`compile_model()`](https://josesalgr.github.io/multiscape/reference/compile_model.md)
is optional and is mainly useful for advanced inspection or debugging
workflows.

**What `solve()` uses from the problem object**

The function uses the information already stored in the `Problem`
object, including:

- baseline planning data,

- actions, effects, profit, and spatial relations,

- targets and constraints,

- registered objectives,

- an optional multi-objective method configuration,

- solver settings.

If a model has not yet been built, it is built internally during the
solve process. If a model snapshot or pointer already exists, the
solving layer may reuse or refresh it depending on the internal model
state.

**Single-objective vs multi-objective behaviour**

The behaviour of `solve()` depends on the problem configuration.

- **Single-objective case.** If exactly one objective is active and no
  multi-objective method is configured, `solve()` runs a single
  optimization problem and returns a
  [`solution-class`](https://josesalgr.github.io/multiscape/reference/solution-class.md)
  object.

- **Multi-objective case.** If a multi-objective method is configured,
  `solve()` dispatches internally according to the stored method name
  and returns a
  [`solutionset-class`](https://josesalgr.github.io/multiscape/reference/solutionset-class.md)
  object.

Currently supported multi-objective method names are:

- `"weighted"`,

- `"epsilon_constraint"`,

- `"augmecon"`.

**Consistency rule**

If multiple objectives are registered but no multi-objective method has
been selected, `solve()` stops with an error. In practical terms:

- one objective and no multi-objective method \\\Rightarrow\\
  single-objective solve,

- multiple objectives and a valid multi-objective method \\\Rightarrow\\
  multi-objective solve,

- multiple objectives and no multi-objective method \\\Rightarrow\\
  error.

**Solver settings**

Solver configuration is read from the `Problem` object, typically after
calling
[`set_solver`](https://josesalgr.github.io/multiscape/reference/set_solver.md)
or one of its convenience wrappers such as
[`set_solver_gurobi`](https://josesalgr.github.io/multiscape/reference/set_solver_gurobi.md)
or
[`set_solver_cbc`](https://josesalgr.github.io/multiscape/reference/set_solver_cbc.md).

These settings may include:

- the selected backend,

- time limits,

- optimality-gap settings,

- CPU cores,

- verbosity options,

- backend-specific solver parameters.

**Method dispatch**

`solve()` is an S3 generic. The public method documented here is
`solve.Problem()`, which operates on `Problem` objects.

## See also

[`problem-class`](https://josesalgr.github.io/multiscape/reference/problem-class.md),
[`solution-class`](https://josesalgr.github.io/multiscape/reference/solution-class.md),
[`solutionset-class`](https://josesalgr.github.io/multiscape/reference/solutionset-class.md),
[`compile_model`](https://josesalgr.github.io/multiscape/reference/compile_model.md),
[`set_solver`](https://josesalgr.github.io/multiscape/reference/set_solver.md),
[`set_solver_cbc`](https://josesalgr.github.io/multiscape/reference/set_solver_cbc.md),
[`set_solver_gurobi`](https://josesalgr.github.io/multiscape/reference/set_solver_gurobi.md),
[`set_method_weighted_sum`](https://josesalgr.github.io/multiscape/reference/set_method_weighted_sum.md),
[`set_method_epsilon_constraint`](https://josesalgr.github.io/multiscape/reference/set_method_epsilon_constraint.md),
[`set_method_augmecon`](https://josesalgr.github.io/multiscape/reference/set_method_augmecon.md)

## Examples

``` r
# ------------------------------------------------------------
# Minimal single-objective example
# ------------------------------------------------------------
pu <- data.frame(
  id = 1:4,
  cost = c(1, 2, 3, 4)
)

features <- data.frame(
  id = 1:2,
  name = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4),
  feature = c(1, 2, 2, 1, 2),
  amount = c(5, 2, 3, 4, 1)
)

actions <- data.frame(
  id = c("conservation", "restoration")
)

effects <- data.frame(
  action = rep(c("conservation", "restoration"), each = 2),
  feature = rep(features$id, times = 2),
  multiplier = c(0.10, 0.10, 0.50, 0.50)
)

x <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
) |>
  add_actions(
    actions = actions,
    cost = c(conservation = 1, restoration = 2)
  ) |>
  add_effects(
    effects = effects,
    effect_type = "after"
  ) |>
  add_constraint_targets_relative(0.05) |>
  add_objective_min_cost(alias = "cost")

if (requireNamespace("rcbc", quietly = TRUE)) {
  x <- set_solver_cbc(x, verbose = FALSE)
  sol <- solve(x)
  print(sol)
}
#> A multiscape solution (<Solution>)
#> ├─result
#> │├─status: .pa_solution_status_inline(st)
#> │├─objective value: "3"
#> │├─gap: 0%
#> │└─runtime: 0.02 sec
#> └─selection
#> │├─planning units: 1 of 4 selected
#> │├─actions: 1 of 8 selected
#> │└─targets met: 2 of 2
#> └─objective values
#> │└─cost: 3
#> └─solver
#> │├─name: `cbc`
#> │├─cores: 2
#> │└─time limit: 2147483647
#> # ℹ Use `x$summary` to inspect user-facing solution summaries.

# ------------------------------------------------------------
# Minimal multi-objective example
# ------------------------------------------------------------
x_mo <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
) |>
  add_actions(
    actions = actions,
    cost = c(conservation = 1, restoration = 2)
  ) |>
  add_effects(
    effects = effects,
    effect_type = "after"
  ) |>
  add_constraint_targets_relative(0.05) |>
  add_objective_min_cost(alias = "cost") |>
  add_objective_max_benefit(alias = "benefit") |>
  set_method_weighted_sum(
    aliases = c("cost", "benefit"),
    weights = c(0.5, 0.5),
    normalize_weights = TRUE
  )

if (requireNamespace("rcbc", quietly = TRUE)) {
  x_mo <- set_solver_cbc(x_mo, verbose = FALSE)
  solset <- solve(x_mo)
  print(solset)
}
#> A multiscape solution set (<SolutionSet>)
#> ├─method
#> │├─name: `weighted`
#> │└─objectives: 2 (cost, benefit)
#> └─content
#> │├─design rows: 1
#> │├─runs: 1
#> │└─solutions: 1
#> └─run summary
#> │├─statuses: optimal: 1
#> │├─runtime: 0
#> │├─gap: 0
#> │├─design cols: none
#> │└─value cols: value_cost, value_benefit
#> # ℹ Use `x$solution$runs`, `x$solution$design`, and `x$solution$solutions[[i]]`
#> to inspect details.
```
