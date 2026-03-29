# Get raw solution vector from a solution

Return the raw decision-variable vector produced by the solver, in the
internal model-variable order used by the optimization backend.

## Usage

``` r
get_solution_vector(x, run = 1L)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

- run:

  Positive integer giving the run index to extract when `x` is a
  `SolutionSet`. Default is `1L`.

## Value

A numeric vector with one value per internal model variable.

## Details

This function extracts the raw solution vector stored at
`x$solution$vector` for a `Solution` or for a selected run of a
`SolutionSet`.

The returned vector is in the internal variable order of the
optimization model. Depending on the problem formulation, it may
include:

- planning-unit selection variables,

- action-allocation variables,

- auxiliary variables introduced for targets, budgets, fragmentation, or
  other constraints/objectives,

- and potentially additional blocks created internally by the model
  builder.

Therefore, this vector is primarily intended for advanced users,
debugging, diagnostics, or internal verification. It is not a
user-facing allocation table.

To inspect selected planning units or selected actions in a more
interpretable form, use
[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md)
or
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md)
instead.

For a single solution, the returned vector corresponds to that solution.
For a `SolutionSet`, the `run` argument selects which run to extract.

## See also

[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
v <- get_solution_vector(sol)
length(v)
} # }
```
