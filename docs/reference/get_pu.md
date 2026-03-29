# Get planning-unit results from a solution

Extract the planning-unit summary table from a `Solution` or
`SolutionSet` object returned by
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

The returned table summarizes solution values at the planning-unit level
and typically includes a `selected` indicator showing whether each
planning unit is selected in the solution.

## Usage

``` r
get_pu(x, only_selected = FALSE, run = NULL)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

- only_selected:

  Logical. If `TRUE`, return only rows where `selected == 1`. Default is
  `FALSE`.

- run:

  Optional positive integer giving the run index to extract from a
  `SolutionSet`. If `NULL`, all runs are returned when available.

## Value

A `data.frame` containing the stored planning-unit summary. Typical
columns include planning-unit identifiers, optional labels, and a
`selected` indicator.

## Details

This function reads the planning-unit summary stored in `x$summary$pu`.
It does not reconstruct the table from the raw decision vector; it
simply returns the stored summary after optional filtering.

Let \\w_i\\ denote the planning-unit selection variable for planning
unit \\i\\. In standard `paretoscape` workflows, the `selected` column
is the user-facing representation of that planning-unit decision,
typically coded as `0` or `1`.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. This requires the summary table to contain a
`run_id` column.

If `only_selected = TRUE`, only rows with `selected == 1` are returned.
This requires the summary table to contain a `selected` column.

This function is intended for user-facing inspection of planning-unit
results. For the raw model variable vector, use
[`get_solution_vector`](https://josesalgr.github.io/paretoscape/reference/get_solution_vector.md).

## See also

[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md),
[`get_solution_vector`](https://josesalgr.github.io/paretoscape/reference/get_solution_vector.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)

pu_tbl <- get_pu(sol)
pu_sel <- get_pu(sol, only_selected = TRUE)
} # }
```
