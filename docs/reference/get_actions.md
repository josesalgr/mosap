# Get action results from a solution

Extract the action-allocation summary table from a `Solution` or
`SolutionSet` object returned by
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

The returned table summarizes solution values at the planning
unit–action level and typically includes a `selected` indicator showing
whether each feasible `(pu, action)` pair is selected in the solution.

## Usage

``` r
get_actions(x, only_selected = FALSE, run = NULL)
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

A `data.frame` containing the stored action-allocation summary. Typical
columns include planning-unit ids, action ids, optional labels, and a
`selected` indicator.

## Details

This function reads the action summary stored in `x$summary$actions`. It
does not reconstruct the table from the raw decision vector; it simply
returns the stored summary after optional filtering.

Let \\x\_{ia}\\ denote the decision variable associated with selecting
action \\a\\ in planning unit \\i\\. In standard `paretoscape`
workflows, the `selected` column is the user-facing representation of
that decision, typically coded as `0` or `1`.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. This requires the summary table to contain a
`run_id` column.

If `only_selected = TRUE`, only rows with `selected == 1` are returned.
This requires the summary table to contain a `selected` column.

This function is intended for user-facing inspection of action
allocations. For the raw model variable vector, use
[`get_solution_vector`](https://josesalgr.github.io/paretoscape/reference/get_solution_vector.md).

## See also

[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md),
[`get_solution_vector`](https://josesalgr.github.io/paretoscape/reference/get_solution_vector.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)

act_tbl <- get_actions(sol)
act_sel <- get_actions(sol, only_selected = TRUE)
} # }
```
