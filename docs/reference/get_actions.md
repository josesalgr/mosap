# Get action results from a Solution

Extract the action-allocation results table from a
[solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
object returned by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md). The
returned table includes a `selected` indicator (typically `0/1`) showing
whether each feasible `(pu, action)` pair is selected in the solution.

## Usage

``` r
get_actions(x, only_selected = FALSE)
```

## Arguments

- x:

  A
  [solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
  object returned by
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

- only_selected:

  Logical. If `TRUE`, return only rows where `selected == 1`. Default
  `FALSE`.

## Value

A `data.frame` with action-allocation information stored in the solution
and a `selected` column.

## Details

This function expects the solution object to store an action-allocation
table at `x$data$tables$actions`. It errors if the table is missing. If
`only_selected = TRUE`, it also errors when the `selected` column is not
present.

## See also

[`get_pu()`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_features()`](https://josesalgr.github.io/mosap/reference/get_features.md),
[`get_targets()`](https://josesalgr.github.io/mosap/reference/get_targets.md),
[`get_solution_vector()`](https://josesalgr.github.io/mosap/reference/get_solution_vector.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
act_tbl <- get_actions(sol)
act_sel <- get_actions(sol, only_selected = TRUE)
} # }
```
