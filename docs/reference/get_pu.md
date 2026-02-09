# Get planning unit results from a Solution

Extract the planning-unit results table from a
[solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
object returned by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md). The
returned table includes a `selected` indicator (typically `0/1`) showing
whether each planning unit is selected in the solution.

## Usage

``` r
get_pu(x, only_selected = FALSE)
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

A `data.frame` with planning-unit information stored in the solution and
a `selected` column.

## Details

This function expects the solution object to store a planning-unit
results table at `x$data$tables$pu`. It errors if the table is missing.
If `only_selected = TRUE`, it also errors when the `selected` column is
not present.

## See also

[`get_actions()`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_features()`](https://josesalgr.github.io/mosap/reference/get_features.md),
[`get_targets()`](https://josesalgr.github.io/mosap/reference/get_targets.md),
[`get_solution_vector()`](https://josesalgr.github.io/mosap/reference/get_solution_vector.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
pu_tbl <- get_pu(sol)
pu_sel <- get_pu(sol, only_selected = TRUE)
} # }
```
