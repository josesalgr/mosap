# Get feature achievement summary from a Solution

Extract the feature achievement table from a
[solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
object returned by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md). This
table typically summarizes, for each feature, how much is achieved by
baseline selection and by action effects, and their total.

## Usage

``` r
get_features(x)
```

## Arguments

- x:

  A
  [solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
  object returned by
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

## Value

A `data.frame` with feature achievement metrics stored in the solution.

## Details

This function expects the feature achievement table at
`x$data$tables$features` and errors if it is missing. The exact columns
depend on the model and reporting options, but commonly include:

- `baseline_contrib`: contribution from baseline / conservation
  selection.

- `recovery_contrib`: contribution from action effects (e.g., benefits).

- `total`: baseline + recovery.

## See also

[`get_pu()`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions()`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_targets()`](https://josesalgr.github.io/mosap/reference/get_targets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
feat_tbl <- get_features(sol)
} # }
```
