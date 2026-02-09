# Get target achievement table from a Solution

Extract the target achievement table (if present) from a
[solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
object returned by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md). The
targets table typically contains the target value, achieved value, and
gap (`achieved - target_value`), plus target metadata such as type and
units.

## Usage

``` r
get_targets(x)
```

## Arguments

- x:

  A
  [solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
  object returned by
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

## Value

A `data.frame` with target achievement metrics, or `NULL` if the
solution does not contain a targets table.

## Details

Targets are optional. If the solution does not include
`x$data$tables$targets`, this function returns `NULL` without error.

## See also

[`get_pu()`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions()`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_features()`](https://josesalgr.github.io/mosap/reference/get_features.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
tgt_tbl <- get_targets(sol)
if (!is.null(tgt_tbl)) head(tgt_tbl)
} # }
```
