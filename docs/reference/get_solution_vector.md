# Get raw solution vector from a Solution

Return the raw decision-variable vector produced by the solver, in the
internal model variable order used by the optimizer backend.

## Usage

``` r
get_solution_vector(x)
```

## Arguments

- x:

  A
  [solution](https://josesalgr.github.io/mosap/reference/solution-class.md)
  object returned by
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

## Value

A numeric vector with one value per model variable.

## Details

This function expects the raw solution vector at `x$data$sol` and errors
if it is missing. The vector is returned as numeric and corresponds to
the model's variable ordering (e.g., planning-unit selection variables,
action variables, and any auxiliary variables such as fragmentation
variables when present).

## See also

[`get_pu()`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions()`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_features()`](https://josesalgr.github.io/mosap/reference/get_features.md),
[`get_targets()`](https://josesalgr.github.io/mosap/reference/get_targets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
v <- get_solution_vector(sol)
length(v)
} # }
```
