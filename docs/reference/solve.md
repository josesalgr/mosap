# Solve optimization model

Solves a model defined by a `Data` object. Solver configuration is read
from `x$data$solve_args` (typically set via
[`set_solver()`](https://josesalgr.github.io/mosap/reference/set_solver.md)
/ `set_solver_*()`).

## Usage

``` r
solve(x, ...)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- ...:

  Optional legacy solver arguments (deprecated).

## Value

A `Solution` object.
