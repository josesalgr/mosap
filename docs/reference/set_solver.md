# Configure solver settings

Stores solver configuration inside a `Data` object, so that `solve(x)`
can run without repeating solver arguments.

This function does not build the model; it only stores runtime solver
options.

## Usage

``` r
set_solver(
  x,
  solver = c("auto", "gurobi", "cplex", "cbc", "symphony"),
  gap_limit = NULL,
  time_limit = NULL,
  solution_limit = NULL,
  cores = NULL,
  verbose = NULL,
  name_output_file = NULL,
  output_file = NULL,
  solver_params = list(),
  ...
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- solver:

  Character. One of `"auto"`, `"gurobi"`, `"cplex"`, `"cbc"`,
  `"symphony"`.

- gap_limit:

  Numeric in 0,1. Relative MIP optimality gap. If `NULL`, keep stored
  value.

- time_limit:

  Numeric. Time limit in seconds. If `NULL`, keep stored value.

- solution_limit:

  Logical. If `NULL`, keep stored value.

- cores:

  Integer. If `NULL`, keep stored value.

- verbose:

  Logical. If `NULL`, keep stored value.

- name_output_file:

  Character. If `NULL`, keep stored value.

- output_file:

  Logical. If `NULL`, keep stored value.

- solver_params:

  List. Solver-specific parameters (merged with stored ones).

- ...:

  Convenience: solver-specific parameters (e.g., `MIPFocus=1`) merged
  into `solver_params`.

## Value

Updated `Data` object.
