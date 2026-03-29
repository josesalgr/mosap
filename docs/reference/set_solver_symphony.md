# Configure SYMPHONY solver settings

Convenience wrapper around
[`set_solver`](https://josesalgr.github.io/paretoscape/reference/set_solver.md)
that stores `solver = "symphony"` in the problem object.

This function does not solve the model. It only updates the stored
solver configuration.

## Usage

``` r
set_solver_symphony(
  x,
  ...,
  solver_params = list(),
  gap_limit = NULL,
  time_limit = NULL,
  solution_limit = NULL,
  cores = NULL,
  verbose = FALSE,
  log_file = NULL,
  write_log = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- ...:

  Additional named solver-specific parameters. These are merged into
  `solver_params`. For example, `MIPFocus = 1` for Gurobi.

- solver_params:

  Named list of solver-specific parameters. These are merged with
  previously stored backend-specific parameters rather than replacing
  them completely.

- gap_limit:

  Optional numeric value in \\\[0,1\]\\ giving the relative optimality
  gap for mixed-integer optimization. If `NULL`, the previously stored
  value is kept unchanged.

- time_limit:

  Optional non-negative numeric value giving the maximum solving time in
  seconds. If `NULL`, the previously stored value is kept unchanged.

- solution_limit:

  Optional logical flag controlling backend-specific early stopping
  after feasible solution discovery. If `NULL`, the previously stored
  value is kept unchanged.

- cores:

  Optional positive integer giving the number of CPU cores to use. If
  `NULL`, the previously stored value is kept unchanged.

- verbose:

  Optional logical flag indicating whether the solver should print log
  output. If `NULL`, the previously stored value is kept unchanged.

- log_file:

  Optional character string giving the name of the solver log file. If
  `NULL`, the previously stored value is kept unchanged.

- write_log:

  Optional logical flag indicating whether solver output should be
  written to a file. If `NULL`, the previously stored value is kept
  unchanged.

## Value

An updated `Problem` object with SYMPHONY solver settings stored in
`x$data$solve_args`.

## See also

[`set_solver`](https://josesalgr.github.io/paretoscape/reference/set_solver.md),
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- set_solver_symphony(
  x,
  gap_limit = 0.05,
  time_limit = 300
)
} # }
```
