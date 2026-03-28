# Configure solver settings

Store solver configuration inside a `Problem` object so that
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md) can
later run using the stored backend and runtime options.

This function does not build or solve the optimization model. It only
updates the solver configuration stored in `x$data$solve_args`.

## Usage

``` r
set_solver(
  x,
  solver = c("auto", "gurobi", "cplex", "cbc", "symphony"),
  gap_limit = NULL,
  time_limit = NULL,
  solution_limit = NULL,
  cores = NULL,
  verbose = FALSE,
  log_file = NULL,
  write_log = NULL,
  solver_params = list(),
  ...
)
```

## Arguments

- x:

  A `Problem` object.

- solver:

  Character string indicating the solver backend to use. Must be one of
  `"auto"`, `"gurobi"`, `"cplex"`, `"cbc"`, or `"symphony"`.

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

- solver_params:

  Named list of solver-specific parameters. These are merged with
  previously stored backend-specific parameters rather than replacing
  them completely.

- ...:

  Additional named solver-specific parameters. These are merged into
  `solver_params`. For example, `MIPFocus = 1` for Gurobi.

## Value

An updated `Problem` object with modified solver settings stored in
`x$data$solve_args`.

## Details

**Purpose**

The `mosap` workflow separates problem specification from solver
configuration. Problem data, actions, effects, targets, objectives, and
methods are stored in the `Problem` object, and solver settings are
stored separately in `x$data$solve_args`.

This function allows solver options to be configured once and reused
later through
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)`(x)`
without repeating the same arguments each time.

**Stored fields**

The solver configuration is stored in `x$data$solve_args`. Typical
entries include:

- `solver`,

- `gap_limit`,

- `time_limit`,

- `solution_limit`,

- `cores`,

- `verbose`,

- `write_log`,

- `log_file`,

- `solver_params`.

**Incremental update semantics**

This function updates solver settings incrementally.

If an argument is supplied as `NULL`, the previously stored value is
kept unchanged. Therefore, repeated calls can be used to modify only
selected components of the solver configuration.

For example, a user may first configure the solver backend and time
limit, and later update only the optimality gap or only a
backend-specific parameter.

**Gap limit**

The argument `gap_limit` is interpreted as a relative optimality gap for
mixed-integer optimization. It must lie in \\\[0,1\]\\.

If the solver stops with incumbent value \\z^{\mathrm{inc}}\\ and best
bound \\z^{\mathrm{bd}}\\, then the exact stopping rule depends on the
solver backend, but conceptually `gap_limit` controls the maximum
accepted relative difference between the incumbent and the bound.

**Time limit**

The argument `time_limit` is interpreted as a maximum wall-clock time in
seconds allowed for the solver.

**Solution limit**

The argument `solution_limit` is stored as a logical flag. Its exact
meaning depends on the backend-specific solving layer, but conceptually
it requests early termination after finding a feasible solution
according to the behaviour supported by the chosen solver.

**Cores**

The argument `cores` specifies the number of CPU cores to use. If the
requested number exceeds the number of detected cores, it is capped to
the detected maximum with a warning.

**Verbose output and log files**

The arguments `verbose`, `write_log`, and `log_file` control how solver
logging is handled. These options are stored and later interpreted by
the solving layer for the selected backend.

**Solver-specific parameters**

Additional backend-specific parameters can be passed in two ways:

- through the named list `solver_params`,

- through additional named arguments in `...`.

These two sources are merged, and the result is then merged with any
previously stored `solver_params`. Existing parameters are therefore
preserved unless explicitly overwritten.

This is particularly useful for backend-specific controls such as node
selection, emphasis parameters, tolerances, or heuristics.

**Supported backends**

The `solver` argument selects the backend to be used later by
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md).
Supported values are:

- `"auto"`: let the solving layer choose an available backend,

- `"gurobi"`,

- `"cplex"`,

- `"cbc"`,

- `"symphony"`.

This function only stores the requested backend. Availability of the
backend is checked later when solving.

## See also

[`solve`](https://josesalgr.github.io/mosap/reference/solve.md),
[`set_solver_gurobi`](https://josesalgr.github.io/mosap/reference/set_solver_gurobi.md),
[`set_solver_cplex`](https://josesalgr.github.io/mosap/reference/set_solver_cplex.md),
[`set_solver_cbc`](https://josesalgr.github.io/mosap/reference/set_solver_cbc.md),
[`set_solver_symphony`](https://josesalgr.github.io/mosap/reference/set_solver_symphony.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)

x <- set_solver(
  x,
  solver = "gurobi",
  gap_limit = 0.01,
  time_limit = 300,
  cores = 4,
  verbose = TRUE,
  MIPFocus = 1
)

# Later:
# sol <- solve(x)
} # }
```
