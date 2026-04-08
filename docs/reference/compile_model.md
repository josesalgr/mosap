# Compile the optimization model stored in a Problem

Materializes the optimization model represented by a `Problem` object
without solving it. This is an advanced function mainly intended for
debugging, inspection, and explicit model preparation.

In standard workflows, users normally do not need to call this function,
because
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)
compiles the model automatically when needed.

## Usage

``` r
compile_model(x, force = FALSE, ...)

# S3 method for class 'Problem'
compile_model(x, force = FALSE, ...)
```

## Arguments

- x:

  A `Problem` object.

- force:

  Logical. If `TRUE`, rebuild the model even if a current compiled model
  already exists.

- ...:

  Reserved for future extensions.

## Value

A `Problem` object with compiled model structures stored internally.

## Details

Use this function when you want to prepare the optimization model
explicitly before solving, inspect compiled model structures, or verify
that the problem compiles successfully.

Conceptually, a `Problem` object stores a declarative optimization
specification: planning data, actions, effects, targets, constraints,
objectives, spatial relations, and optional method or solver settings.
`compile_model()` transforms that stored specification into an internal
compiled model representation that can later be reused by the solving
layer.

The exact compiled representation is implementation-specific, but it may
include indexed variables, prepared constraint blocks, objective
structures, and internal model snapshots or pointers.

Compilation does not solve the optimization problem. Therefore, a
problem may compile successfully and still later be infeasible,
numerically difficult, or otherwise fail during solver execution.

## See also

[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
)

x <- x |>
  add_constraint_targets_relative(0.3) |>
  add_objective_min_cost(alias = "cost")

x <- compile_model(x)

# Force recompilation
x <- compile_model(x, force = TRUE)
} # }
```
