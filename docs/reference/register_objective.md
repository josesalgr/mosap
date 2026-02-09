# Register an atomic objective in an MOProblem

This does not solve anything. It only records how to evaluate/build the
objective. Multi-objective methods (weighted, epsilon, etc.) will use
this registry later.

## Usage

``` r
register_objective(
  alias,
  sense = c("min", "max"),
  build_fun,
  eval_fun = NULL,
  ...
)
```

## Arguments

- alias:

  `character(1)` unique objective identifier used by MO methods.

- sense:

  `character(1)` either "min" or "max".

- build_fun:

  A function `(data, ...) -> data_or_problem` that activates this
  objective. For now you can store it and decide later whether it
  returns a new model pointer, an objective vector, or a configured
  `OptimizationProblem`.

- eval_fun:

  Optional function `(solution, data, ...) -> numeric(1)` to evaluate
  objective value.

- ...:

  Reserved for future extensions.

- x:

  `MOProblem`.

## Value

A function that takes an `MOProblem` (for DSL `x + ...`) and returns the
updated object.
