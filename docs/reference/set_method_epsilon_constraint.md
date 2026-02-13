# Configure epsilon-constraint multi-objective method

Configure epsilon-constraint multi-objective method

## Usage

``` r
set_method_epsilon_constraint(x, primary, eps, aliases = NULL)
```

## Arguments

- x:

  A Data or MOProblem.

- primary:

  character(1). Alias of the primary objective to optimize.

- eps:

  Either:

  - named numeric vector: eps for constrained objectives (single run),
    or

  - named list of numeric vectors: grid per objective (multiple runs).
    Names must be objective aliases (usually all except `primary`).

- aliases:

  Optional character vector of objective aliases to consider. Default:
  all registered objectives in x (MOProblem) / in
  x\$data\$model_registry (Data-side).

- normalize:

  Logical. Reserved (future). Stored in method config.

- keep_primary_eps:

  Logical. If TRUE and eps contains primary, keep it (not typical).

## Value

MOProblem with method configured.
