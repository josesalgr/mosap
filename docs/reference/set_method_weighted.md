# Set the weighted-sum multi-objective method

Configure a `Problem` object to be solved with a weighted-sum
multi-objective method.

In the weighted-sum method, several registered atomic objectives are
combined into a single scalar objective using a weighted linear
combination. This function stores that configuration in `x$data$method`
so that it can be used later by
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md).

## Usage

``` r
set_method_weighted(
  x,
  aliases,
  weights,
  normalize_weights = FALSE,
  objective_scaling = FALSE
)
```

## Arguments

- x:

  A `Problem` object.

- aliases:

  Character vector of objective aliases to combine. Each alias must
  correspond to a previously registered atomic objective.

- weights:

  Numeric vector of weights, with the same length and order as
  `aliases`. Weights must be finite. If `normalize_weights = TRUE`, they
  are rescaled to sum to 1 before being stored.

- normalize_weights:

  Logical. If `TRUE`, normalize the supplied weights to sum to 1 before
  storing them.

- objective_scaling:

  Logical. If `TRUE`, request scaling of the participating objectives
  before weighted aggregation in the solving layer.

## Value

The updated `Problem` object with the weighted-sum method configuration
stored in `x$data$method`.

## Details

**General idea**

Suppose that a set of atomic objectives has already been registered in
the problem under aliases \\k \in \mathcal{K}\\. Let \\f_k(x)\\ denote
the scalar value of objective \\k\\, and let \\w_k\\ denote its
user-supplied weight.

The weighted-sum method combines them into a single scalar objective of
the form:

\$\$ \sum\_{k \in \mathcal{K}} w_k \\ f_k(x). \$\$

In practice, the exact sign convention used internally depends on the
sense of each registered atomic objective, for example whether it is a
minimization-type or maximization-type objective. The solving layer is
therefore responsible for constructing a solver-ready scalar objective
from the stored objective specifications and the requested weights.

**Atomic objectives requirement**

The weighted-sum method can only be used with atomic objectives that
have already been registered under aliases. These aliases are typically
created by calling objective setters with an `alias` argument, for
example:

    x <- x |>
      add_objective_min_cost(alias = "cost") |>
      add_objective_min_fragmentation(alias = "frag")

Internally, each atomic objective is stored in
`x$data$objectives[[alias]]` together with its metadata, such as:

- `objective_id`,

- `model_type`,

- `sense`,

- `objective_args`.

The `aliases` argument passed to this function selects which of those
registered atomic objectives are included in the weighted combination.

**Weight normalization**

If `normalize_weights = TRUE`, the supplied weights are rescaled to sum
to 1:

\$\$ \tilde{w}\_k = \frac{w_k}{\sum\_{j \in \mathcal{K}} w_j}. \$\$

This normalization does not change the optimizer's solution in a pure
weighted-sum formulation as long as all weights are multiplied by the
same positive constant, but it can improve interpretability and
numerical conditioning.

If `normalize_weights = FALSE`, the supplied weights are stored exactly
as provided.

**Objective scaling**

If `objective_scaling = TRUE`, the solving layer is expected to scale
the participating objectives before combining them. The purpose of
scaling is to reduce distortions caused by objectives being measured on
very different numerical ranges.

Conceptually, if \\R_k\\ denotes a scale or range associated with
objective \\k\\, then a scaled weighted sum may be interpreted as:

\$\$ \sum\_{k \in \mathcal{K}} w_k \\ \frac{f_k(x)}{R_k}. \$\$

The exact scaling rule is implemented later in the solving layer. This
function only stores whether objective scaling has been requested.

**Mixed objective senses**

Weighted sums are straightforward when all participating objectives have
the same optimization sense. When minimization and maximization
objectives are mixed, the solving layer must standardize them internally
before building the scalar objective.

This function validates that the requested aliases exist, but it does
not itself resolve objective-sense compatibility. That logic is
delegated to the downstream solving layer.

**Theoretical limitation**

The weighted-sum method typically recovers only *supported* efficient
solutions, that is, solutions lying on the convex hull of the Pareto
front in objective space. In non-convex multi-objective problems,
especially mixed integer problems, some efficient solutions cannot be
obtained by any weighted combination. In such cases, methods such as
[`set_method_epsilon_constraint`](https://josesalgr.github.io/mosap/reference/set_method_epsilon_constraint.md)
or
[`set_method_augmecon`](https://josesalgr.github.io/mosap/reference/set_method_augmecon.md)
may be preferable.

**Stored configuration**

This function stores the method definition in `x$data$method` with:

- `name = "weighted"`,

- `aliases`,

- `weights`,

- `normalize_weights`,

- `objective_scaling`.

The actual scalarization is performed later by
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md).

## See also

[`set_method_epsilon_constraint`](https://josesalgr.github.io/mosap/reference/set_method_epsilon_constraint.md),
[`set_method_augmecon`](https://josesalgr.github.io/mosap/reference/set_method_augmecon.md),
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ------------------------------------------------------------
# Example 1: cost vs fragmentation
# ------------------------------------------------------------
pu <- data.frame(id = 1:4, cost = c(1, 2, 2, 3))
features <- data.frame(id = 1, name = "sp1")
dist_features <- data.frame(
  pu = c(1, 2, 3, 4),
  feature = 1,
  amount = c(1, 1, 1, 1)
)

x <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)

bnd <- data.frame(
  id1 = c(1, 2, 3, 1, 2, 3, 4),
  id2 = c(1, 2, 3, 2, 3, 4, 4),
  boundary = c(2, 2, 2, 1, 1, 1, 2)
)

x <- add_spatial_boundary(x, boundary = bnd, name = "boundary")

x <- x |>
  add_targets_relative(0.5) |>
  add_objective_min_cost(alias = "cost") |>
  add_objective_min_fragmentation(
    alias = "frag",
    relation_name = "boundary",
    weight_multiplier = 0.01
  )

x <- set_method_weighted(
  x,
  aliases = c("cost", "frag"),
  weights = c(1, 1),
  normalize_weights = TRUE
)

# sol <- solve(x)

# ------------------------------------------------------------
# Example 2: scan weights to explore trade-offs
# ------------------------------------------------------------
weight_grid <- seq(0, 1, by = 0.25)
xs <- vector("list", length(weight_grid))

for (i in seq_along(weight_grid)) {
  w <- weight_grid[i]
  xs[[i]] <- set_method_weighted(
    x,
    aliases = c("cost", "frag"),
    weights = c(1 - w, w),
    normalize_weights = TRUE
  )
  # sols[[i]] <- solve(xs[[i]])
}

# ------------------------------------------------------------
# Example 3: request objective scaling
# ------------------------------------------------------------
x <- set_method_weighted(
  x,
  aliases = c("cost", "frag"),
  weights = c(0.7, 0.3),
  normalize_weights = FALSE,
  objective_scaling = TRUE
)
} # }
```
