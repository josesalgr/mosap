# Set multi-objective method: weighted sum

Configure the **weighted-sum** multi-objective method on top of *atomic
objectives* previously registered in the planning object.

The weighted-sum method combines multiple objectives into a single
scalar objective: \$\$\sum\_{k \in K} w_k \\ f_k(x)\$\$ where each
\\f_k(x)\\ is an *atomic objective* (e.g., minimum cost, minimum
fragmentation, maximum benefit) and \\w_k \ge 0\\ are user-supplied
weights.

This function **does not solve** the problem. It stores the
multi-objective configuration inside an `"MOProblem"` object so it can
later be used by a multi-objective solving workflow (e.g., weighted
runs, epsilon-constraint, AUGMECON, interactive methods).

## Usage

``` r
set_method_weighted(x, aliases, weights, normalize = FALSE)
```

## Arguments

- x:

  A planning object (`Data`) or an existing `"MOProblem"`. If a `Data`
  object is provided, it is promoted to `"MOProblem"` via
  `.pamo_as_mo()`.

- aliases:

  `character`. Objective aliases to combine (e.g. `c("cost","frag")`).
  Each alias must refer to a registered atomic objective.

- weights:

  `numeric`. Non-negative weights, same length as `aliases`. If
  `normalize = TRUE`, these weights are rescaled to sum to 1.

- normalize:

  `logical`. If `TRUE`, normalize weights to sum to 1.

## Value

The same object `x`, promoted to class `"MOProblem"`, with the method
configuration stored in `x$method`:

    x$method <- list(
      name = "weighted",
      params = list(
        aliases = <character>,
        weights = <numeric>,
        normalize = <logical>
      )
    )

## Atomic objectives requirement

Weighted-sum requires that the base planning object contains one or more
*atomic objectives* registered under unique aliases. Atomic objectives
are registered by calling objective setters with an `alias` argument,
for example:

    x <- x |>
      add_objective_min_cost(alias = "cost") |>
      add_objective_min_fragmentation(alias = "frag")

Internally, atomic objectives are stored (by convention) in
`x$data$objectives[[alias]]`, each entry including:

- `objective_id`: stable identifier (e.g., `"min_cost"`,
  `"min_fragmentation"`)

- `model_type`: builder identifier (used later when materializing the
  model)

- `sense`: `"min"` or `"max"`

- `objective_args`: objective-specific arguments

## Objective sense (min vs max)

Combining objectives with different senses (mixing `"min"` and `"max"`)
requires a **standardization step** in the multi-objective solving layer
(e.g., converting maximization terms into minimization terms by negation
or by shifting/scaling). This function validates that the requested
`aliases` exist; additional checks (e.g., forbidding mixed senses) can
be implemented in `.pamo_get_objective_specs()` or enforced later by the
solver.

## Weight scaling and normalization

- Weights must be finite and non-negative.

- If `normalize = TRUE`, weights are rescaled to sum to 1.

- Multiplying all weights by a positive constant does not change the
  optimizer’s solution in a pure weighted-sum formulation, but can
  affect numerical conditioning.

## Limitations of weighted sums

Weighted sums typically recover only **supported** Pareto-optimal
solutions (on the convex hull of the Pareto front). If the Pareto front
is non-convex (common in integer programs), some efficient solutions
cannot be obtained with any weight vector; consider epsilon-constraint
or AUGMECON methods in those cases.

## Examples

``` r
# \donttest{
# ------------------------------------------------------------
# Example 1: Cost vs Fragmentation (both minimization)
# ------------------------------------------------------------
pu <- data.frame(id = 1:4, cost = c(1, 2, 2, 3))
features <- data.frame(id = 1, name = "sp1")
dist_features <- data.frame(pu = c(1,2,3,4), feature = 1, amount = c(1,1,1,1))

x <- inputData(pu = pu, features = features, dist_features = dist_features)

# (Optional) register a boundary relation used by fragmentation objectives
bnd <- data.frame(
  id1 = c(1,2,3,1,2,3,4),
  id2 = c(1,2,3,2,3,4,4),
  boundary = c(2,2,2,1,1,1,2)
)
x <- add_spatial_boundary(x, boundary = bnd, name = "boundary")

# targets + atomic objectives
x <- x |>
  add_conservation_targets_relative(0.5) |>
  add_objective_min_cost(alias = "cost") |>
  add_objective_min_fragmentation(
    alias = "frag",
    relation_name = "boundary",
    weight_multiplier = 0.01
  )

# configure weighted sum
mo <- set_method_weighted(
  x,
  aliases = c("cost", "frag"),
  weights = c(1, 1),
  normalize = TRUE
)

# Later (in your MO workflow):
# sol <- solve(mo)


# ------------------------------------------------------------
# Example 2: Scan weights to explore trade-offs
# ------------------------------------------------------------
weight_grid <- seq(0, 1, by = 0.25)
mos <- vector("list", length(weight_grid))

for (i in seq_along(weight_grid)) {
  w <- weight_grid[i]
  mos[[i]] <- set_method_weighted(
    x,
    aliases = c("cost", "frag"),
    weights = c(1 - w, w),
    normalize = TRUE
  )
  # sols[[i]] <- solve(mos[[i]])
}


# ------------------------------------------------------------
# Example 3: Mixing max and min objectives (requires solver support)
# ------------------------------------------------------------
# If your multi-objective solver standardizes senses, you can mix:
# - a minimization objective (e.g., cost)
# - a maximization objective (e.g., benefit)
#
# Otherwise, use epsilon-constraint or re-encode the objective.
#
# x2 <- x |>
#   add_actions(actions_df) |>
#   add_benefits(benefits_df) |>
#   add_objective_min_cost(alias = "cost") |>
#   add_objective_max_benefit(alias = "benefit")
# mo2 <- set_method_weighted(x2, aliases = c("cost","benefit"), weights = c(1, 0.2))
# }
```
