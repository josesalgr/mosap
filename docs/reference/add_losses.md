# Add losses

Convenience wrapper around
[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md)
that keeps only negative effects, represented by rows with `loss > 0`.

This function is useful when the workflow is focused only on damaging
consequences of actions and a loss-only wrapper is more convenient than
calling `add_effects(..., component = "loss")` directly. Internally, it
calls
[`add_effects()`](https://josesalgr.github.io/multiscape/reference/add_effects.md)
with `component = "loss"`. The canonical stored result therefore
contains only rows whose effect satisfies \\\mathrm{loss}\_{iaf} \> 0\\.

In addition, a mirror table containing only the loss component is stored
in `x$data$dist_loss`.

## Usage

``` r
add_losses(
  x,
  losses = NULL,
  effect_type = c("delta", "after"),
  effect_aggregation = c("sum", "mean")
)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md).
  It must already contain feasible actions; run
  [`add_actions`](https://josesalgr.github.io/multiscape/reference/add_actions.md)
  first.

- losses:

  Alias of `effects`, used for symmetry with
  [`add_benefits()`](https://josesalgr.github.io/multiscape/reference/add_benefits.md).

- effect_type:

  Character string indicating how supplied effect values are
  interpreted. Must be one of:

  - `"delta"`: values represent signed net changes,

  - `"after"`: values represent after-action amounts and are converted
    to net changes relative to baseline feature amounts.

- effect_aggregation:

  Character string giving the aggregation used when converting raster
  values to planning-unit level. Must be one of `"sum"` or `"mean"`.

## Value

An updated `Problem` object containing:

- `dist_effects`:

  The canonical filtered effects table, containing only rows with
  `loss > 0`.

- `dist_loss`:

  A convenience table containing only the loss component.

- `losses_meta`:

  Metadata for the stored loss table.

## See also

[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md),
[`add_benefits`](https://josesalgr.github.io/multiscape/reference/add_benefits.md),
[`add_objective_min_loss`](https://josesalgr.github.io/multiscape/reference/add_objective_min_loss.md)

## Examples

``` r
pu <- data.frame(id = 1:2, cost = c(1, 2))
features <- data.frame(id = 1, name = "sp1")
dist_features <- data.frame(pu = 1:2, feature = 1, amount = c(5, 10))

p <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
)

p <- add_actions(
  p,
  data.frame(id = "harvest")
)

eff <- data.frame(
  pu = c(1, 2),
  action = c("harvest", "harvest"),
  feature = c(1, 1),
  delta = c(2, -4)
)

p <- add_losses(p, losses = eff)
p$data$dist_loss
#>   pu  action feature loss internal_pu internal_action internal_feature
#> 2  2 harvest       1    4           2               1                1
#>   feature_name action_name
#> 2          sp1     harvest
```
