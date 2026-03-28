# Add benefits

Convenience wrapper around
[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md)
that keeps only positive effects, that is, rows with `benefit > 0`.

This function is useful when the user wants to work only with beneficial
consequences of actions. Internally, it calls
[`add_effects()`](https://josesalgr.github.io/mosap/reference/add_effects.md)
with `component = "benefit"` and stores the resulting canonical effects
table in `x$data$dist_effects`.

For backwards compatibility, a mirror table containing only the benefit
component is also written to `x$data$dist_benefit`.

## Usage

``` r
add_benefits(
  x,
  benefits = NULL,
  effect_type = c("delta", "after"),
  effect_aggregation = c("sum", "mean")
)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/mosap/reference/input_data.md).
  It must already contain `x$data$dist_actions`; run
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  first.

- benefits:

  Alias of `effects`, kept for backwards compatibility.

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

An updated `Problem` object with:

- `x$data$dist_effects`:

  The canonical filtered effects table, containing only rows with
  `benefit > 0`.

- `x$data$dist_benefit`:

  A backwards-compatible table containing only the benefit component.

## See also

[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md),
[`add_losses`](https://josesalgr.github.io/mosap/reference/add_losses.md)

## Examples

``` r
pu <- data.frame(id = 1:2, cost = c(1, 2))
features <- data.frame(id = 1, name = "sp1")
dist_features <- data.frame(pu = 1:2, feature = 1, amount = c(5, 10))

p <- input_data(pu = pu, features = features, dist_features = dist_features)
p <- add_actions(p, data.frame(id = "restoration"))

eff <- data.frame(
  pu = c(1, 2),
  action = c("restoration", "restoration"),
  feature = c(1, 1),
  delta = c(2, -1)
)

p <- add_benefits(p, benefits = eff)
p$data$dist_benefit
#>   pu      action feature benefit internal_pu internal_action internal_feature
#> 1  1 restoration       1       2           1               1                1
#>   feature_name action_name
#> 1          sp1 restoration
```
