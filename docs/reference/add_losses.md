# Add losses (negative effects magnitude)

Convenience wrapper around
[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md)
that keeps only rows with `loss > 0` (i.e., the magnitude of negative
changes). Also writes `x$data$dist_loss` containing only the loss
component.

## Usage

``` r
add_losses(
  x,
  losses = NULL,
  ...,
  effect_type = c("delta", "after"),
  effect_aggregation = c("sum", "mean"),
  align_rasters = TRUE,
  keep_zero = FALSE,
  drop_locked_out = TRUE,
  na_to_zero = TRUE
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
  Must contain `x$data$dist_actions` (run
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  first).

- effect_type:

  Character. How to interpret provided values for explicit tables or
  raster lists:

  - `"delta"`: values represent signed deltas (default),

  - `"after"`: values represent after-action amounts (converted to
    deltas using baseline).

- effect_aggregation:

  Character. Aggregation used to compute PU-level values from rasters.
  One of `"sum"` or `"mean"`.

- align_rasters:

  Logical. If `TRUE`, attempt to align effect rasters to the PU raster
  grid before zonal operations (default `TRUE`).

- keep_zero:

  Logical. If `TRUE`, keep rows where `benefit == 0` and `loss == 0`.
  Default `FALSE`.

- drop_locked_out:

  Logical. If `TRUE`, drop rows for `(pu, action)` pairs with
  `status == 3` in `x$data$dist_actions` (if the column exists). Default
  `TRUE`.

- na_to_zero:

  Logical. If `TRUE`, treat missing values as 0 when computing
  benefit/loss. Default `TRUE`.

## Value

The updated `Data` object.
