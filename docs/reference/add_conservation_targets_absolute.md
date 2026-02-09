# Add conservation targets (absolute)

Adds absolute conservation targets per feature. The provided values are
treated as absolute thresholds in the same units as the feature amounts
stored in `x$data$dist_features$amount`.

## Usage

``` r
add_conservation_targets_absolute(x, targets, overwrite = FALSE, label = NULL)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- targets:

  Target specification. See **Targets format** in
  [`targets`](https://josesalgr.github.io/mosap/reference/targets.md)
  details.

- overwrite:

  Logical. If `TRUE`, replace existing conservation targets for the same
  features.

- label:

  Optional character label stored with the targets (useful for
  reporting).

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.
