# Add recovery targets (absolute)

Adds absolute recovery targets per feature. Recovery targets represent
thresholds on action-driven improvements (deltas) rather than baseline
representation. The provided values are treated as absolute thresholds
in the same units as the effect/benefit amounts used by the model
builder.

## Usage

``` r
add_recovery_targets_absolute(x, targets, overwrite = FALSE, label = NULL)
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

  Logical. If `TRUE`, replace existing recovery targets for the same
  features.

- label:

  Optional character label stored with the targets.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.
