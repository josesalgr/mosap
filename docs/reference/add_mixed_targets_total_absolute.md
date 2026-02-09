# Add mixed total targets (absolute)

Adds absolute mixed-total targets per feature. A mixed-total target
enforces that baseline representation plus action-induced deltas jointly
reach a single threshold: \$\$\sum_i z\_{is} r\_{is} + \sum\_{i,a}
x\_{ia}\Delta\_{ias} \ge T^{mix}\_s.\$\$

## Usage

``` r
add_mixed_targets_total_absolute(x, targets, overwrite = FALSE, label = NULL)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- targets:

  Target specification (absolute). See **Targets format** in
  [`targets`](https://josesalgr.github.io/mosap/reference/targets.md)
  details.

- overwrite:

  Logical. If `TRUE`, replace existing mixed-total targets for the same
  features (but still errors if conservation/recovery targets exist for
  those features).

- label:

  Optional character label stored with the targets.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.

## Details

Mixed-total targets are conceptually different from specifying separate
conservation and recovery targets for the same feature. As such,
mixed-total targets are intended to be mutually exclusive with
conservation/recovery targets for the same feature. Conflict checks are
handled by `.pa_store_targets()`.
