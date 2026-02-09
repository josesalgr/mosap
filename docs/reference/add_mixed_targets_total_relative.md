# Add mixed total targets (relative to baseline)

Adds relative mixed-total targets as proportions in \\\[0,1\]\\ of
baseline totals per feature. Baseline totals are computed via
`.pa_feature_totals()` and stored in `basis_total`.

## Usage

``` r
add_mixed_targets_total_relative(x, targets, overwrite = FALSE, label = NULL)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- targets:

  Target specification (proportions in \\\[0,1\]\\).

- overwrite:

  Logical. If `TRUE`, replace existing mixed-total targets for the same
  features (subject to mutual exclusivity rules enforced by
  `.pa_store_targets()`).

- label:

  Optional character label stored with the targets.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.
