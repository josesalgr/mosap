# Add conservation targets (relative to baseline)

Adds relative conservation targets as proportions in \\\[0,1\]\\ of the
baseline total representation of each feature in the study area.
Baseline totals are computed from the input data (via
`.pa_feature_totals()`).

## Usage

``` r
add_conservation_targets_relative(x, targets, overwrite = FALSE, label = NULL)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- targets:

  Target specification (proportions in \\\[0,1\]\\).

- overwrite:

  Logical. If `TRUE`, replace existing conservation targets for the same
  features.

- label:

  Optional character label stored with the targets.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.
