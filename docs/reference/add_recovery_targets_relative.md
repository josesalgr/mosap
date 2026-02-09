# Add recovery targets (relative)

Adds relative recovery targets as proportions in \\\[0,1\]\\ of a
per-feature basis. By default, the basis is the *potential* maximum
improvement per feature derived from available actions (via
`.pa_feature_potential()`). Alternatively, `relative_basis="baseline"`
uses baseline totals (via `.pa_feature_totals()`).

## Usage

``` r
add_recovery_targets_relative(
  x,
  targets,
  relative_basis = c("potential", "baseline"),
  overwrite = FALSE,
  label = NULL
)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- targets:

  Target specification (proportions in \\\[0,1\]\\).

- relative_basis:

  Character. Basis used to convert relative targets to absolute
  thresholds: `"potential"` (default) or `"baseline"`.

- overwrite:

  Logical. If `TRUE`, replace existing recovery targets for the same
  features.

- label:

  Optional character label stored with the targets.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.
