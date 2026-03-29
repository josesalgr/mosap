# Get feature summary from a solution

Extract the per-feature summary table from a `Solution` or `SolutionSet`
object returned by
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

The returned table summarizes, for each feature, the total amount
available in the landscape together with the positive and negative
contributions induced by the selected actions in the solution.

## Usage

``` r
get_features(x, run = NULL)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

- run:

  Optional positive integer giving the run index to extract from a
  `SolutionSet`. If `NULL`, all runs are returned when available.

## Value

A `data.frame` with one row per feature, or one row per feature–run
combination when multiple runs are present. The returned table always
includes the columns `total_available`, `benefit`, `loss`, `net`, and
`total`.

## Details

This function reads the feature summary stored in `x$summary$features`.
It errors if that table is missing.

Let \\B_f\\ denote the total baseline amount available in the landscape
for feature \\f\\. Let \\G_f\\ denote the total positive contribution
induced by selected actions, and let \\L_f\\ denote the total negative
contribution. Then the returned table is intended to summarize
quantities of the form:

\$\$ \mathrm{net}\_f = G_f - L_f, \$\$

\$\$ \mathrm{total}\_f = B_f + \mathrm{net}\_f. \$\$

In the stored summary, these quantities are typically represented by the
columns:

- `total_available`, corresponding to \\B_f\\,

- `benefit`, corresponding to \\G_f\\,

- `loss`, corresponding to \\L_f\\,

- `net`, corresponding to \\G_f - L_f\\,

- `total`, corresponding to \\B_f + G_f - L_f\\.

If any of the core numeric columns `total_available`, `benefit`, or
`loss` are missing, they are created and filled with zero. If `net` is
missing, it is computed as `benefit - loss`. If `total` is missing, it
is computed as `total_available + net`.

Thus, this function guarantees that the returned table contains the
columns `total_available`, `benefit`, `loss`, `net`, and `total`, even
if some of them were absent from the stored summary.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. If the result contains a `run_id` column but only
a single run is present and `run` was not requested explicitly, the
`run_id` column is removed for convenience.

This function summarizes feature outcomes in the solution. It is
different from
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md),
which focuses on target achievement rather than total feature balance.

## See also

[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_targets`](https://josesalgr.github.io/paretoscape/reference/get_targets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sol <- solve(problem)
feat_tbl <- get_features(sol)
} # }
```
