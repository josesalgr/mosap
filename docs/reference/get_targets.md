# Get target achievement summary from a solution

Extract a user-facing target-achievement table from a `Solution` or
`SolutionSet` object returned by
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

The returned table summarizes, for each stored target, the target level,
the achieved value in the solution, the gap between achieved and
required values, and whether the target was met.

## Usage

``` r
get_targets(x, run = NULL)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md).

- run:

  Optional positive integer giving the run index to extract from a
  `SolutionSet`. If `NULL`, all runs are returned when available.

## Value

A simplified `data.frame` target summary, or `NULL` if the solution does
not contain targets. Typical columns include `feature`, `feature_name`,
`target_level`, `total_available`, `target`, `achieved`, `gap`, and
`met`.

## Details

Targets are optional in `paretoscape`. If the solution object does not
contain a targets summary table at `x$summary$targets`, this function
returns `NULL` without error.

This function reads the stored targets summary and returns a simplified
user-facing table. If the summary contains `achieved` and
`target_value`, target satisfaction is evaluated as follows.

For lower-bound targets: \$\$ \mathrm{met} = (\mathrm{achieved} \ge
\mathrm{target}), \$\$

and for upper-bound targets: \$\$ \mathrm{met} = (\mathrm{achieved} \le
\mathrm{target}). \$\$

The interpretation of the target direction is taken from the `sense`
column when available:

- `"ge"`, `">="`, or `"min"` are treated as lower-bound targets,

- `"le"`, `"<="`, or `"max"` are treated as upper-bound targets,

- if `sense` is missing, the target is treated as a lower bound by
  default.

The returned table is simplified and renames some internal fields for
readability:

- `target_raw` is returned as `target_level`,

- `basis_total` is returned as `total_available`,

- `target_value` is returned as `target`.

If `x` is a `SolutionSet` and `run` is provided, only rows belonging to
that run are returned. If the result contains a `run_id` column but only
a single run is present and `run` was not requested explicitly, the
`run_id` column is removed for convenience.

The `gap` column is expected to be part of the stored summary. When
present, it typically represents: \$\$ \mathrm{gap} =
\mathrm{achieved} - \mathrm{target}. \$\$

## See also

[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/paretoscape/reference/get_features.md)
