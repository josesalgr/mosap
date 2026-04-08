# Add relative targets

Add feature-level relative targets to a planning problem.

These targets are stored in the problem object and later translated into
linear constraints when the optimization model is built. Relative
targets are supplied as proportions in \\\[0,1\]\\ and are converted
internally into absolute thresholds using the current total amount of
each feature in the landscape.

Each call appends one or more target definitions to the problem. This
makes it possible to combine multiple target rules, including targets
associated with different action subsets.

## Usage

``` r
add_constraint_targets_relative(
  x,
  targets,
  features = NULL,
  actions = NULL,
  label = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- targets:

  Target specification as proportions in \\\[0,1\]\\. It may be a
  scalar, vector, named vector, or `data.frame`. See Details.

- features:

  Optional feature specification indicating which features the supplied
  target values refer to when `targets` does not identify features
  explicitly. If `NULL`, all features are targeted.

- actions:

  Optional character vector indicating which actions count toward target
  achievement. Entries may match action ids, `action_set` labels, or
  both. If `NULL`, all actions count.

- label:

  Optional character string stored with the targets for reporting and
  bookkeeping.

## Value

An updated `Problem` object with relative targets appended to the stored
target table.

## Details

Use this function when target requirements are naturally expressed as
proportions of current baseline feature totals rather than in original
feature units.

Let \\\mathcal{F}\\ denote the set of features. For each targeted
feature \\f \in \mathcal{F}\\, let \\B_f\\ denote the current baseline
total amount of that feature in the landscape, as computed by
`.pa_feature_totals()`.

If the user supplies a relative target \\r_f \in \[0,1\]\\, then this
function converts it to an absolute threshold: \$\$ T_f = r_f \times
B_f. \$\$

The absolute threshold \\T_f\\ is stored in `target_value`, while:

- the original user-supplied proportion \\r_f\\ is stored in
  `target_raw`,

- the baseline total \\B_f\\ is stored in `basis_total`.

When the optimization model is built, the resulting target is
interpreted as: \$\$ \sum\_{(i,a) \in \mathcal{D}\_f^{\star}} c\_{iaf}
x\_{ia} \ge T_f, \$\$ where:

- \\i \in \mathcal{I}\\ indexes planning units,

- \\a \in \mathcal{A}\\ indexes actions,

- \\x\_{ia}\\ indicates whether action \\a\\ is selected in planning
  unit \\i\\,

- \\c\_{iaf}\\ is the contribution of that action to feature \\f\\,

- \\\mathcal{D}\_f^{\star}\\ is the subset of planning unit–action pairs
  allowed to count toward the target for feature \\f\\.

The `actions` argument restricts which actions may contribute toward
target achievement, but it does not affect the baseline amount \\B_f\\
used to compute the threshold. In other words, relative targets are
always scaled against the current full landscape baseline.

Therefore, `actions` changes who may satisfy the target, but not how the
threshold itself is scaled.

The `targets` argument is parsed by `.pa_parse_targets()` and may be
supplied in several equivalent forms, including:

- a single numeric value recycled to all selected features,

- a numeric vector aligned to `features`,

- a named numeric vector where names identify features,

- a `data.frame` with `feature` and `target` columns.

If `targets` does not explicitly identify features:

- if `features = NULL`, the target is applied to all features,

- if `features` is supplied, the target values are interpreted with
  respect to that feature set.

Relative targets must lie in \\\[0,1\]\\.

Repeated calls append new target rules rather than replacing previous
ones. This allows cumulative target modelling, including multiple rules
on the same feature with different contributing action subsets.

## See also

[`add_constraint_targets_absolute`](https://josesalgr.github.io/multiscape/reference/add_constraint_targets_absolute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Require 30% of the current baseline total for all features
p <- add_constraint_targets_relative(p, 0.3)

# Require 20% of baseline totals for selected features only
p <- add_constraint_targets_relative(
  p,
  0.2,
  features = c("sp1", "sp2")
)

# Require 20% of baseline totals, counting only recovery actions
p <- add_constraint_targets_relative(
  p,
  0.2,
  actions = "recovery"
)

# Combine multiple target rules
p <- p |>
  add_constraint_targets_relative(0.1, actions = "conservation") |>
  add_constraint_targets_absolute(100, actions = "restoration")
} # }
```
