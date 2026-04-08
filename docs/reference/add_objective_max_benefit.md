# Add objective: maximize benefit

Define an objective that maximizes the total positive effects generated
by selected actions on selected features.

This objective is based on the canonical effects table and uses only the
non-negative `benefit` component.

## Usage

``` r
add_objective_max_benefit(x, actions = NULL, features = NULL, alias = NULL)
```

## Arguments

- x:

  A `Problem` object.

- actions:

  Optional subset of actions to include in the objective. Values may
  match `x$data$actions$id` and, if present,
  `x$data$actions$action_set`. If `NULL`, all actions are included.

- features:

  Optional subset of features to include in the objective. Values may
  match `x$data$features$id` and, if present, `x$data$features$name`. If
  `NULL`, all features are included.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Use this function when positive ecological gains should be maximized
explicitly, without offsetting them against harmful effects.

Let \\b\_{iaf} \ge 0\\ denote the stored benefit associated with
planning unit \\i\\, action \\a\\, and feature \\f\\. Since the effects
table is already expressed in canonical form, \\b\_{iaf}\\ represents
the positive part of the net effect associated with the corresponding
selected action decision.

If no subsets are supplied, the objective can be written as:

\$\$ \max \sum\_{(i,a,f) \in \mathcal{R}} b\_{iaf} \\ x\_{ia}, \$\$

where \\\mathcal{R}\\ denotes the set of stored benefit rows and
\\x\_{ia} \in \\0,1\\\\ indicates whether action \\a\\ is selected in
planning unit \\i\\.

If `actions` is provided, only rows whose action belongs to the selected
subset contribute to the objective.

If `features` is provided, only rows whose feature belongs to the
selected subset contribute to the objective.

More generally, letting \\\mathcal{R}^{\star}\\ be the subset induced by
the selected actions and features, the objective is:

\$\$ \max \sum\_{(i,a,f) \in \mathcal{R}^{\star}} b\_{iaf} \\ x\_{ia}.
\$\$

This objective maximizes gains only. It does not subtract losses. If
harmful effects should also be accounted for, they must be handled
separately through additional objectives or constraints.

## See also

[`add_objective_min_loss`](https://josesalgr.github.io/multiscape/reference/add_objective_min_loss.md),
[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_objective_max_benefit(p)

p <- add_objective_max_benefit(
  p,
  actions = c("restoration", "conservation")
)

p <- add_objective_max_benefit(
  p,
  features = c("sp1", "sp3")
)

p <- add_objective_max_benefit(
  p,
  actions = "restoration",
  features = c("sp1", "sp2")
)
} # }
```
