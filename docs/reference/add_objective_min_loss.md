# Add objective: minimize loss

Define an objective that minimizes the total negative effects generated
by selected actions on selected features.

This objective is based on the canonical effects table stored in
`x$data$dist_effects` and uses only the non-negative `loss` component.

## Usage

``` r
add_objective_min_loss(x, actions = NULL, features = NULL, alias = NULL)
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

Let \\\mathcal{E}\\ denote the set of rows in `x$data$dist_effects`. For
each row associated with planning unit \\i\\, action \\a\\, and feature
\\f\\, let \\\ell\_{iaf} \ge 0\\ denote the stored value in the `loss`
column.

If no subsets are supplied, the objective can be written as:

\$\$ \min \sum\_{(i,a,f) \in \mathcal{E}} \ell\_{iaf} \\ x\_{ia}. \$\$

If `actions` is provided, only rows whose action belongs to the selected
subset contribute to the objective.

If `features` is provided, only rows whose feature belongs to the
selected subset contribute to the objective.

More generally, letting \\\mathcal{E}^{\star}\\ be the subset induced by
the selected actions and features, the objective is:

\$\$ \min \sum\_{(i,a,f) \in \mathcal{E}^{\star}} \ell\_{iaf} \\
x\_{ia}. \$\$

This objective minimizes harmful effects only. It does not offset losses
against benefits unless benefits are handled elsewhere through
additional objectives or constraints.

## See also

[`add_objective_max_benefit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_benefit.md),
[`add_effects`](https://josesalgr.github.io/paretoscape/reference/add_effects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_objective_min_loss(p)

p <- add_objective_min_loss(
  p,
  actions = c("restoration", "harvest")
)

p <- add_objective_min_loss(
  p,
  features = c("sp1", "sp3")
)

p <- add_objective_min_loss(
  p,
  actions = "harvest",
  features = c("sp1", "sp2")
)
} # }
```
