# Add objective: maximize profit

Define an objective that maximizes total profit from selected planning
unit–action decisions.

## Usage

``` r
add_objective_max_profit(
  x,
  profit_col = "profit",
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- profit_col:

  Character string giving the profit column in `x$data$dist_profit`.

- actions:

  Optional subset of actions to include. Values may match
  `x$data$actions$id` and, if present, `x$data$actions$action_set`. If
  `NULL`, all actions are included.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Let \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
planning unit \\i\\, and let \\\pi\_{ia}\\ denote the profit associated
with that decision, as taken from column `profit_col` in
`x$data$dist_profit`.

If all actions are included, the objective is:

\$\$ \max \sum\_{(i,a) \in \mathcal{F}} \pi\_{ia} x\_{ia}, \$\$

where \\\mathcal{F}\\ denotes the set of feasible planning unit–action
pairs.

If `actions` is provided, only the selected subset contributes to the
objective. Letting \\\mathcal{F}^{\star}\\ denote the feasible pairs
whose action belongs to the selected subset, the objective becomes:

\$\$ \max \sum\_{(i,a) \in \mathcal{F}^{\star}} \pi\_{ia} x\_{ia}. \$\$

This objective considers profit only. It does not subtract planning-unit
costs or action costs. For a net-profit formulation, use
[`add_objective_max_net_profit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_net_profit.md).

## See also

[`add_objective_min_cost`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_cost.md),
[`add_objective_max_net_profit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_net_profit.md)
