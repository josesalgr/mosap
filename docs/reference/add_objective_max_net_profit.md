# Add objective: maximize net profit

Define an objective that maximizes net profit by combining profits with
optional planning-unit and action-cost penalties.

## Usage

``` r
add_objective_max_net_profit(
  x,
  profit_col = "profit",
  include_pu_cost = TRUE,
  include_action_cost = TRUE,
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- profit_col:

  Character string giving the profit column in `x$data$dist_profit`.

- include_pu_cost:

  Logical. If `TRUE`, subtract planning-unit costs.

- include_action_cost:

  Logical. If `TRUE`, subtract action costs.

- actions:

  Optional subset of actions to include in the profit and action-cost
  terms. Values may match `x$data$actions$id` and, if present,
  `x$data$actions$action_set`.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Let:

- \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
  planning unit \\i\\,

- \\w_i \in \\0,1\\\\ denote whether planning unit \\i\\ is selected,

- \\\pi\_{ia}\\ denote the profit associated with decision \\(i,a)\\,

- \\c_i^{PU} \ge 0\\ denote the planning-unit cost,

- \\c\_{ia}^{A} \ge 0\\ denote the action cost.

In its most general form, the objective is:

\$\$ \max \left( \sum\_{(i,a) \in \mathcal{F}^{\star}} \pi\_{ia}
x\_{ia} - \sum\_{i \in \mathcal{P}} c_i^{PU} w_i - \sum\_{(i,a) \in
\mathcal{F}^{\star}} c\_{ia}^{A} x\_{ia} \right), \$\$

where \\\mathcal{F}^{\star}\\ denotes the subset of feasible planning
unit–action pairs included in the objective.

If `actions = NULL`, all feasible actions contribute to both the profit
term and the action-cost term.

If `actions` is provided, the profit term and the action-cost term are
restricted to that subset. The planning-unit cost term, if included,
remains global.

If `include_pu_cost = FALSE`, the planning-unit cost term is omitted.

If `include_action_cost = FALSE`, the action-cost term is omitted.

This objective is appropriate when decisions generate revenues or
returns and the analyst wishes to optimize the resulting net balance
after accounting for selected cost components.

## See also

[`add_objective_max_profit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_profit.md),
[`add_objective_min_cost`](https://josesalgr.github.io/paretoscape/reference/add_objective_min_cost.md)
