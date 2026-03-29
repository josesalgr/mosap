# Add objective: minimize cost

Define an objective that minimizes the total cost of the solution.

Depending on the function arguments, the objective may include
planning-unit costs, action costs, or both. Action costs can optionally
be restricted to a subset of actions.

## Usage

``` r
add_objective_min_cost(
  x,
  include_pu_cost = TRUE,
  include_action_cost = TRUE,
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- include_pu_cost:

  Logical. If `TRUE`, include planning-unit costs in the objective.

- include_action_cost:

  Logical. If `TRUE`, include action costs in the objective.

- actions:

  Optional subset of actions to include in the action-cost component.
  Values may match `x$data$actions$id` and, if present,
  `x$data$actions$action_set`. If `NULL`, all feasible actions are
  included in the action-cost term.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Let \\\mathcal{P}\\ be the set of planning units and let
\\\mathcal{A}\_i\\ be the set of feasible actions in planning unit \\i
\in \mathcal{P}\\.

Let:

- \\w_i \in \\0,1\\\\ denote whether planning unit \\i\\ is selected,

- \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
  planning unit \\i\\,

- \\c_i^{PU} \ge 0\\ denote the planning-unit cost of unit \\i\\,

- \\c\_{ia}^{A} \ge 0\\ denote the cost of selecting action \\a\\ in
  planning unit \\i\\.

The most general form of this objective is:

\$\$ \min \left( \sum\_{i \in \mathcal{P}} c_i^{PU} w_i + \sum\_{i \in
\mathcal{P}} \sum\_{a \in \mathcal{A}\_i^\star} c\_{ia}^{A} x\_{ia}
\right), \$\$

where \\\mathcal{A}\_i^\star\\ denotes the subset of feasible actions
that contribute to the action-cost term.

If `include_pu_cost = FALSE`, the planning-unit cost term is omitted.

If `include_action_cost = FALSE`, the action-cost term is omitted.

If `actions = NULL`, all feasible actions contribute to the action-cost
term. If `actions` is supplied, only the selected subset contributes to
that term. Planning-unit costs are never subset by `actions`; they are
always global whenever `include_pu_cost = TRUE`.

This objective is useful when the decision problem is framed primarily
as a cost-minimization problem, optionally combining fixed planning-unit
costs with action-specific implementation costs.

## See also

[`add_objective_max_profit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_profit.md),
[`add_objective_max_net_profit`](https://josesalgr.github.io/paretoscape/reference/add_objective_max_net_profit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimize both planning-unit and action costs
p <- add_objective_min_cost(p)

# Minimize only action costs
p <- add_objective_min_cost(
  p,
  include_pu_cost = FALSE,
  include_action_cost = TRUE
)

# Minimize costs considering only a subset of actions
p <- add_objective_min_cost(
  p,
  actions = c("restoration", "conservation")
)
} # }
```
