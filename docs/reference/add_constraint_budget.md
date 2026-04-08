# Add budget constraint

Add a budget constraint to a planning problem.

This function stores one budget-constraint specification in the
`Problem` object so that it can later be incorporated when the
optimization model is assembled. Multiple budget constraints can be
added by calling this function repeatedly, provided that no duplicated
combination of action subset and constraint sense is introduced.

## Usage

``` r
add_constraint_budget(
  x,
  budget,
  sense,
  tolerance = 0,
  actions = NULL,
  include_pu_cost = TRUE,
  include_action_cost = TRUE,
  name = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- budget:

  Numeric scalar greater than or equal to zero. Target value for the
  constrained budget.

- sense:

  Character string indicating the type of budget constraint. Must be one
  of `"min"`, `"max"`, or `"equal"`.

- tolerance:

  Numeric scalar greater than or equal to zero. Only used when
  `sense = "equal"`. In that case, equality is interpreted as a band
  around `budget` with half-width `tolerance`. Ignored otherwise.

- actions:

  Optional subset of actions to which the constraint applies. If `NULL`,
  the constraint applies to the whole problem. Otherwise, it applies
  only to the selected decision variables associated with the specified
  subset of actions. This argument is resolved using the package's
  standard action subset parser.

- include_pu_cost:

  Logical scalar indicating whether planning-unit costs should be
  included in the constrained budget. This is only supported when
  `actions = NULL`.

- include_action_cost:

  Logical scalar indicating whether action costs should be included in
  the constrained budget.

- name:

  Optional character string used as the label of the stored linear
  constraint when it is later added to the optimization model. If
  `NULL`, a default name is generated.

## Value

An updated `Problem` object with the new budget-constraint specification
appended to the stored budget-constraint table.

## Details

Use this function when spending limits or minimum spending requirements
must be imposed either on the full problem or on the subset of selected
decisions associated with specific actions.

Let \\\mathcal{I}\\ denote the set of planning units and let
\\\mathcal{A}\\ denote the set of actions. Let \\w_i \in \\0,1\\\\
denote the binary variable indicating whether planning unit \\i \in
\mathcal{I}\\ is selected by at least one decision in the model, and let
\\x\_{ia} \in \\0,1\\\\ denote the binary variable indicating whether
action \\a \in \mathcal{A}\\ is selected in planning unit \\i \in
\mathcal{I}\\.

The total constrained budget can include two cost components:

- planning-unit costs, associated with \\w_i\\;

- action costs, associated with \\x\_{ia}\\.

The arguments `include_pu_cost` and `include_action_cost` determine
which of these components are included in the stored budget constraint.

When `actions = NULL`, the constraint refers to the total budget across
the whole problem. In that case, depending on the values of
`include_pu_cost` and `include_action_cost`, the constrained quantity is
one of the following:

If only planning-unit costs are included: \$\$ \sum\_{i \in \mathcal{I}}
c_i^{pu} w_i \$\$

If only action costs are included: \$\$ \sum\_{i \in \mathcal{I}}
\sum\_{a \in \mathcal{A}} c\_{ia}^{act} x\_{ia} \$\$

If both components are included: \$\$ \sum\_{i \in \mathcal{I}} c_i^{pu}
w_i + \sum\_{i \in \mathcal{I}} \sum\_{a \in \mathcal{A}} c\_{ia}^{act}
x\_{ia} \$\$

Depending on `sense`, this function stores one of the following
constraints:

If `sense = "min"`: \$\$ C \ge B \$\$

If `sense = "max"`: \$\$ C \le B \$\$

If `sense = "equal"` and `tolerance = 0`: \$\$ C = B \$\$

If `sense = "equal"` and `tolerance > 0`, the equality is stored as a
two-sided band: \$\$ B - \tau \le C \le B + \tau \$\$

where \\C\\ denotes the selected cost expression and \\\tau\\ is the
value supplied through `tolerance`.

When `actions` is not `NULL`, only action costs can be included. In that
case, let \\\mathcal{A}^\star \subseteq \mathcal{A}\\ denote the
selected subset of actions, and the constrained quantity is \$\$
\sum\_{i \in \mathcal{I}} \sum\_{a \in \mathcal{A}^\star} c\_{ia}^{act}
x\_{ia}. \$\$

Action-specific budget constraints only support action costs. Therefore,
`include_pu_cost = TRUE` is only allowed when `actions = NULL`, because
planning-unit costs are not action-specific.

This function only stores the constraint specification; it does not
validate the feasibility of the threshold against the available cost
data at this stage.

Multiple budget constraints can be stored in a `Problem` object.
However, at most one can be stored for the same combination of action
subset and constraint sense. Attempting to add a duplicated
`actions`–`sense` combination results in an error.

## See also

[`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)

## Examples

``` r
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4)
)

features <- data.frame(
  id = 1:2,
  name = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 4),
  feature = c(1, 2, 1, 2, 1, 2),
  amount = c(1, 2, 1, 3, 2, 1)
)

actions <- data.frame(
  id = c("conservation", "restoration")
)

p <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
)

p <- add_actions(
  p,
  actions = actions,
  cost = c(conservation = 1, restoration = 2)
)

p <- add_constraint_budget(
  x = p,
  budget = 10,
  sense = "max",
  include_pu_cost = TRUE,
  include_action_cost = TRUE
)

p <- add_constraint_budget(
  x = p,
  budget = 4,
  sense = "max",
  actions = "restoration",
  include_pu_cost = FALSE,
  include_action_cost = TRUE
)

p <- add_constraint_budget(
  x = p,
  budget = 1,
  sense = "min",
  actions = "restoration",
  include_pu_cost = FALSE,
  include_action_cost = TRUE
)

p$data$constraints$budget
#>     type sense value tolerance     actions include_pu_cost include_action_cost
#> 1 budget   max    10         0        <NA>            TRUE                TRUE
#> 2 budget   max     4         0 restoration           FALSE                TRUE
#> 3 budget   min     1         0 restoration           FALSE                TRUE
#>                            name
#> 1              budget_total_max
#> 2 budget_action_max_restoration
#> 3 budget_action_min_restoration
```
