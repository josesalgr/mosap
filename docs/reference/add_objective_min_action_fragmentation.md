# Add objective: minimize action fragmentation

Define an objective that minimizes fragmentation at the action level
over a stored spatial relation.

Unlike
[`add_objective_min_fragmentation`](https://josesalgr.github.io/multiscape/reference/add_objective_min_fragmentation.md),
which acts on the selected planning-unit set through \\w_i\\, this
objective acts on the spatial arrangement of individual action decisions
through the action variables \\x\_{ia}\\.

## Usage

``` r
add_objective_min_action_fragmentation(
  x,
  relation_name = "boundary",
  weight_multiplier = 1,
  action_weights = NULL,
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- relation_name:

  Character string giving the name of the spatial relation to use. The
  relation must already exist in `x$data$spatial_relations`.

- weight_multiplier:

  Numeric scalar greater than or equal to zero. Global multiplier
  applied to the relation weights when the objective is built.

- action_weights:

  Optional action weights. Either a named numeric vector with names
  equal to action ids, or a `data.frame` with columns `action` and
  `weight`. These weights scale the contribution of each action to the
  final objective.

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

Use this function when spatial cohesion should be encouraged separately
for each selected action pattern.

Let \\\mathcal{I}\\ denote the set of planning units and let
\\\mathcal{A}\\ denote the set of actions.

Let \\x\_{ia} \in \\0,1\\\\ indicate whether action \\a \in
\mathcal{A}\\ is selected in planning unit \\i \in \mathcal{I}\\.

Let the chosen spatial relation define weighted pairs with weights
\\\omega\_{ij} \ge 0\\, and let \\\lambda = \code{weight_multiplier}\\
be the global scaling factor applied to these weights.

If `actions` is supplied, only the selected subset \\\mathcal{A}^{\star}
\subseteq \mathcal{A}\\ contributes to the final objective. If
`actions = NULL`, all actions are included.

The internal preparation step constructs one auxiliary variable
\\b\_{ija} \in \[0,1\]\\ for each unique non-diagonal undirected edge
\\(i,j)\\ with \\i \< j\\ and for each action \\a\\. The intended
semantics is: \$\$ b\_{ija} = x\_{ia} \land x\_{ja}. \$\$

Whenever both decision variables \\x\_{ia}\\ and \\x\_{ja}\\ exist in
the model, this conjunction is enforced by the linearization: \$\$
b\_{ija} \le x\_{ia}, \$\$ \$\$ b\_{ija} \le x\_{ja}, \$\$ \$\$ b\_{ija}
\ge x\_{ia} + x\_{ja} - 1. \$\$

If one of the two action variables does not exist because the
corresponding `(pu, action)` pair is not feasible, the auxiliary
variable is forced to zero.

Therefore, \\b\_{ija}=1\\ if and only if action \\a\\ is selected in
both adjacent planning units \\i\\ and \\j\\; otherwise \\b\_{ija}=0\\.

The exact objective coefficients are assembled later by the model
builder from:

- the action decision variables \\x\_{ia}\\,

- the edge-conjunction variables \\b\_{ija}\\,

- the relation weights \\\omega\_{ij}\\,

- the multiplier \\\lambda\\,

- and, if supplied, the action-specific weights.

If action-specific weights are provided, let \\\alpha_a \ge 0\\ denote
the weight associated with action \\a\\. Then the resulting objective
can be interpreted as an action-wise compactness or fragmentation
functional of the form: \$\$ \min \sum\_{a \in \mathcal{A}^{\star}}
\alpha_a \\ F_a(x\_{\cdot a}, b\_{\cdot\cdot a}; \lambda \omega), \$\$
where \\F_a\\ is the fragmentation expression induced by the selected
relation and the internal coefficient construction for action \\a\\.

In practical terms, this objective penalizes solutions in which the same
action is spatially scattered or broken into separate patches, while
allowing different actions to form different spatial patterns.

This differs from planning-unit fragmentation:

- [`add_objective_min_fragmentation()`](https://josesalgr.github.io/multiscape/reference/add_objective_min_fragmentation.md)
  encourages cohesion of the union of selected planning units,

- `add_objective_min_action_fragmentation()` encourages cohesion of each
  selected action pattern separately.

Setting `weight_multiplier = 0` removes the contribution of the spatial
relation from the objective after scaling.

## See also

[`add_objective_min_fragmentation`](https://josesalgr.github.io/multiscape/reference/add_objective_min_fragmentation.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md),
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md)
