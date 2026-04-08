# Add objective: minimize fragmentation

Define an objective that minimizes planning-unit fragmentation over a
stored spatial relation.

This objective acts on the planning-unit selection pattern through the
binary planning-unit variables \\w_i\\. It is therefore appropriate when
spatial cohesion is to be encouraged at the level of the selected
planning-unit set as a whole.

## Usage

``` r
add_objective_min_fragmentation(
  x,
  relation_name = "boundary",
  weight_multiplier = 1,
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

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Use this function when spatial cohesion should be encouraged at the
level of the selected planning-unit set as a whole.

Let \\\mathcal{I}\\ denote the set of planning units and let \\w_i \in
\\0,1\\\\ indicate whether planning unit \\i \in \mathcal{I}\\ is
selected.

Let the chosen spatial relation define a set of weighted pairs with
weights \\\omega\_{ij} \ge 0\\. These relation weights are interpreted
by the model builder after scaling by \\\lambda =
\code{weight_multiplier}\\.

The internal preparation step constructs one auxiliary variable
\\y\_{ij} \in \[0,1\]\\ for each unique non-diagonal undirected edge
\\(i,j)\\ with \\i \< j\\. The intended semantics is: \$\$ y\_{ij} = w_i
\land w_j. \$\$

This is enforced by the standard linearization: \$\$ y\_{ij} \le w_i,
\$\$ \$\$ y\_{ij} \le w_j, \$\$ \$\$ y\_{ij} \ge w_i + w_j - 1. \$\$

Thus, \\y\_{ij}=1\\ if and only if both planning units \\i\\ and \\j\\
are selected, and \\y\_{ij}=0\\ otherwise.

The exact objective coefficients are assembled later by the model
builder from:

- the planning-unit variables \\w_i\\,

- the edge-conjunction variables \\y\_{ij}\\,

- the stored relation weights \\\omega\_{ij}\\,

- and the multiplier \\\lambda\\.

Conceptually, the resulting objective is a boundary- or relation-based
compactness functional that penalizes exposed or weakly connected
selected patterns while rewarding adjacency among selected planning
units.

In the common case where `relation_name = "boundary"` and the relation
was built with
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md),
the objective corresponds to a boundary-length-style fragmentation
penalty.

Setting `weight_multiplier = 0` removes the contribution of the spatial
relation from the objective after scaling.

This objective does not distinguish between different actions within the
same planning unit. If action-specific spatial cohesion is required, use
[`add_objective_min_action_fragmentation`](https://josesalgr.github.io/multiscape/reference/add_objective_min_action_fragmentation.md)
instead.

## See also

[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md),
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md),
[`add_objective_min_action_fragmentation`](https://josesalgr.github.io/multiscape/reference/add_objective_min_action_fragmentation.md)
