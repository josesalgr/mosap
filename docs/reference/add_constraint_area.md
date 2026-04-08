# Add area constraint

Add an area constraint to a planning problem.

This function stores one area-constraint specification in the `Problem`
object so that it can later be incorporated when the optimization model
is assembled. Multiple area constraints can be added by calling this
function repeatedly, provided that no duplicated combination of action
subset and constraint sense is introduced.

## Usage

``` r
add_constraint_area(
  x,
  area,
  sense,
  tolerance = 0,
  area_col = NULL,
  area_unit = c("m2", "ha", "km2"),
  actions = NULL,
  name = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- area:

  Numeric scalar greater than or equal to zero. Target value for the
  constrained area.

- sense:

  Character string indicating the type of area constraint. Must be one
  of `"min"`, `"max"`, or `"equal"`.

- tolerance:

  Numeric scalar greater than or equal to zero. Only used when
  `sense = "equal"`. In that case, equality is interpreted as a band
  around `area` with half-width `tolerance`. Ignored otherwise.

- area_col:

  Optional character string giving the name of the area column in
  `x$data$pu`. If `NULL`, the area source is resolved later by the model
  builder.

- area_unit:

  Character string indicating the unit of `area` and `tolerance`. Must
  be one of `"m2"`, `"ha"`, or `"km2"`.

- actions:

  Optional subset of actions to which the constraint applies. If `NULL`,
  the constraint applies to the total selected area in the problem
  through the planning-unit selection variables. Otherwise, it applies
  to the selected decision variables associated with the specified
  subset of actions. This argument is resolved using the package's
  standard action subset parser.

- name:

  Optional character string used as the label of the stored linear
  constraint when it is later added to the optimization model. If
  `NULL`, a default name is generated.

## Value

An updated `Problem` object with the new area-constraint specification
appended to `x$data$constraints$area`.

## Details

Use this function when area requirements must be imposed either on the
total selected landscape or on the subset of selected decisions
associated with specific actions.

Let \\\mathcal{I}\\ denote the set of planning units and let \\a_i \ge
0\\ be the area associated with planning unit \\i \in \mathcal{I}\\

When `actions = NULL`, the constraint refers to the total selected area
in the problem. In that case, let \\w_i \in \\0,1\\\\ denote the binary
variable indicating whether planning unit \\i\\ is selected by at least
one decision in the model.

Depending on `sense`, this function stores one of the following
constraints:

If `sense = "min"`: \$\$ \sum\_{i \in \mathcal{I}} a_i w_i \ge A \$\$

If `sense = "max"`: \$\$ \sum\_{i \in \mathcal{I}} a_i w_i \le A \$\$

If `sense = "equal"` and `tolerance = 0`: \$\$ \sum\_{i \in \mathcal{I}}
a_i w_i = A \$\$

If `sense = "equal"` and `tolerance > 0`, the equality is stored as a
two-sided band: \$\$ A - \tau \le \sum\_{i \in \mathcal{I}} a_i w_i \le
A + \tau \$\$ where \\\tau\\ is the value supplied through `tolerance`.

When `actions` is not `NULL`, the constraint is applied only to the
selected decisions associated with the specified subset of actions. Let
\\\mathcal{A}^\star \subseteq \mathcal{A}\\ denote that subset and let
\\x\_{ia} \in \\0,1\\\\ denote the binary variable indicating whether
action \\a \in \mathcal{A}^\star\\ is selected in planning unit \\i \in
\mathcal{I}\\. In that case, the constrained quantity is \$\$ \sum\_{i
\in \mathcal{I}} \sum\_{a \in \mathcal{A}^\star} a_i x\_{ia}. \$\$

Under formulations where at most one action can be selected per planning
unit, this coincides with the area allocated to that subset of actions.

Areas are obtained from the planning-unit table. If `area_col` is
provided, that column is used. Otherwise, the model builder later
determines the default area source according to the internal rules of
the package. The value of `area_unit` indicates the unit in which `area`
and `tolerance` are expressed and therefore how the stored threshold
should be interpreted.

This function only stores the constraint specification; it does not
validate the feasibility of the threshold against the available planning
units at this stage.

Multiple area constraints can be stored in a `Problem` object. However,
at most one can be stored for the same combination of action subset and
constraint sense. Attempting to add a duplicated `actions`–`sense`
combination results in an error.

## See also

[`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)

## Examples

``` r
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4),
  area_ha = c(10, 15, 8, 20)
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

p <- add_constraint_area(
  x = p,
  area = 25,
  sense = "min",
  area_col = "area_ha",
  area_unit = "ha"
)

p <- add_constraint_area(
  x = p,
  area = 15,
  sense = "max",
  area_col = "area_ha",
  area_unit = "ha",
  actions = "restoration"
)

p <- add_constraint_area(
  x = p,
  area = 5,
  sense = "min",
  area_col = "area_ha",
  area_unit = "ha",
  actions = "restoration"
)

p$data$constraints$area
#>   type sense value tolerance unit area_col     actions                 name
#> 1 area   min    25         0   ha  area_ha        <NA>             area_min
#> 2 area   max    15         0   ha  area_ha restoration area_max_restoration
#> 3 area   min     5         0   ha  area_ha restoration area_min_restoration
```
