# Add spatial boundary-length relations

Build and register a boundary-length spatial relation between planning
units.

Boundary relations represent shared edge length between adjacent
polygons. In contrast to queen adjacency, they only account for boundary
segments of positive length and ignore point-only contacts.

## Usage

``` r
add_spatial_boundary(
  x,
  boundary = NULL,
  geometry = NULL,
  name = "boundary",
  weight_col = NULL,
  weight_multiplier = 1,
  include_self = TRUE,
  edge_factor = 1
)
```

## Arguments

- x:

  A `Problem` object.

- boundary:

  Optional `data.frame` describing boundary lengths. Accepted formats
  are:

  - `(id1, id2, boundary)`, or

  - `(pu1, pu2, weight)`.

- geometry:

  Optional `sf` object with planning-unit polygons and an `id` column.
  If `NULL`, `x$data$pu_sf` is used.

- name:

  Character string giving the key under which the relation is stored.

- weight_col:

  Optional character string giving the name of the weight column in
  `boundary`. If `NULL`, the function tries to infer it from
  `"boundary"` or `"weight"`.

- weight_multiplier:

  Positive numeric scalar applied to all boundary weights.

- include_self:

  Logical. If `TRUE`, include diagonal entries representing exposed
  boundary.

- edge_factor:

  Numeric scalar greater than or equal to zero. Multiplier applied to
  exposed boundary when constructing diagonal entries.

## Value

An updated `Problem` object with the stored relation in
`x$data$spatial_relations[[name]]`.

## Details

Two input modes are supported:

1.  **Boundary-table mode.** If `boundary` is supplied, it is
    interpreted as a boundary table, for example a Marxan-style
    `bound.dat`.

2.  **Geometry mode.** If `boundary = NULL`, boundary lengths are
    derived from polygon geometry using `geometry` or `x$data$pu_sf`.

Let \\\omega\_{ij} \ge 0\\ denote the shared boundary length between
planning units \\i\\ and \\j\\, multiplied by `weight_multiplier`.

For off-diagonal entries \\i \neq j\\, the stored weight is: \$\$
\omega\_{ij} = \mathrm{BLM} \times b\_{ij}, \$\$ where \\b\_{ij}\\ is
the shared boundary length and \\\mathrm{BLM}\\ is the user-supplied
`weight_multiplier`.

If `include_self = TRUE`, diagonal entries are also created. These are
not geometric self-neighbours in the graph sense; instead, they
represent the effective boundary exposed to the outside of the solution.

Let \\p_i\\ be the total perimeter of planning unit \\i\\, and let
\\\sum\_{j \neq i} \omega\_{ij}\\ be the total incident shared boundary
recorded for that planning unit. Then the exposed boundary is: \$\$ e_i
= \max\left\\ p_i \times \mathrm{BLM} - \sum\_{j \neq i} \omega\_{ij}, 0
\right\\, \$\$ and the stored diagonal term is: \$\$ \omega\_{ii} =
\mathrm{edge\\factor} \times e_i. \$\$

These diagonal terms are useful in boundary-based compactness or
fragmentation objectives, because they encode the portion of each
planning unit's perimeter that would remain exposed if the unit were
selected.

**Boundary-table mode**

If `boundary` is provided, accepted formats are:

- `(id1, id2, boundary)`, or

- `(pu1, pu2, weight)`.

If the table contains diagonal rows \\(i,i)\\, these are interpreted as
total perimeter values in boundary-table mode.

**Geometry mode**

If `boundary = NULL`, shared boundary lengths are derived directly from
polygon geometry. Only positive-length intersections are retained. Point
touches are ignored.

**Storage**

The final relation is stored through
[`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md),
typically as an undirected relation with optional diagonal entries.

## See also

[`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md),
[`add_objective_min_fragmentation`](https://josesalgr.github.io/mosap/reference/add_objective_min_fragmentation.md),
[`add_objective_min_action_fragmentation`](https://josesalgr.github.io/mosap/reference/add_objective_min_action_fragmentation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# From a boundary table
p <- add_spatial_boundary(
  x = p,
  boundary = bound_df,
  name = "boundary"
)

# From sf polygons
p <- add_spatial_boundary(
  x = p,
  geometry = pu_sf,
  include_self = TRUE,
  edge_factor = 1
)
} # }
```
