# Spatial relations infrastructure

Define a unified internal representation for spatial relations between
planning units and provide constructors for building such relations from
common spatial inputs.

Spatial relations in `paretoscape` are graph-like structures defined
over the set of planning units. They are used to represent adjacency,
shared boundary length, proximity, neighbourhood graphs, or other
pairwise spatial relationships that may later be used by objectives,
constraints, or diagnostics.

Relations are stored inside the problem object as
`x$data$spatial_relations[[name]]`.

## Details

**Internal representation**

Each spatial relation is stored as a `data.frame` containing, at a
minimum, the columns:

- `internal_pu1`:

  Integer index of the first planning unit.

- `internal_pu2`:

  Integer index of the second planning unit.

- `weight`:

  Non-negative edge weight. Its interpretation depends on the relation
  type.

Additional columns may also be present, such as `pu1`, `pu2`,
`distance`, `source`, or other diagnostic fields. These extra columns
are preserved when possible.

Thus, a spatial relation can be interpreted as a weighted graph \\G =
(V, E, \omega)\\, where:

- \\V\\ is the set of planning units,

- \\E\\ is the set of stored edges,

- \\\omega\_{ij} \ge 0\\ is the weight associated with edge \\(i,j)\\.

**Directed and undirected relations**

Some relation constructors create undirected relations by default. In
that case, each unordered pair is typically stored once, usually with
\\i \< j\\, unless self-edges are explicitly allowed.

**Boundary relations and diagonal terms**

The function
[`add_spatial_boundary`](https://josesalgr.github.io/paretoscape/reference/add_spatial_boundary.md)
supports boundary-length relations derived either from a boundary table
or from polygon geometry.

If `include_self = TRUE`, diagonal entries \\(i,i)\\ are added to
represent exposed boundary. These diagonal weights are intended for
boundary-based compactness or fragmentation formulations, where
perimeter exposed to the outside of the selected solution should
contribute to the objective.

In that case, the diagonal entry for planning unit \\i\\ is computed as:
\$\$ \omega\_{ii}^{\mathrm{diag}} = \mathrm{edge\\factor} \times
\max\left\\ p_i - \sum\_{j \neq i} \omega\_{ij}, 0 \right\\, \$\$ where
\\p_i\\ is the total perimeter of planning unit \\i\\ and \\\sum\_{j
\neq i} \omega\_{ij}\\ is the total shared boundary length with
neighbouring planning units.

**Geometry safety**

All `sf`-based constructors operate only through spatial predicates,
topological relations, or boundary-length calculations. They never
subdivide, cut, or alter planning-unit geometries. Planning units are
aligned to `x$data$pu$id` before spatial relations are computed.

**Available constructors**

The following functions build and register spatial relations:

- [`add_spatial_relations`](https://josesalgr.github.io/paretoscape/reference/add_spatial_relations.md):
  register a precomputed relation,

- [`add_spatial_boundary`](https://josesalgr.github.io/paretoscape/reference/add_spatial_boundary.md):
  shared boundary length relation,

- [`add_spatial_rook`](https://josesalgr.github.io/paretoscape/reference/add_spatial_rook.md):
  rook adjacency from polygons,

- [`add_spatial_queen`](https://josesalgr.github.io/paretoscape/reference/add_spatial_queen.md):
  queen adjacency from polygons,

- [`add_spatial_knn`](https://josesalgr.github.io/paretoscape/reference/add_spatial_knn.md):
  k-nearest-neighbours graph from coordinates,

- [`add_spatial_distance`](https://josesalgr.github.io/paretoscape/reference/add_spatial_distance.md):
  distance-threshold graph from coordinates.
