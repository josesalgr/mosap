# Add rook adjacency from polygons

Build and register a rook adjacency relation from planning-unit
polygons.

Two planning units are rook-adjacent if they share a boundary segment of
positive length. Corner-only contact does not count as rook adjacency.

## Usage

``` r
add_spatial_rook(x, geometry = NULL, name = "rook", weight = 1)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)
  or another object containing aligned planning-unit polygons.

- geometry:

  Optional `sf` object with planning-unit polygons and an `id` column.
  If `NULL`, `x$data$pu_sf` is used.

- name:

  Character string giving the key under which the relation is stored.

- weight:

  Numeric scalar giving the edge weight assigned to each rook adjacency.

## Value

An updated `Problem` object.

## Details

Use this function when neighbourhood should be defined by shared polygon
edges rather than by point-touching or coordinate-based proximity.

This constructor derives an adjacency graph from polygon geometry using
a rook criterion. If planning units \\i\\ and \\j\\ share a common edge
of non-zero length, then an edge \\(i,j)\\ is added to the relation.

Let \\G = (\mathcal{I}, E)\\ denote the resulting graph. Then: \$\$
(i,j) \in E \quad \Longleftrightarrow \quad \mathrm{length}(\partial i
\cap \partial j) \> 0. \$\$

All edges receive the same user-supplied weight.

The resulting relation is stored as an undirected spatial relation.

## See also

[`add_spatial_queen`](https://josesalgr.github.io/multiscape/reference/add_spatial_queen.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_spatial_rook(
  x = p,
  name = "rook",
  weight = 1
)
} # }
```
