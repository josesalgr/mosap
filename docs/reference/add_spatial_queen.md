# Add queen adjacency from polygons

Build and register a queen adjacency relation from planning-unit
polygons.

Two planning units are queen-adjacent if their boundaries touch, either
along a shared edge or at a shared vertex.

## Usage

``` r
add_spatial_queen(x, geometry = NULL, name = "queen", weight = 1)
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

  Numeric scalar giving the edge weight assigned to each queen
  adjacency.

## Value

An updated `Problem` object.

## Details

Use this function when neighbourhood should include both shared edges
and corner-touching polygon contacts.

This constructor derives an adjacency graph from polygon geometry using
a queen criterion. If planning units \\i\\ and \\j\\ touch at any
boundary point, then an edge \\(i,j)\\ is added to the relation.

Let \\G = (\mathcal{I}, E)\\ denote the resulting graph. Then: \$\$
(i,j) \in E \quad \Longleftrightarrow \quad \partial i \cap \partial j
\neq \varnothing. \$\$

Thus, queen adjacency includes all rook neighbours plus corner-touching
neighbours.

All edges receive the same user-supplied weight.

The resulting relation is stored as an undirected spatial relation.

## See also

[`add_spatial_rook`](https://josesalgr.github.io/multiscape/reference/add_spatial_rook.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_spatial_queen(
  x = p,
  name = "queen",
  weight = 1
)
} # }
```
