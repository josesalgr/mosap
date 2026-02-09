# Add spatial relations (core)

Register an externally computed spatial relation inside a `Data` object
using the unified internal representation. Most users should prefer the
convenience wrappers:
[`add_spatial_boundary`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),
[`add_spatial_rook`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md),
[`add_spatial_queen`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md),
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
or
[`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md).

## Usage

``` r
add_spatial_relations(
  x,
  relations,
  name = "default",
  directed = FALSE,
  allow_self = FALSE,
  duplicate_agg = c("sum", "max", "min", "mean"),
  symmetric = FALSE
)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object created with
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- relations:

  A `data.frame` describing edges. Must contain either:

  - `pu1`, `pu2`, `weight` (external PU ids), or

  - `internal_pu1`, `internal_pu2`, `weight` (internal indices).

  Extra columns (e.g., `distance`, `source`) are allowed and preserved
  when possible.

- name:

  Character. Name/key under which to store the relation (default
  `"default"`).

- directed:

  Logical. If `FALSE` (default), treats edges as undirected and
  collapses duplicates. If `TRUE`, keeps directed edges as provided.

- allow_self:

  Logical. Whether to allow self-edges `(i,i)`. Default `FALSE`.

- duplicate_agg:

  Aggregation for duplicate undirected edges when `directed=FALSE`. One
  of `"sum"`, `"max"`, `"min"`, `"mean"`.

- symmetric:

  Logical. If `TRUE`, expands an undirected relation into a directed one
  by duplicating off-diagonal edges in both directions. Default `FALSE`.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md) object
with `x$data$spatial_relations[[name]]`.

## Details

The input can be given in external PU ids (`pu1`, `pu2`) or in internal
PU indices (`internal_pu1`, `internal_pu2`). If external ids are
provided, they are mapped using `x$data$pu$id` and
`x$data$pu$internal_id`.

If `directed = FALSE`, edges are treated as undirected and duplicates
are collapsed using `duplicate_agg`. If `symmetric = TRUE`, the relation
is expanded to a directed edge list by adding swapped copies for
off-diagonal edges.

## Examples

``` r
if (FALSE) { # \dontrun{
# Register an externally computed adjacency list:
rel <- data.frame(pu1 = c(1, 1, 2), pu2 = c(2, 3, 3), weight = 1)
x <- x |> add_spatial_relations(relations = rel, name = "my_adj")
} # }
```
