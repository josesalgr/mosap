# Add spatial relations

Register an externally computed spatial relation inside a `Problem`
object using the unified internal representation adopted by `mosap`.

Most users will typically prefer one of the convenience constructors
such as
[`add_spatial_boundary`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),
[`add_spatial_rook`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md),
[`add_spatial_queen`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md),
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
or
[`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md).
This function is the advanced low-level entry point for adding an
already computed relation.

## Usage

``` r
add_spatial_relations(x, relations, name, directed = FALSE, allow_self = FALSE)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/mosap/reference/input_data.md).

- relations:

  A `data.frame` describing relation edges. It must contain either:

  - `pu1`, `pu2`, and `weight`, using external planning-unit ids, or

  - `internal_pu1`, `internal_pu2`, and `weight`, using internal
    planning-unit indices.

  Extra columns such as `distance` or `source` are allowed and are
  preserved when possible.

- name:

  Character string giving the key under which the relation is stored.

- directed:

  Logical. If `FALSE`, treat edges as undirected and collapse duplicate
  unordered pairs. If `TRUE`, keep edges as directed ordered pairs.

- allow_self:

  Logical. If `TRUE`, allow self-edges \\(i,i)\\. Default is `FALSE`.

## Value

An updated `Problem` object with the relation stored in
`x$data$spatial_relations[[name]]`.

## Details

The input relation may be provided either in terms of external
planning-unit identifiers or in terms of internal planning-unit indices.

Specifically, the input `relations` table must contain either:

- `pu1`, `pu2`, and `weight`, or

- `internal_pu1`, `internal_pu2`, and `weight`.

If external ids are supplied, they are mapped to internal indices using
`x$data$pu$id` and `x$data$index$pu`.

Let \\E\\ denote the set of rows supplied in `relations`. If
`directed = FALSE`, each edge is treated as undirected, so pairs
\\(i,j)\\ and \\(j,i)\\ are interpreted as the same edge. In that case,
duplicated undirected edges are collapsed automatically using the
maximum weight observed for each unordered pair.

If `directed = TRUE`, edges are preserved as ordered pairs, so \\(i,j)\\
and \\(j,i)\\ are distinct unless the user provides both.

Self-edges \\(i,i)\\ are permitted only if `allow_self = TRUE`.

The final relation is stored in `x$data$spatial_relations[[name]]`.

## See also

[`add_spatial_boundary`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),
[`add_spatial_rook`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md),
[`add_spatial_queen`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md),
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
[`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md)

## Examples

``` r
pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
features <- data.frame(id = "sp1")
dist_features <- data.frame(pu = 1:3, feature = "sp1", amount = c(1, 1, 1))

p <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)
#> Error: features$id must be numeric/integer ids (got non-numeric strings).

rel <- data.frame(
  pu1 = c(1, 1, 2),
  pu2 = c(2, 3, 3),
  weight = c(1, 1, 2)
)

p <- add_spatial_relations(
  x = p,
  relations = rel,
  name = "my_relation"
)
#> Error: object 'p' not found

p$data$spatial_relations$my_relation
#> Error: object 'p' not found
```
