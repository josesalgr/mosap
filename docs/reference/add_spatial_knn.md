# Add k-nearest-neighbours spatial relations

Build and register a k-nearest-neighbours graph between planning units
using coordinates.

This constructor does not require polygon geometry. It uses
planning-unit coordinates supplied explicitly or stored in the `Problem`
object.

## Usage

``` r
add_spatial_knn(
  x,
  coords = NULL,
  k = 8,
  name = "knn",
  weight_mode = c("constant", "inverse", "inverse_sq"),
  distance_eps = 1e-09
)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/mosap/reference/input_data.md).

- coords:

  Optional coordinates specification. This may be:

  - a `data.frame(id, x, y)`, or

  - a numeric matrix with two columns `(x, y)` aligned to the order of
    planning units.

  If `NULL`, coordinates are taken from `x$data$pu_coords` or from
  columns `x$data$pu$x` and `x$data$pu$y`.

- k:

  Integer giving the number of neighbours per planning unit. Must be at
  least 1 and strictly less than the number of planning units.

- name:

  Character string giving the key under which the relation is stored.

- weight_mode:

  Character string indicating how distance is converted to weight. Must
  be one of `"constant"`, `"inverse"`, or `"inverse_sq"`.

- distance_eps:

  Small positive numeric constant used to avoid division by zero in
  inverse-distance weighting.

## Value

An updated `Problem` object.

## Details

Let \\s_i = (x_i, y_i)\\ denote the coordinates of planning unit \\i\\.
For each planning unit, this function identifies the `k` nearest
distinct planning units under Euclidean distance.

If \\d\_{ij}\\ denotes the Euclidean distance between units \\i\\ and
\\j\\, then the k-nearest-neighbours relation is constructed by adding
an edge from \\i\\ to each of its `k` nearest neighbours.

Edge weights are then assigned according to `weight_mode`:

- `"constant"`: \$\$\omega\_{ij} = 1,\$\$

- `"inverse"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)},\$\$

- `"inverse_sq"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)^2},\$\$

where \\\varepsilon = \code{distance_eps}\\ is a small constant to avoid
division by zero.

The raw k-nearest-neighbours structure is directional by construction,
but the stored relation is registered as undirected by default through
[`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md),
which collapses duplicate unordered pairs.

If the RANN package is available, it is used for efficient nearest
neighbour search. Otherwise, a full distance matrix is computed.

## See also

[`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md),
[`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_spatial_knn(
  x = p,
  k = 8,
  name = "knn8",
  weight_mode = "inverse"
)
} # }
```
