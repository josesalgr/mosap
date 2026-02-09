# Add distance-threshold spatial relations from coordinates

Build and register edges between planning units whose Euclidean distance
is less than or equal to `dmax`, based on coordinates. This constructor
does not require `sf`.

## Usage

``` r
add_spatial_distance(
  x,
  coords = NULL,
  dmax,
  name = "default",
  weight_fn = c("constant", "inverse", "inverse_sq"),
  eps = 1e-09
)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object created with
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- coords:

  Optional coordinates specification (see
  [`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)).

- dmax:

  Numeric. Maximum distance for an edge (must be finite and positive).

- name:

  Character name/key under which to store the relation.

- weight_fn:

  Character. How to convert distance to weight: `"constant"`,
  `"inverse"`, or `"inverse_sq"`.

- eps:

  Small numeric constant to avoid division by zero when using inverse
  weights.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.

## Details

This constructor uses an \\O(n^2)\\ distance computation and is
therefore best suited to small or moderate numbers of planning units.
For large instances, consider
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
instead.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- x |> add_spatial_distance(dmax = 1000, name = "within_1km", weight_fn = "constant")
} # }
```
