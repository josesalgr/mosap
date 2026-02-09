# Add k-nearest-neighbours spatial relations from coordinates

Build and register a k-nearest-neighbours (kNN) graph between planning
units based on coordinates. This constructor does not require `sf`. If
the `RANN` package is available, it is used for speed.

## Usage

``` r
add_spatial_knn(
  x,
  coords = NULL,
  k = 8,
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

  Optional coordinates specification:

  - a `data.frame(id, x, y)`, or

  - a matrix with two columns `(x,y)` aligned to the PU order.

  If `NULL`, uses `x$data$pu_coords` or `x$data$pu$x/y`.

- k:

  Integer. Number of neighbours per planning unit (must be `>= 1` and
  `< n_pu`).

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

Coordinates can be supplied explicitly via `coords`, stored in
`x$data$pu_coords`, or stored as columns `x$data$pu$x` and
`x$data$pu$y`.

Edge weights can be constant or derived from distance using `weight_fn`.
The stored relation is undirected by default (duplicates are collapsed).

## Examples

``` r
if (FALSE) { # \dontrun{
x <- x |> add_spatial_knn(k = 8, name = "knn8", weight_fn = "inverse")
} # }
```
