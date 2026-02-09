# Add rook adjacency from sf polygons

Build and register a rook adjacency relation (shared edge) from
planning-unit polygons. Rook adjacency detects pairs of polygons that
share a non-zero-length boundary segment.

## Usage

``` r
add_spatial_rook(x, pu_sf = NULL, name = "default", weight = 1)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object created with
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- pu_sf:

  Optional `sf` object with PU polygons and an `id` column. If `NULL`,
  uses `x$data$pu_sf`.

- name:

  Character name/key under which to store the relation.

- weight:

  Numeric edge weight assigned to each rook adjacency (default 1).

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- x |> add_spatial_rook(name = "rook", weight = 1)
} # }
```
