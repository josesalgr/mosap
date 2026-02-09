# Add queen adjacency from sf polygons

Build and register a queen adjacency relation (shared edge or shared
vertex) from planning-unit polygons. Queen adjacency includes all rook
neighbours plus corner-touching neighbours.

## Usage

``` r
add_spatial_queen(x, pu_sf = NULL, name = "default", weight = 1)
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

  Numeric edge weight assigned to each queen adjacency (default 1).

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- x |> add_spatial_queen(name = "queen", weight = 1)
} # }
```
