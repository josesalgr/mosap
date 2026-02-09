# Add spatial boundary-length relations from sf polygons or a boundary table

Register a boundary-length relation between planning units. Boundary
relations represent shared edge length between adjacent polygons (not
queen touches).

## Usage

``` r
add_spatial_boundary(
  x,
  boundary = NULL,
  pu_sf = NULL,
  name = "boundary",
  weight_col = NULL,
  weight_multiplier = 1,
  progress = FALSE,
  include_self = TRUE,
  edge_factor = 1
)
```

## Arguments

- x:

  A [data](https://josesalgr.github.io/mosap/reference/data-class.md)
  object.

- boundary:

  Optional `data.frame` describing boundaries. Accepted formats:

  - `(id1, id2, boundary)` (Marxan-style), or

  - `(pu1, pu2, weight)`.

- pu_sf:

  Optional `sf` object with PU polygons and an `id` column. If `NULL`,
  uses `x$data$pu_sf`.

- name:

  Character name/key under which to store the relation (default
  `"boundary"`).

- weight_col:

  Character. Column in `boundary` to use as weights. If `NULL`, attempts
  to guess `"boundary"` or `"weight"`.

- weight_multiplier:

  Numeric multiplier applied to boundary weights (e.g., BLM scaling).
  Must be finite and positive.

- progress:

  Logical. If `TRUE`, prints basic progress messages for large
  instances.

- include_self:

  Logical. If `TRUE` (default), include diagonal `(i,i)` entries
  representing effective exposed boundary length.

- edge_factor:

  Numeric \\\ge 0\\. Multiplier applied to exposed boundary when
  computing diagonal weights.

## Value

Updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md) object
with a stored relation `x$data$spatial_relations[[name]]`.

## Details

Two input modes are supported:

1.  **Boundary table mode.** If `boundary` is provided, it is
    interpreted as a table containing PU pairs and a boundary-length
    weight (e.g., Marxan-style `bound.dat`).

2.  **Geometry mode.** If `boundary` is `NULL`, boundary lengths are
    derived from planning-unit polygons (`pu_sf` or `x$data$pu_sf`).

If `include_self=TRUE`, the function also adds diagonal entries `(i,i)`
with weights equal to an "effective exposed boundary" (scaled by
`edge_factor`). This is useful for objectives such as boundary-length
modifier / fragmentation penalties where perimeter exposed to the
outside should be counted.

## Examples

``` r
if (FALSE) { # \dontrun{
# From a Marxan-style boundary table:
x <- x |> add_spatial_boundary(boundary = bound_df, name = "boundary")

# From sf polygons:
x <- x |> add_spatial_boundary(pu_sf = pu_sf, include_self = TRUE, edge_factor = 1)
} # }
```
