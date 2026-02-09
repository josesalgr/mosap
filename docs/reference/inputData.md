# Create a planning problem input object

Builds a `Data` object from either tabular inputs or spatial inputs.
This is the entry point for the **mosap** workflow.

The function supports three input styles:

1.  **Tabular mode**: If `pu`, `features`, and `dist_features` are
    `data.frame`s, a purely tabular workflow is used (no spatial
    packages required).

2.  **Vector-PU spatial mode**: If `dist_features` is missing (or
    `NULL`) and `pu` is a spatial vector (e.g.
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html),
    `sf`, a vector file path), while `features` is a raster stack (one
    layer per feature), then feature amounts are aggregated by polygon
    (default `sum`) and stored as `dist_features`.

3.  **Raster-cell fast mode**: If `dist_features` is missing (or `NULL`)
    and `pu` and `features` are rasters (e.g.
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    or raster file paths), then each valid raster cell becomes a
    planning unit. In this mode:

    - cells are valid if `cost` is finite and `cost > 0`

    - `pu` is used as a mask/template (cells with `NA` in `pu` are
      excluded) This avoids raster-to-polygon conversion and is
      substantially faster for large grids.

## Usage

``` r
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  locked_in_col = "locked_in",
  locked_out_col = "locked_out",
  pu_status = NULL,
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'data.frame,data.frame,data.frame'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  locked_in_col = "locked_in",
  locked_out_col = "locked_out",
  pu_status = NULL,
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,missing'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  locked_in_col = "locked_in",
  locked_out_col = "locked_out",
  pu_status = NULL,
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,NULL'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  locked_in_col = "locked_in",
  locked_out_col = "locked_out",
  pu_status = NULL,
  cost_aggregation = c("mean", "sum"),
  ...
)
```

## Arguments

- pu:

  Planning units input. Either:

  - a `data.frame` with an `id` column (tabular mode),

  - a spatial vector
    ([`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html),
    `sf`, or a vector file path) (vector-PU mode), or

  - a raster
    ([`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    or raster file path) used as a mask/template (raster-cell mode).

- features:

  Features input. Either:

  - a `data.frame` with `id` (and optionally `name`) (tabular mode), or

  - a raster stack with one layer per feature
    ([`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    or file path) (vector-PU mode or raster-cell mode).

- dist_features:

  Distribution of features. A `data.frame` with columns `pu`, `feature`,
  and `amount`. If missing or `NULL`, spatial workflows derive it
  automatically.

- cost:

  In spatial modes, required. Either a column name in the PU vector
  attribute table (vector-PU mode) or a raster/file path (vector-PU or
  raster-cell mode). In raster-cell mode, cells with `cost <= 0` or `NA`
  are excluded (no PU is created).

- pu_id_col:

  For vector-PU mode, the name of the id column in the PU layer.

- locked_in_col:

  For vector-PU mode, the name of a logical column indicating locked-in
  PUs.

- locked_out_col:

  For vector-PU mode, the name of a logical column indicating locked-out
  PUs.

- pu_status:

  Optional (vector-PU or raster-cell) status input. Either a column name
  or a raster/file path. Values `2` mark locked-in and `3` mark
  locked-out (Marxan-style).

- cost_aggregation:

  In vector-PU mode, how to aggregate raster cell costs to polygons when
  `cost` is a raster. Either `"mean"` or `"sum"`.

- ...:

  Additional arguments forwarded to internal builders.

## Value

A `Data` object used by downstream functions (`add_*()`,
[`set_solver()`](https://josesalgr.github.io/mosap/reference/set_solver.md),
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md),
etc.).

## Details

**Spatial relations are not created automatically.** This version of
`inputData()` does *not* take a `boundary` argument. Spatial relations
(boundary/rook/queen/kNN/distance) must be registered explicitly after
`inputData()` using
[`add_spatial_boundary()`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),
[`add_spatial_rook()`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md),
[`add_spatial_queen()`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md),
[`add_spatial_knn()`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
[`add_spatial_distance()`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md),
or
[`add_spatial_relations()`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md).

In raster-cell mode you typically build relations from coordinates with
[`add_spatial_knn()`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
or
[`add_spatial_distance()`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# ------------------------------------------------------
# 1) Tabular mode
# ------------------------------------------------------
pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
features <- data.frame(id = 1:2, name = c("sp1", "sp2"))
dist_features <- data.frame(
  pu = c(1, 1, 2, 3),
  feature = c(1, 2, 2, 1),
  amount = c(1, 5, 2, 1)
)

x <- inputData(pu = pu, features = features, dist_features = dist_features)

# Optional: register a spatial relation later (if you need spatial objectives)
rel <- data.frame(pu1 = 1, pu2 = 2, weight = 1)
x <- add_spatial_relations(x, rel, name = "rook_like")


# ------------------------------------------------------
# 2) Raster-cell fast mode (1 PU per valid cell)
# ------------------------------------------------------
library(terra)
pu_mask <- rast("pu_mask.tif")           # NA outside study area
cost_r  <- rast("cost.tif")              # cost per cell
feat_r  <- rast(c("sp1.tif", "sp2.tif")) # layers = features

x <- inputData(pu = pu_mask, features = feat_r, cost = cost_r)

# Optional: add spatial relations after inputData (e.g., kNN)
x <- add_spatial_knn(x, k = 8, name = "knn8", weight_fn = "inverse")


# ------------------------------------------------------
# 3) Vector-PU spatial mode (polygons + raster features)
# ------------------------------------------------------
pu_v   <- vect("pus.gpkg")               # polygon PUs
feat_r <- rast(c("sp1.tif", "sp2.tif"))  # features as raster layers
cost_r <- rast("cost.tif")               # raster cost (aggregated to polygons)

x <- inputData(
  pu = pu_v,
  features = feat_r,
  cost = cost_r,
  pu_id_col = "id",
  cost_aggregation = "mean"
)

# Add boundary-length relation explicitly (requires sf)
x <- add_spatial_boundary(x, pu_sf = x$data$pu_sf, name = "boundary")
} # }
```
