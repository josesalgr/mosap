# Create a planning problem from spatial inputs

Convert common spatial inputs (rasters and vector layers such as
Shapefiles or GeoPackages) into the tabular inputs required by
[`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md).

This function only builds the *base* planning problem: planning units
(pu), features, dist_features, and optionally boundary. Actions and
action-benefits are added later via
[`add_actions()`](https://josesalgr.github.io/mosap/reference/add_actions.md)
and `add_action_benefit()`.

Spatial backends are **optional** (the package does not import them by
default). To use this function you need:

- `terra` for reading rasters/vectors and extracting raster values.

- `sf` only if you ask to automatically derive the `boundary` table.

## Usage

``` r
inputDataSpatial(
  pu,
  features,
  cost,
  pu_id_col = "id",
  locked_in_col = "locked_in",
  locked_out_col = "locked_out",
  pu_status = NULL,
  cost_aggregation = c("mean", "sum"),
  boundary = "auto",
  ...
)
```

## Arguments

- pu:

  Planning units as either:

  - a file path to a raster (e.g. GeoTIFF) where cell values are PU ids,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    with PU ids,

  - a file path to a vector layer (e.g. `.shp`, `.gpkg`),

  - a
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
    or `sf` object with a column of PU ids.

- features:

  A raster (or file path) with one layer per feature. Values are
  interpreted as *amount* per cell. Layer names are used as feature
  names.

- cost:

  Either:

  - a column name in `pu` (when `pu` is a vector), or

  - a raster (or file path) providing cost per cell (when `pu` is a
    raster or when `pu` lacks a cost column).

- pu_id_col:

  When `pu` is a vector layer, name of the column containing planning
  unit ids.

- locked_in_col, locked_out_col:

  Column names in `pu` (vector) with logical lock flags. If missing,
  defaults to FALSE.

- pu_status:

  Optional. Either a column name (when `pu` is a vector) or a raster (or
  file path) with values 0,2,3 meaning available, locked-in, locked-out.
  This is converted to `locked_in`/`locked_out`. If both column locks
  and `pu_status` are provided, column locks take precedence.

- cost_aggregation:

  Aggregation used to compute PU-level cost from a cost raster (default:
  mean).

- boundary:

  Either `NULL`, a `data.frame` with columns `id1`, `id2`, `boundary`,
  or the string `"auto"` to derive adjacency-based boundaries from
  polygon planning units (boundary weight is set to 1 for touching PU
  pairs).

- ...:

  Passed to
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md).

## Value

An object created by
[`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md).
