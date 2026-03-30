# Create a planning problem input object

Create a `Problem` object from tabular or spatial inputs.

This is the main entry point to the `paretoscape` workflow. The function
standardizes planning-unit data, feature definitions, and feature
distributions into the internal structure required by downstream
functions such as
[`add_actions()`](https://josesalgr.github.io/paretoscape/reference/add_actions.md),
[`add_effects()`](https://josesalgr.github.io/paretoscape/reference/add_effects.md),
[`add_targets_absolute()`](https://josesalgr.github.io/paretoscape/reference/add_targets_absolute.md),
[`set_solver()`](https://josesalgr.github.io/paretoscape/reference/set_solver.md),
and
[`solve()`](https://josesalgr.github.io/paretoscape/reference/solve.md).

Depending on the input types, `input_data()` dispatches to one of
several supported workflows. In all cases, the result is a `Problem`
object with a tabular internal core, optionally augmented with spatial
metadata such as coordinates, geometry, raster references, or raw
planning-unit attributes.

## Usage

``` r
input_data(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'data.frame,data.frame,data.frame'
input_data(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,missing'
input_data(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,NULL'
input_data(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,data.frame,data.frame'
input_data(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)
```

## Arguments

- pu:

  Planning-units input. Depending on the selected method, this may be:

  - a `data.frame` with planning-unit information,

  - an `sf` object,

  - a
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html),

  - a vector file path readable by terra,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),

  - or a raster file path.

- features:

  Feature input. Depending on the selected method, this may be:

  - a `data.frame` with at least an `id` column,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    with one layer per feature,

  - or a raster file path.

- dist_features:

  Feature-distribution input. In tabular and hybrid modes, this must be
  a `data.frame` with columns `pu`, `feature`, and `amount`. In spatial
  modes it may be omitted or set to `NULL`, in which case it is derived
  automatically.

- cost:

  In spatial modes, planning-unit cost information. Depending on the
  input mode, this may be:

  - a column name in the planning-unit attribute table,

  - a raster object,

  - or a raster file path.

  In raster-cell mode, `cost` is required and valid planning units are
  defined only where cost is finite and strictly positive.

- pu_id_col:

  Character string giving the name of the planning-unit id column in
  vector or `sf` inputs. Ignored in purely tabular mode and in
  raster-cell mode.

- cost_aggregation:

  Character string used in vector-PU mode when `cost` is a raster and
  must be aggregated over polygons. Must be one of `"mean"` or `"sum"`.

- ...:

  Additional arguments forwarded to internal builders.

## Value

A `Problem` object to be used in downstream `paretoscape` workflows. The
object always contains a tabular internal planning core and may
additionally contain spatial metadata depending on the input mode.

## Details

**Overview**

The role of `input_data()` is to construct the internal input data model
used by `paretoscape`. Regardless of the input mode, the function aims
to produce a consistent problem representation containing, at minimum:

- a planning-unit table `x$data$pu`,

- a feature table `x$data$features`,

- a feature-distribution table `x$data$dist_features`.

Internally, this means that the problem is always reduced to a tabular
core of the form:

- planning units \\i \in \mathcal{P}\\,

- features \\f \in \mathcal{F}\\,

- non-negative baseline amounts \\a\_{if} \ge 0\\ stored in
  `dist_features`,

- and, when available, spatial metadata associated with planning units.

Thus, after `input_data()` has run, the landscape is represented by
baseline feature amounts \\a\_{if}\\, where \\a\_{if}\\ denotes the
amount of feature \\f\\ in planning unit \\i\\.

These baseline amounts are later combined with actions, effects,
targets, objectives, and constraints to build the optimization model.

**Supported input modes**

Four workflows are supported.

**1. Tabular mode**

If `pu`, `features`, and `dist_features` are all `data.frame` objects,
the problem is built directly from tabular data. No spatial package is
required.

**2. Vector-PU spatial mode**

If `dist_features` is missing or `NULL`, `pu` is a spatial vector
object, and `features` is a raster stack, the function derives
`dist_features` by aggregating raster values over planning-unit
polygons.

**3. Raster-cell fast mode**

If `dist_features` is missing or `NULL`, and both `pu` and `features`
are rasters, the function enters a raster-cell workflow in which each
valid raster cell becomes one planning unit.

In this mode:

- `pu` is used as a mask or template,

- `cost` must be a single-layer raster,

- a raster cell becomes a planning unit if and only if the corresponding
  `pu` cell is not `NA`, and the corresponding `cost` cell is finite and
  strictly positive,

- equivalently, if \\m_i\\ denotes the raster-mask value and \\c_i\\ the
  cost of cell \\i\\, then cell \\i\\ is retained only if \\m_i \neq
  \mathrm{NA}\\ and \\c_i \> 0\\,

- each valid cell is assigned a new sequential planning-unit id,

- feature values are extracted directly by cell without
  raster-to-polygon conversion,

- feature values equal to `NA` are treated as zero before building
  `dist_features`,

- only strictly positive feature amounts are stored in
  `x$data$dist_features`.

This mode is substantially faster than converting large rasters to
polygons and is generally the preferred option for large regular grids.

**4. Hybrid sf-tabular mode**

If `pu` is an `sf` object, while `features` and `dist_features` are
`data.frame`s, the problem is built from the supplied tabular feature
distribution while preserving planning-unit geometry and raw spatial
attributes.

This mode is useful when the user already has a curated tabular
`dist_features` table but still wants to retain geometry for later
spatial operations such as boundary construction, plotting, or spatial
feasibility specifications.

**What is stored in the resulting object**

In all modes, the returned object contains a tabular planning core.
Depending on the input mode, additional components may also be stored:

- `x$data$pu`:

  Planning-unit table, always present.

- `x$data$features`:

  Feature table, always present.

- `x$data$dist_features`:

  Baseline planning unit–feature amounts, always present.

- `x$data$pu_data_raw`:

  Raw planning-unit attributes aligned by planning-unit id, when
  available.

- `x$data$pu_coords`:

  Planning-unit coordinates, when available or derivable.

- `x$data$pu_sf`:

  Planning-unit geometry as `sf`, when available and safely alignable.

- `x$data$pu_raster_id`:

  Raster mask/template used to derive planning units from raster cells
  or rasterized planning units, when available.

- `x$data$cell_index`:

  Indices of valid raster cells in raster-cell mode.

- `x$data$cost_raster`:

  Stored cost raster in raster-cell mode.

- `x$data$features_raster`:

  Stored feature raster stack in raster-cell mode.

**Spatial relations are not created automatically**

This function does *not* create boundary, adjacency, distance, or other
spatial relations automatically. Spatial relations must be added
explicitly afterwards using functions such as:

- [`add_spatial_boundary`](https://josesalgr.github.io/paretoscape/reference/add_spatial_boundary.md),

- [`add_spatial_rook`](https://josesalgr.github.io/paretoscape/reference/add_spatial_rook.md),

- [`add_spatial_queen`](https://josesalgr.github.io/paretoscape/reference/add_spatial_queen.md),

- [`add_spatial_knn`](https://josesalgr.github.io/paretoscape/reference/add_spatial_knn.md),

- [`add_spatial_distance`](https://josesalgr.github.io/paretoscape/reference/add_spatial_distance.md),

- [`add_spatial_relations`](https://josesalgr.github.io/paretoscape/reference/add_spatial_relations.md).

In raster-cell mode, spatial relations will typically be created from
coordinates rather than polygons, for example using
[`add_spatial_knn`](https://josesalgr.github.io/paretoscape/reference/add_spatial_knn.md)
or
[`add_spatial_distance`](https://josesalgr.github.io/paretoscape/reference/add_spatial_distance.md).

**Locks are not interpreted automatically**

This function does not interpret planning-unit lock information, even if
the input data contain fields such as `locked_in`, `locked_out`, or
Marxan-style status codes. Such information may be preserved in
`x$data$pu_data_raw`, but it is not activated automatically.

Locked planning units should be defined later with
[`add_locked_pu`](https://josesalgr.github.io/paretoscape/reference/add_locked_pu.md).

**Feature identifiers**

Features are internally standardized to an `id`-based representation. In
spatial modes where raster layers are used, one feature is created per
raster layer, with:

- `id = 1, 2, ..., nlyr(features)`,

- `name` equal to the raster layer name, if available,

- otherwise a generated name of the form `"feature.1"`, `"feature.2"`,
  and so on.

**Planning-unit identifiers**

In vector and hybrid modes, planning-unit ids are taken from the column
named by `pu_id_col`. If that column is missing and `pu_id_col = "id"`,
sequential ids are created with a warning.

In raster-cell mode, planning-unit ids are always created sequentially
from the valid raster cells retained after masking and cost filtering.

**Cost handling**

In purely tabular mode, `cost` is not used by this generic method
because planning-unit costs are expected to already be present in the
`pu` table supplied to the internal tabular builder.

In spatial modes, `cost` is required:

- in vector-PU mode, it may be either:

  - the name of a numeric attribute column in the planning-unit layer,

  - or a cost raster to be aggregated over polygons;

- in raster-cell mode, it must be a single-layer raster aligned with
  `pu` and `features`;

- in hybrid `sf` + tabular mode, it must be either:

  - the name of an attribute column in the `sf` object,

  - or omitted if the `sf` attributes already contain a column literally
    named `cost`.

**Ordering and alignment**

In spatial modes, planning units, derived coordinates, raw attributes,
geometry, and extracted feature amounts are aligned to the same
planning-unit id order before the final `Problem` object is created.

This alignment is critical because later functions assume that all
stored planning-unit components refer to the same ordered set of
planning units.

## See also

[`add_actions`](https://josesalgr.github.io/paretoscape/reference/add_actions.md),
[`add_locked_pu`](https://josesalgr.github.io/paretoscape/reference/add_locked_pu.md),
[`add_spatial_boundary`](https://josesalgr.github.io/paretoscape/reference/add_spatial_boundary.md),
[`add_spatial_knn`](https://josesalgr.github.io/paretoscape/reference/add_spatial_knn.md),
[`solve`](https://josesalgr.github.io/paretoscape/reference/solve.md)

## Examples

``` r
# ------------------------------------------------------
# 1) Tabular mode
# ------------------------------------------------------
pu_tbl <- data.frame(
  id = 1:4,
  cost = c(1, 2, 3, 4)
)

feat_tbl <- data.frame(
  id = 1:2,
  name = c("feature_1", "feature_2")
)

dist_feat_tbl <- data.frame(
  pu = c(1, 1, 2, 3, 4),
  feature = c(1, 2, 2, 1, 2),
  amount = c(5, 2, 3, 4, 1)
)

p1 <- input_data(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl
)

print(p1)
#> A paretoscape object (<Problem>)
#> ├─data
#> │├─planning units: <data.frame> (4 total)
#> │├─costs: min: 1, max: 4
#> │└─features: 2 total ("feature_1", "feature_2")
#> └─actions and effects
#> │├─actions: none
#> │├─feasible action pairs: none
#> │├─effect data: none
#> │└─profit data: none
#> └─spatial
#> │├─geometry: none
#> │├─coordinates: none
#> │└─relations: none
#> └─targets and constraints
#> │├─targets: none
#> │├─area constraints: none
#> │├─planning-unit locks: none
#> │└─action locks: none
#> └─model
#> │├─status: not built yet (will build in solve())
#> │├─objectives: none
#> │├─method: single-objective
#> │├─solver: not set (auto)
#> │└─checks: incomplete (no objective registered)
#> # ℹ Use `x$data` to inspect stored tables and model snapshots.

# ------------------------------------------------------
# 2) Hybrid sf + tabular mode using package data
# ------------------------------------------------------

p2 <- input_data(
  pu = sim_pu_sf,
  features = sim_features,
  dist_features = sim_dist_features,
  cost = "cost"
)
#> Warning: The following pu's do not contain features: 8012 8033 8147 8263

print(p2)
#> A paretoscape object (<Problem>)
#> ├─data
#> │├─planning units: <tbl_df> (11109 total)
#> │├─costs: min: 1, max: 1
#> │└─features: 155 total ("ACCGENT", "ACCNISU", "ACRARUN", ...)
#> └─actions and effects
#> │├─actions: none
#> │├─feasible action pairs: none
#> │├─effect data: none
#> │└─profit data: none
#> └─spatial
#> │├─geometry: sf (11109 rows)
#> │├─coordinates: 11109 rows (x: 2868900..3007900, y: 2110700..2280700)
#> │└─relations: none
#> └─targets and constraints
#> │├─targets: none
#> │├─area constraints: none
#> │├─planning-unit locks: none
#> │└─action locks: none
#> └─model
#> │├─status: not built yet (will build in solve())
#> │├─objectives: none
#> │├─method: single-objective
#> │├─solver: not set (auto)
#> │└─checks: incomplete (no objective registered)
#> # ℹ Use `x$data` to inspect stored tables and model snapshots.
```
