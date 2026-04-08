# Create a planning problem input object

Create a `Problem` object from tabular or spatial inputs.

`create_problem()` is the main entry point to the `multiscape` workflow.
Its role is to standardize heterogeneous planning inputs into a common
internal representation that can later be used by actions, effects,
targets, constraints, spatial relations, objectives, and solvers.

In all supported workflows, the result is a `Problem` object with a
canonical tabular core, optionally enriched with aligned spatial
metadata such as coordinates, geometry, raster references, or raw
planning-unit attributes.

## Usage

``` r
create_problem(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'data.frame,data.frame,data.frame'
create_problem(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,missing'
create_problem(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,NULL'
create_problem(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,data.frame,data.frame'
create_problem(
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

  Feature input. Depending on the selected workflow, this may be:

  - a `data.frame` with at least an `id` column,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    with one layer per feature,

  - or a raster file path.

- dist_features:

  Feature-distribution input. In tabular and hybrid workflows, this must
  be a `data.frame` with columns `pu`, `feature`, and `amount`. In
  spatial modes it may be omitted or set to `NULL`, in which case it is
  derived automatically.

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

A `Problem` object for downstream `multiscape` workflows. The returned
object always contains a canonical tabular core with planning units,
features, and feature-distribution data, and may additionally contain
aligned spatial metadata depending on the input mode.

## Details

A `Problem` object created by `create_problem()` is the basic input
structure used throughout downstream `multiscape` workflows.

The returned object always contains a canonical tabular planning core
and may additionally store aligned spatial metadata depending on the
input workflow. This is the internal representation on which later
functions such as
[`add_actions()`](https://josesalgr.github.io/multiscape/reference/add_actions.md),
[`add_effects()`](https://josesalgr.github.io/multiscape/reference/add_effects.md),
`add_constraint_targets()`,
[`add_spatial_relations()`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md),
and
[`solve()`](https://josesalgr.github.io/multiscape/reference/solve.md)
operate.

**Conceptual role of `create_problem()`**

Regardless of the input workflow, `create_problem()` aims to produce a
consistent internal representation containing at least:

- a planning-unit table,

- a feature table,

- a feature-distribution table.

Internally, the problem is always reduced to a tabular core of the form:

- planning units \\i \in \mathcal{I}\\,

- features \\f \in \mathcal{F}\\,

- non-negative baseline amounts \\a\_{if} \ge 0\\,

- and, when available, spatial metadata associated with planning units.

The feature-distribution table provides the canonical sparse
representation of these baseline amounts. Thus, after `create_problem()`
has run, the landscape is internally represented by baseline feature
amounts \\a\_{if}\\, where \\a\_{if}\\ denotes the amount of feature
\\f\\ in planning unit \\i\\. These baseline amounts are later combined
with actions, effects, targets, objectives, and constraints to build the
optimization model.

**Which input mode should I use?**

`create_problem()` supports four main workflows. The best choice depends
on the form of your data and on whether you want to preserve geometry.

- **Tabular mode.** Use this when `pu`, `features`, and `dist_features`
  are already available as `data.frame` objects and no spatial
  derivation is needed. This is the simplest workflow: all problem
  components are already tabular, so `create_problem()` only
  standardizes them into the canonical internal representation.

- **Vector-PU spatial mode.** Use this when planning units are polygons
  and feature information is stored in one or more raster layers. In
  this mode, `dist_features` is derived by aggregating raster values
  over planning-unit polygons.

- **Raster-cell fast mode.** Use this when both `pu` and `features` are
  rasters and each valid raster cell should become one planning unit.
  This mode avoids raster-to-polygon conversion, treats `NA` feature
  values as zero before building `dist_features`, keeps only strictly
  positive amounts in the stored distribution table, and is generally
  the preferred option for large regular grids.

- **Hybrid sf + tabular mode.** Use this when you already have a curated
  tabular `dist_features` table but still want to preserve planning-unit
  geometry and attributes for later plotting, spatial relations, or
  feasibility specifications. Here, the feature distribution is already
  tabular, but geometry and raw attributes are preserved from the `sf`
  planning-unit object for later spatial operations.

**How cost is interpreted across modes**

`cost` handling depends on the selected workflow.

- **Tabular mode.** In purely tabular mode, `cost` is not used by this
  generic method. Planning-unit costs are expected to already be present
  in the `pu` table supplied to the internal tabular builder.

- **Vector-PU spatial mode.** In vector-PU mode, `cost` is required and
  may be either:

  - the name of a numeric attribute column in the planning-unit layer,

  - or a cost raster to be aggregated over polygons using the
    `cost_aggregation` argument.

- **Raster-cell fast mode.** In raster-cell mode, `cost` must be a
  single-layer raster aligned with `pu` and `features`. A raster cell
  becomes a planning unit only if the mask cell is not missing and the
  corresponding cost value is finite and strictly positive. In other
  words, if \\m_i\\ denotes the mask value of cell \\i\\ and \\c_i\\ its
  cost value, then cell \\i\\ is retained only when the mask is observed
  and \\c_i \> 0\\.

- **Hybrid sf + tabular mode.** In hybrid mode, `cost` must be either:

  - the name of a numeric attribute column in the `sf` layer,

  - or omitted if the `sf` attributes already contain a column literally
    named `cost`.

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

**Ordering and alignment**

In spatial modes, planning units, derived coordinates, raw attributes,
geometry, and extracted feature amounts are aligned to the same
planning-unit id order before the final `Problem` object is created.

This alignment is critical because later functions assume that all
stored planning-unit components refer to the same ordered set of
planning units.

After `create_problem()`, typical next steps include adding actions,
spatial relations, targets or other constraints, objectives, and then
solving the resulting problem.

## See also

[`add_actions`](https://josesalgr.github.io/multiscape/reference/add_actions.md),
[`add_constraint_locked_pu`](https://josesalgr.github.io/multiscape/reference/add_constraint_locked_pu.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md),
[`add_spatial_knn`](https://josesalgr.github.io/multiscape/reference/add_spatial_knn.md),
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)

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

p1 <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl
)

print(p1)
#> A multiscape object (<Problem>)
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
#> │├─budget constraints: none
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

p2 <- create_problem(
  pu = sim_pu_sf,
  features = sim_features,
  dist_features = sim_dist_features,
  cost = "cost"
)
#> Warning: The following pu's do not contain features: 8012 8033 8147 8263

print(p2)
#> A multiscape object (<Problem>)
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
#> │├─budget constraints: none
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
