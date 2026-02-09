# Add effects (benefit/loss) to a planning problem

Define the ecological (or any feature-based) effects of implementing
actions in planning units. Effects are stored in `x$data$dist_effects`
as two non-negative components per feasible `(pu, action, feature)`
triple:

- `benefit` \\\ge 0\\: positive change (improvement),

- `loss` \\\ge 0\\: magnitude of negative change (damage), computed as
  `loss = -min(delta, 0)`.

## Usage

``` r
add_effects(
  x,
  effects = NULL,
  effect_type = c("delta", "after"),
  effect_aggregation = c("sum", "mean"),
  align_rasters = TRUE,
  keep_zero = FALSE,
  drop_locked_out = TRUE,
  na_to_zero = TRUE,
  filter = c("any", "benefit", "loss")
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
  Must contain `x$data$dist_actions` (run
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  first).

- effects:

  Effect specification. One of:

  - `NULL`: store an empty effects table.

  - `data.frame(action, feature, multiplier)`: apply signed multipliers
    to baseline amounts. `feature` may be feature ids or feature names
    (matching `x$data$features$name`).

  - `data.frame(pu, action, feature, ...)`: explicit effects with one
    of:

    - `delta` (signed) or `effect` (signed),

    - `after` (after-action amount; set `effect_type = "after"`),

    - `benefit` and/or `loss` (both non-negative; missing component
      treated as 0),

    - legacy signed `benefit` without `loss` (treated as signed delta).

  - A named list of
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    objects: names = action ids; one layer per feature.

- effect_type:

  Character. How to interpret provided values for explicit tables or
  raster lists:

  - `"delta"`: values represent signed deltas (default),

  - `"after"`: values represent after-action amounts (converted to
    deltas using baseline).

- effect_aggregation:

  Character. Aggregation used to compute PU-level values from rasters.
  One of `"sum"` or `"mean"`.

- align_rasters:

  Logical. If `TRUE`, attempt to align effect rasters to the PU raster
  grid before zonal operations (default `TRUE`).

- keep_zero:

  Logical. If `TRUE`, keep rows where `benefit == 0` and `loss == 0`.
  Default `FALSE`.

- drop_locked_out:

  Logical. If `TRUE`, drop rows for `(pu, action)` pairs with
  `status == 3` in `x$data$dist_actions` (if the column exists). Default
  `TRUE`.

- na_to_zero:

  Logical. If `TRUE`, treat missing values as 0 when computing
  benefit/loss. Default `TRUE`.

- filter:

  Character. Filter rows by non-zero component:

  - `"any"`: keep both benefit and loss rows (default),

  - `"benefit"`: keep only rows with `benefit > 0`,

  - `"loss"`: keep only rows with `loss > 0`.

## Value

The updated `Data` object with `x$data$dist_effects` created/updated,
and metadata stored in `x$data$effects_meta`.

## Details

Internally, effects may originate from signed values (deltas) or from
after-action amounts. Regardless of input, `dist_effects` always stores
`benefit` and `loss` as non-negative values to avoid ambiguity and to
support models that separately account for gains and damages.

**Baseline and after-action amounts.** If `effect_type = "after"`,
provided values are interpreted as after-action amounts and converted to
signed deltas using the baseline amounts in
`x$data$dist_features$amount`: \$\$\mathrm{delta} = \mathrm{after} -
\mathrm{baseline}.\$\$ Missing baseline values are treated as 0.

**Supported effect specifications.**

1.  *NULL*: store an empty effects table (recommended default when
    effects are not available yet).

2.  *Multipliers*: a `data.frame(action, feature, multiplier)` that
    applies a signed multiplier to baseline amounts: \\\mathrm{delta} =
    \mathrm{amount} \times \mathrm{multiplier}\\.

3.  *Explicit rows*: a `data.frame(pu, action, feature, ...)` providing
    either signed deltas (`delta` or `effect`, or legacy signed
    `benefit`), after-action amounts (`after`), or already split
    non-negative `benefit`/`loss`.

4.  *Rasters per action*: a named list of
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    objects (names are action ids), each with one layer per feature.
    Raster values are aggregated by planning unit using
    `effect_aggregation` and then interpreted as deltas or after-action
    amounts depending on `effect_type`.

**Feasibility and locks.** Effects are retained only for feasible
`(pu, action)` pairs present in `x$data$dist_actions`. If
`drop_locked_out = TRUE` and `x$data$dist_actions$status` exists, pairs
with `status == 3` are excluded before effects are processed.

**Filtering.** You can keep only beneficial effects (`benefit > 0`) or
only losses (`loss > 0`) using `filter`. By default, rows with both
`benefit == 0` and `loss == 0` are dropped unless `keep_zero = TRUE`.

## See also

[`add_benefits`](https://josesalgr.github.io/mosap/reference/add_benefits.md),
[`add_losses`](https://josesalgr.github.io/mosap/reference/add_losses.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Empty effects (default)
p <- add_effects(p, effects = NULL)

# 2) Multipliers (action x feature): delta = amount * multiplier
mult <- data.frame(
  action = c("harvest", "harvest", "restoration"),
  feature = c("sp1", "sp2", "sp1"),      # feature names (requires x$data$features$name)
  multiplier = c(-0.2, -0.1, 0.3)
)
p <- add_effects(p, effects = mult, effect_type = "delta")

# 3) Explicit deltas by (pu, action, feature)
eff <- data.frame(
  pu = c(1, 1, 2),
  action = c("harvest", "harvest", "restoration"),
  feature = c(1, 2, 1),
  delta = c(-0.5, 0.2, 1.0)
)
p <- add_effects(p, effects = eff)

# 4) After-action amounts (converted to delta = after - baseline)
after_tbl <- transform(eff, after = delta) # example only; typically after is an absolute amount
p <- add_effects(p, effects = after_tbl, effect_type = "after")

# 5) Raster effects per action (one layer per feature)
# effects_rasters <- list(harvest = r_harv, restoration = r_rest) # terra::SpatRaster
# p <- add_effects(p, effects = effects_rasters, effect_type = "delta", effect_aggregation = "sum")

# Keep only beneficial effects
p <- add_effects(p, effects = eff, filter = "benefit")
} # }
```
