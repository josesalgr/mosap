# Add action effects to a planning problem

Define the effects of management actions on features across planning
units.

Effects are stored in a canonical representation in
`x$data$dist_effects`, with one row per `(pu, action, feature)` triple
and two non-negative columns:

- `benefit`: the positive component of the effect,

- `loss`: the magnitude of the negative component of the effect.

The net effect is therefore interpreted as \$\$ \Delta\_{i a f} =
\mathrm{benefit}\_{i a f} - \mathrm{loss}\_{i a f}, \$\$ where \\i\\
indexes planning units, \\a\\ indexes actions, and \\f\\ indexes
features.

Under the semantics adopted by this package, each
`(pu, action, feature)` triple represents a single net effect.
Consequently, after validation and aggregation, a stored row cannot have
both `benefit > 0` and `loss > 0` at the same time.

## Usage

``` r
add_effects(
  x,
  effects = NULL,
  effect_type = c("delta", "after"),
  effect_aggregation = c("sum", "mean"),
  component = c("any", "benefit", "loss")
)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md).
  It must already contain `x$data$dist_actions`; run
  [`add_actions`](https://josesalgr.github.io/paretoscape/reference/add_actions.md)
  first.

- effects:

  Effect specification. One of:

  - `NULL`, to store an empty effects table,

  - a `data.frame(action, feature, multiplier)`,

  - a `data.frame(pu, action, feature, ...)` with explicit effects,

  - a named list of
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    objects, one per action.

- effect_type:

  Character string indicating how supplied effect values are
  interpreted. Must be one of:

  - `"delta"`: values represent signed net changes,

  - `"after"`: values represent after-action amounts and are converted
    to net changes relative to baseline feature amounts.

- effect_aggregation:

  Character string giving the aggregation used when converting raster
  values to planning-unit level. Must be one of `"sum"` or `"mean"`.

- component:

  Character string controlling which component of the canonical effects
  table is retained. Must be one of:

  - `"any"`: keep all non-zero rows,

  - `"benefit"`: keep only rows with `benefit > 0`,

  - `"loss"`: keep only rows with `loss > 0`.

## Value

An updated `Problem` object with:

- `x$data$dist_effects`:

  A canonical effects table with columns `pu`, `action`, `feature`,
  `benefit`, `loss`, `internal_pu`, `internal_action`,
  `internal_feature`, and optional labels such as `feature_name` and
  `action_name`.

- `x$data$effects_meta`:

  Metadata describing how effects were interpreted and stored.

## Details

This function provides a unified interface for specifying action effects
from several input formats while enforcing a single internal
representation. Regardless of how the user supplies the effects, the
stored output always follows the same canonical structure based on
non-negative `benefit`/`loss` components.

Let \\b\_{if}\\ denote the baseline amount of feature \\f\\ in planning
unit \\i\\, taken from `x$data$dist_features$amount`. Let \\\Delta\_{i a
f}\\ denote the net change caused by applying action \\a\\ in planning
unit \\i\\ to feature \\f\\. The canonical stored representation is:

\$\$ \mathrm{benefit}\_{i a f} = \max(\Delta\_{i a f}, 0), \$\$

\$\$ \mathrm{loss}\_{i a f} = \max(-\Delta\_{i a f}, 0). \$\$

Hence:

- if \\\Delta\_{i a f} \> 0\\, then `benefit > 0` and `loss = 0`,

- if \\\Delta\_{i a f} \< 0\\, then `benefit = 0` and `loss > 0`,

- if \\\Delta\_{i a f} = 0\\, then both are zero.

**Why split effects into benefit and loss?**

This representation avoids ambiguity in downstream optimization models.
It allows the package to support, for example, objectives that maximize
beneficial effects, minimize damages, impose no-net-loss conditions, or
combine both components differently in multi-objective formulations.

**Supported effect specifications**

The `effects` argument may be provided in one of the following forms:

1.  `NULL`. An empty effects table is stored.

2.  A `data.frame(action, feature, multiplier)`. In this case, effects
    are constructed by multiplying baseline feature amounts by the
    supplied multiplier: \$\$ \Delta\_{i a f} = b\_{if} \times m\_{a f},
    \$\$ where \\m\_{a f}\\ is the multiplier associated with action
    \\a\\ and feature \\f\\. This specification is expanded over all
    feasible `(pu, action)` pairs.

3.  A `data.frame(pu, action, feature, ...)` giving explicit effects for
    individual triples. The table may contain:

    - `delta` or `effect`: interpreted as signed net changes,

    - `after`: interpreted as after-action amounts if
      `effect_type = "after"`,

    - `benefit` and/or `loss`: explicit non-negative split components,

    - legacy signed `benefit` without `loss`: interpreted as a signed
      net effect for backwards compatibility.

4.  A named list of
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    objects, one per action. In this case, names must match action ids,
    and each raster must contain one layer per feature. Raster values
    are aggregated to planning-unit level using `effect_aggregation`.

**Interpretation of `effect_type`**

If `effect_type = "delta"`, supplied values are interpreted as net
changes directly.

If `effect_type = "after"`, supplied values are interpreted as
after-action amounts and converted internally to net effects using:

\$\$ \Delta\_{i a f} = \mathrm{after}\_{i a f} - b\_{if}. \$\$

Missing baseline values are treated as zero.

**Feasibility and locked-out decisions**

Effects are only retained for feasible `(pu, action)` pairs listed in
`x$data$dist_actions`. Thus,
[`add_actions()`](https://josesalgr.github.io/paretoscape/reference/add_actions.md)
must be called first. Pairs marked as locked out (`status == 3`) are
removed before storing the final effects table.

**Duplicate rows and semantic validation**

If multiple rows are supplied for the same `(pu, action, feature)`
triple, they are aggregated by summing `benefit` and `loss` separately.
The resulting triple must still respect the package semantics, namely
that both components cannot be strictly positive simultaneously. Inputs
violating this rule are rejected.

**Component selection**

After canonicalization and validation, rows can be restricted to:

- `component = "any"`: keep all non-zero effects,

- `component = "benefit"`: keep only rows with `benefit > 0`,

- `component = "loss"`: keep only rows with `loss > 0`.

Rows with `benefit = 0` and `loss = 0` are always removed.

**Raster handling**

When effects are supplied as rasters, they are automatically aligned to
the planning-unit raster or geometry when needed before extraction or
zonal aggregation.

**Stored output**

The resulting table `x$data$dist_effects` contains user-facing ids,
internal integer ids, and optional labels for actions and features.
Metadata describing the stored representation and input interpretation
are written to `x$data$effects_meta`.

## See also

[`add_actions`](https://josesalgr.github.io/paretoscape/reference/add_actions.md),
[`add_benefits`](https://josesalgr.github.io/paretoscape/reference/add_benefits.md),
[`add_losses`](https://josesalgr.github.io/paretoscape/reference/add_losses.md)

## Examples

``` r
# ------------------------------------------------------
# Minimal problem with actions
# ------------------------------------------------------
pu <- data.frame(
  id = 1:3,
  cost = c(1, 2, 3)
)

features <- data.frame(
  id = 1:2,
  name = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3),
  feature = c(1, 2, 1, 2),
  amount = c(10, 5, 8, 4)
)

p <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)

actions <- data.frame(
  id = c("conservation", "restoration"),
  name = c("Conservation", "Restoration")
)

p <- add_actions(
  x = p,
  actions = actions,
  cost = c(conservation = 2, restoration = 4)
)

print(p)
#> A paretoscape object (<Problem>)
#> ├─data
#> │├─planning units: <data.frame> (3 total)
#> │├─costs: min: 1, max: 3
#> │└─features: 2 total ("sp1", "sp2")
#> └─actions and effects
#> │├─actions: 2 total ("Conservation", "Restoration")
#> │├─dist_actions: 6 feasible rows
#> │├─action costs: min: 2, max: 4
#> │├─dist_effects: none specified
#> │└─dist_profit: none specified
#> └─spatial
#> │├─geometry: none
#> │├─pu_coords: none
#> │└─relations: none
#> └─targets and constraints
#> │├─targets: none
#> │├─area constraints: none
#> │├─pu_locks: none
#> │└─action_locks: none
#> └─model
#> │├─status: not built yet (will build in solve())
#> │├─objectives: none
#> │├─method: single-objective
#> │├─solver: not set (auto)
#> │└─checks: incomplete (no objective registered)
#> # ℹ Use `x$data` to inspect stored tables and model snapshots.

# ------------------------------------------------------
# 1) Empty effects
# ------------------------------------------------------
p0 <- add_effects(p, effects = NULL)
#> Warning: All effect values are zero after filtering.
p0$data$dist_effects
#>  [1] pu               action           feature          benefit         
#>  [5] loss             internal_pu      internal_action  internal_feature
#>  [9] feature_name     action_name     
#> <0 rows> (or 0-length row.names)

# ------------------------------------------------------
# 2) Multipliers by action and feature
# delta = baseline amount * multiplier
# ------------------------------------------------------
mult <- data.frame(
  action = c("conservation", "conservation",
             "restoration", "restoration"),
  feature = c("sp1", "sp2", "sp1", "sp2"),
  multiplier = c(0.10, 0.00, -0.20, 0.25)
)

p1 <- add_effects(
  x = p,
  effects = mult,
  effect_type = "delta"
)

p1$data$dist_effects
#>   pu       action feature benefit loss internal_pu internal_action
#> 1  1 conservation       1    1.00  0.0           1               1
#> 2  2 conservation       1    0.80  0.0           2               1
#> 3  1  restoration       1    0.00  2.0           1               2
#> 4  2  restoration       1    0.00  1.6           2               2
#> 7  1  restoration       2    1.25  0.0           1               2
#> 8  3  restoration       2    1.00  0.0           3               2
#>   internal_feature feature_name  action_name
#> 1                1          sp1 Conservation
#> 2                1          sp1 Conservation
#> 3                1          sp1  Restoration
#> 4                1          sp1  Restoration
#> 7                2          sp2  Restoration
#> 8                2          sp2  Restoration

# ------------------------------------------------------
# 3) Explicit net effects using signed deltas
# ------------------------------------------------------
eff_delta <- data.frame(
  pu = c(1, 2, 3, 3),
  action = c("conservation", "restoration", "restoration", "conservation"),
  feature = c(1, 1, 2, 2),
  delta = c(2, -3, 1, 0.5)
)

p2 <- add_effects(
  x = p,
  effects = eff_delta
)

p2$data$dist_effects
#>   pu       action feature benefit loss internal_pu internal_action
#> 1  1 conservation       1     2.0    0           1               1
#> 2  2  restoration       1     0.0    3           2               2
#> 3  3 conservation       2     0.5    0           3               1
#> 4  3  restoration       2     1.0    0           3               2
#>   internal_feature feature_name  action_name
#> 1                1          sp1 Conservation
#> 2                1          sp1  Restoration
#> 3                2          sp2 Conservation
#> 4                2          sp2  Restoration

# ------------------------------------------------------
# 4) Explicit after-action amounts
# delta = after - baseline
# ------------------------------------------------------
eff_after <- data.frame(
  pu = c(1, 2, 3),
  action = c("conservation", "restoration", "restoration"),
  feature = c(1, 1, 2),
  after = c(12, 5, 6)
)

p3 <- add_effects(
  x = p,
  effects = eff_after,
  effect_type = "after"
)

p3$data$dist_effects
#>   pu       action feature benefit loss internal_pu internal_action
#> 1  1 conservation       1       2    0           1               1
#> 2  2  restoration       1       0    3           2               2
#> 3  3  restoration       2       2    0           3               2
#>   internal_feature feature_name  action_name
#> 1                1          sp1 Conservation
#> 2                1          sp1  Restoration
#> 3                2          sp2  Restoration

# ------------------------------------------------------
# 5) Keep only beneficial effects
# ------------------------------------------------------
p4 <- add_effects(
  x = p,
  effects = eff_delta,
  component = "benefit"
)

p4$data$dist_effects
#>   pu       action feature benefit loss internal_pu internal_action
#> 1  1 conservation       1     2.0    0           1               1
#> 3  3 conservation       2     0.5    0           3               1
#> 4  3  restoration       2     1.0    0           3               2
#>   internal_feature feature_name  action_name
#> 1                1          sp1 Conservation
#> 3                2          sp2 Conservation
#> 4                2          sp2  Restoration
```
