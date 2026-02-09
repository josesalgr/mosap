# Add actions to a planning problem

Define the set of management actions, where they can be implemented
(feasibility), their costs, and optional lock status per `(pu, action)`
pair. The function stores the action catalogue in `x$data$actions` and
the feasible action distribution in `x$data$dist_actions`.

Feasibility is controlled via `include` / `exclude`:

- If `include` is `NULL` and `feasible_default = TRUE`, all
  `(pu, action)` pairs are feasible.

- If `include` is provided, only those `(pu, action)` pairs are
  feasible.

- If `exclude` is provided, those `(pu, action)` pairs are removed from
  the feasible set (either the full set or `include`).

## Usage

``` r
add_actions(
  x,
  actions,
  include = NULL,
  exclude = NULL,
  cost = NULL,
  status = NULL,
  feasible_default = TRUE,
  na_is_infeasible = TRUE,
  sort_actions = TRUE
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
  Must contain at least `x$data$pu`, `x$data$features`, and
  `x$data$dist_features`.

- actions:

  A `data.frame` defining the action catalogue. Must contain a unique
  `id` column (action identifiers). For backwards compatibility, a
  column named `action` is also accepted and will be renamed to `id`.
  Additional columns are kept.

- include:

  Optional feasibility specification. If provided, only these
  `(pu, action)` pairs are feasible. Accepts `NULL`, a `data.frame`, or
  a named list (see Details).

- exclude:

  Optional infeasibility specification. Removed from the feasible set
  defined by `include` (or from the full set if `include` is `NULL` and
  `feasible_default = TRUE`). Accepts the same formats as `include`.

- cost:

  Optional cost specification for feasible `(pu, action)` pairs.
  Accepts:

  - `NULL`: default cost = 1 for all pairs,

  - a single numeric scalar,

  - a named numeric vector (names = action ids),

  - a `data.frame(action, cost)` or `data.frame(pu, action, cost)`.

- status:

  Optional `data.frame` with columns `pu`, `action`, and `status`
  specifying lock status for feasible pairs. Status must be in
  `{0, 2, 3}`.

- feasible_default:

  Logical. If `include` is `NULL`, should all actions be feasible in all
  planning units? If `FALSE` and `include` is `NULL`, the function
  errors.

- na_is_infeasible:

  Logical. Only relevant when `include`/`exclude` are provided as
  `data.frame`s with a `feasible` column. If `TRUE`, treat `NA` as
  `FALSE`.

- sort_actions:

  Logical. If `TRUE`, sort `actions` by `id` before assigning internal
  ids.

## Value

The updated `Data` object with:

- `x$data$actions`: action catalogue including `internal_id`,

- `x$data$dist_actions`: feasible `(pu, action)` pairs with columns
  `pu`, `action`, `cost`, `status`, `internal_pu`, and
  `internal_action`,

- `x$data$index$pu` and `x$data$index$action`: id-to-internal-id
  mappings.

The updated
[data](https://josesalgr.github.io/mosap/reference/data-class.md)
object.

## Details

**Accepted formats for `include` and `exclude`:**

- `NULL`: no restriction (only valid for `include` when
  `feasible_default = TRUE`).

- A `data.frame` with columns `pu` and `action`. An optional `feasible`
  column is supported as a filter (only rows with `feasible == TRUE` are
  used). If `na_is_infeasible = TRUE`, `NA` values in `feasible` are
  treated as `FALSE`. Action ids must match `actions$id`, and PU ids
  must match `x$data$pu$id`.

- A named list with names equal to action ids. Each element can be:

  - an integer vector of PU ids, or

  - an `sf` object defining a spatial zone for that action.

**Spatial feasibility:** when `include`/`exclude` are provided as `sf`
layers, feasibility is computed using spatial predicates (no geometries
are cut). Specifically, planning units in `x$data$pu_sf` are matched to
zones via
[`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).
This procedure does not create new planning unit geometries.

**Costs:** costs can be specified as a single scalar applied to all
feasible pairs, a named numeric vector by action id, or a `data.frame`
providing costs by action (`action, cost`) or by pair
(`pu, action, cost`). Costs must be finite and non-negative.

**Locks:** `status` can set the lock status for specific feasible pairs.
Status must be one of `0` (free), `2` (locked-in), or `3` (locked-out).
If `x$data$pu` contains a logical `locked_out` column, then all action
pairs in those PUs are forced to `status = 3`.

## See also

[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md),
[`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Action catalogue
actions_df <- data.frame(id = c("harvest", "sustainable", "restoration"))

# 1) Full feasibility (all actions everywhere), constant cost
p <- add_actions(p, actions = actions_df, cost = 10)

# 2) Only allow some pairs (include)
include_df <- data.frame(
  pu = c(1, 2, 10),
  action = c("harvest", "harvest", "restoration")
)
p <- add_actions(p, actions = actions_df, include = include_df, cost = 10)

# 3) Allow everything except some impossible pairs (exclude)
exclude_df <- data.frame(pu = c(1, 2, 3, 5), action = "harvest")
p <- add_actions(p, actions = actions_df, exclude = exclude_df, cost = 10)

# 4) Feasibility as a named list (PU ids per action)
include_list <- list(
  harvest = c(1, 2, 3),
  restoration = c(10, 11)
)
p <- add_actions(p, actions = actions_df, include = include_list)

# 5) Action-specific costs (named vector)
costs <- c(harvest = 5, sustainable = 2, restoration = 8)
p <- add_actions(p, actions = actions_df, cost = costs)

# 6) Lock status for specific pairs
st <- data.frame(pu = c(1, 10), action = c("harvest", "restoration"), status = c(3L, 2L))
p <- add_actions(p, actions = actions_df, status = st)
} # }
```
