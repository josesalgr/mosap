# Limit the number of actions per planning unit (maximum)

Store a constraint that limits the number of actions that can be
selected within each planning unit (PU). The constraint has the form:
\$\$\sum\_{a \in A} x\_{pu,a} \le \mathrm{max}\$\$ for each PU in the
selected set.

This function is **data-only**: it records the constraint specification
in the `Data` object but does not build or modify the optimization
model. The constraint is later translated into linear constraints by the
model builder (e.g., `.pa_build_model_apply_constraints()`).

## Usage

``` r
add_action_max_per_pu(
  x,
  max = 1L,
  pu = NULL,
  actions = NULL,
  overwrite = FALSE
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
  Must already contain actions (i.e., run
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  first).

- max:

  Integer scalar \\\ge 0\\. Maximum number of actions allowed per PU.
  Default is `1L`.

- pu:

  Optional integer vector of planning unit ids (external ids, i.e.,
  `x$data$pu$id`) to which the constraint is applied. If `NULL`
  (default), the constraint applies to all PUs.

- actions:

  Optional character vector of action ids (i.e., `x$data$actions$id`) to
  include in the sum. If `NULL` (default), all actions are included.

- overwrite:

  Logical. If `TRUE`, replace any existing `action_max_per_pu`
  constraint stored in `x$data$constraints`. If `FALSE` (default) and a
  constraint already exists, an error is raised.

## Value

The updated `Data` object with `x$data$constraints$action_max_per_pu`
set to a list containing the constraint specification.

## Details

The constraint can be applied to:

- all planning units and all actions (default), or

- a subset of planning units via `pu`, and/or

- a subset of actions via `actions`.

Note that the subset is applied over the existing feasible
`(pu, action)` pairs in `x$data$dist_actions`. If the requested
`pu`/`actions` subset yields zero feasible pairs, the function stops
with an informative error.

If `x$data$model_ptr` is already present (a model has been built
previously), the function marks the model as dirty by setting
`x$data$meta$model_dirty <- TRUE`, signalling that the model should be
rebuilt before solving.

## See also

[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Limit to at most 1 action per PU (default)
p <- add_action_max_per_pu(p, max = 1)

# Limit to at most 2 actions per PU
p <- add_action_max_per_pu(p, max = 2, overwrite = TRUE)

# Apply the limit only to a subset of PUs
p <- add_action_max_per_pu(p, max = 1, pu = c(1, 2, 3), overwrite = TRUE)

# Limit the number of actions among a subset of actions (e.g., only harvest-like actions)
p <- add_action_max_per_pu(p, max = 1, actions = c("harvest", "sustainable"), overwrite = TRUE)

# Combine PU and action subsets
p <- add_action_max_per_pu(
  p, max = 1,
  pu = c(10, 11, 12),
  actions = c("harvest", "sustainable"),
  overwrite = TRUE
)
} # }
```
