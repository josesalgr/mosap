# Add objective: minimize action fragmentation

Specify an objective that minimizes fragmentation at the action level
over a spatial relation. This objective is intended for models where
action allocations (rather than only PU selection) drive spatial
cohesion.

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built.

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_min_action_fragmentation(
  x,
  relation_name = "boundary",
  weight_multiplier = 1,
  action_weights = NULL,
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- relation_name:

  Character. Name of the spatial relation in `x$data$spatial_relations`.

- weight_multiplier:

  Numeric \\\ge 0\\. Multiplier applied to all relation weights. Default
  `1`.

- action_weights:

  Optional action weights. Either a named numeric vector (names = action
  ids) or a `data.frame(action, weight)`.

- actions:

  Optional subset of action ids to include in the objective.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.

## Details

Action-level fragmentation can optionally weight actions differently via
`action_weights` and can optionally restrict the objective to a subset
of actions via `actions`. The exact linearization and interpretation are
implemented in the model builder.
