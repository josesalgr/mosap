# Add objective: minimize intervention fragmentation

Specify an objective that minimizes fragmentation at the intervention
level over a spatial relation. This objective is intended for models
where interventions represent a coarser grouping of actions, and spatial
cohesion is evaluated at that grouping level.

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built.

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_min_intervention_fragmentation(
  x,
  relation_name = "boundary",
  weight_multiplier = 1,
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

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.
