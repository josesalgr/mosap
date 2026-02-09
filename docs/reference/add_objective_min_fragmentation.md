# Add objective: minimize fragmentation (PU cut)

Specify an objective that minimizes spatial fragmentation measured as
the weighted cut between selected and non-selected planning units over a
spatial relation: \$\$\sum\_{(i,j)} w\_{ij}\\\|z_i - z_j\|.\$\$

In the model builder, this objective is linearized using auxiliary edge
variables. Relation weights \\w\_{ij}\\ are read from
`x$data$spatial_relations[[relation_name]]` and can be scaled by
`weight_multiplier` (BLM-like scaling).

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built.

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_min_fragmentation(
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

  Character. Name of the spatial relation in `x$data$spatial_relations`
  (e.g., `"boundary"`, `"rook"`, `"queen"`, `"knn"`). Default
  `"boundary"`.

- weight_multiplier:

  Numeric \\\ge 0\\. Multiplier applied to all relation weights. Default
  `1`.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.
