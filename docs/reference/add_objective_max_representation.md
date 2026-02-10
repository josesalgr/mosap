# Add objective: maximize representation

Specify an objective that maximizes total representation across
features, using the z variables associated with (pu, feature) rows in
`dist_features`.

This is a data-only setter: it stores the objective specification inside
the `Data` object so it can be materialized later when the optimization
model is built.

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_max_representation(
  x,
  amount_col = "amount",
  features = NULL,
  internal = FALSE,
  alias = NULL
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial()`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- amount_col:

  Character. Column name in `dist_features` containing non-negative
  amounts.

- features:

  Optional subset of feature ids to include in the objective. Can be:

  - integer/numeric feature ids (matching `x$data$features$id`), or

  - character feature ids (matching `x$data$features$id` if those are
    character), or

  - internal feature indices (1..n_features) if `internal = TRUE`.

- internal:

  Logical. If `TRUE`, interpret `features` as internal feature indices.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

Updated `Data` object.
