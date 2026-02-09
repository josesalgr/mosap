# Add objective: maximize benefit

Specify an objective that maximizes total benefit delivered by selected
actions. Benefit values are taken from the benefit table produced by
[`add_benefits`](https://josesalgr.github.io/mosap/reference/add_benefits.md)
(stored in `x$data$dist_benefit` and/or its model-ready variant).

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built (typically when calling
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)).

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_max_benefit(x, benefit_col = "benefit", alias = NULL)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- benefit_col:

  Character. Column name in the model-ready benefit table containing
  numeric benefits. Default `"benefit"`.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.

## Details

The function updates `x$data$model_args` with:

- `model_type`:

  `"maximizeBenefits"`

- `objective_id`:

  `"max_benefit"`

- `objective_args`:

  a list with `benefit_col`

The model builder will require benefit data to exist and will error if
benefits are missing. If another objective setter is called afterwards,
it overwrites the active single-objective specification in
`x$data$model_args`.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") |>
  add_actions(actions_df) |>
  add_benefits(benefits_df) |>
  add_objective_max_benefit(alias = "benefit")
} # }
```
