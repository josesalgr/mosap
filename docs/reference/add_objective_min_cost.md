# Register an atomic objective (internal)

Internal helper used by objective setter functions to optionally
register an objective as an *atomic objective* for multi-objective
workflows.

If `alias` is non-`NULL`, the objective definition is stored in
`x$data$objectives[[alias]]`. This allows external multi-objective
orchestration (e.g., weighted sum, \\\epsilon\\-constraint, AUGMECON,
interactive methods) to refer to objectives by a stable user-facing
identifier.

The function is fully backward compatible with single-objective
workflows: when `alias` is `NULL`, no entry is added to
`x$data$objectives` and only the active single-objective specification
stored in `x$data$model_args` (set by the calling objective setter) is
used.

Specify an objective that minimizes total costs associated with the
solution. Costs may include planning-unit costs and/or action costs
depending on the flags provided.

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built (typically when calling
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)).

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows, while preserving the legacy single-objective behavior (the
most recently set objective remains the active one in
`x$data$model_args`).

## Usage

``` r
add_objective_min_cost(
  x,
  include_pu_cost = TRUE,
  include_action_cost = TRUE,
  alias = NULL
)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- include_pu_cost:

  Logical. If `TRUE`, include planning-unit costs in the objective.

- include_action_cost:

  Logical. If `TRUE`, include action costs in the objective.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

- objective_id:

  Character. Stable objective identifier (e.g., `"min_cost"`).

- model_type:

  Character. Model type label used by the model builder (e.g.,
  `"minimizeCosts"`).

- objective_args:

  List. Objective-specific arguments to be stored with the objective.

- sense:

  Character. Either `"min"` or `"max"`.

## Value

The updated `Data` object.

The updated `Data` object.

## Details

The function updates `x$data$model_args` with:

- `model_type`:

  `"minimizeCosts"`

- `objective_id`:

  `"min_cost"`

- `objective_args`:

  a list with `include_pu_cost` and `include_action_cost`

The model builder must interpret these fields to set the objective
coefficients.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") |>
  add_actions(actions_df) |>
  add_objective_min_cost()

# Register as atomic objective for multi-objective workflows
x <- x |> add_objective_min_cost(alias = "cost")
} # }
```
