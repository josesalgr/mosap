# Add objective: maximize profit

Specify an objective that maximizes economic profit from selected
`(pu, action)` pairs. Profit values are taken from `x$data$dist_profit`,
typically created with
[`add_profit`](https://josesalgr.github.io/mosap/reference/add_profit.md).

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built (typically when calling
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)).

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_max_profit(x, profit_col = "profit", alias = NULL)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).

- profit_col:

  Character. Column name in `x$data$dist_profit` containing numeric
  profits. Default `"profit"`.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.

## Details

The function updates `x$data$model_args` with:

- `model_type`:

  `"maximizeProfit"`

- `objective_id`:

  `"max_profit"`

- `objective_args`:

  a list with `profit_col`

If another objective setter is called afterwards, it overwrites the
active single-objective specification in `x$data$model_args`.
