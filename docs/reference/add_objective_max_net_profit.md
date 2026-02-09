# Add objective: maximize net profit

Specify an objective that maximizes net profit, defined as total profit
from selected `(pu, action)` pairs minus total costs: \$\$\sum
\mathrm{profit}\\x \\-\\ \left(\sum \mathrm{pu\\cost}\\w + \sum
\mathrm{action\\cost}\\x\right).\$\$

Profit is taken from `x$data$dist_profit` (created with
[`add_profit`](https://josesalgr.github.io/mosap/reference/add_profit.md)).
Planning-unit costs are taken from `x$data$pu`; action costs are taken
from `x$data$dist_actions`.

This function is **data-only**: it stores the objective specification
inside the `Data` object so it can be materialized later when the
optimization model is built.

If `alias` is provided, the objective is also registered in
`x$data$objectives` as an atomic objective for multi-objective
workflows.

## Usage

``` r
add_objective_max_net_profit(
  x,
  profit_col = "profit",
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

- profit_col:

  Character. Column name in `x$data$dist_profit` containing numeric
  profits. Default `"profit"`.

- include_pu_cost:

  Logical. If `TRUE`, subtract planning-unit costs.

- include_action_cost:

  Logical. If `TRUE`, subtract action costs.

- alias:

  Character scalar or `NULL`. Optional identifier to register this
  objective as an atomic objective for multi-objective workflows.

## Value

The updated `Data` object.

## Details

The function updates `x$data$model_args` with:

- `model_type`:

  `"maximizeNetProfit"`

- `objective_id`:

  `"max_net_profit"`

- `objective_args`:

  a list with `profit_col`, `include_pu_cost`, and `include_action_cost`

If another objective setter is called afterwards, it overwrites the
active single-objective specification in `x$data$model_args`.
