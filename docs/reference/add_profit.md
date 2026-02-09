# Add profit to a planning problem

Define (economic) profit values for feasible `(pu, action)` pairs and
store them in `x$data$dist_profit`. Profit is intentionally stored
separately from ecological `benefit`/`loss` (see
[`add_effects`](https://josesalgr.github.io/mosap/reference/add_effects.md)
/
[`add_benefits`](https://josesalgr.github.io/mosap/reference/add_benefits.md)).

## Usage

``` r
add_profit(x, profit = NULL, keep_zero = FALSE, na_to_zero = TRUE)
```

## Arguments

- x:

  A `Data` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md)
  or
  [`inputDataSpatial`](https://josesalgr.github.io/mosap/reference/inputDataSpatial.md).
  Must contain `x$data$dist_actions` and `x$data$actions` (run
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
  first).

- profit:

  Profit specification. One of:

  - `NULL`: profit is set to 0 for all feasible `(pu, action)` pairs.

  - A numeric scalar: recycled to all feasible `(pu, action)` pairs.

  - A named numeric vector: names are action ids; assigns a global
    profit per action.

  - A `data.frame(action, profit)`: assigns a global profit per action.

  - A `data.frame(pu, action, profit)`: assigns explicit profit values
    by pair.

- keep_zero:

  Logical. If `TRUE`, keep rows with `profit == 0` in the stored table.
  Default `FALSE` (zero-profit rows are dropped).

- na_to_zero:

  Logical. If `TRUE`, treat missing profit values as 0 after
  joins/matching. Default `TRUE`.

## Value

The updated `Data` object with `x$data$dist_profit` created/updated.

## Details

This function creates a profit table aligned to the current feasibility
table `x$data$dist_actions`. The resulting `x$data$dist_profit`
contains:

- `pu`: external planning unit id,

- `action`: action id,

- `profit`: numeric profit value,

- `internal_pu`: internal PU index,

- `internal_action`: internal action index.

Profit values can later be used to build objectives (e.g., maximize
profit or maximize net profit), budget constraints (e.g., net cost =
cost - profit), or reporting summaries.

The function is **data-only**: it does not build or modify the
optimization model. It also does not change feasibility; it simply
assigns profits to rows currently present in `dist_actions`. Any
additional filtering (e.g., dropping locked-out pairs) should be applied
when preparing model-ready tables (typically inside the model builder
invoked by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Default: profit = 0 everywhere (table may be empty if keep_zero = FALSE)
x <- x |> add_profit()

# 2) Constant profit for every feasible (pu, action)
x <- x |> add_profit(profit = 10)

# 3) Profit per action (named vector)
pr <- c(harvest = 50, sustainable = 20, restoration = -5)
x <- x |> add_profit(profit = pr)

# 4) Profit per action (data.frame)
pr_df <- data.frame(action = c("harvest", "sustainable"), profit = c(50, 20))
x <- x |> add_profit(profit = pr_df)

# 5) Profit per (pu, action) pair
pr_sp <- data.frame(
  pu = c(1, 2, 2),
  action = c("harvest", "harvest", "sustainable"),
  profit = c(100, 80, 30)
)
x <- x |> add_profit(profit = pr_sp, keep_zero = TRUE)
} # }
```
