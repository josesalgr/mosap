# Add profit to a planning problem

Define economic profit values for feasible planning unit–action pairs
and store them in a profit table.

Profit is stored separately from ecological effects. In particular,
`profit` is not the same as ecological `benefit` or `loss` as
represented in
[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md).
This separation allows the package to distinguish economic returns from
ecological consequences when building objectives, constraints, and
reporting summaries.

## Usage

``` r
add_profit(x, profit = NULL)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md).
  It must already contain feasible actions and an action catalogue; run
  [`add_actions`](https://josesalgr.github.io/multiscape/reference/add_actions.md)
  first.

- profit:

  Profit specification. One of:

  - `NULL`: profit is set to 0 for all feasible `(pu, action)` pairs,

  - a numeric scalar: recycled to all feasible pairs,

  - a named numeric vector: names are action ids and values define
    action-level profit,

  - a `data.frame(action, profit)` defining action-level profit,

  - a `data.frame(pu, action, profit)` defining pair-specific profit.

## Value

An updated `Problem` object with a stored profit table created or
replaced. The stored table contains columns `pu`, `action`, `profit`,
`internal_pu`, and `internal_action`, and includes only rows with
non-zero profit.

## Details

**When to use `add_profit()`.**

Use this function when economic returns, penalties, or other
action-specific financial values are part of the planning problem.
Typical downstream uses include objectives such as
[`add_objective_max_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_profit.md)
and
[`add_objective_max_net_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_net_profit.md).

Let \\\mathcal{I}\\ denote the set of planning units and \\\mathcal{A}\\
the set of actions. Let \\\mathcal{D} \subseteq \mathcal{I} \times
\mathcal{A}\\ denote the set of feasible planning unit–action pairs
currently stored in the problem.

This function assigns to each feasible pair \\(i,a) \in \mathcal{D}\\ a
numeric profit value \\\pi\_{ia} \in \mathbb{R}\\ and stores the result
in a profit table.

Thus, the stored table can be interpreted as a mapping \$\$ \pi :
\mathcal{D} \to \mathbb{R}, \$\$ where \\\pi\_{ia}\\ represents the
economic return associated with selecting action \\a\\ in planning unit
\\i\\.

Profit values may be positive, zero, or negative. Positive values
represent gains or revenues, zero represents no net profit contribution,
and negative values can be used to encode penalties or net economic
losses.

The stored table contains:

- `pu`: external planning-unit id,

- `action`: action id,

- `profit`: numeric profit value,

- `internal_pu`: internal planning-unit index,

- `internal_action`: internal action index.

**Supported input formats**

The `profit` argument may be specified in several ways:

- `NULL`: assign profit 0 to all feasible `(pu, action)` pairs,

- a numeric scalar: assign the same profit value to all feasible pairs,

- a named numeric vector: names are action ids, assigning one global
  profit value per action,

- a `data.frame(action, profit)`: assign one global profit value per
  action,

- a `data.frame(pu, action, profit)`: assign pair-specific profit
  values.

When action-level profit is supplied, the same profit value is assigned
to all feasible planning units for that action. When pair-specific
profit is supplied, only the listed `(pu, action)` pairs receive
explicit values; unmatched feasible pairs are interpreted as zero-profit
pairs.

**Storage behaviour**

This function stores only rows with non-zero profit values. Feasible
pairs whose final profit is zero are omitted from the stored profit
table. Missing values produced during matching or joins are treated as
zero before this filtering step. Therefore, the resulting table is a
sparse representation of economic returns over the feasible decision
space.

**Data-only behaviour**

This function is purely data-oriented. It does not build or modify the
optimization model, and it does not change feasibility. It simply
assigns profit values to rows already present in the feasible action
table.

In particular:

- it does not add new feasible `(pu, action)` pairs,

- it does not remove infeasible pairs,

- it does not apply solver-side filtering such as dropping locked-out
  decisions,

- it does not modify ecological effect tables.

Any such filtering is expected to occur later when model-ready tables
are prepared, typically during the build stage invoked by
[`solve()`](https://josesalgr.github.io/multiscape/reference/solve.md).

**Use in optimization**

Profit values stored by this function can later be used in objectives
such as
[`add_objective_max_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_profit.md)
or
[`add_objective_max_net_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_net_profit.md),
in derived budget expressions, or in reporting and summary functions.

For example, if \\x\_{ia} \in \\0,1\\\\ denotes whether action \\a\\ is
selected in planning unit \\i\\, then a profit-maximization objective
typically takes the form \$\$ \max \sum\_{(i,a) \in \mathcal{D}}
\pi\_{ia} x\_{ia}. \$\$

## See also

[`add_actions`](https://josesalgr.github.io/multiscape/reference/add_actions.md),
[`add_objective_max_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_profit.md),
[`add_objective_max_net_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_net_profit.md),
[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md)

## Examples

``` r
# Minimal problem
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4)
)

features <- data.frame(
  id = 1:2,
  name = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 4),
  feature = c(1, 2, 1, 2, 1, 2),
  amount = c(1, 2, 1, 3, 2, 1)
)

p <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
)

p <- add_actions(
  x = p,
  actions = data.frame(id = c("harvest", "restoration"))
)

# 1) Constant profit for every feasible (pu, action)
p1 <- add_profit(p, profit = 10)
p1$data$dist_profit
#>   pu      action profit internal_pu internal_action
#> 1  1     harvest     10           1               1
#> 5  1 restoration     10           1               2
#> 2  2     harvest     10           2               1
#> 6  2 restoration     10           2               2
#> 3  3     harvest     10           3               1
#> 7  3 restoration     10           3               2
#> 4  4     harvest     10           4               1
#> 8  4 restoration     10           4               2

# 2) Profit per action using a named vector
pr <- c(harvest = 50, restoration = -5)
p2 <- add_profit(p, profit = pr)
p2$data$dist_profit
#>   pu      action profit internal_pu internal_action
#> 1  1     harvest     50           1               1
#> 5  1 restoration     -5           1               2
#> 2  2     harvest     50           2               1
#> 6  2 restoration     -5           2               2
#> 3  3     harvest     50           3               1
#> 7  3 restoration     -5           3               2
#> 4  4     harvest     50           4               1
#> 8  4 restoration     -5           4               2

# 3) Profit per action using a data frame
pr_df <- data.frame(
  action = c("harvest", "restoration"),
  profit = c(40, 15)
)
p3 <- add_profit(p, profit = pr_df)
p3$data$dist_profit
#>   pu      action profit internal_pu internal_action
#> 1  1     harvest     40           1               1
#> 2  1 restoration     15           1               2
#> 3  2     harvest     40           2               1
#> 4  2 restoration     15           2               2
#> 5  3     harvest     40           3               1
#> 6  3 restoration     15           3               2
#> 7  4     harvest     40           4               1
#> 8  4 restoration     15           4               2

# 4) Profit per (pu, action) pair
pr_pair <- data.frame(
  pu = c(1, 2, 3),
  action = c("harvest", "harvest", "restoration"),
  profit = c(100, 80, 30)
)
p4 <- add_profit(p, profit = pr_pair)
p4$data$dist_profit
#>   pu      action profit internal_pu internal_action
#> 1  1     harvest    100           1               1
#> 3  2     harvest     80           2               1
#> 6  3 restoration     30           3               2
```
