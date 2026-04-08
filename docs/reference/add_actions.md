# Add management actions to a planning problem

Define the action catalogue, the set of feasible planning unit–action
pairs, and their implementation costs.

This function adds two core components to a `Problem` object. First, it
stores the action catalogue. Second, it creates the feasible planning
unit–action table, including implementation costs, status codes, and
internal indices used by the optimization backend.

Conceptually, if \\\mathcal{I}\\ is the set of planning units and
\\\mathcal{A}\\ is the set of actions, this function determines which
pairs \\(i,a) \in \mathcal{I} \times \mathcal{A}\\ are feasible
decisions and assigns a non-negative implementation cost to each
feasible pair.

## Usage

``` r
add_actions(
  x,
  actions,
  include_pairs = NULL,
  exclude_pairs = NULL,
  cost = NULL
)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md).

- actions:

  A `data.frame` defining the action catalogue. It must contain a unique
  `id` column. A column named `action` is also accepted and
  automatically renamed to `id`.

- include_pairs:

  Optional specification of feasible `(pu, action)` pairs. It can be
  `NULL`, a `data.frame` with columns `pu` and `action` (optionally also
  `feasible`), or a named list whose names are action ids and whose
  elements are vectors of planning unit ids or `sf` objects.

- exclude_pairs:

  Optional specification of infeasible `(pu, action)` pairs. It uses the
  same formats as `include_pairs` and removes matching pairs from the
  feasible set.

- cost:

  Optional cost specification for feasible pairs. It may be `NULL`, a
  scalar numeric value, a named numeric vector indexed by action id, or
  a `data.frame` with columns `action, cost` or `pu, action, cost`.

## Value

An updated `Problem` object with:

- `actions`:

  The action catalogue, including a unique integer `internal_id` for
  each action.

- `dist_actions`:

  The feasible planning unit–action table with columns `pu`, `action`,
  `cost`, `status`, `internal_pu`, and `internal_action`.

- `pu index`:

  A mapping from user-supplied planning-unit ids to internal integer
  ids.

- `action index`:

  A mapping from action ids to internal integer ids.

## Details

**When to use `add_actions()`.**

Use this function when you want to move from a planning problem defined
only by planning units and features to a problem in which decisions are
explicitly represented as actions applied in planning units.

**Action catalogue.**

The `actions` argument must be a `data.frame` with a unique `id` column
identifying each action. If a column named `action` is supplied instead,
it is renamed internally to `id`. Additional columns are preserved. If
no `name` column is provided, action labels are taken from `id`. If an
`action_set` column is present, it is also preserved and can later be
used to refer to groups of actions.

Actions are stored sorted by `id` to ensure reproducible internal
indexing.

**Feasible planning unit–action pairs.**

Feasibility is controlled through `include_pairs` and `exclude_pairs`.

If `include_pairs = NULL`, all possible `(pu, action)` pairs are
initially considered feasible, that is, all pairs \\(i,a) \in
\mathcal{I} \times \mathcal{A}\\.

If `include_pairs` is supplied, only those pairs are retained. If
`exclude_pairs` is also supplied, matching pairs are removed afterwards.

More precisely, let \\\mathcal{D}^{\mathrm{inc}}\\ denote the set of
included planning unit–action pairs and let
\\\mathcal{D}^{\mathrm{exc}}\\ denote the set of excluded pairs.

If `include_pairs = NULL`, the feasible decision set is: \$\$ \\(i,a) :
i \in \mathcal{I},\\ a \in \mathcal{A}\\ \setminus
\mathcal{D}^{\mathrm{exc}}. \$\$

If `include_pairs` is supplied, the feasible decision set is: \$\$
\mathcal{D}^{\mathrm{inc}} \setminus \mathcal{D}^{\mathrm{exc}}. \$\$

Both `include_pairs` and `exclude_pairs` can be specified as:

- `NULL`,

- a `data.frame` with columns `pu` and `action`,

- or a named list whose names are action ids.

When supplied as a `data.frame`, the object must contain columns `pu`
and `action`. An optional logical-like column `feasible` may also be
provided; only rows with `feasible = TRUE` are retained. Missing values
in `feasible` are treated as `FALSE`.

When supplied as a named list, names must match action ids. Each element
may contain either:

- a vector of planning-unit ids, or

- an `sf` object defining the spatial zone where the action is feasible.

In the spatial case, feasible planning units are identified using
[`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
against the stored planning-unit geometry.

**Feasibility versus decision fixing.**

This function only determines whether a pair \\(i,a)\\ exists in the
model. It does not force a feasible action to be selected or forbidden
beyond structural infeasibility. Fixed decisions should instead be
imposed later with
[`add_constraint_locked_actions`](https://josesalgr.github.io/multiscape/reference/add_constraint_locked_actions.md).

**Costs.**

Costs can be supplied in several ways:

- If `cost = NULL`, all feasible pairs receive a default cost of `1`.

- If `cost` is a scalar, that value is assigned to all feasible pairs.

- If `cost` is a named numeric vector, names must match action ids and
  costs are assigned by action.

- If `cost` is a `data.frame`, it must define either:

  - action-level costs through columns `action` and `cost`, or

  - pair-specific costs through columns `pu`, `action`, and `cost`.

In all cases, costs must be finite and non-negative.

In practice, a scalar cost is useful when all actions cost the same
everywhere, a named vector is useful when cost depends only on action
type, and a `(pu, action, cost)` table is useful when cost varies by
both planning unit and action.

**Status values.**

Internally, all feasible pairs are initialized with `status = 0`,
meaning that the decision is free. If planning units have already been
marked as locked out, then all feasible actions in those planning units
are assigned `status = 3`. This preserves consistency with planning-unit
exclusions already stored in the problem.

**Replacement behaviour.**

Calling `add_actions()` replaces any previous action catalogue and
feasible action table stored in the problem object.

After defining actions, typical next steps include adding effects,
optional decision-fixing constraints, objectives, and solver settings
before calling
[`solve()`](https://josesalgr.github.io/multiscape/reference/solve.md).

## See also

[`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md),
[`add_constraint_locked_actions`](https://josesalgr.github.io/multiscape/reference/add_constraint_locked_actions.md)

## Examples

``` r
# ------------------------------------------------------
# Minimal planning problem
# ------------------------------------------------------
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

actions <- data.frame(
  id = c("conservation", "restoration"),
  name = c("Conservation", "Restoration")
)

# Example 1: all actions feasible in all planning units
p1 <- add_actions(
  x = p,
  actions = actions,
  cost = c(conservation = 5, restoration = 12)
)

print(p1)
#> A multiscape object (<Problem>)
#> ├─data
#> │├─planning units: <data.frame> (4 total)
#> │├─costs: min: 1, max: 4
#> │└─features: 2 total ("sp1", "sp2")
#> └─actions and effects
#> │├─actions: 2 total ("Conservation", "Restoration")
#> │├─feasible action pairs: 8 feasible rows
#> │├─action costs: min: 5, max: 12
#> │├─effect data: none
#> │└─profit data: none
#> └─spatial
#> │├─geometry: none
#> │├─coordinates: none
#> │└─relations: none
#> └─targets and constraints
#> │├─targets: none
#> │├─area constraints: none
#> │├─budget constraints: none
#> │├─planning-unit locks: none
#> │└─action locks: none
#> └─model
#> │├─status: not built yet (will build in solve())
#> │├─objectives: none
#> │├─method: single-objective
#> │├─solver: not set (auto)
#> │└─checks: incomplete (no objective registered)
#> # ℹ Use `x$data` to inspect stored tables and model snapshots.
utils::head(p1$data$dist_actions)
#>   pu       action cost status internal_pu internal_action
#> 1  1 conservation    5      0           1               1
#> 5  1  restoration   12      0           1               2
#> 2  2 conservation    5      0           2               1
#> 6  2  restoration   12      0           2               2
#> 3  3 conservation    5      0           3               1
#> 7  3  restoration   12      0           3               2

# Example 2: specify feasible pairs explicitly
include_df <- data.frame(
  pu = c(1, 2, 3, 4),
  action = c("conservation", "conservation", "restoration", "restoration")
)

p2 <- add_actions(
  x = p,
  actions = actions,
  include_pairs = include_df,
  cost = 10
)

p2$data$dist_actions
#>   pu       action cost status internal_pu internal_action
#> 1  1 conservation   10      0           1               1
#> 2  2 conservation   10      0           2               1
#> 3  3  restoration   10      0           3               2
#> 4  4  restoration   10      0           4               2

# Example 3: remove selected pairs after full expansion
exclude_df <- data.frame(
  pu = c(2, 4),
  action = c("restoration", "conservation")
)

p3 <- add_actions(
  x = p,
  actions = actions,
  exclude_pairs = exclude_df,
  cost = c(conservation = 3, restoration = 8)
)

p3$data$dist_actions
#>   pu       action cost status internal_pu internal_action
#> 1  1 conservation    3      0           1               1
#> 5  1  restoration    8      0           1               2
#> 2  2 conservation    3      0           2               1
#> 3  3 conservation    3      0           3               1
#> 7  3  restoration    8      0           3               2
#> 8  4  restoration    8      0           4               2
```
