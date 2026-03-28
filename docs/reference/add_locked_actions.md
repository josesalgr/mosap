# Add locked action decisions to a planning problem

Fix feasible planning unit–action decisions to be selected or excluded.

This function modifies the status of existing feasible `(pu, action)`
pairs stored in `x$data$dist_actions`. It does not create new feasible
action pairs and therefore must be used only after
[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md)
has been called.

Locked decisions are represented through status codes:

- `0`: free decision,

- `2`: locked in,

- `3`: locked out.

## Usage

``` r
add_locked_actions(x, locked_in = NULL, locked_out = NULL)
```

## Arguments

- x:

  A `Problem` object with action feasibility already defined via
  [`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md).

- locked_in:

  Optional specification of feasible `(pu, action)` pairs that must be
  selected. It may be `NULL`, a `data.frame`, or a named list.

- locked_out:

  Optional specification of feasible `(pu, action)` pairs that must not
  be selected. It may be `NULL`, a `data.frame`, or a named list.

## Value

An updated `Problem` object in which `x$data$dist_actions$status` has
been modified to reflect locked-in and locked-out decisions.

## Details

Let \\\mathcal{F} \subseteq \mathcal{P} \times \mathcal{A}\\ denote the
set of feasible planning unit–action pairs already defined in
`x$data$dist_actions`, where \\\mathcal{P}\\ is the set of planning
units and \\\mathcal{A}\\ is the set of actions.

This function allows the user to define two subsets:

- \\\mathcal{L}^{in} \subseteq \mathcal{F}\\, the set of feasible pairs
  that must be selected,

- \\\mathcal{L}^{out} \subseteq \mathcal{F}\\, the set of feasible pairs
  that must not be selected.

These sets are encoded by updating the `status` column of
`x$data$dist_actions`. The function validates that all requested
locked-in and locked-out pairs are already feasible. Therefore, it
cannot be used to introduce new planning unit–action combinations into
the problem.

In optimization terms, if \\x\_{ia}\\ denotes the decision variable
associated with planning unit \\i\\ and action \\a\\, then:

- locked-in pairs conceptually impose \\x\_{ia} = 1\\,

- locked-out pairs conceptually impose \\x\_{ia} = 0\\.

The exact translation into solver-side constraints occurs later when the
model is built.

**Accepted formats**

Both `locked_in` and `locked_out` accept the same formats:

- `NULL`,

- a `data.frame` with columns `pu` and `action`, optionally including a
  `feasible` column used as a filter,

- a named list whose names are action ids and whose elements are either
  vectors of planning unit ids or `sf` objects.

If a `feasible` column is supplied in a `data.frame`, only rows with
`feasible = TRUE` are used. Missing values in `feasible` are treated as
`FALSE`.

If an `sf` specification is supplied, the problem object must contain
`x$data$pu_sf`, and planning units are matched spatially using
[`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

**Conflict checking**

A given `(pu, action)` pair cannot be simultaneously requested in both
`locked_in` and `locked_out`. Such overlaps are rejected.

In addition, if a planning unit is already marked as locked out at the
planning-unit level through `x$data$pu$locked_out`, then all feasible
actions in that planning unit are forced to `status = 3`. Any attempt to
lock in an action within such a planning unit raises an error.

**Order of precedence**

User-supplied locked-in and locked-out action requests are first applied
to `x$data$dist_actions`. Afterwards, any planning-unit-level
`locked_out` flag stored in `x$data$pu` is enforced, overriding
action-level status and ensuring consistency with planning-unit
exclusions.

## See also

[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_locked_pu`](https://josesalgr.github.io/mosap/reference/add_locked_pu.md)

## Examples

``` r
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4)
)

features <- data.frame(
  id = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 4),
  feature = c("sp1", "sp2", "sp1", "sp2", "sp1", "sp2"),
  amount = c(1, 2, 1, 3, 2, 1)
)

p <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)
#> Error: features$id must be numeric/integer ids (got non-numeric strings).

p <- add_actions(
  x = p,
  actions = data.frame(id = c("conservation", "restoration")),
  cost = c(conservation = 3, restoration = 8)
)
#> Error: object 'p' not found

# Lock a few feasible decisions
p <- add_locked_actions(
  x = p,
  locked_in = data.frame(
    pu = c(1, 2),
    action = c("conservation", "restoration")
  ),
  locked_out = data.frame(
    pu = c(4),
    action = c("conservation")
  )
)
#> Error: object 'p' not found

p$data$dist_actions
#> Error: object 'p' not found

# Named-list interface
p2 <- add_locked_actions(
  x = p,
  locked_in = list(
    conservation = c(1, 3)
  ),
  locked_out = list(
    restoration = c(2)
  )
)
#> Error: object 'p' not found

p2$data$dist_actions
#> Error: object 'p2' not found
```
