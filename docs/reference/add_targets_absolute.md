# Add absolute targets

Add feature-level absolute targets to a planning problem.

These targets are stored in `x$data$targets` and later translated into
linear constraints when the optimization model is built.

Absolute targets are interpreted directly in the same units as the
feature contributions used by the model.

Each call appends one or more target definitions to the problem. This
makes it possible to combine multiple target rules, including targets
associated with different action subsets.

## Usage

``` r
add_targets_absolute(x, targets, features = NULL, actions = NULL, label = NULL)
```

## Arguments

- x:

  A `Problem` object.

- targets:

  Target specification. This is interpreted as an absolute target value
  in the same units as the modelled feature contributions. It may be a
  scalar, vector, named vector, or `data.frame`. See Details.

- features:

  Optional feature specification indicating which features the supplied
  target values refer to when `targets` does not identify features
  explicitly. If `NULL`, all features are targeted.

- actions:

  Optional character vector indicating which actions count toward target
  achievement. Entries may match action ids, `action_set` labels, or
  both. If `NULL`, all actions count.

- label:

  Optional character string stored with the targets for reporting and
  bookkeeping.

## Value

An updated `Problem` object with absolute targets appended to
`x$data$targets`.

## Details

Let \\\mathcal{F}\\ denote the set of features. For each targeted
feature \\f \in \mathcal{F}\\, this function stores an absolute target
threshold \\T_f \ge 0\\.

When the optimization model is built, each such target is interpreted as
a lower-bound constraint of the form: \$\$ \sum\_{(i,a) \in
\mathcal{S}\_f} c\_{iaf} x\_{ia} \ge T_f, \$\$ where:

- \\x\_{ia}\\ indicates whether action \\a\\ is selected in planning
  unit \\i\\,

- \\c\_{iaf}\\ is the contribution of that action to feature \\f\\,

- \\\mathcal{S}\_f\\ is the set of planning unit–action pairs allowed to
  count toward the target.

In the absolute case, the stored target threshold is simply: \$\$ T_f =
t_f, \$\$ where \\t_f\\ is the user-supplied target value for feature
\\f\\.

The `actions` argument restricts which actions may contribute toward
achievement of the target, but it does not modify the value of \\T_f\\
itself.

The `targets` argument is parsed by `.pa_parse_targets()` and may be
supplied in several equivalent forms, including:

- a single numeric value recycled to all selected features,

- a numeric vector aligned to `features`,

- a named numeric vector where names identify features,

- a `data.frame` with `feature` and `target` columns.

If `targets` does not explicitly identify features:

- if `features = NULL`, the target is applied to all features,

- if `features` is supplied, the target values are interpreted with
  respect to that feature set.

Internally, targets added by this function are stored with:

- `type = "actions"`,

- `sense = "ge"`,

- `target_unit = "absolute"`,

- `target_raw = target_value`,

- `basis_total = NA`.

## See also

[`add_targets_relative`](https://josesalgr.github.io/paretoscape/reference/add_targets_relative.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Same absolute target for all features
p <- add_targets_absolute(p, 10)

# Different targets by feature id
p <- add_targets_absolute(
  p,
  c("1" = 5, "2" = 8, "3" = 12)
)

# Same target for a selected subset of features
p <- add_targets_absolute(
  p,
  5,
  features = c("sp1", "sp2")
)

# Only actions in the "recovery" set count toward target achievement
p <- add_targets_absolute(
  p,
  5,
  actions = "recovery"
)

# Combine target rules with different action subsets
p <- p |>
  add_targets_relative(0.1, actions = "conservation") |>
  add_targets_absolute(100, actions = "restoration")
} # }
```
