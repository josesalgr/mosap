# Set the epsilon-constraint multi-objective method

Configure a `Problem` object to be solved with the epsilon-constraint
multi-objective method.

In this method, one objective is designated as the *primary* objective
and is optimized directly, while the remaining objectives are
transformed into \\\varepsilon\\-constraints.

Two operating modes are supported:

- **manual mode**: the user supplies the \\\varepsilon\\-levels
  explicitly,

- **automatic mode**: the \\\varepsilon\\-levels are generated later
  during
  [`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)
  from extreme-point or payoff information.

This function does not solve the problem. It stores the method
configuration in `x$data$method`, to be used later by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## Usage

``` r
set_method_epsilon_constraint(
  x,
  primary,
  eps = NULL,
  aliases = NULL,
  mode = c("manual", "auto"),
  n_points = 10,
  include_extremes = TRUE,
  lexicographic = TRUE,
  lexicographic_tol = 1e-08
)
```

## Arguments

- x:

  A `Problem` object.

- primary:

  Character string giving the alias of the primary objective to optimize
  directly.

- eps:

  Optional epsilon specification used only in `mode = "manual"`. It may
  be:

  - a named numeric vector, defining epsilon values for a single run,

  - or a named list of numeric vectors, defining epsilon values for a
    grid of runs.

  Names must correspond exactly to the constrained objective aliases.

- aliases:

  Optional character vector of objective aliases to include. By default,
  all registered objective aliases are used. The value of `primary` must
  be included in `aliases`.

- mode:

  Character string. Must be either `"manual"` or `"auto"`.

- n_points:

  Integer scalar used only in `mode = "auto"`. Number of epsilon points
  to generate automatically for the constrained objective. Must be at
  least 2.

- include_extremes:

  Logical scalar used only in `mode = "auto"`. If `TRUE`, include
  extreme epsilon values in the automatically generated grid.

- lexicographic:

  Logical scalar used only in `mode = "auto"`. If `TRUE`, compute
  extreme points lexicographically.

- lexicographic_tol:

  Numeric scalar \\\ge 0\\. Tolerance used in lexicographic
  extreme-point computation.

## Value

An updated `Problem` object with the epsilon-constraint method
configuration stored in `x$data$method`.

In manual mode, `x$data$method$runs` contains the explicit
epsilon-design grid.

In automatic mode, `x$data$method$runs` is `NULL` until the grid is
generated later during
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## Details

Use this method when one objective should be optimized directly while
the remaining objectives are controlled through explicit performance
thresholds.

**General idea**

Suppose that \\m \ge 2\\ objective functions have already been
registered in the problem: \$\$ f_1(x), f_2(x), \dots, f_m(x). \$\$

The epsilon-constraint method selects one of them as the primary
objective, say \\f_p(x)\\, and treats the remaining objectives as
constrained objectives.

For a fixed vector of epsilon levels, the method solves subproblems in
which the primary objective is optimized directly and the remaining
objectives are imposed through epsilon constraints.

A representative formulation is:

\$\$ \max \\ f_p(x) \$\$

subject to

\$\$ f_k(x) \ge \varepsilon_k, \qquad k \in \mathcal{C}, \$\$

together with all original feasibility constraints of the planning
problem, where \\\mathcal{C}\\ is the set of constrained objectives.

Depending on objective sense, the internal implementation may transform
minimization objectives into equivalent constrained forms, but the
method always follows the same principle:

- one objective is optimized directly,

- all remaining objectives are imposed through
  \\\varepsilon\\-constraints.

By solving the problem repeatedly for different epsilon levels, the
method generates a set of efficient trade-off solutions.

**Atomic objectives requirement**

The epsilon-constraint method can only be used with atomic objectives
that have already been registered under aliases. These aliases are
typically created by calling objective setters with an `alias` argument,
for example:

    x <- x |>
      add_objective_max_benefit(alias = "benefit") |>
      add_objective_min_cost(alias = "cost") |>
      add_objective_min_fragmentation(alias = "frag")

The `primary` argument selects which registered objective is optimized
directly. The remaining aliases are treated as constrained objectives.

**Manual mode**

In `mode = "manual"`, the user must provide `eps`.

The `eps` argument can be supplied as:

- a named numeric vector, defining a single run,

- or a named list of numeric vectors, defining a grid of runs.

The names of `eps` must correspond exactly to the constrained objective
aliases, that is, to all aliases in `aliases` except `primary`.

If the constrained objectives are \\\mathcal{C} = \\c_1, \dots, c_q\\\\,
then manual mode creates a design grid containing all combinations of
the supplied epsilon levels for the constrained objectives.

Each row of this grid defines one subproblem to be solved later.

**Important:** manual mode supports **two or more objectives**. In
particular, it can be used with:

- 2 objectives: 1 primary + 1 constrained objective,

- 3 or more objectives: 1 primary + multiple constrained objectives.

Thus, manual mode is the general way to use the epsilon-constraint
method when more than two objectives are involved.

In manual mode, the generated design grid is stored immediately in
`x$data$method$runs`. Its epsilon columns are named `eps_<alias>`, for
example `eps_frag`.

**Automatic mode**

In `mode = "auto"`, the user omits `eps` and instead supplies
`n_points`.

In this case, the epsilon grid is not built immediately. Instead, it is
constructed later during
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)
using extreme-point or payoff-table information.

In the current implementation, automatic mode supports **exactly two
objectives only**:

- one primary objective,

- one constrained objective.

Therefore, if `mode = "auto"`, then `aliases` must contain exactly two
objective aliases. Problems with three or more objectives must use
`mode = "manual"`.

If `include_extremes = TRUE`, the automatically generated grid includes
the extreme values of the constrained objective. Otherwise, only
interior values are used.

If `lexicographic = TRUE`, the extreme points used to generate the grid
are computed lexicographically. In that case, one objective is optimized
first, and then the second objective is optimized while constraining the
first to remain within `lexicographic_tol` of its optimum.

**Stored configuration**

The configured method stores:

- `name = "epsilon_constraint"`,

- `mode`,

- `primary`,

- `aliases`,

- `constrained`,

- epsilon design information,

- lexicographic configuration.

In manual mode, `x$data$method$runs` contains the explicit design grid.

In automatic mode, `x$data$method$runs` is initially `NULL` and is
generated later during
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

For more than two objectives, automatic grid generation is currently
unavailable because the number of epsilon combinations grows rapidly and
requires explicit user control.

## See also

[`set_method_augmecon`](https://josesalgr.github.io/multiscape/reference/set_method_augmecon.md),
[`set_method_weighted_sum`](https://josesalgr.github.io/multiscape/reference/set_method_weighted_sum.md),
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Manual mode: one constrained objective, one run
p <- p |>
  set_method_epsilon_constraint(
    primary = "cost",
    mode = "manual",
    eps = c(frag = 1000)
  )

# Manual mode: one constrained objective, multiple epsilon values
p <- p |>
  set_method_epsilon_constraint(
    primary = "cost",
    mode = "manual",
    eps = list(frag = c(1000, 2000, 3000))
  )

# Manual mode: more than two objectives
p <- p |>
  set_method_epsilon_constraint(
    primary = "benefit",
    aliases = c("benefit", "cost", "frag"),
    mode = "manual",
    eps = list(
      cost = c(100, 200, 300),
      frag = c(10, 20)
    )
  )

# Automatic mode: currently only two objectives
p <- p |>
  set_method_epsilon_constraint(
    primary = "cost",
    aliases = c("cost", "frag"),
    mode = "auto",
    n_points = 5,
    include_extremes = TRUE,
    lexicographic = TRUE,
    lexicographic_tol = 1e-8
  )
} # }
```
