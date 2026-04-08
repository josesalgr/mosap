# Set the AUGMECON multi-objective method

Configure a `Problem` object to be solved with the augmented
epsilon-constraint method (AUGMECON).

AUGMECON is an exact multi-objective optimization method in which one
objective is treated as the primary objective and the remaining
objectives are converted into \\\varepsilon\\-constraints. In the
augmented formulation, each secondary objective is associated with a
non-negative slack variable, and the primary objective is augmented with
a small reward term based on the normalized slacks. This augmentation is
used to avoid weakly efficient solutions, following Mavrotas (2009).

This function does not solve the problem directly. It stores the
AUGMECON configuration in `x$data$method`, to be used later by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## Usage

``` r
set_method_augmecon(
  x,
  primary,
  aliases = NULL,
  grid = NULL,
  n_points = 10,
  include_extremes = TRUE,
  lexicographic = TRUE,
  lexicographic_tol = 1e-09,
  augmentation = 0.001,
  slack_upper_bound = 1e+06
)
```

## Arguments

- x:

  A `Problem` object.

- primary:

  Character string giving the alias of the primary objective, that is,
  the objective optimized directly in the AUGMECON formulation.

- aliases:

  Optional character vector of objective aliases to include in the
  method. If `NULL`, all registered objective aliases are used. The
  value of `primary` must be included in `aliases`.

- grid:

  Optional named list defining manual epsilon levels for the secondary
  objectives. Each name must correspond to a secondary objective alias,
  and each element must be a non-empty numeric vector of finite values.
  If `NULL`, the grid is generated automatically.

- n_points:

  Number of automatically generated epsilon levels per secondary
  objective when `grid = NULL`. Must be at least 2. Ignored when `grid`
  is supplied.

- include_extremes:

  Logical. If `TRUE`, automatically generated grids include extreme
  values of each secondary objective.

- lexicographic:

  Logical. If `TRUE`, use lexicographic anchoring when computing extreme
  points for automatic grid construction.

- lexicographic_tol:

  Non-negative numeric tolerance used in lexicographic anchoring.

- augmentation:

  Positive numeric augmentation coefficient \\\rho\\. The effective
  coefficient of each secondary slack is computed internally as \\\rho /
  R_k\\, where \\R_k\\ is the payoff-table range of the corresponding
  secondary objective.

- slack_upper_bound:

  Positive numeric upper bound imposed on the explicit non-negative
  slack variables associated with the secondary objectives.

## Value

The updated `Problem` object with the AUGMECON method configuration
stored in `x$data$method`.

## Details

Use this method when one objective should be optimized directly, the
remaining objectives should be controlled through epsilon levels, and
weakly efficient solutions should be reduced through the augmented
formulation.

**General idea**

Suppose that \\m \ge 2\\ objective functions have already been
registered in the problem: \$\$ f_1(x), f_2(x), \dots, f_m(x). \$\$

AUGMECON selects one of them as the primary objective, say \\f_p(x)\\,
and treats the remaining \\m - 1\\ objectives as secondary objectives.

For a fixed combination of epsilon levels, the method solves a sequence
of single-objective subproblems of the form:

\$\$ \max \\ f_p(x) + \rho \sum\_{k \in \mathcal{S}} \frac{s_k}{R_k}
\$\$

subject to

\$\$ f_k(x) - s_k = \varepsilon_k, \qquad k \in \mathcal{S}, \$\$

\$\$ s_k \ge 0, \qquad k \in \mathcal{S}, \$\$

together with all original feasibility constraints of the planning
problem.

Here:

- \\f_p(x)\\ is the primary objective,

- \\\mathcal{S}\\ is the set of secondary objectives,

- \\\varepsilon_k\\ is the imposed level for secondary objective \\k\\,

- \\s_k\\ is a non-negative slack variable,

- \\R_k\\ is the payoff-table range used to normalize objective \\k\\,

- \\\rho \> 0\\ is a small augmentation coefficient.

In the original AUGMECON formulation of Mavrotas (2009), the
augmentation term ensures that, among solutions with the same primary
objective value, the solver prefers those with larger normalized slack,
thereby avoiding weakly efficient points and improving Pareto-front
generation.

**Secondary-objective equalities and slacks**

The key difference between standard epsilon-constraint and AUGMECON is
that the secondary objectives are written as equalities with slacks
rather than as simple inequalities. For a maximization-type secondary
objective, this takes the form:

\$\$ f_k(x) - s_k = \varepsilon_k, \qquad s_k \ge 0. \$\$

This implies: \$\$ f_k(x) \ge \varepsilon_k, \$\$

while explicitly measuring the excess above the imposed epsilon level
through \\s_k\\. The augmentation term then rewards such excess in
normalized form.

In implementation terms, the exact sign convention for each objective
depends on whether it is internally treated as a minimization or
maximization objective, but the method always preserves the same
AUGMECON principle:

- one objective is optimized directly,

- all others are turned into constrained objectives,

- non-negative slacks measure controlled deviation from the imposed
  epsilon levels,

- the primary objective is augmented with a small slack-based reward.

**Manual and automatic epsilon grids**

AUGMECON requires a grid of epsilon levels for each secondary objective.

If `grid` is supplied, it must be a named list with one numeric vector
per secondary objective. Each vector defines the exact epsilon levels to
be explored for that objective.

If `grid = NULL`, the grid is generated automatically later during
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md). In
that case, the method first computes extreme points and payoff-table
ranges for the secondary objectives, and then generates `n_points`
levels for each one.

If `include_extremes = TRUE`, the automatic grid includes the extreme
values of each secondary objective.

If `lexicographic = TRUE`, extreme points are computed using
lexicographic anchoring, which can improve payoff-table quality when
objectives are tightly competing. The tolerance used for lexicographic
anchoring is controlled by `lexicographic_tol`.

**Normalization and augmentation**

The augmentation term is scaled using the payoff-table ranges of the
secondary objectives. If \\R_k\\ denotes the range of secondary
objective \\k\\, then the effective coefficient applied to the slack is:

\$\$ \frac{\rho}{R_k}, \$\$

where \\\rho = \code{augmentation}\\.

This normalization is important because different objectives may be
measured on very different numerical scales. Without normalization, a
slack belonging to a large-scale objective could dominate the
augmentation term simply due to units.

In this implementation, the user supplies `augmentation` as the base
coefficient \\\rho\\, while the normalized slack coefficients are
computed internally at solve time using the corresponding payoff-table
ranges.

**Stored configuration**

This function stores the method definition in `x$data$method` with:

- `name = "augmecon"`,

- the primary objective alias,

- the full set of participating aliases,

- the set of secondary aliases,

- either a manual grid or the information required to generate one
  automatically,

- augmentation and slack-bound parameters.

The actual payoff table, grid construction, and subproblem solution loop
are performed later by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## References

Mavrotas, G. (2009). Effective implementation of the
\\\varepsilon\\-constraint method in multi-objective mathematical
programming problems. *Applied Mathematics and Computation*, 213(2),
455–465.

## See also

[`set_method_epsilon_constraint`](https://josesalgr.github.io/multiscape/reference/set_method_epsilon_constraint.md),
[`set_method_weighted_sum`](https://josesalgr.github.io/multiscape/reference/set_method_weighted_sum.md),
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Automatic epsilon grids from payoff ranges
p <- p |>
  set_method_augmecon(
    primary = "benefit",
    aliases = c("benefit", "cost"),
    n_points = 10,
    include_extremes = TRUE,
    lexicographic = TRUE,
    augmentation = 1e-3
  )

# Manual epsilon grids for two secondary objectives
p <- p |>
  set_method_augmecon(
    primary = "benefit",
    aliases = c("benefit", "cost", "frag"),
    grid = list(
      cost = c(100, 150, 200, 250),
      frag = c(2, 4, 6, 8)
    ),
    augmentation = 1e-3,
    slack_upper_bound = 1e6
  )
} # }
```
