# Changelog

## multiscape 1.0.5

- Release candidate for CRAN.

## multiscape 1.0.4

- First CRAN release of `multiscape`.
- Provides a modular workflow for exact multi-objective spatial planning
  based on mixed-integer programming (MIP).
- Introduces the core `Problem`, `Solution`, and `SolutionSet` classes.
- Adds support for modular problem construction through
  [`create_problem()`](https://josesalgr.github.io/multiscape/reference/create_problem.md),
  `add_*()`, `set_*()`, and
  [`solve()`](https://josesalgr.github.io/multiscape/reference/solve.md).
- Supports atomic objective registration and multi-objective solution
  methods, including weighted-sum, epsilon-constraint, and AUGMECON.
- Includes support for spatial relations such as boundary, rook, queen,
  k-nearest neighbours, and distance-based relations.
- Supports commercial and open-source solvers, including Gurobi, CPLEX,
  CBC, and SYMPHONY.
- Adds user-facing extraction and visualization tools for planning
  units, actions, features, targets, spatial outputs, and trade-offs.
- Includes substantial updates to documentation, package structure, and
  contribution guidelines.
