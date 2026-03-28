# Changelog

## mosap 1.0.4

- First CRAN release of `mosap`.
- Provides a modular workflow for exact multi-objective spatial planning
  based on mixed-integer programming (MIP).
- Introduces the core `Problem`, `Solution`, and `SolutionSet` classes.
- Adds support for modular problem construction through `inputData()`,
  `add_*()`, `set_*()`, and
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).
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
