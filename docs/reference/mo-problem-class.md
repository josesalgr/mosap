# MOProblem class

A container for multi-objective optimization workflows built on top of a
prioriactions `Data` object.

## Details

The goal of `MOProblem` is to keep `prioriactions` focused on defining
data, constraints and *atomic objectives*, while `prioriactionsMO`
orchestrates *multi-objective methods* (weighted sum,
epsilon-constraint, AUGMECON, goal programming, interactive, etc.).

Conceptually, an `MOProblem` wraps:

1.  A base `Data` object (coming from `prioriactions::inputData()`),

2.  A registry of atomic objectives (each with alias, direction and
    builders),

3.  A multi-objective "method" configuration (e.g., weights, epsilons),

4.  Results (solutions, objective values, solver logs).

## Fields

- data:

  A `Data` object from `prioriactions`.

- objectives:

  A named list of registered objectives (by alias).

- method:

  A list describing the selected multi-objective method and its
  parameters.

- results:

  A list storing solutions, objective values and diagnostics.

- meta:

  A list for bookkeeping (package versions, creation time, etc.).

## Methods

- print()::

  Print a compact summary.

- show()::

  Alias of
  [`print()`](https://josesalgr.github.io/mosap/reference/print.md).

- repr()::

  Short representation.

- getData()::

  Return the underlying `Data` object.

- listObjectives()::

  Return objective aliases currently registered.

- getObjective(alias)::

  Return an objective definition by alias.
