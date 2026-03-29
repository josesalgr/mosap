# Register an atomic objective (internal)

Internal helper used by objective setter functions to define the active
single-objective configuration of a `Problem` object and, optionally,
register that objective as an atomic objective for multi-objective
workflows.

## Arguments

- x:

  A `Problem` object.

- alias:

  Optional character scalar used to register the objective as an atomic
  objective. If `NULL`, no registration entry is created.

- objective_id:

  Character string giving the stable internal identifier of the
  objective, for example `"min_cost"` or `"max_benefit"`.

- model_type:

  Character string giving the model-builder label associated with this
  objective, for example `"minimizeCosts"`.

- objective_args:

  A list of objective-specific arguments to be stored with the objective
  definition.

- sense:

  Character string giving the optimization direction. Must be either
  `"min"` or `"max"`.

## Value

An updated `Problem` object.

## Details

In `paretoscape`, an *atomic objective* is a fully specified objective
definition that can later be reused by a multi-objective method such as
a weighted-sum formulation, an \\\epsilon\\-constraint method, AUGMECON,
or other objective-orchestration procedures.

Each atomic objective is identified by:

- a stable internal identifier `objective_id`,

- a solver-facing model label `model_type`,

- a list of objective-specific arguments stored in `objective_args`,

- an optimization sense, either `"min"` or `"max"`,

- and, optionally, a user-facing identifier `alias`.

If `alias` is not `NULL`, the objective is stored in
`x$data$objectives[[alias]]`. This makes it possible to refer to the
same objective later by a stable user-facing name. For example, a user
may register objectives under aliases such as `"cost"`, `"benefit"`, or
`"frag"` and then pass those aliases to a multi-objective method.

If `alias` is `NULL`, no atomic-objective entry is created in
`x$data$objectives`. In that case, the calling function still defines
the currently active single-objective configuration through
`x$data$model_args`, but no reusable multi-objective registration is
created.

Thus, this helper supports two complementary modes:

- **single-objective mode**: only the active objective is stored,

- **multi-objective-ready mode**: the active objective is stored and
  also registered under an alias for later reuse.

Conceptually, if an objective function is denoted by \\f(x)\\, this
helper does not itself define the mathematical form of \\f\\; rather, it
stores the metadata required so that downstream code can reconstruct the
correct objective expression, its direction of optimization, and its
user-visible identity.
