# MOResult class

Container for results produced by multi-objective methods in
prioriactionsMO.

## Details

An `MOResult` stores:

1.  A run table (one row per run/solution),

2.  The list of underlying single-objective
    [`prioriactions::Solution`](https://prioriactions.github.io/prioriactions/reference/solution-class.html)
    objects,

3.  Metadata (method config, call, timestamps, etc.).

## Fields

- runs:

  A `data.frame` with one row per run (weights/epsilons, status,
  runtime, gap, objective values, etc.).

- solutions:

  A list of
  [`prioriactions::Solution`](https://prioriactions.github.io/prioriactions/reference/solution-class.html)
  objects (one per run).

- meta:

  A list with method configuration, call, and any additional
  bookkeeping.
