# Add locked planning units to a problem

Define planning units that must be included in, or excluded from, the
optimization problem.

This function updates the planning-unit table stored in `x$data$pu` by
creating or replacing the logical columns `locked_in` and `locked_out`.
These columns are later used by the model builder when translating the
`Problem` object into optimization constraints.

Lock information may be supplied either directly as logical vectors, as
vectors of planning-unit ids, or by referencing columns in the raw
planning-unit data originally passed to
[`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md).

## Usage

``` r
add_locked_pu(x, locked_in = NULL, locked_out = NULL)
```

## Arguments

- x:

  A `Problem` object created with
  [`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md).

- locked_in:

  Optional locked-in specification. It may be `NULL`, a column name in
  `x$data$pu_data_raw`, a logical vector, or a vector of planning-unit
  ids.

- locked_out:

  Optional locked-out specification. It may be `NULL`, a column name in
  `x$data$pu_data_raw`, a logical vector, or a vector of planning-unit
  ids.

## Value

An updated `Problem` object in which `x$data$pu` contains logical
columns `locked_in` and `locked_out`.

## Details

Let \\\mathcal{P}\\ denote the set of planning units and let \\w_i \in
\\0,1\\\\ denote the binary variable indicating whether planning unit
\\i \in \mathcal{P}\\ is selected by the model.

This function defines two subsets:

- \\\mathcal{P}^{in} \subseteq \mathcal{P}\\, the planning units that
  must be included,

- \\\mathcal{P}^{out} \subseteq \mathcal{P}\\, the planning units that
  must be excluded.

Conceptually, these sets correspond to the following conditions:

- if \\i \in \mathcal{P}^{in}\\, then \\w_i = 1\\,

- if \\i \in \mathcal{P}^{out}\\, then \\w_i = 0\\.

These constraints are not imposed immediately by this function; instead,
they are stored in `x$data$pu$locked_in` and `x$data$pu$locked_out` and
enforced later when building the optimization model.

**Philosophy**

The role of
[`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md)
is to construct and normalize the basic inputs of the planning problem.
Locking planning units is treated as a separate modelling step so that
users can define or revise selection restrictions after the `Problem`
object has already been created.

**Supported input formats**

For both `locked_in` and `locked_out`, the function accepts:

- `NULL`, meaning that no planning units are locked on that side,

- a single character string, interpreted as a column name in
  `x$data$pu_data_raw`,

- a logical vector of length `nrow(x$data$pu)`,

- a vector of planning-unit ids.

When a column name is supplied, the referenced column is coerced to
logical. Numeric values are interpreted as non-zero = `TRUE`; character
and factor values are interpreted using common logical strings such as
`"true"`, `"t"`, `"1"`, `"yes"`, and `"y"`. Missing values are treated
as `FALSE`.

**Replacement behaviour**

Each call to `add_locked_pu()` replaces any existing `locked_in` and
`locked_out` columns in `x$data$pu`. In other words, the function
defines the complete current set of locked planning units; it does not
merge new values with previous ones.

**Consistency checks**

The function checks that no planning unit is simultaneously assigned to
both `locked_in` and `locked_out`. If such conflicts are found, an error
is raised.

## See also

[`input_data`](https://josesalgr.github.io/paretoscape/reference/input_data.md),
[`add_actions`](https://josesalgr.github.io/paretoscape/reference/add_actions.md),
[`add_locked_actions`](https://josesalgr.github.io/paretoscape/reference/add_locked_actions.md)

## Examples

``` r
pu <- data.frame(
  id = 1:5,
  cost = c(2, 3, 1, 4, 2),
  lock_col = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  out_col = c(FALSE, FALSE, FALSE, FALSE, TRUE)
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

p <- input_data(
  pu = pu,
  features = features,
  dist_features = dist_features
)
#> Warning: The following pu's do not contain features: 5

# 1) Lock by planning-unit ids
p1 <- add_locked_pu(
  x = p,
  locked_in = c(1, 3),
  locked_out = c(5)
)

p1$data$pu[, c("id", "locked_in", "locked_out")]
#>   id locked_in locked_out
#> 1  1      TRUE      FALSE
#> 2  2     FALSE      FALSE
#> 3  3      TRUE      FALSE
#> 4  4     FALSE      FALSE
#> 5  5     FALSE       TRUE

# 2) Read lock information from raw PU data columns
p2 <- add_locked_pu(
  x = p,
  locked_in = "lock_col",
  locked_out = "out_col"
)

p2$data$pu[, c("id", "locked_in", "locked_out")]
#>   id locked_in locked_out
#> 1  1      TRUE      FALSE
#> 2  2     FALSE      FALSE
#> 3  3     FALSE      FALSE
#> 4  4      TRUE      FALSE
#> 5  5     FALSE       TRUE

# 3) Use logical vectors
p3 <- add_locked_pu(
  x = p,
  locked_in = c(TRUE, FALSE, TRUE, FALSE, FALSE),
  locked_out = c(FALSE, FALSE, FALSE, TRUE, FALSE)
)

p3$data$pu[, c("id", "locked_in", "locked_out")]
#>   id locked_in locked_out
#> 1  1      TRUE      FALSE
#> 2  2     FALSE      FALSE
#> 3  3      TRUE      FALSE
#> 4  4     FALSE       TRUE
#> 5  5     FALSE      FALSE
```
