# Add maximum selected area constraint

Add a linear constraint enforcing a maximum total selected area using
only planning unit selection variables \\w_i\\: \$\$\sum_i
\mathrm{area}\_i \\ w_i \le A\_{\max}.\$\$

## Usage

``` r
add_area_max_constraint(
  x,
  area_max,
  area_col = NULL,
  area_unit = c("m2", "ha", "km2"),
  name = "area_max"
)
```

## Arguments

- x:

  A `Data` object.

- area_max:

  Numeric scalar \\\ge 0\\. Maximum area to select (expressed in
  `area_unit`).

- area_col:

  Optional character. Name of the column in `x$data$pu` containing
  areas. If `NULL`, areas are obtained using internal defaults (see
  Details).

- area_unit:

  Character. Units for `area_max`. One of `"m2"`, `"ha"`, or `"km2"`.

- name:

  Character. Name for the constraint in the model. Default `"area_max"`.

## Value

The updated `Data` object with the new linear constraint added to the
model and metadata stored in `x$data$constraints$area_max`.

## Details

This function adds a linear constraint to the current optimization model
snapshot. Areas are retrieved from the planning unit table (`x$data$pu`)
via `area_col` or via the package defaults implemented in
`.pa_get_area_vec()`, and are converted to the requested `area_unit`.
The coefficient vector is aligned with the model's planning unit order
(`n_pu` and the internal \\w\\ variable offset stored in
`x$data$model_list`).

Constraint metadata is also stored in `x$data$constraints$area_max` for
printing/reporting.

## See also

[`add_area_min_constraint`](https://josesalgr.github.io/mosap/reference/add_area_min_constraint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Enforce selecting at most 500 km2
p <- add_area_max_constraint(p, area_max = 500, area_unit = "km2")

# Use a custom area column stored in x$data$pu$area_m2
p <- add_area_max_constraint(p, area_max = 2e6, area_unit = "m2", area_col = "area_m2")
} # }
```
