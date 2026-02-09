# Add minimum selected area constraint

Add a linear constraint enforcing a minimum total selected area using
only planning unit selection variables \\w_i\\: \$\$\sum_i
\mathrm{area}\_i \\ w_i \ge A\_{\min}.\$\$

## Usage

``` r
add_area_min_constraint(
  x,
  area_min,
  area_col = NULL,
  area_unit = c("m2", "ha", "km2"),
  name = "area_min"
)
```

## Arguments

- x:

  A `Data` object.

- area_min:

  Numeric scalar \\\ge 0\\. Minimum area to select (expressed in
  `area_unit`).

- area_col:

  Optional character. Name of the column in `x$data$pu` containing
  areas. If `NULL`, areas are obtained using internal defaults (see
  Details).

- area_unit:

  Character. Units for `area_min`. One of `"m2"`, `"ha"`, or `"km2"`.

- name:

  Character. Name for the constraint in the model. Default `"area_min"`.

## Value

The updated `Data` object with the new linear constraint added to the
model and metadata stored in `x$data$constraints$area_min`.

## Details

This function adds a linear constraint to the current optimization model
snapshot. Areas are retrieved from the planning unit table (`x$data$pu`)
via `area_col` or via the package defaults implemented in
`.pa_get_area_vec()`, and are converted to the requested `area_unit`.
The coefficient vector is aligned with the model's planning unit order
(`n_pu` and the internal \\w\\ variable offset stored in
`x$data$model_list`).

Constraint metadata is also stored in `x$data$constraints$area_min` for
printing/reporting.

## See also

[`add_area_max_constraint`](https://josesalgr.github.io/mosap/reference/add_area_max_constraint.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Enforce selecting at least 1000 ha
p <- add_area_min_constraint(p, area_min = 1000, area_unit = "ha")

# Use a custom area column stored in x$data$pu$Area_km2
p <- add_area_min_constraint(p, area_min = 250, area_unit = "km2", area_col = "Area_km2")
} # }
```
