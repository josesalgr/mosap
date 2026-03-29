# Plot selected planning units in space

Plot the spatial distribution of selected planning units from a
`Solution` or `SolutionSet`.

This function maps the planning-unit selection summary returned by
[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md)
onto the planning-unit geometry stored in the associated `Problem`
object.

## Usage

``` r
plot_spatial_pu(
  x,
  runs = NULL,
  ...,
  base_alpha = 0.1,
  selected_alpha = 0.9,
  base_fill = "grey92",
  base_color = NA,
  selected_color = NA,
  draw_borders = FALSE,
  show_base = TRUE
)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object.

- runs:

  Optional integer vector of run ids. If `NULL`, a `Solution` is used
  directly and a `SolutionSet` defaults to the first run.

- ...:

  Reserved for future extensions.

- base_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the base
  planning-unit layer.

- selected_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the selected
  planning-unit layer.

- base_fill:

  Fill colour for the base planning-unit layer.

- base_color:

  Border colour for the base planning-unit layer.

- selected_color:

  Border colour for selected planning units.

- draw_borders:

  Logical. If `FALSE`, borders are not drawn.

- show_base:

  Logical. If `TRUE`, draw the base planning-unit layer underneath the
  selected units.

## Value

Invisibly returns a `ggplot` object.

## Details

Let \\w_i \in \\0,1\\\\ denote the planning-unit selection variable for
planning unit \\i\\. This function plots the user-facing `selected == 1`
representation of \\w_i\\.

If several runs are requested, the output is faceted by `run_id`.

Planning-unit geometry must be available in `x$problem$data$pu_sf`.

## See also

[`get_pu`](https://josesalgr.github.io/paretoscape/reference/get_pu.md),
[`plot_spatial`](https://josesalgr.github.io/paretoscape/reference/plot_spatial.md),
[`plot_spatial_actions`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_actions.md),
[`plot_spatial_features`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_features.md)
