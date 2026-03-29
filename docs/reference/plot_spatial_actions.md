# Plot selected actions in space

Plot the spatial distribution of selected actions from a `Solution` or
`SolutionSet`.

This function maps the selected planning unit–action pairs returned by
[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md)`(only_selected = TRUE)`
onto the planning-unit geometry stored in the associated `Problem`
object.

## Usage

``` r
plot_spatial_actions(
  x,
  runs = NULL,
  actions = NULL,
  layout = NULL,
  max_facets = 4L,
  ...,
  base_alpha = 0.1,
  selected_alpha = 0.9,
  base_fill = "grey92",
  base_color = NA,
  selected_color = NA,
  draw_borders = FALSE,
  show_base = TRUE,
  fill_values = NULL,
  fill_na = "grey80",
  use_viridis = TRUE
)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object.

- runs:

  Optional integer vector of run ids. If `NULL`, a `Solution` is used
  directly and a `SolutionSet` defaults to the first run.

- actions:

  Optional action subset to display. Entries may match action ids or
  action-set labels.

- layout:

  Character string controlling the layout. Must be one of `"single"` or
  `"facet"`. If `NULL`, the default is `"single"`.

- max_facets:

  Maximum number of action facets shown when `actions` is `NULL` and
  faceting would otherwise create many panels.

- ...:

  Reserved for future extensions.

- base_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the base
  planning-unit layer.

- selected_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the highlighted
  action layer.

- base_fill:

  Fill colour for the base planning-unit layer.

- base_color:

  Border colour for the base planning-unit layer.

- selected_color:

  Border colour for highlighted layers.

- draw_borders:

  Logical. If `FALSE`, borders are not drawn.

- show_base:

  Logical. If `TRUE`, draw the base planning-unit layer underneath the
  highlighted output.

- fill_values:

  Optional named vector of colours for discrete action maps.

- fill_na:

  Fill colour for missing values.

- use_viridis:

  Logical. If `TRUE` and the viridis package is available, use viridis
  discrete scales.

## Value

Invisibly returns a `ggplot` object.

## Details

Let \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
planning unit \\i\\. This function plots the selected `(pu, action)`
pairs in geographic space.

If `layout = "facet"` and only one run is plotted, one panel is drawn
per action.

If `layout = "single"`, all selected actions are drawn in a single map
using discrete fills. If more than one action is selected in the same
planning unit, the action labels are collapsed using `"+"`.

When plotting multiple runs, only `layout = "single"` is supported.

Planning-unit geometry must be available in `x$problem$data$pu_sf`.

## See also

[`get_actions`](https://josesalgr.github.io/paretoscape/reference/get_actions.md),
[`plot_spatial`](https://josesalgr.github.io/paretoscape/reference/plot_spatial.md),
[`plot_spatial_pu`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_pu.md),
[`plot_spatial_features`](https://josesalgr.github.io/paretoscape/reference/plot_spatial_features.md)
