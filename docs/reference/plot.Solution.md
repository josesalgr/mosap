# Plot a Solution (if spatial geometry is available)

Plot a solution using planning unit (PU) geometries when available.

The method supports two views controlled by `what`:

- `"pu"`: highlights selected planning units.

- `"actions"`: highlights selected actions within planning units.
  Optionally, selected PUs with no selected action can be labelled as
  “conservation”.

For `what = "actions"`, the function aggregates selected actions per PU
into a single label by concatenating unique action names using `"+"`
(e.g., `"harvest+sustainable"`). If many actions can co-occur in the
same PU, the number of distinct labels can grow quickly; in such cases,
consider filtering a single action via `action = "..."`, or ensure at
most one action per PU.

## Usage

``` r
# S3 method for class 'Solution'
plot(
  x,
  what = c("pu", "actions"),
  only_selected = FALSE,
  action = NULL,
  ...,
  include_conservation = TRUE,
  conservation_label = "conservation",
  base_alpha = 0.1,
  selected_alpha = 0.9,
  base_fill = "grey60",
  base_color = NA,
  selected_color = NA,
  fill_values = NULL,
  fill_na = "grey60",
  use_viridis = TRUE
)
```

## Arguments

- x:

  A `Solution` object. It must contain PU geometry as an `sf` object in
  `x$Data$data$pu_sf` (preferred) or `x$data$pu_sf`.

- what:

  Character string indicating what to plot. One of `"pu"` or
  `"actions"`.

- only_selected:

  Logical. Only used when `what = "pu"`. If `TRUE`, throw an error when
  no selected PUs exist. If `FALSE`, the map is still drawn with the
  base layer.

- action:

  Optional character scalar. Only used when `what = "actions"`. If
  provided, filter the plot to a single action label.

- ...:

  Additional arguments (reserved for future extensions).

- include_conservation:

  Logical. Only used when `what = "actions"`. If `TRUE`, selected PUs
  that have no selected action are added to the map with label
  `conservation_label`.

- conservation_label:

  Character scalar. Label used for selected PUs with no selected action
  when `include_conservation = TRUE`.

- base_alpha:

  Numeric in \\\[0,1\]\\. Alpha used for the base PU layer.

- selected_alpha:

  Numeric in \\\[0,1\]\\. Alpha used for highlighted (selected)
  geometries.

- base_fill:

  Fill color for the base PU layer.

- base_color:

  Border color for the base PU layer. Use `NA` for no border.

- selected_color:

  Border color for highlighted geometries. Use `NA` for no border.

- fill_values:

  Optional named character vector of colors. Names must match the labels
  shown in the legend (i.e., values of the `action` field used in the
  plot, including `conservation_label` if enabled). If provided,
  overrides viridis.

- fill_na:

  Color used for missing values in the manual scale (only relevant when
  `fill_values` is provided).

- use_viridis:

  Logical. If `TRUE` and `fill_values` is `NULL`, use a discrete viridis
  palette when viridis is installed.

## Value

Invisibly returns a `ggplot` object. The plot is printed as a side
effect.

## Details

This plotting method requires PU geometry stored as an `sf` object in
either `x$Data$data$pu_sf` or `x$data$pu_sf`. The geometry must contain
an `id` column matching planning unit identifiers.

Colors for `what = "actions"` are controlled by either:

- `fill_values`: a named vector mapping action labels to colors; or

  - a discrete viridis palette if `fill_values` is `NULL`,
    `use_viridis = TRUE`, and the viridis package is installed.

## See also

[`get_pu`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/mosap/reference/get_actions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot selected planning units
plot(sol, what = "pu")

# Plot selected actions
plot(sol, what = "actions")

# Plot a single action only
plot(sol, what = "actions", action = "harvest")

# Manual colors for actions (and conservation)
cols <- c(harvest = "#E41A1C", sustainable = "#4DAF4A", conservation = "#377EB8")
plot(sol, what = "actions", fill_values = cols, conservation_label = "conservation")
} # }
```
