# mosap: Integrated Multi-Objective Spatial Action Planning

**⚠️ Experimental package: mosap is an experimental research package
under active development. Breaking changes may occur at any time (API,
internal tables, solver interfaces, outputs). Use it at your own risk
and do not rely on it for production workflows yet.**

## Overview

The `mosap` is designed to support atomic objectives (e.g., minimize
cost, minimize fragmentation, maximize benefits) and to combine them
through multi-objective methods (e.g., weighted sum). The package
supports both tabular and spatial inputs (vector- and raster-based
workflows), and stores intermediate artifacts (tables, spatial
relations, targets, solver configuration) to enable reproducible
experimentation and reporting.

## Installation

The latest development version of `mosap` can be installed from GitHub:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("josesalgr/mosap")
```

## Usage

Below is a minimal end-to-end example showing the intended pipeline.

``` r
library(mosap)

# --- Example inputs (replace with your own)
# pu_data            : data.frame(id, cost, locked_in?, locked_out?)
# features_data      : data.frame(id, name)
# dist_features_data : data.frame(pu, feature, amount)

# 1) Build the Data object
x <- inputData(
  pu = pu_data,
  features = features_data,
  dist_features = dist_features_data
)

# 2) Add actions/effects/targets/spatial relations as needed
x <- x |>
  add_actions(actions_df) |>
  add_effects(effects_df) |>
  add_conservation_targets_relative(0.17) |>
  add_spatial_boundary()

# 3) Register atomic objectives (aliases are later used by MO methods)
x <- x |>
  add_objective_min_cost(alias = "cost") |>
  add_objective_min_fragmentation(alias = "frag")

# 4) Configure a multi-objective method (weighted sum)
mo <- set_method_weighted(
  x,
  aliases = c("cost", "frag"),
  weights = c(1, 1),
  normalize = TRUE
)

# 5) Configure solver (example; adapt to your solver and preferences)
mo <- set_solver(
  mo,
  solver = "auto",
  time_limit = 60,
  gap_limit = 0.01,
  verbose = TRUE
)

# 6) Solve
res <- solve(mo)

# 7) Inspect results (examples)
get_pu(res, only_selected = TRUE)
get_actions(res, only_selected = TRUE)
get_features(res)
get_targets(res)
```

If you believe you’ve found a bug in mosap, please file an issue
(ideally with a reproducible example) at:
<https://github.com/josesalgr/mosap/issues>.
