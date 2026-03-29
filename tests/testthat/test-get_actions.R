test_that("get_actions returns selected actions for Solution", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.5) |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- paretoscape::solve(p)

  a_all <- paretoscape::get_actions(s)
  expect_true(is.data.frame(a_all))
  expect_gt(nrow(a_all), 0)

  a_sel <- paretoscape::get_actions(s, only_selected = TRUE)
  expect_true(is.data.frame(a_sel))
})

test_that("get_actions returns actions for SolutionSet run", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.5) |>
    paretoscape::add_spatial_boundary(boundary = toy$boundary, include_self = TRUE) |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::add_objective_min_fragmentation(alias = "frag") |>
    paretoscape::set_method_weighted(
      aliases = c("cost", "frag"),
      weights = c(1, 1)
    ) |>
    paretoscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- paretoscape::solve(p)

  a <- paretoscape::get_actions(s, run = 1)
  expect_true(is.data.frame(a))
})
