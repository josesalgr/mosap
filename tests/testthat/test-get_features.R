test_that("get_features returns a feature summary for Solution", {
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
  f <- paretoscape::get_features(s)

  expect_true(is.data.frame(f))
  expect_gt(nrow(f), 0)
  expect_true(any(c("feature", "feature_name", "total") %in% names(f)))
})

test_that("get_features returns a feature summary for SolutionSet run", {
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
  f <- paretoscape::get_features(s, run = 1)

  expect_true(is.data.frame(f))
  expect_gt(nrow(f), 0)
})

test_that("get_features errors for invalid run in SolutionSet", {
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

  expect_error(
    paretoscape::get_features(s, run = 999),
    "run"
  )
})
