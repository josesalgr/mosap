test_that("get_actions returns selected actions for Solution", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = toy$effects, effect_type = "after") |>
    mosap::add_targets_relative(0.5) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- mosap::solve(p)

  a_all <- mosap::get_actions(s)
  expect_true(is.data.frame(a_all))
  expect_gt(nrow(a_all), 0)

  a_sel <- mosap::get_actions(s, only_selected = TRUE)
  expect_true(is.data.frame(a_sel))
})

test_that("get_actions returns actions for SolutionSet run", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = toy$effects, effect_type = "after") |>
    mosap::add_targets_relative(0.5) |>
    mosap::add_spatial_boundary(boundary = toy$boundary, include_self = TRUE) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::add_objective_min_fragmentation(alias = "frag") |>
    mosap::set_method_weighted(
      aliases = c("cost", "frag"),
      weights = c(1, 1)
    ) |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- mosap::solve(p)

  a <- mosap::get_actions(s, run = 1)
  expect_true(is.data.frame(a))
})
