test_that("printing Problem does not fail", {
  toy <- toy_equivalent_basic()

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  expect_no_error(print(p))
})

test_that("printing Solution does not fail", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(actions = toy$actions, cost = 0) |>
    multiscape::add_effects(effects = toy$effects, effect_type = "after") |>
    multiscape::add_constraint_targets_relative(0.5) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- multiscape::solve(p)

  expect_no_error(print(s))
})

test_that("printing SolutionSet does not fail", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(actions = toy$actions, cost = 0) |>
    multiscape::add_effects(effects = toy$effects, effect_type = "after") |>
    multiscape::add_constraint_targets_relative(0.5) |>
    multiscape::add_spatial_boundary(boundary = toy$boundary, include_self = TRUE) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::add_objective_min_fragmentation(alias = "frag") |>
    multiscape::set_method_weighted_sum(
      aliases = c("cost", "frag"),
      weights = c(1, 1)
    ) |>
    multiscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- multiscape::solve(p)

  expect_no_error(print(s))
})


test_that("printing Problem with area and budget constraints does not fail", {
  toy <- toy_multiaction_semantics()

  p <- multiscape::create_problem(
    pu = transform(toy$pu, area_ha = c(1, 1, 1, 1, 1)),
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(actions = toy$actions, cost = c(conservation = 1, restoration = 2)) |>
    multiscape::add_constraint_area(area = 2, sense = "max", area_col = "area_ha") |>
    multiscape::add_constraint_budget(
      budget = 10,
      sense = "max",
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    )

  expect_no_error(print(p))
})
