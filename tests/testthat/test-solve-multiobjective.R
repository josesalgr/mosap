test_that("weighted method returns a SolutionSet with runs", {
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

  expect_s3_class(s, "SolutionSet")

  expect_true("solution" %in% names(s))
  expect_true(is.list(s$solution))

  expect_true("runs" %in% names(s$solution))
  expect_true("solutions" %in% names(s$solution))

  expect_true(is.data.frame(s$solution$runs))
  expect_gte(nrow(s$solution$runs), 1)

  expect_true(is.list(s$solution$solutions))
  expect_gte(length(s$solution$solutions), 1)
})
