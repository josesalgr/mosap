test_that("solve uses restoration when restoration-only feature target requires it", {
  skip_if_no_cbc()

  toy <- toy_multiaction_semantics()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = toy$effects, effect_type = "after") |>
    mosap::add_targets_relative(0.8, features = 2) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- mosap::solve(p)

  expect_s3_class(s, "Solution")
  expect_true(is.data.frame(s$summary$actions))

  acts_sel <- s$summary$actions[s$summary$actions$selected > 0.5, , drop = FALSE]

  expect_true(any(acts_sel$action == "restoration"))
})
