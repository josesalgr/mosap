test_that("solve uses restoration when restoration-only feature target requires it", {
  skip_if_no_cbc()

  toy <- toy_multiaction_semantics()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.8, features = 2) |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- paretoscape::solve(p)

  expect_s3_class(s, "Solution")
  expect_true(is.data.frame(s$summary$actions))

  acts_sel <- s$summary$actions[s$summary$actions$selected > 0.5, , drop = FALSE]

  expect_true(any(acts_sel$action == "restoration"))
})
