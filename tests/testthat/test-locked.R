test_that("solve respects locked_in and locked_out planning units", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()
  toy$pu$locked_in[1] <- TRUE
  toy$pu$locked_out[4] <- TRUE

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.5) |>
    paretoscape::add_locked_pu(locked_in = "locked_in", locked_out = "locked_out") |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- paretoscape::solve(p)

  expect_s3_class(s, "Solution")
  expect_true(is.data.frame(s$summary$actions))

  selected <- s$summary$actions[s$summary$actions$selected > 0.5, , drop = FALSE]

  expect_true(any(selected$pu == 1))
  expect_false(any(selected$pu == 4))
})
