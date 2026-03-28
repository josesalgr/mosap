test_that("single-objective solve returns a Solution and respects locked units", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()
  toy$pu$locked_in[1] <- 1
  toy$pu$locked_out[4] <- 1

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = toy$effects, effect_type = "after") |>
    mosap::add_targets_relative(0.5) |>
    mosap::add_locked_pu(locked_in = "locked_in", locked_out = "locked_out") |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- mosap::solve(p)

  expect_s3_class(s, "Solution")

  acts <- mosap::get_actions(s, only_selected = TRUE)
  expect_true(any(acts$pu == 1))
  expect_false(any(acts$pu == 4))
})
