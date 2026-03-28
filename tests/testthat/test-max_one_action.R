test_that("solve selects at most one action per planning unit", {
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
    mosap::add_targets_relative(0.2, features = 1) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- mosap::solve(p)

  expect_s3_class(s, "Solution")
  expect_true(is.data.frame(s$summary$actions))

  acts <- s$summary$actions
  acts_sel <- acts[acts$selected > 0.5, , drop = FALSE]

  if (nrow(acts_sel) > 0) {
    n_by_pu <- table(acts_sel$pu)
    expect_true(all(as.integer(n_by_pu) <= 1L))
  } else {
    succeed()
  }
})
