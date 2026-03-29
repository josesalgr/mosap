test_that("paretoscape matches prioritizr on a one-action min-cost problem", {
  skip_if_no_cbc()
  skip_if_no_prioritizr()

  toy <- toy_equivalent_basic()

  p_paretoscape <- paretoscape::input_data(
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

  s_paretoscape <- paretoscape::solve(p_paretoscape)

  acts_paretoscape <- paretoscape::get_actions(s_paretoscape, only_selected = TRUE)
  sel_paretoscape <- sort(unique(acts_paretoscape$pu))
  cost_paretoscape <- sum(toy$pu$cost[toy$pu$id %in% sel_paretoscape])

  p_prio <- build_prioritizr_basic(toy, target = 0.5)

  s_prio <- solve_prioritizr(p_prio)

  sel_prio <- sort(which(s_prio$solution_1 > 0.5))
  cost_prio <- sum(toy$pu$cost[toy$pu$id %in% sel_prio])

  expect_equal(cost_paretoscape, cost_prio)
  expect_equal(sel_paretoscape, sel_prio)
})
