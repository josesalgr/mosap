test_that("mosap matches prioritizr on a one-action boundary-penalized problem", {
  skip_if_no_cbc()
  skip_if_no_prioritizr()

  toy <- toy_equivalent_basic()

  bnd <- toy$boundary
  names(bnd)[names(bnd) == "pu1"] <- "id1"
  names(bnd)[names(bnd) == "pu2"] <- "id2"

  p_mosap <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = toy$effects, effect_type = "after") |>
    mosap::add_targets_relative(0.5) |>
    mosap::add_spatial_boundary(boundary = toy$boundary, include_self = FALSE) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::add_objective_min_fragmentation(alias = "frag") |>
    mosap::set_method_weighted(
      aliases = c("cost", "frag"),
      weights = c(1, 1)
    ) |>
    mosap::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s_mosap <- mosap::solve(p_mosap)
  acts_mosap <- mosap::get_actions(s_mosap, run = 1, only_selected = TRUE)
  sel_mosap <- sort(unique(acts_mosap$pu))
  cost_mosap <- sum(toy$pu$cost[toy$pu$id %in% sel_mosap])

  p_prio <- build_prioritizr_basic(
    toy,
    target = 0.5,
    boundary_matrix = bnd
  )

  s_prio <- solve_prioritizr(p_prio)

  sel_prio <- sort(which(s_prio$solution_1 > 0.5))
  cost_prio <- sum(toy$pu$cost[toy$pu$id %in% sel_prio])

  expect_equal(cost_mosap, cost_prio)
  expect_equal(sel_mosap, sel_prio)
})
