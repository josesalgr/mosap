test_that("augmecon returns a SolutionSet with runs", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  bnd <- toy$boundary
  names(bnd)[names(bnd) == "id1"] <- "pu1"
  names(bnd)[names(bnd) == "id2"] <- "pu2"

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.5) |>
    paretoscape::add_spatial_boundary(
      boundary = bnd,
      weight_col = "boundary",
      include_self = TRUE
    ) |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::add_objective_max_benefit(alias = "benefit") |>
    paretoscape::add_objective_min_fragmentation(alias = "frag") |>
    paretoscape::set_method_augmecon(
      primary = "cost",
      aliases = c("cost", "benefit", "frag"),
      n_points = 2,
      include_extremes = TRUE,
      augmentation = 1e-3
    ) |>
    paretoscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- paretoscape::solve(p)

  expect_s3_class(s, "SolutionSet")
  expect_true(is.list(s$solution))
  expect_true("runs" %in% names(s$solution))
  expect_true("solutions" %in% names(s$solution))
  expect_gte(nrow(s$solution$runs), 1)
  expect_gte(length(s$solution$solutions), 1)
})
