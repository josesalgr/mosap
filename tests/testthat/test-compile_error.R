test_that("compile_model errors if multiple objectives are present without a MO method", {
  toy <- toy_equivalent_basic()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    paretoscape::add_actions(actions = toy$actions, cost = 0) |>
    paretoscape::add_effects(effects = toy$effects, effect_type = "after") |>
    paretoscape::add_targets_relative(0.5) |>
    paretoscape::add_spatial_boundary(boundary = toy$boundary, include_self = TRUE) |>
    paretoscape::add_objective_min_cost(alias = "cost") |>
    paretoscape::add_objective_min_fragmentation(alias = "frag")

  expect_error(
    paretoscape::compile_model(p),
    "Multiple objectives are registered"
  )
})
