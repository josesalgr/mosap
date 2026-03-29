test_that("compile_model builds MO superset with boundary auxiliaries when fragmentation is included", {
  toy <- toy_equivalent_basic()

  p2 <- paretoscape::input_data(
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
    paretoscape::add_objective_min_fragmentation(alias = "frag") |>
    paretoscape::set_method_weighted(
      aliases = c("cost", "frag"),
      weights = c(1, 1)
    )

  p2 <- paretoscape::compile_model(p2)

  expect_s3_class(p2, "Problem")
  expect_false(is.null(p2$data$model_ptr))
  expect_true(is.list(p2$data$model_list))
  expect_false(isTRUE(p2$data$meta$model_dirty))

  n_y_pu <- as.integer(p2$data$model_list$n_y_pu %||% 0L)
  expect_gt(n_y_pu, 0)
})
