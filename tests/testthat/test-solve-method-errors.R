test_that("solve errors for unknown MO method configuration", {
  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_spatial_boundary(
      boundary = toy$boundary,
      weight_col = "boundary",
      include_self = TRUE
    ) |>
    mosap::add_objective_min_cost(alias = "cost") |>
    mosap::add_objective_min_fragmentation(alias = "frag")

  p$data$method <- list(type = "not_a_method")

  expect_error(
    mosap::solve(p),
    "Unknown|unsupported"
  )
})
