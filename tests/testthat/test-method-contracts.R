test_that("set_method_weighted errors when aliases and weights have different lengths", {
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

  expect_error(
    mosap::set_method_weighted(
      p,
      aliases = c("cost", "frag"),
      weights = c(1)
    ),
    "length"
  )
})

test_that("set_method_weighted errors when aliases contain duplicates", {
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

  expect_error(
    mosap::set_method_weighted(
      p,
      aliases = c("cost", "cost"),
      weights = c(1, 1)
    )
  )
})
