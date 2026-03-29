test_that("add_spatial_boundary stores non-diagonal relations correctly", {
  toy <- toy_equivalent_basic()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  p <- paretoscape::add_spatial_boundary(
    p,
    boundary = toy$boundary,
    include_self = FALSE
  )

  expect_true("spatial_relations" %in% names(p$data))
  expect_true("boundary" %in% names(p$data$spatial_relations))

  bnd <- p$data$spatial_relations$boundary
  expect_true(is.data.frame(bnd))
  expect_true(all(c("pu1", "pu2") %in% names(bnd)))

  expect_false(any(bnd$pu1 == bnd$pu2))
})

test_that("add_spatial_boundary with include_self = TRUE adds diagonal rows", {
  toy <- toy_equivalent_basic()

  p <- paretoscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  p <- paretoscape::add_spatial_boundary(
    p,
    boundary = toy$boundary,
    include_self = TRUE
  )

  bnd <- p$data$spatial_relations$boundary
  expect_true(any(bnd$pu1 == bnd$pu2))
})
