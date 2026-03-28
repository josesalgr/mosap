test_that("input_data builds the core problem tables correctly", {
  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  expect_s3_class(p, "Problem")

  expect_true(is.data.frame(p$data$pu))
  expect_true(is.data.frame(p$data$features))
  expect_true(is.data.frame(p$data$dist_features))

  expect_equal(nrow(p$data$pu), 4)
  expect_equal(nrow(p$data$features), 2)
  expect_equal(nrow(p$data$dist_features), 8)

  expect_true(all(c("id", "cost") %in% names(p$data$pu)))
  expect_true("id" %in% names(p$data$features))
  expect_true(all(c("pu", "feature", "amount") %in% names(p$data$dist_features)))

  expect_true("internal_id" %in% names(p$data$pu))
  expect_true("internal_id" %in% names(p$data$features))

  expect_identical(anyDuplicated(p$data$pu$id), 0L)
  expect_identical(anyDuplicated(p$data$features$id), 0L)
})

test_that("input_data fails if required feature distribution columns are missing", {
  toy <- toy_equivalent_basic()
  bad_dist <- toy$dist_features
  bad_dist$amount <- NULL

  expect_error(
    mosap::input_data(
      pu = toy$pu,
      features = toy$features,
      dist_features = bad_dist,
      cost = "cost"
    )
  )
})
