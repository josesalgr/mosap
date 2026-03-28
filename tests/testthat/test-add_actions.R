test_that("add_actions creates one feasible row per pu-action pair", {
  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  p <- mosap::add_actions(
    p,
    actions = toy$actions,
    cost = 0
  )

  expect_true(is.data.frame(p$data$actions))
  expect_true(is.data.frame(p$data$dist_actions))

  expect_equal(nrow(p$data$actions), 1)
  expect_equal(nrow(p$data$dist_actions), nrow(toy$pu) * nrow(toy$actions))

  expect_true(all(c("pu", "action") %in% names(p$data$dist_actions)))
  expect_equal(anyDuplicated(p$data$dist_actions[, c("pu", "action")]), 0)

  expect_true("internal_action" %in% names(p$data$dist_actions))
  expect_true("internal_pu" %in% names(p$data$dist_actions))
})

test_that("add_actions stores a constant action cost correctly", {
  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  p <- mosap::add_actions(
    p,
    actions = toy$actions,
    cost = 7
  )

  expect_true("cost" %in% names(p$data$dist_actions))
  expect_equal(unique(p$data$dist_actions$cost), 7)
})
