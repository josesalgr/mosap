test_that("add_targets_relative computes absolute targets from total amounts", {
  toy <- toy_equivalent_basic()

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  )

  p <- mosap::add_targets_relative(p, 0.5)

  expect_true(is.data.frame(p$data$targets))
  expect_true(all(c("feature", "target_value") %in% names(p$data$targets)))

  total_f1 <- sum(toy$dist_features$amount[toy$dist_features$feature == 1])
  total_f2 <- sum(toy$dist_features$amount[toy$dist_features$feature == 2])

  tgt <- p$data$targets
  tgt_f1 <- tgt$target_value[tgt$feature == 1]
  tgt_f2 <- tgt$target_value[tgt$feature == 2]

  expect_equal(tgt_f1, 0.5 * total_f1)
  expect_equal(tgt_f2, 0.5 * total_f2)

  expect_true(all(tgt$type == "actions"))
  expect_true(all(tgt$sense == "ge"))
  expect_true(all(tgt$target_unit == "relative_baseline"))
  expect_equal(tgt$target_raw[tgt$feature == 1], 0.5)
  expect_equal(tgt$basis_total[tgt$feature == 1], total_f1)
})
