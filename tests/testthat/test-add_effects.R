test_that("add_effects with delta separates benefit and loss correctly", {
  toy <- toy_equivalent_basic()

  effects_delta <- data.frame(
    pu = c(1, 2, 3, 4),
    action = c("conservation", "conservation", "conservation", "conservation"),
    feature = c(1, 1, 2, 2),
    delta = c(5, -2, 0, -7)
  )

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = effects_delta, effect_type = "delta")

  de <- p$data$dist_effects

  expect_true(all(c("benefit", "loss") %in% names(de)))

  row1 <- de[de$pu == 1 & de$feature == 1, ]
  row2 <- de[de$pu == 2 & de$feature == 1, ]
  row4 <- de[de$pu == 4 & de$feature == 2, ]

  expect_equal(row1$benefit, 5)
  expect_equal(row1$loss, 0)

  expect_equal(row2$benefit, 0)
  expect_equal(row2$loss, 2)

  expect_equal(row4$benefit, 0)
  expect_equal(row4$loss, 7)
})

test_that("add_effects with after computes delta from baseline amounts", {
  toy <- toy_equivalent_basic()

  effects_after <- data.frame(
    pu = c(1, 2),
    action = c("conservation", "conservation"),
    feature = c(1, 2),
    after = c(10, 1)
  )

  # baseline: pu1-feature1 = 8 ; pu2-feature2 = 2
  # delta esperado: +2 y -1

  p <- mosap::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    mosap::add_actions(actions = toy$actions, cost = 0) |>
    mosap::add_effects(effects = effects_after, effect_type = "after")

  de <- p$data$dist_effects

  r1 <- de[de$pu == 1 & de$feature == 1, ]
  r2 <- de[de$pu == 2 & de$feature == 2, ]

  expect_equal(r1$benefit, 2)
  expect_equal(r1$loss, 0)

  expect_equal(r2$benefit, 0)
  expect_equal(r2$loss, 1)
})
