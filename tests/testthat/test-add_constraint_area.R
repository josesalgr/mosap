test_that("add_constraint_area stores one area constraint row", {
  toy <- toy_multiaction_semantics()
  toy$pu$area_ha <- c(1, 2, 3, 4, 5)

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(
      actions = toy$actions,
      cost = c(conservation = 1, restoration = 2)
    )

  p <- multiscape::add_constraint_area(
    p,
    area = 10,
    sense = "max",
    area_col = "area_ha",
    area_unit = "ha"
  )

  expect_true(is.data.frame(p$data$constraints$area))
  expect_equal(nrow(p$data$constraints$area), 1L)

  expect_true(all(c(
    "type", "sense", "value", "tolerance",
    "unit", "area_col", "actions", "name"
  ) %in% names(p$data$constraints$area)))

  expect_equal(p$data$constraints$area$type[1], "area")
  expect_equal(p$data$constraints$area$sense[1], "max")
  expect_equal(p$data$constraints$area$value[1], 10)
  expect_equal(p$data$constraints$area$tolerance[1], 0)
  expect_equal(p$data$constraints$area$unit[1], "ha")
  expect_equal(p$data$constraints$area$area_col[1], "area_ha")
  expect_true(is.na(p$data$constraints$area$actions[1]))
})

test_that("add_constraint_area appends constraints for different subsets or senses", {
  toy <- toy_multiaction_semantics()
  toy$pu$area_ha <- c(1, 2, 3, 4, 5)

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(
      actions = toy$actions,
      cost = c(conservation = 1, restoration = 2)
    )

  p <- p |>
    multiscape::add_constraint_area(
      area = 10,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha"
    ) |>
    multiscape::add_constraint_area(
      area = 4,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha",
      actions = "restoration"
    ) |>
    multiscape::add_constraint_area(
      area = 1,
      sense = "min",
      area_col = "area_ha",
      area_unit = "ha",
      actions = "restoration"
    )

  expect_true(is.data.frame(p$data$constraints$area))
  expect_equal(nrow(p$data$constraints$area), 3L)
})

test_that("add_constraint_area rejects duplicated actions-sense combinations", {
  toy <- toy_multiaction_semantics()
  toy$pu$area_ha <- c(1, 2, 3, 4, 5)

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(
      actions = toy$actions,
      cost = c(conservation = 1, restoration = 2)
    ) |>
    multiscape::add_constraint_area(
      area = 4,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha",
      actions = "restoration"
    )

  expect_error(
    p |>
      multiscape::add_constraint_area(
        area = 5,
        sense = "max",
        area_col = "area_ha",
        area_unit = "ha",
        actions = "restoration"
      ),
    "same subset of actions|same combination"
  )

  expect_no_error(
    p |>
      multiscape::add_constraint_area(
        area = 1,
        sense = "min",
        area_col = "area_ha",
        area_unit = "ha",
        actions = "restoration"
      )
  )
})

test_that("add_constraint_area validates inputs", {
  toy <- toy_multiaction_semantics()
  toy$pu$area_ha <- c(1, 2, 3, 4, 5)

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(
      actions = toy$actions,
      cost = c(conservation = 1, restoration = 2)
    )

  expect_error(
    multiscape::add_constraint_area(
      p,
      area = -1,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha"
    )
  )

  expect_error(
    multiscape::add_constraint_area(
      p,
      area = 1,
      sense = "equal",
      tolerance = -1,
      area_col = "area_ha",
      area_unit = "ha"
    )
  )

  expect_error(
    multiscape::add_constraint_area(
      p,
      area = 1,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha",
      name = ""
    ),
    "`name`"
  )

  expect_error(
    multiscape::add_constraint_area(
      p,
      area = 1,
      sense = "max",
      area_col = "area_ha",
      area_unit = "ha",
      actions = "does_not_exist"
    ),
    "subset did not match any action ids"
  )
})

test_that("compile_model works with stored area constraints", {
  skip_if_no_cbc()

  toy <- toy_multiaction_semantics()
  toy$pu$area_ha <- c(1, 1, 1, 1, 1)

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(
      actions = toy$actions,
      cost = c(conservation = 1, restoration = 2)
    ) |>
    multiscape::add_effects(
      effects = toy$effects,
      effect_type = "after"
    ) |>
    multiscape::add_constraint_targets_relative(0.2) |>
    multiscape::add_constraint_area(
      area = 6,
      sense = "max",
      area_col = "cost",
      area_unit = "m2"
    ) |>
    multiscape::add_constraint_area(
      area = 3,
      sense = "max",
      area_col = "cost",
      area_unit = "m2",
      actions = "restoration"
    ) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  expect_no_error(multiscape::compile_model(p))
})
