test_that("add_constraint_budget stores one global budget constraint row", {
  toy <- toy_multiaction_semantics()

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

  p <- multiscape::add_constraint_budget(
    p,
    budget = 10,
    sense = "max",
    include_pu_cost = TRUE,
    include_action_cost = TRUE
  )

  expect_true(is.data.frame(p$data$constraints$budget))
  expect_equal(nrow(p$data$constraints$budget), 1L)

  expect_true(all(c(
    "type", "sense", "value", "tolerance", "actions",
    "include_pu_cost", "include_action_cost", "name"
  ) %in% names(p$data$constraints$budget)))

  expect_equal(p$data$constraints$budget$type[1], "budget")
  expect_equal(p$data$constraints$budget$sense[1], "max")
  expect_equal(p$data$constraints$budget$value[1], 10)
  expect_equal(p$data$constraints$budget$tolerance[1], 0)
  expect_true(is.na(p$data$constraints$budget$actions[1]))
  expect_true(p$data$constraints$budget$include_pu_cost[1])
  expect_true(p$data$constraints$budget$include_action_cost[1])
})

test_that("add_constraint_budget appends constraints for different subsets or senses", {
  toy <- toy_multiaction_semantics()

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
    multiscape::add_constraint_budget(
      budget = 10,
      sense = "max",
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    ) |>
    multiscape::add_constraint_budget(
      budget = 4,
      sense = "max",
      actions = "restoration",
      include_pu_cost = FALSE,
      include_action_cost = TRUE
    ) |>
    multiscape::add_constraint_budget(
      budget = 1,
      sense = "min",
      actions = "restoration",
      include_pu_cost = FALSE,
      include_action_cost = TRUE
    )

  expect_true(is.data.frame(p$data$constraints$budget))
  expect_equal(nrow(p$data$constraints$budget), 3L)
})

test_that("add_constraint_budget rejects duplicated actions-sense combinations", {
  toy <- toy_multiaction_semantics()

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
    multiscape::add_constraint_budget(
      budget = 4,
      sense = "max",
      actions = "restoration",
      include_pu_cost = FALSE,
      include_action_cost = TRUE
    )

  expect_error(
    p |>
      multiscape::add_constraint_budget(
        budget = 5,
        sense = "max",
        actions = "restoration",
        include_pu_cost = FALSE,
        include_action_cost = TRUE
      ),
    "same subset of actions|same combination"
  )

  expect_no_error(
    p |>
      multiscape::add_constraint_budget(
        budget = 1,
        sense = "min",
        actions = "restoration",
        include_pu_cost = FALSE,
        include_action_cost = TRUE
      )
  )
})

test_that("add_constraint_budget validates cost-component settings", {
  toy <- toy_multiaction_semantics()

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
    multiscape::add_constraint_budget(
      p,
      budget = 5,
      sense = "max",
      include_pu_cost = FALSE,
      include_action_cost = FALSE
    ),
    "At least one"
  )

  expect_error(
    multiscape::add_constraint_budget(
      p,
      budget = 5,
      sense = "max",
      actions = "restoration",
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    ),
    "include_pu_cost = TRUE.*actions = NULL|not action-specific"
  )
})

test_that("add_constraint_budget validates other inputs", {
  toy <- toy_multiaction_semantics()

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
    multiscape::add_constraint_budget(
      p,
      budget = -1,
      sense = "max",
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    )
  )

  expect_error(
    multiscape::add_constraint_budget(
      p,
      budget = 1,
      sense = "equal",
      tolerance = -1,
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    )
  )

  expect_error(
    multiscape::add_constraint_budget(
      p,
      budget = 1,
      sense = "max",
      include_pu_cost = TRUE,
      include_action_cost = TRUE,
      name = ""
    ),
    "`name`"
  )

  expect_error(
    multiscape::add_constraint_budget(
      p,
      budget = 1,
      sense = "max",
      actions = "does_not_exist",
      include_pu_cost = FALSE,
      include_action_cost = TRUE
    ),
    "subset did not match any action ids"
  )
})

test_that("compile_model works with stored budget constraints", {
  skip_if_no_cbc()

  toy <- toy_multiaction_semantics()

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
    multiscape::add_constraint_budget(
      budget = 12,
      sense = "max",
      include_pu_cost = TRUE,
      include_action_cost = TRUE
    ) |>
    multiscape::add_constraint_budget(
      budget = 4,
      sense = "max",
      actions = "restoration",
      include_pu_cost = FALSE,
      include_action_cost = TRUE
    ) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  expect_no_error(multiscape::compile_model(p))
})
