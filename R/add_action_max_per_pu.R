#' @include internal.R
#'
#' @title Limit the number of actions per planning unit (max)
#'
#' @description
#' Stores a constraint of the form:
#' \deqn{\sum_a x_{pu,a} \le \mathrm{max}}
#' for each planning unit (optionally for a subset of PUs and/or actions).
#'
#' This function is **data-only**: it does not build or modify the optimization model.
#' The stored constraint must be applied later inside the model builder
#' (e.g., in `.pa_build_model_apply_constraints()`).
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param max Integer >= 0. Maximum number of actions allowed per PU.
#'   Default 1.
#' @param pu Optional vector of PU ids (external ids, i.e., `x$data$pu$id`) to which
#'   the constraint is applied. If NULL (default), applies to all PUs.
#' @param actions Optional vector of action ids (character, i.e., `x$data$actions$id`)
#'   to include in the sum. If NULL (default), includes all actions.
#' @param overwrite Logical. If TRUE, replaces any existing max-per-PU action limit constraint.
#'
#' @return Updated [data-class] object.
#' @export
add_action_max_per_pu <- function(
    x,
    max = 1L,
    pu = NULL,
    actions = NULL,
    overwrite = FALSE
) {
  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a prioriactions Data object")
  assertthat::assert_that(!is.null(x$data$pu), msg = "x$data$pu is missing. Run inputData()/inputDataSpatial() first.")
  assertthat::assert_that(!is.null(x$data$dist_actions), msg = "No actions found. Run add_actions() first.")
  assertthat::assert_that(!is.null(x$data$actions), msg = "No action catalog found. Run add_actions() first.")

  # ---- validate max
  if (length(max) != 1 || is.na(max)) stop("'max' must be a single non-NA integer.", call. = FALSE)
  max <- as.integer(max)
  if (max < 0L) stop("'max' must be >= 0.", call. = FALSE)

  pu_df   <- x$data$pu
  da      <- x$data$dist_actions
  acts_df <- x$data$actions

  # required cols
  assertthat::assert_that("id" %in% names(pu_df), msg = "x$data$pu must have column 'id'.")
  assertthat::assert_that(all(c("pu","action") %in% names(da)),
                          msg = "x$data$dist_actions must have columns 'pu' and 'action'.")
  assertthat::assert_that("id" %in% names(acts_df), msg = "x$data$actions must have column 'id'.")

  # normalize types
  pu_ids_all <- as.integer(pu_df$id)
  act_ids_all <- as.character(acts_df$id)

  # ---- subset of PUs
  if (!is.null(pu)) {
    pu <- as.integer(pu)
    if (anyNA(pu)) stop("'pu' contains NA after coercion.", call. = FALSE)
    bad <- setdiff(unique(pu), pu_ids_all)
    if (length(bad) > 0) stop("Unknown PU id(s) in 'pu': ", paste(bad, collapse = ", "), call. = FALSE)
    pu_ids_use <- unique(pu)
  } else {
    pu_ids_use <- pu_ids_all
  }

  # ---- subset of actions
  if (!is.null(actions)) {
    actions <- as.character(actions)
    if (anyNA(actions)) stop("'actions' contains NA.", call. = FALSE)
    bad <- setdiff(unique(actions), act_ids_all)
    if (length(bad) > 0) stop("Unknown action id(s) in 'actions': ", paste(bad, collapse = ", "), call. = FALSE)
    action_ids_use <- unique(actions)
  } else {
    action_ids_use <- act_ids_all
  }

  # ---- sanity: does it select at least one feasible (pu,action) row?
  da2 <- da
  da2$pu <- as.integer(da2$pu)
  da2$action <- as.character(da2$action)

  da2 <- da2[da2$pu %in% pu_ids_use & da2$action %in% action_ids_use, , drop = FALSE]
  if (nrow(da2) == 0) {
    stop(
      "The (pu, actions) subset produces zero feasible (pu, action) pairs.\n",
      "Check that 'pu' and 'actions' match what exists in dist_actions.",
      call. = FALSE
    )
  }

  # ---- store constraint spec (data-only)
  spec <- list(
    type = "action_max_per_pu",
    max = max,
    pu = pu_ids_use,
    actions = action_ids_use
  )

  if (is.null(x$data$constraints) || !is.list(x$data$constraints)) x$data$constraints <- list()

  if (!isTRUE(overwrite) && !is.null(x$data$constraints$action_max_per_pu)) {
    stop(
      "An action_max_per_pu constraint already exists. Use overwrite=TRUE to replace it.",
      call. = FALSE
    )
  }

  x$data$constraints$action_max_per_pu <- spec

  # mark model dirty if already built
  if (!is.null(x$data$model_ptr)) {
    if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
