#' Compile the optimization model stored in a Problem
#'
#' @description
#' Materializes the optimization model represented by a \code{Problem} object
#' without solving it. This is an advanced function mainly intended for
#' debugging, inspection, and explicit model preparation.
#'
#' In standard workflows, users normally do not need to call this function,
#' because \code{\link{solve}} compiles the model automatically when needed.
#'
#' @details
#' Use this function when you want to prepare the optimization model explicitly
#' before solving, inspect compiled model structures, or verify that the problem
#' compiles successfully.
#'
#' Conceptually, a \code{Problem} object stores a declarative optimization
#' specification: planning data, actions, effects, targets, constraints,
#' objectives, spatial relations, and optional method or solver settings.
#' \code{compile_model()} transforms that stored specification into an internal
#' compiled model representation that can later be reused by the solving layer.
#'
#' The exact compiled representation is implementation-specific, but it may
#' include indexed variables, prepared constraint blocks, objective structures,
#' and internal model snapshots or pointers.
#'
#' Compilation does not solve the optimization problem. Therefore, a problem may
#' compile successfully and still later be infeasible, numerically difficult, or
#' otherwise fail during solver execution.
#'
#' @param x A \code{Problem} object.
#' @param force Logical. If \code{TRUE}, rebuild the model even if a current
#'   compiled model already exists.
#' @param ... Reserved for future extensions.
#'
#' @return
#' A \code{Problem} object with compiled model structures stored internally.
#'
#' @examples
#' \dontrun{
#' x <- create_problem(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' x <- x |>
#'   add_constraint_targets_relative(0.3) |>
#'   add_objective_min_cost(alias = "cost")
#'
#' x <- compile_model(x)
#'
#' # Force recompilation
#' x <- compile_model(x, force = TRUE)
#' }
#'
#' @seealso
#' \code{\link{solve}}
#'
#' @export
compile_model <- function(x, force = FALSE, ...) {
  UseMethod("compile_model")
}

#' @rdname compile_model
#' @export
compile_model.Problem <- function(x, force = FALSE, ...) {
  assertthat::assert_that(inherits(x, "Problem"))

  force <- isTRUE(force)

  objs <- x$data$objectives %||% list()
  n_obj <- if (is.list(objs)) length(objs) else 0L

  active_model_type <- x$data$model_args$model_type %||% NULL
  has_active_objective <- !is.null(active_model_type) &&
    nzchar(as.character(active_model_type)[1])

  method <- x$data$method %||% NULL
  has_method <- is.list(method) && length(method) > 0L

  if (!force && .pa_model_is_current(x)) {
    return(x)
  }

  if (has_method) {
    method_name <- as.character(method$type %||% method$name %||% NA_character_)[1]

    if (is.na(method_name) || !nzchar(method_name)) {
      stop(
        "Invalid multi-objective method configuration: missing method name.",
        call. = FALSE
      )
    }

    .pamo_validate_objectives(x)

    x <- .pamo_compile_problem(
      x = x,
      method_name = method_name,
      ...
    )

    if (is.null(x$data$model_ptr)) {
      stop(
        "Internal error: multi-objective compilation did not create model_ptr.",
        call. = FALSE
      )
    }

    if (is.null(x$data$model_list) || isTRUE(x$data$meta$model_dirty)) {
      x <- .pa_refresh_model_snapshot(x)
    }

    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- FALSE
    x$data$has_model <- TRUE

    return(x)
  }

  if (n_obj > 1L) {
    stop(
      "Multiple objectives are registered but no multi-objective method was selected.\n",
      "Use set_method_weighted(), set_method_epsilon_constraint(), etc.",
      call. = FALSE
    )
  }

  if (n_obj < 1L && !isTRUE(has_active_objective)) {
    stop(
      "No objective configured. Add an objective before compiling the model.",
      call. = FALSE
    )
  }

  x <- .pa_compile_problem(x, ...)

  if (is.null(x$data$model_ptr)) {
    stop(
      "Internal error: model compilation did not create model_ptr.",
      call. = FALSE
    )
  }

  if (is.null(x$data$model_list) || isTRUE(x$data$meta$model_dirty)) {
    x <- .pa_refresh_model_snapshot(x)
  }

  x$data$meta <- x$data$meta %||% list()
  x$data$meta$model_dirty <- FALSE
  x$data$has_model <- TRUE

  x
}
