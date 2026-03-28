#' Compile the optimization model stored in a Problem
#'
#' @description
#' Materializes the optimization model represented by a `Problem` object without
#' solving it. This is an advanced function mainly intended for debugging,
#' inspection, and explicit model preparation.
#'
#' In standard workflows, users normally do not need to call this function,
#' because [solve()] compiles the model automatically when needed.
#'
#' @param x A `Problem` object.
#' @param force Logical. If `TRUE`, rebuild the model even if a current compiled
#'   model already exists.
#' @param ... Reserved for future extensions.
#'
#' @return
#' A `Problem` object with compiled model structures stored internally.
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

  if (n_obj < 1L) {
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
