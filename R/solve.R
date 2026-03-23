#' @include internal.R internalMO.R
#'
#' @title Solve optimization model
#'
#' @description
#' Solves a model defined by a \code{Problem} object. Solver configuration is read from
#' \code{x$data$solve_args} (typically set via \code{set_solver()} / \code{set_solver_*()}).
#'
#' @param x A \code{Problem} object created with \code{inputData()} or \code{inputDataSpatial()}.
#' @param ... Optional legacy solver arguments (deprecated).
#'
#' @return A \code{Solution} object.
#' @export
solve <- function(x, ...) {
  UseMethod("solve")
}

#' @export
solve.Problem <- function(x, ...) {

  assertthat::assert_that(inherits(x, "Problem"))

  # registro de objetivos
  objs <- x$data$objectives %||% list()
  n_obj <- if (is.list(objs)) length(objs) else 0L

  # método MO configurado
  method <- x$data$method %||% NULL
  has_method <- is.list(method) && length(method) > 0L

  if (has_method) {
    method_name <- as.character(method$type %||% method$name %||% NA_character_)[1]

    if (is.na(method_name) || !nzchar(method_name)) {
      stop("Invalid multi-objective method configuration: missing method name.", call. = FALSE)
    }

    .pamo_validate_objectives(x)

    res <- switch(
      method_name,
      weighted = .pamo_solve_weighted(x, ...),
      epsilon_constraint = .pamo_solve_epsilon_constraint(x, ...),
      augmecon = .pamo_solve_augmecon(x, ...),
      stop("Unknown/unsupported multi-objective method: '", method_name, "'.", call. = FALSE)
    )

    if (!inherits(res, "SolutionSet")) {
      stop(
        "Internal error: multi-objective solve did not return a SolutionSet object.\n",
        "Returned class: ", paste(class(res), collapse = ", "),
        call. = FALSE
      )
    }

    return(res)
  }

  # si no hay método pero hay múltiples objetivos, error
  if (n_obj > 1L) {
    stop(
      "Multiple objectives are registered but no multi-objective method was selected.\n",
      "Use set_method_weighted(), set_method_epsilon_constraint(), etc.",
      call. = FALSE
    )
  }

  # caso normal single-objective
  res <- .pa_solve_single_problem(x, ...)

  if (!inherits(res, "Solution")) {
    stop(
      "Internal error: single-objective solve did not return a Solution object.\n",
      "Returned class: ", paste(class(res), collapse = ", "),
      call. = FALSE
    )
  }

  res
}


#'
#' #' @method solve MOProblem
#' #' @export
#' solve.MOProblem <- function(x, ..., return = c("problem", "results")) {
#'
#'   x <- .pamo_as_mo(x)
#'   stopifnot(inherits(x, "MOProblem"))
#'
#'   return <- match.arg(return)
#'
#'   .pamo_validate_objectives(x)
#'
#'   if (is.null(x$method) || !is.list(x$method) || identical(x$method$name %||% "none", "none")) {
#'     stop("No multi-objective method configured. Use set_method_*().", call. = FALSE)
#'   }
#'
#'   method_name <- as.character(x$method$name %||% NA_character_)[1]
#'   if (is.na(method_name) || !nzchar(method_name)) {
#'     stop("Invalid multi-objective method configuration: missing method name.", call. = FALSE)
#'   }
#'
#'   if (identical(method_name, "weighted")) {
#'     res <- .pamo_solve_weighted(x, ...)
#'   } else if (identical(method_name, "epsilon_constraint")) {
#'     res <- .pamo_solve_epsilon_constraint(x, ...)
#'   } else {
#'     stop("Unknown/unsupported method: '", method_name, "'.", call. = FALSE)
#'   }
#'
#'   if (!inherits(res, "SolutionSet")) {
#'     stop(
#'       "Internal error: multi-objective solve did not return a SolutionSet object.\n",
#'       "Returned class: ", paste(class(res), collapse = ", "),
#'       call. = FALSE
#'     )
#'   }
#'
#'   x$results <- res
#'
#'   if (identical(return, "results")) {
#'     return(res)
#'   }
#'
#'   x
#' }
#'
#'





