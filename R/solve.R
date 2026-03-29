#' @include internal.R
#'
#' @title Solve a planning problem
#'
#' @description
#' Solve a planning problem stored in a \code{Problem} object.
#'
#' This is the main execution step of the \pkg{paretoscape} workflow. It reads the
#' problem specification stored in \code{x$data}, builds the corresponding
#' optimization model when needed, applies the configured solver settings, and
#' returns either a \code{\link{solution-class}} or a
#' \code{\link{solutionset-class}} depending on whether the workflow is
#' single-objective or multi-objective.
#'
#' @details
#' \strong{Role of \code{solve()}}
#'
#' The typical \pkg{paretoscape} workflow is:
#' \preformatted{
#' x <- input_data(...)
#' x <- add_...(x, ...)
#' x <- set_...(x, ...)
#' res <- solve(x)
#' }
#'
#' Thus, \code{solve()} is the stage at which the stored problem specification is
#' turned into one or more optimization runs.
#'
#' \strong{What \code{solve()} reads from the problem object}
#'
#' The function uses the information stored in \code{x$data}, including:
#' \itemize{
#'   \item baseline planning data,
#'   \item actions, effects, profit, and spatial relations,
#'   \item targets and constraints,
#'   \item registered objectives,
#'   \item an optional multi-objective method configuration in
#'   \code{x$data$method},
#'   \item solver settings in \code{x$data$solve_args}.
#' }
#'
#' If a model has not yet been built, it is built internally during the solve
#' process. If a model snapshot or pointer already exists, the solving layer may
#' reuse or refresh it depending on the internal model state.
#'
#' \strong{Single-objective vs multi-objective behaviour}
#'
#' The behaviour of \code{solve()} depends on the problem configuration.
#'
#' \strong{Single-objective case}
#'
#' If exactly one objective is registered and no multi-objective method is
#' configured, \code{solve()} runs a single optimization problem and returns a
#' \code{\link{solution-class}} object.
#'
#' \strong{Multi-objective case}
#'
#' If a multi-objective method is configured in \code{x$data$method},
#' \code{solve()} dispatches internally according to the stored method name.
#'
#' Currently supported method names are:
#' \itemize{
#'   \item \code{"weighted"},
#'   \item \code{"epsilon_constraint"},
#'   \item \code{"augmecon"}.
#' }
#'
#' In these cases, \code{solve()} runs the corresponding multi-objective solving
#' workflow and returns a \code{\link{solutionset-class}} object.
#'
#' \strong{Consistency check}
#'
#' If multiple objectives are registered but no multi-objective method has been
#' selected, \code{solve()} stops with an error. In other words:
#'
#' \itemize{
#'   \item one objective and no MO method \eqn{\Rightarrow} single-objective
#'   solve,
#'   \item multiple objectives and a valid MO method \eqn{\Rightarrow}
#'   multi-objective solve,
#'   \item multiple objectives and no MO method \eqn{\Rightarrow} error.
#' }
#'
#' \strong{Solver settings}
#'
#' Solver configuration is read from \code{x$data$solve_args}, typically created
#' with \code{\link{set_solver}} or one of its convenience wrappers such as
#' \code{\link{set_solver_gurobi}}.
#'
#' These settings may include:
#' \itemize{
#'   \item the selected backend,
#'   \item time limits,
#'   \item optimality-gap settings,
#'   \item CPU cores,
#'   \item verbosity options,
#'   \item backend-specific solver parameters.
#' }
#'
#' \strong{Return value}
#'
#' The return type depends on the configured workflow:
#' \itemize{
#'   \item \code{Solution}: for single-objective optimization,
#'   \item \code{SolutionSet}: for multi-objective optimization.
#' }
#'
#' A \code{Solution} represents one optimization run. A \code{SolutionSet}
#' represents multiple runs together with their run table, design information,
#' and individual \code{Solution} objects.
#'
#' \strong{Method dispatch}
#'
#' \code{solve()} is an S3 generic. The public method documented here is
#' \code{solve.Problem()}, which operates on \code{Problem} objects. solve() is
#' the only function that should normally materialize the optimization model.
#'
#' @param x A \code{Problem} object created with \code{\link{input_data}} and
#'   optionally enriched with actions, effects, targets, constraints,
#'   objectives, spatial relations, method settings, and solver settings.
#' @param ... Additional arguments reserved for internal or legacy solver
#'   handling. These are not part of the main recommended user interface.
#'
#' @return
#' Either:
#' \itemize{
#'   \item a \code{\link{solution-class}} object when solving a single-objective
#'   problem, or
#'   \item a \code{\link{solutionset-class}} object when solving a configured
#'   multi-objective problem.
#' }
#'
#' @examples
#' \dontrun{
#' # ------------------------------------------------------------
#' # Single-objective solve
#' # ------------------------------------------------------------
#' x <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' x <- x |>
#'   add_targets_relative(0.3) |>
#'   add_objective_min_cost(alias = "cost") |>
#'   set_solver_gurobi(time_limit = 300, gap_limit = 0.01)
#'
#' sol <- solve(x)
#'
#' # ------------------------------------------------------------
#' # Multi-objective solve
#' # ------------------------------------------------------------
#' x <- x |>
#'   add_objective_min_fragmentation(alias = "frag") |>
#'   set_method_weighted(
#'     aliases = c("cost", "frag"),
#'     weights = c(0.5, 0.5),
#'     normalize_weights = TRUE
#'   )
#'
#' solset <- solve(x)
#' }
#'
#' @seealso
#' \code{\link{problem-class}},
#' \code{\link{solution-class}},
#' \code{\link{solutionset-class}},
#' \code{\link{set_solver}},
#' \code{\link{set_method_weighted}},
#' \code{\link{set_method_epsilon_constraint}},
#' \code{\link{set_method_augmecon}}
#'
#' @export
solve <- function(x, ...) {
  UseMethod("solve")
}

#' @rdname solve
#' @export
solve.Problem <- function(x, ...) {

  assertthat::assert_that(inherits(x, "Problem"))

  objs <- x$data$objectives %||% list()
  n_obj <- if (is.list(objs)) length(objs) else 0L

  method <- x$data$method %||% NULL
  has_method <- is.list(method) && length(method) > 0L

  if (has_method) {
    method_name <- as.character(method$type %||% method$name %||% NA_character_)[1]

    if (is.na(method_name) || !nzchar(method_name)) {
      stop("Invalid multi-objective method configuration: missing method name.", call. = FALSE)
    }

    .pamo_validate_objectives(x)
    #x <- compile_model(x)

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

  if (n_obj > 1L) {
    stop(
      "Multiple objectives are registered but no multi-objective method was selected.\n",
      "Use set_method_weighted(), set_method_epsilon_constraint(), etc.",
      call. = FALSE
    )
  }

  x <- compile_model(x)
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
