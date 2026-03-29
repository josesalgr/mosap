#' @title Configure solver settings
#'
#' @description
#' Store solver configuration inside a \code{Problem} object so that
#' \code{\link{solve}} can later run using the stored backend and runtime
#' options.
#'
#' This function does not build or solve the optimization model. It only updates
#' the solver configuration stored in \code{x$data$solve_args}.
#'
#' @details
#' \strong{Purpose}
#'
#' The \code{paretoscape} workflow separates problem specification from solver
#' configuration. Problem data, actions, effects, targets, objectives, and
#' methods are stored in the \code{Problem} object, and solver settings are
#' stored separately in \code{x$data$solve_args}.
#'
#' This function allows solver options to be configured once and reused later
#' through \code{\link{solve}(x)} without repeating the same arguments each time.
#'
#' \strong{Stored fields}
#'
#' The solver configuration is stored in \code{x$data$solve_args}. Typical
#' entries include:
#' \itemize{
#'   \item \code{solver},
#'   \item \code{gap_limit},
#'   \item \code{time_limit},
#'   \item \code{solution_limit},
#'   \item \code{cores},
#'   \item \code{verbose},
#'   \item \code{write_log},
#'   \item \code{log_file},
#'   \item \code{solver_params}.
#' }
#'
#' \strong{Incremental update semantics}
#'
#' This function updates solver settings incrementally.
#'
#' If an argument is supplied as \code{NULL}, the previously stored value is
#' kept unchanged. Therefore, repeated calls can be used to modify only selected
#' components of the solver configuration.
#'
#' For example, a user may first configure the solver backend and time limit,
#' and later update only the optimality gap or only a backend-specific
#' parameter.
#'
#' \strong{Gap limit}
#'
#' The argument \code{gap_limit} is interpreted as a relative optimality gap for
#' mixed-integer optimization. It must lie in \eqn{[0,1]}.
#'
#' If the solver stops with incumbent value \eqn{z^{\mathrm{inc}}} and best
#' bound \eqn{z^{\mathrm{bd}}}, then the exact stopping rule depends on the
#' solver backend, but conceptually \code{gap_limit} controls the maximum
#' accepted relative difference between the incumbent and the bound.
#'
#' \strong{Time limit}
#'
#' The argument \code{time_limit} is interpreted as a maximum wall-clock time in
#' seconds allowed for the solver.
#'
#' \strong{Solution limit}
#'
#' The argument \code{solution_limit} is stored as a logical flag. Its exact
#' meaning depends on the backend-specific solving layer, but conceptually it
#' requests early termination after finding a feasible solution according to the
#' behaviour supported by the chosen solver.
#'
#' \strong{Cores}
#'
#' The argument \code{cores} specifies the number of CPU cores to use. If the
#' requested number exceeds the number of detected cores, it is capped to the
#' detected maximum with a warning.
#'
#' \strong{Verbose output and log files}
#'
#' The arguments \code{verbose}, \code{write_log}, and \code{log_file}
#' control how solver logging is handled. These options are stored and later
#' interpreted by the solving layer for the selected backend.
#'
#' \strong{Solver-specific parameters}
#'
#' Additional backend-specific parameters can be passed in two ways:
#' \itemize{
#'   \item through the named list \code{solver_params},
#'   \item through additional named arguments in \code{...}.
#' }
#'
#' These two sources are merged, and the result is then merged with any
#' previously stored \code{solver_params}. Existing parameters are therefore
#' preserved unless explicitly overwritten.
#'
#' This is particularly useful for backend-specific controls such as node
#' selection, emphasis parameters, tolerances, or heuristics.
#'
#' \strong{Supported backends}
#'
#' The \code{solver} argument selects the backend to be used later by
#' \code{\link{solve}}. Supported values are:
#' \itemize{
#'   \item \code{"auto"}: let the solving layer choose an available backend,
#'   \item \code{"gurobi"},
#'   \item \code{"cplex"},
#'   \item \code{"cbc"},
#'   \item \code{"symphony"}.
#' }
#'
#' This function only stores the requested backend. Availability of the backend
#' is checked later when solving.
#'
#' @param x A \code{Problem} object.
#' @param solver Character string indicating the solver backend to use. Must be
#'   one of \code{"auto"}, \code{"gurobi"}, \code{"cplex"},
#'   \code{"cbc"}, or \code{"symphony"}.
#' @param gap_limit Optional numeric value in \eqn{[0,1]} giving the relative
#'   optimality gap for mixed-integer optimization. If \code{NULL}, the
#'   previously stored value is kept unchanged.
#' @param time_limit Optional non-negative numeric value giving the maximum
#'   solving time in seconds. If \code{NULL}, the previously stored value is
#'   kept unchanged.
#' @param solution_limit Optional logical flag controlling backend-specific
#'   early stopping after feasible solution discovery. If \code{NULL}, the
#'   previously stored value is kept unchanged.
#' @param cores Optional positive integer giving the number of CPU cores to use.
#'   If \code{NULL}, the previously stored value is kept unchanged.
#' @param verbose Optional logical flag indicating whether the solver should
#'   print log output. If \code{NULL}, the previously stored value is kept
#'   unchanged.
#' @param log_file Optional character string giving the name of the
#'   solver log file. If \code{NULL}, the previously stored value is kept
#'   unchanged.
#' @param write_log Optional logical flag indicating whether solver output
#'   should be written to a file. If \code{NULL}, the previously stored value is
#'   kept unchanged.
#' @param solver_params Named list of solver-specific parameters. These are
#'   merged with previously stored backend-specific parameters rather than
#'   replacing them completely.
#' @param ... Additional named solver-specific parameters. These are merged into
#'   \code{solver_params}. For example, \code{MIPFocus = 1} for Gurobi.
#'
#' @return An updated \code{Problem} object with modified solver settings stored
#'   in \code{x$data$solve_args}.
#'
#' @seealso
#' \code{\link{solve}},
#' \code{\link{set_solver_gurobi}},
#' \code{\link{set_solver_cplex}},
#' \code{\link{set_solver_cbc}},
#' \code{\link{set_solver_symphony}}
#'
#' @examples
#' \dontrun{
#' x <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' x <- set_solver(
#'   x,
#'   solver = "gurobi",
#'   gap_limit = 0.01,
#'   time_limit = 300,
#'   cores = 4,
#'   verbose = TRUE,
#'   MIPFocus = 1
#' )
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
set_solver <- function(x,
                       solver = c("auto", "gurobi", "cplex", "cbc", "symphony"),
                       gap_limit = NULL,
                       time_limit = NULL,
                       solution_limit = NULL,
                       cores = NULL,
                       verbose = FALSE,
                       log_file = NULL,
                       write_log = NULL,
                       solver_params = list(),
                       ...) {

  stopifnot(inherits(x, "Problem"))
  solver <- match.arg(solver)

  dots <- list(...)
  if (length(dots) > 0) {
    solver_params <- modifyList(solver_params %||% list(), dots)
  }
  if (!is.list(solver_params)) stop("solver_params must be a list.", call. = FALSE)

  if (is.null(x$data$solve_args) || !is.list(x$data$solve_args)) x$data$solve_args <- list()

  # start from stored (so we can keep values when args are NULL)
  out <- x$data$solve_args

  # always set solver if explicitly provided
  out$solver <- solver

  # set numeric/logical args only if not NULL
  if (!is.null(gap_limit)) {
    assertthat::assert_that(assertthat::is.scalar(gap_limit), is.finite(gap_limit), gap_limit >= 0, gap_limit <= 1)
    out$gap_limit <- base::round(as.numeric(gap_limit), 3)
  }
  if (!is.null(time_limit)) {
    assertthat::assert_that(assertthat::is.scalar(time_limit), is.finite(time_limit), time_limit >= 0)
    out$time_limit <- base::round(as.numeric(time_limit), 3)
  }
  if (!is.null(solution_limit)) {
    assertthat::assert_that(assertthat::is.flag(solution_limit))
    out$solution_limit <- isTRUE(solution_limit)
  }
  if (!is.null(cores)) {
    assertthat::assert_that(assertthat::is.count(cores))
    cores <- as.integer(cores)
    max_cores <- parallel::detectCores(TRUE)
    if (is.finite(max_cores) && cores > max_cores) {
      warning("cores is larger than detected cores; capping to detected cores.", call. = FALSE, immediate. = TRUE)
      cores <- as.integer(max_cores)
    }
    out$cores <- cores
  }
  if (!is.null(verbose)) {
    assertthat::assert_that(assertthat::is.flag(verbose))
    out$verbose <- isTRUE(verbose)
  }
  if (!is.null(write_log)) {
    assertthat::assert_that(assertthat::is.flag(write_log))
    out$output_file <- isTRUE(write_log)
  }
  if (!is.null(log_file)) {
    assertthat::assert_that(assertthat::is.string(log_file))
    out$name_output_file <- as.character(log_file)[1]
  }

  # merge solver_params with stored solver_params (do not drop existing ones)
  out$solver_params <- modifyList(out$solver_params %||% list(), solver_params)

  x$data$solve_args <- out
  x
}

#' @title Configure Gurobi solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that stores
#' \code{solver = "gurobi"} in the problem object.
#'
#' This function does not solve the model. It only updates the stored solver
#' configuration.
#'
#' @inheritParams set_solver
#'
#' @return An updated \code{Problem} object with Gurobi solver settings stored
#'   in \code{x$data$solve_args}.
#'
#' @seealso
#' \code{\link{set_solver}},
#' \code{\link{solve}}
#'
#' @examples
#' \dontrun{
#' x <- set_solver_gurobi(
#'   x,
#'   gap_limit = 0.01,
#'   time_limit = 600,
#'   cores = 4,
#'   MIPFocus = 1
#' )
#' }
#'
#' @export
set_solver_gurobi <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                              solution_limit = NULL, cores = NULL, verbose = FALSE,
                              log_file = NULL, write_log = NULL) {
  set_solver(
    x,
    solver = "gurobi",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    log_file = log_file,
    write_log = write_log,
    solver_params = solver_params,
    ...
  )
}

#' @title Configure CPLEX solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that stores
#' \code{solver = "cplex"} in the problem object.
#'
#' This function does not solve the model. It only updates the stored solver
#' configuration.
#'
#' @inheritParams set_solver
#'
#' @return An updated \code{Problem} object with CPLEX solver settings stored in
#'   \code{x$data$solve_args}.
#'
#' @seealso
#' \code{\link{set_solver}},
#' \code{\link{solve}}
#'
#' @examples
#' \dontrun{
#' x <- set_solver_cplex(
#'   x,
#'   gap_limit = 0.001,
#'   time_limit = 1200,
#'   cores = 8
#' )
#' }
#'
#' @export
set_solver_cbc <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                           solution_limit = NULL, cores = NULL, verbose = FALSE,
                           log_file = NULL, write_log = NULL) {
  set_solver(
    x,
    solver = "cbc",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    log_file = log_file,
    write_log = write_log,
    solver_params = solver_params,
    ...
  )
}

#' Configure CPLEX solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that sets
#' \code{solver = "cplex"}.
#'
#' @inheritParams set_solver
#'
#' @return
#' An updated \code{Problem} object with CPLEX solver settings.
#'
#' @seealso
#' \code{\link{set_solver}},
#' \code{\link{solve}}
#'
#' @examples
#' \dontrun{
#' x <- set_solver_cplex(
#'   x,
#'   gap_limit = 0.001,
#'   time_limit = 1200,
#'   cores = 8
#' )
#' }
#'
#' @export
set_solver_cplex <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                             solution_limit = NULL, cores = NULL, verbose = FALSE,
                             log_file = NULL, write_log = NULL) {
  set_solver(
    x,
    solver = "cplex",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    log_file = log_file,
    write_log = write_log,
    solver_params = solver_params,
    ...
  )
}

#' @title Configure SYMPHONY solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that stores
#' \code{solver = "symphony"} in the problem object.
#'
#' This function does not solve the model. It only updates the stored solver
#' configuration.
#'
#' @inheritParams set_solver
#'
#' @return An updated \code{Problem} object with SYMPHONY solver settings stored
#'   in \code{x$data$solve_args}.
#'
#' @seealso
#' \code{\link{set_solver}},
#' \code{\link{solve}}
#'
#' @examples
#' \dontrun{
#' x <- set_solver_symphony(
#'   x,
#'   gap_limit = 0.05,
#'   time_limit = 300
#' )
#' }
#'
#' @export
set_solver_symphony <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                                solution_limit = NULL, cores = NULL, verbose = FALSE,
                                log_file = NULL, write_log = NULL) {
  set_solver(
    x,
    solver = "symphony",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    log_file = log_file,
    write_log = write_log,
    solver_params = solver_params,
    ...
  )
}
