#' Configure solver settings
#'
#' @description
#' Store solver configuration inside a \code{Problem} object so that
#' \code{solve(x)} can run without repeating solver arguments.
#'
#' This function does not build or solve the optimization model. It only stores
#' runtime options related to the solver backend.
#'
#' @param x A \code{Problem} object created with \code{\link{inputData}} or
#'   \code{\link{inputDataSpatial}}.
#'
#' @param solver Character string indicating the solver backend to use.
#'   One of \code{"auto"}, \code{"gurobi"}, \code{"cplex"},
#'   \code{"cbc"}, or \code{"symphony"}.
#'
#' @param gap_limit Numeric value in \code{[0, 1]} giving the relative
#'   optimality gap for mixed-integer optimization. If \code{NULL},
#'   the currently stored value is kept unchanged.
#'
#' @param time_limit Numeric. Maximum solving time in seconds.
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param solution_limit Logical. Whether the solver should stop after
#'   finding a feasible solution according to the backend-specific behavior.
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param cores Integer. Number of CPU cores to use.
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param verbose Logical. Should the solver print log output?
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param name_output_file Character string with the name of the solver log file.
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param output_file Logical. Should solver output be written to a file?
#'   If \code{NULL}, the currently stored value is kept unchanged.
#'
#' @param solver_params A named list of solver-specific parameters.
#'   These values are merged with previously stored solver parameters rather
#'   than replacing them completely.
#'
#' @param ... Additional solver-specific parameters passed as named arguments.
#'   These are merged into \code{solver_params}. For example,
#'   \code{MIPFocus = 1} for Gurobi.
#'
#' @details
#' The function updates the \code{solve_args} slot stored in the input
#' \code{Problem} object. Arguments set to \code{NULL} are not modified, so the
#' function can be used incrementally to update only selected solver options.
#'
#' Solver-specific parameters supplied through \code{solver_params} and
#' \code{...} are merged with any previously stored parameters.
#'
#' @return
#' An updated \code{Problem} object with modified solver settings.
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
#' x <- inputData(pu = pu, features = features)
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
                       name_output_file = NULL,
                       output_file = NULL,
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
  if (!is.null(output_file)) {
    assertthat::assert_that(assertthat::is.flag(output_file))
    out$output_file <- isTRUE(output_file)
  }
  if (!is.null(name_output_file)) {
    assertthat::assert_that(assertthat::is.string(name_output_file))
    out$name_output_file <- as.character(name_output_file)[1]
  }

  # merge solver_params with stored solver_params (do not drop existing ones)
  out$solver_params <- modifyList(out$solver_params %||% list(), solver_params)

  x$data$solve_args <- out
  x
}

#' Configure Gurobi solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that sets
#' \code{solver = "gurobi"}.
#'
#' @inheritParams set_solver
#'
#' @return
#' An updated \code{Problem} object with Gurobi solver settings.
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
                              name_output_file = NULL, output_file = NULL) {
  set_solver(
    x,
    solver = "gurobi",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    name_output_file = name_output_file,
    output_file = output_file,
    solver_params = solver_params,
    ...
  )
}

#' Configure CBC solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that sets
#' \code{solver = "cbc"}.
#'
#' @inheritParams set_solver
#'
#' @return
#' An updated \code{Problem} object with CBC solver settings.
#'
#' @seealso
#' \code{\link{set_solver}},
#' \code{\link{solve}}
#'
#' @examples
#' \dontrun{
#' x <- set_solver_cbc(
#'   x,
#'   gap_limit = 0.01,
#'   time_limit = 600,
#'   verbose = TRUE
#' )
#' }
#'
#' @export
set_solver_cbc <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                           solution_limit = NULL, cores = NULL, verbose = FALSE,
                           name_output_file = NULL, output_file = NULL) {
  set_solver(
    x,
    solver = "cbc",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    name_output_file = name_output_file,
    output_file = output_file,
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
                             name_output_file = NULL, output_file = NULL) {
  set_solver(
    x,
    solver = "cplex",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    name_output_file = name_output_file,
    output_file = output_file,
    solver_params = solver_params,
    ...
  )
}

#' Configure SYMPHONY solver settings
#'
#' @description
#' Convenience wrapper around \code{\link{set_solver}} that sets
#' \code{solver = "symphony"}.
#'
#' @inheritParams set_solver
#'
#' @return
#' An updated \code{Problem} object with SYMPHONY solver settings.
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
                                name_output_file = NULL, output_file = NULL) {
  set_solver(
    x,
    solver = "symphony",
    gap_limit = gap_limit,
    time_limit = time_limit,
    solution_limit = solution_limit,
    cores = cores,
    verbose = verbose,
    name_output_file = name_output_file,
    output_file = output_file,
    solver_params = solver_params,
    ...
  )
}
