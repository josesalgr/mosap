#' @title Configure solver settings
#'
#' @description
#' Stores solver configuration inside a \code{Data} object, so that \code{solve(x)}
#' can run without repeating solver arguments.
#'
#' This function does not build the model; it only stores runtime solver options.
#'
#' @param x A \code{Data} object created with \code{inputData()} or \code{inputDataSpatial()}.
#' @param solver Character. One of \code{"auto"}, \code{"gurobi"}, \code{"cplex"}, \code{"cbc"}, \code{"symphony"}.
#'
#' @param gap_limit Numeric in [0,1]. Relative MIP optimality gap. If \code{NULL}, keep stored value.
#' @param time_limit Numeric. Time limit in seconds. If \code{NULL}, keep stored value.
#' @param solution_limit Logical. If \code{NULL}, keep stored value.
#' @param cores Integer. If \code{NULL}, keep stored value.
#' @param verbose Logical. If \code{NULL}, keep stored value.
#' @param output_file Logical. If \code{NULL}, keep stored value.
#' @param name_output_file Character. If \code{NULL}, keep stored value.
#'
#' @param solver_params List. Solver-specific parameters (merged with stored ones).
#' @param ... Convenience: solver-specific parameters (e.g., \code{MIPFocus=1}) merged into \code{solver_params}.
#'
#' @return Updated \code{Data} object.
#' @export
set_solver <- function(x,
                       solver = c("auto", "gurobi", "cplex", "cbc", "symphony"),
                       gap_limit = NULL,
                       time_limit = NULL,
                       solution_limit = NULL,
                       cores = NULL,
                       verbose = NULL,
                       name_output_file = NULL,
                       output_file = NULL,
                       solver_params = list(),
                       ...) {

  stopifnot(inherits(x, "Data"))
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

# Wrappers: keep them thin and let set_solver do the heavy lifting
#' @export
set_solver_gurobi <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                              solution_limit = NULL, cores = NULL, verbose = NULL,
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

#' @export
set_solver_cbc <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                           solution_limit = NULL, cores = NULL, verbose = NULL,
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

#' @export
set_solver_cplex <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                             solution_limit = NULL, cores = NULL, verbose = NULL,
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

#' @export
set_solver_symphony <- function(x, ..., solver_params = list(), gap_limit = NULL, time_limit = NULL,
                                solution_limit = NULL, cores = NULL, verbose = NULL,
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
