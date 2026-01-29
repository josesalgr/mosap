#' @include internal.R
#' @title Add minimum selected area constraint
#'
#' @description
#' Adds \eqn{\sum_i area_i w_i \ge A_{min}} using only PU selection vars \code{w_i}.
#'
#' @param x A [data-class] object.
#' @param area_min Numeric. Minimum area to select (in \code{area_unit}).
#' @param area_col Character. Column in \code{x$data$pu} containing area (optional).
#' @param area_unit Character. Units for \code{area_min}: "m2", "ha", or "km2".
#' @param name Optional constraint name.
#'
#' @return Updated [data-class] object.
#' @export
add_area_min_constraint <- function(x,
                                    area_min,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_min") {

  stopifnot(inherits(x, "Data"))
  assertthat::assert_that(assertthat::is.number(area_min), is.finite(area_min), area_min >= 0)

  x <- .pa_ensure_model_snapshot(x)

  ml <- x$data$model_list
  n_pu <- as.integer(ml$n_pu %||% 0L)
  if (n_pu <= 0L) stop("Model has n_pu=0; cannot add area constraint.", call. = FALSE)

  # areas aligned to pu order
  a <- .pa_get_area_vec(x, area_col = area_col, area_unit = match.arg(area_unit))
  if (length(a) != n_pu) {
    stop("Area vector length (", length(a), ") != n_pu (", n_pu, "). Check pu table / area source.", call. = FALSE)
  }

  # indices of w variables in the full var vector
  w0 <- as.integer(ml$w_offset %||% 0L) # 0-based offset
  j0 <- w0 + (0:(n_pu - 1L))           # 0-based indices for C++

  x <- .pa_add_linear_constraint(
    x,
    var_index_0based = j0,
    coeff = a,
    sense = ">=",
    rhs = area_min,
    name = name
  )

  # store metadata for printing
  if (is.null(x$data$constraints)) x$data$constraints <- list()
  x$data$constraints$area_min <- list(value = area_min, unit = match.arg(area_unit), area_col = area_col, name = name)

  x
}

#' @title Add maximum selected area constraint
#'
#' @description
#' Adds \eqn{\sum_i area_i w_i \le A_{max}} using only PU selection vars \code{w_i}.
#'
#' @param x A [data-class] object.
#' @param area_max Numeric. Maximum area to select (in \code{area_unit}).
#' @param area_col Character. Column in \code{x$data$pu} containing area (optional).
#' @param area_unit Character. Units for \code{area_max}: "m2", "ha", or "km2".
#' @param name Optional constraint name.
#'
#' @return Updated [data-class] object.
#' @export
add_area_max_constraint <- function(x,
                                    area_max,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_max") {

  stopifnot(inherits(x, "Data"))
  assertthat::assert_that(assertthat::is.number(area_max), is.finite(area_max), area_max >= 0)

  x <- .pa_ensure_model_snapshot(x)

  ml <- x$data$model_list
  n_pu <- as.integer(ml$n_pu %||% 0L)
  if (n_pu <= 0L) stop("Model has n_pu=0; cannot add area constraint.", call. = FALSE)

  a <- .pa_get_area_vec(x, area_col = area_col, area_unit = match.arg(area_unit))
  if (length(a) != n_pu) {
    stop("Area vector length (", length(a), ") != n_pu (", n_pu, "). Check pu table / area source.", call. = FALSE)
  }

  w0 <- as.integer(ml$w_offset %||% 0L)
  j0 <- w0 + (0:(n_pu - 1L))

  x <- .pa_add_linear_constraint(
    x,
    var_index_0based = j0,
    coeff = a,
    sense = "<=",
    rhs = area_max,
    name = name
  )

  if (is.null(x$data$constraints)) x$data$constraints <- list()
  x$data$constraints$area_max <- list(value = area_max, unit = match.arg(area_unit), area_col = area_col, name = name)

  x
}
