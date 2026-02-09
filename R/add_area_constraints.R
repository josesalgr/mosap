#' @include internal.R
#' @title Add minimum selected area constraint
#'
#' @description
#' Add a linear constraint enforcing a minimum total selected area using only planning unit
#' selection variables \eqn{w_i}:
#' \deqn{\sum_i \mathrm{area}_i \, w_i \ge A_{\min}.}
#'
#' @details
#' This function adds a linear constraint to the current optimization model snapshot.
#' Areas are retrieved from the planning unit table (\code{x$data$pu}) via \code{area_col}
#' or via the package defaults implemented in \code{.pa_get_area_vec()}, and are converted
#' to the requested \code{area_unit}. The coefficient vector is aligned with the model's
#' planning unit order (\code{n_pu} and the internal \eqn{w} variable offset stored in
#' \code{x$data$model_list}).
#'
#' Constraint metadata is also stored in \code{x$data$constraints$area_min} for printing/reporting.
#'
#' @param x A \code{Data} object.
#' @param area_min Numeric scalar \eqn{\ge 0}. Minimum area to select (expressed in \code{area_unit}).
#' @param area_col Optional character. Name of the column in \code{x$data$pu} containing areas.
#'   If \code{NULL}, areas are obtained using internal defaults (see Details).
#' @param area_unit Character. Units for \code{area_min}. One of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#' @param name Character. Name for the constraint in the model. Default \code{"area_min"}.
#'
#' @return The updated \code{Data} object with the new linear constraint added to the model
#'   and metadata stored in \code{x$data$constraints$area_min}.
#'
#' @examples
#' \dontrun{
#' # Enforce selecting at least 1000 ha
#' p <- add_area_min_constraint(p, area_min = 1000, area_unit = "ha")
#'
#' # Use a custom area column stored in x$data$pu$Area_km2
#' p <- add_area_min_constraint(p, area_min = 250, area_unit = "km2", area_col = "Area_km2")
#' }
#'
#' @seealso \code{\link{add_area_max_constraint}}
#'
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
#' Add a linear constraint enforcing a maximum total selected area using only planning unit
#' selection variables \eqn{w_i}:
#' \deqn{\sum_i \mathrm{area}_i \, w_i \le A_{\max}.}
#'
#' @details
#' This function adds a linear constraint to the current optimization model snapshot.
#' Areas are retrieved from the planning unit table (\code{x$data$pu}) via \code{area_col}
#' or via the package defaults implemented in \code{.pa_get_area_vec()}, and are converted
#' to the requested \code{area_unit}. The coefficient vector is aligned with the model's
#' planning unit order (\code{n_pu} and the internal \eqn{w} variable offset stored in
#' \code{x$data$model_list}).
#'
#' Constraint metadata is also stored in \code{x$data$constraints$area_max} for printing/reporting.
#'
#' @param x A \code{Data} object.
#' @param area_max Numeric scalar \eqn{\ge 0}. Maximum area to select (expressed in \code{area_unit}).
#' @param area_col Optional character. Name of the column in \code{x$data$pu} containing areas.
#'   If \code{NULL}, areas are obtained using internal defaults (see Details).
#' @param area_unit Character. Units for \code{area_max}. One of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#' @param name Character. Name for the constraint in the model. Default \code{"area_max"}.
#'
#' @return The updated \code{Data} object with the new linear constraint added to the model
#'   and metadata stored in \code{x$data$constraints$area_max}.
#'
#' @examples
#' \dontrun{
#' # Enforce selecting at most 500 km2
#' p <- add_area_max_constraint(p, area_max = 500, area_unit = "km2")
#'
#' # Use a custom area column stored in x$data$pu$area_m2
#' p <- add_area_max_constraint(p, area_max = 2e6, area_unit = "m2", area_col = "area_m2")
#' }
#'
#' @seealso \code{\link{add_area_min_constraint}}
#'
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
