#' @include internal.R
#' @title Add minimum selected area constraint
#'
#' @description
#' Store a minimum selected area constraint to be applied later by the model builder.
#'
#' @param x A \code{Problem} object.
#' @param area_min Numeric scalar \eqn{\ge 0}. Minimum area to select.
#' @param area_col Optional character. Name of area column in \code{x$data$pu}.
#' @param area_unit Character. One of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#' @param name Character. Constraint name.
#' @param overwrite Logical. Replace existing stored area_min constraint if present.
#'
#' @return Updated \code{Problem} object.
#' @export
add_area_min_constraint <- function(x,
                                    area_min,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_min",
                                    overwrite = FALSE) {

  stopifnot(inherits(x, "Problem"))
  area_unit <- match.arg(area_unit)

  assertthat::assert_that(
    assertthat::is.number(area_min),
    is.finite(area_min),
    area_min >= 0
  )

  x <- .pa_clone_data(x)

  if (is.null(x$data$constraints) || !is.list(x$data$constraints)) {
    x$data$constraints <- list()
  }

  if (!isTRUE(overwrite) && !is.null(x$data$constraints$area_min)) {
    stop(
      "An area_min constraint already exists. Use overwrite=TRUE to replace it.",
      call. = FALSE
    )
  }

  x$data$constraints$area_min <- list(
    type = "area_min",
    value = as.numeric(area_min),
    unit = area_unit,
    area_col = area_col,
    name = as.character(name)[1]
  )

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}

#' @title Add maximum selected area constraint
#'
#' @description
#' Store a maximum selected area constraint to be applied later by the model builder.
#'
#' @param x A \code{Problem} object.
#' @param area_max Numeric scalar \eqn{\ge 0}. Maximum area to select.
#' @param area_col Optional character. Name of area column in \code{x$data$pu}.
#' @param area_unit Character. One of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#' @param name Character. Constraint name.
#' @param overwrite Logical. Replace existing stored area_max constraint if present.
#'
#' @return Updated \code{Problem} object.
#' @export
add_area_max_constraint <- function(x,
                                    area_max,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_max",
                                    overwrite = FALSE) {

  stopifnot(inherits(x, "Problem"))
  area_unit <- match.arg(area_unit)

  assertthat::assert_that(
    assertthat::is.number(area_max),
    is.finite(area_max),
    area_max >= 0
  )

  x <- .pa_clone_data(x)

  if (is.null(x$data$constraints) || !is.list(x$data$constraints)) {
    x$data$constraints <- list()
  }

  if (!isTRUE(overwrite) && !is.null(x$data$constraints$area_max)) {
    stop(
      "An area_max constraint already exists. Use overwrite=TRUE to replace it.",
      call. = FALSE
    )
  }

  x$data$constraints$area_max <- list(
    type = "area_max",
    value = as.numeric(area_max),
    unit = area_unit,
    area_col = area_col,
    name = as.character(name)[1]
  )

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
