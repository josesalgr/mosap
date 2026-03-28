#' @include internal.R
#'
#' @title Add minimum selected area constraint
#'
#' @description
#' Add a lower bound on the total selected area in a planning problem.
#'
#' This function stores a minimum-area constraint in the \code{Problem} object
#' so that it can be incorporated later by the model builder when the
#' optimization model is assembled.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{a_i \ge 0} be the area associated with planning unit \eqn{i \in \mathcal{P}}.
#' Let \eqn{w_i \in \{0,1\}} denote the binary variable indicating whether
#' planning unit \eqn{i} is selected by at least one decision in the model.
#'
#' This function stores the following constraint:
#'
#' \deqn{
#' \sum_{i \in \mathcal{P}} a_i w_i \ge A_{\min},
#' }
#'
#' where \eqn{A_{\min}} is the value supplied through \code{area_min}.
#'
#' Areas are obtained from \code{x$data$pu}. If \code{area_col} is provided, that
#' column is used. Otherwise, the model builder will later determine the default
#' area source according to the internal rules of the package. The value of
#' \code{area_unit} indicates the unit in which \code{area_min} is expressed and
#' therefore how the stored threshold should be interpreted.
#'
#' This function only stores the constraint specification in
#' \code{x$data$constraints$area_min}; it does not validate the feasibility of
#' the threshold against the available planning units at this stage.
#'
#' At most one minimum-area constraint can be stored in a \code{Problem} object.
#' If one already exists, this function raises an error.
#'
#' @param x A \code{Problem} object.
#'
#' @param area_min Numeric scalar greater than or equal to zero. Minimum total
#'   selected area required in the solution.
#'
#' @param area_col Optional character string giving the name of the area column
#'   in \code{x$data$pu}. If \code{NULL}, the area source is resolved later by
#'   the model builder.
#'
#' @param area_unit Character string indicating the unit of \code{area_min}.
#'   Must be one of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#'
#' @param name Character string used as the label of the stored linear
#'   constraint when it is later added to the optimization model.
#'
#' @return An updated \code{Problem} object with a stored minimum-area
#'   constraint in \code{x$data$constraints$area_min}.
#'
#' @seealso
#' \code{\link{add_area_max_constraint}},
#' \code{\link{input_data}}
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:4,
#'   cost = c(2, 3, 1, 4),
#'   area_ha = c(10, 15, 8, 20)
#' )
#'
#' features <- data.frame(
#'   id = 1:2,
#'   name = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3, 4, 4),
#'   feature = c(1, 2, 1, 2, 1, 2),
#'   amount = c(1, 2, 1, 3, 2, 1)
#' )
#'
#' p <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_area_min_constraint(
#'   x = p,
#'   area_min = 25,
#'   area_col = "area_ha",
#'   area_unit = "ha"
#' )
#'
#' p$data$constraints$area_min
#'
#' @export
add_area_min_constraint <- function(x,
                                    area_min,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_min") {

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

  if (!is.null(x$data$constraints$area_min)) {
    stop(
      "An area_min constraint already exists. Remove or replace it explicitly before adding a new one.",
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
#' Add an upper bound on the total selected area in a planning problem.
#'
#' This function stores a maximum-area constraint in the \code{Problem} object
#' so that it can be incorporated later by the model builder when the
#' optimization model is assembled.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{a_i \ge 0} be the area associated with planning unit \eqn{i \in \mathcal{P}}.
#' Let \eqn{w_i \in \{0,1\}} denote the binary variable indicating whether
#' planning unit \eqn{i} is selected by at least one decision in the model.
#'
#' This function stores the following constraint:
#'
#' \deqn{
#' \sum_{i \in \mathcal{P}} a_i w_i \le A_{\max},
#' }
#'
#' where \eqn{A_{\max}} is the value supplied through \code{area_max}.
#'
#' Areas are obtained from \code{x$data$pu}. If \code{area_col} is provided, that
#' column is used. Otherwise, the model builder will later determine the default
#' area source according to the internal rules of the package. The value of
#' \code{area_unit} indicates the unit in which \code{area_max} is expressed and
#' therefore how the stored threshold should be interpreted.
#'
#' This function only stores the constraint specification in
#' \code{x$data$constraints$area_max}; it does not validate the feasibility of
#' the threshold against the available planning units at this stage.
#'
#' At most one maximum-area constraint can be stored in a \code{Problem} object.
#' If one already exists, this function raises an error.
#'
#' @param x A \code{Problem} object.
#'
#' @param area_max Numeric scalar greater than or equal to zero. Maximum total
#'   selected area allowed in the solution.
#'
#' @param area_col Optional character string giving the name of the area column
#'   in \code{x$data$pu}. If \code{NULL}, the area source is resolved later by
#'   the model builder.
#'
#' @param area_unit Character string indicating the unit of \code{area_max}.
#'   Must be one of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#'
#' @param name Character string used as the label of the stored linear
#'   constraint when it is later added to the optimization model.
#'
#' @return An updated \code{Problem} object with a stored maximum-area
#'   constraint in \code{x$data$constraints$area_max}.
#'
#' @seealso
#' \code{\link{add_area_min_constraint}},
#' \code{\link{input_data}}
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:4,
#'   cost = c(2, 3, 1, 4),
#'   area_ha = c(10, 15, 8, 20)
#' )
#'
#' features <- data.frame(
#'   id = 1:2,
#'   name = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3, 4, 4),
#'   feature = c(1, 2, 1, 2, 1, 2),
#'   amount = c(1, 2, 1, 3, 2, 1)
#' )
#'
#' p <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_area_max_constraint(
#'   x = p,
#'   area_max = 30,
#'   area_col = "area_ha",
#'   area_unit = "ha"
#' )
#'
#' p$data$constraints$area_max
#'
#' @export
add_area_max_constraint <- function(x,
                                    area_max,
                                    area_col = NULL,
                                    area_unit = c("m2", "ha", "km2"),
                                    name = "area_max") {

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

  if (!is.null(x$data$constraints$area_max)) {
    stop(
      "An area_max constraint already exists. Remove or replace it explicitly before adding a new one.",
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
