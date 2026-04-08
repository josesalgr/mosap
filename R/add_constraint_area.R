#' @include internal.R
#'
#' @title Add area constraint
#'
#' @description
#' Add an area constraint to a planning problem.
#'
#' This function stores one area-constraint specification in the
#' \code{Problem} object so that it can later be incorporated when the
#' optimization model is assembled. Multiple area constraints can be added by
#' calling this function repeatedly, provided that no duplicated combination of
#' action subset and constraint sense is introduced.
#'
#' @details
#' Use this function when area requirements must be imposed either on the total
#' selected landscape or on the subset of selected decisions associated with
#' specific actions.
#'
#' Let \eqn{\mathcal{I}} denote the set of planning units and let
#' \eqn{a_i \ge 0} be the area associated with planning unit
#' \eqn{i \in \mathcal{I}}
#'
#' When \code{actions = NULL}, the constraint refers to the total selected area
#' in the problem. In that case, let \eqn{w_i \in \{0,1\}} denote the binary
#' variable indicating whether planning unit \eqn{i} is selected by at least one
#' decision in the model.
#'
#' Depending on \code{sense}, this function stores one of the following
#' constraints:
#'
#' If \code{sense = "min"}:
#' \deqn{
#' \sum_{i \in \mathcal{I}} a_i w_i \ge A
#' }
#'
#' If \code{sense = "max"}:
#' \deqn{
#' \sum_{i \in \mathcal{I}} a_i w_i \le A
#' }
#'
#' If \code{sense = "equal"} and \code{tolerance = 0}:
#' \deqn{
#' \sum_{i \in \mathcal{I}} a_i w_i = A
#' }
#'
#' If \code{sense = "equal"} and \code{tolerance > 0}, the equality is stored as
#' a two-sided band:
#' \deqn{
#' A - \tau \le \sum_{i \in \mathcal{I}} a_i w_i \le A + \tau
#' }
#' where \eqn{\tau} is the value supplied through \code{tolerance}.
#'
#' When \code{actions} is not \code{NULL}, the constraint is applied only to the
#' selected decisions associated with the specified subset of actions. Let
#' \eqn{\mathcal{A}^\star \subseteq \mathcal{A}} denote that subset and let
#' \eqn{x_{ia} \in \{0,1\}} denote the binary variable indicating whether action
#' \eqn{a \in \mathcal{A}^\star} is selected in planning unit
#' \eqn{i \in \mathcal{I}}. In that case, the constrained quantity is
#' \deqn{
#' \sum_{i \in \mathcal{I}} \sum_{a \in \mathcal{A}^\star} a_i x_{ia}.
#' }
#'
#' Under formulations where at most one action can be selected per planning
#' unit, this coincides with the area allocated to that subset of actions.
#'
#' Areas are obtained from the planning-unit table. If \code{area_col} is
#' provided, that column is used. Otherwise, the model builder later determines
#' the default area source according to the internal rules of the package. The
#' value of \code{area_unit} indicates the unit in which \code{area} and
#' \code{tolerance} are expressed and therefore how the stored threshold should
#' be interpreted.
#'
#' This function only stores the constraint specification; it does not validate
#' the feasibility of the threshold against the available planning units at this
#' stage.
#'
#' Multiple area constraints can be stored in a \code{Problem} object. However,
#' at most one can be stored for the same combination of action subset and
#' constraint sense. Attempting to add a duplicated
#' \code{actions}--\code{sense} combination results in an error.
#'
#' @param x A \code{Problem} object.
#'
#' @param area Numeric scalar greater than or equal to zero. Target value for
#'   the constrained area.
#'
#' @param sense Character string indicating the type of area constraint. Must be
#'   one of \code{"min"}, \code{"max"}, or \code{"equal"}.
#'
#' @param tolerance Numeric scalar greater than or equal to zero. Only used when
#'   \code{sense = "equal"}. In that case, equality is interpreted as a band
#'   around \code{area} with half-width \code{tolerance}. Ignored otherwise.
#'
#' @param area_col Optional character string giving the name of the area column
#'   in \code{x$data$pu}. If \code{NULL}, the area source is resolved later by
#'   the model builder.
#'
#' @param area_unit Character string indicating the unit of \code{area} and
#'   \code{tolerance}. Must be one of \code{"m2"}, \code{"ha"}, or
#'   \code{"km2"}.
#'
#' @param actions Optional subset of actions to which the constraint applies.
#'   If \code{NULL}, the constraint applies to the total selected area in the
#'   problem through the planning-unit selection variables. Otherwise, it applies
#'   to the selected decision variables associated with the specified subset of
#'   actions. This argument is resolved using the package's standard action
#'   subset parser.
#'
#' @param name Optional character string used as the label of the stored linear
#'   constraint when it is later added to the optimization model. If
#'   \code{NULL}, a default name is generated.
#'
#' @return An updated \code{Problem} object with the new area-constraint
#'   specification appended to \code{x$data$constraints$area}.
#'
#' @seealso
#' \code{\link{create_problem}}
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
#' actions <- data.frame(
#'   id = c("conservation", "restoration")
#' )
#'
#' p <- create_problem(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_actions(
#'   p,
#'   actions = actions,
#'   cost = c(conservation = 1, restoration = 2)
#' )
#'
#' p <- add_constraint_area(
#'   x = p,
#'   area = 25,
#'   sense = "min",
#'   area_col = "area_ha",
#'   area_unit = "ha"
#' )
#'
#' p <- add_constraint_area(
#'   x = p,
#'   area = 15,
#'   sense = "max",
#'   area_col = "area_ha",
#'   area_unit = "ha",
#'   actions = "restoration"
#' )
#'
#' p <- add_constraint_area(
#'   x = p,
#'   area = 5,
#'   sense = "min",
#'   area_col = "area_ha",
#'   area_unit = "ha",
#'   actions = "restoration"
#' )
#'
#' p$data$constraints$area
#'
#' @export
add_constraint_area <- function(x,
                                area,
                                sense,
                                tolerance = 0,
                                area_col = NULL,
                                area_unit = c("m2", "ha", "km2"),
                                actions = NULL,
                                name = NULL) {

  stopifnot(inherits(x, "Problem"))

  if (missing(sense) || is.null(sense)) {
    stop(
      "`sense` must be explicitly provided. Use one of: 'min', 'max', or 'equal'.",
      call. = FALSE
    )
  }

  sense <- match.arg(sense, c("min", "max", "equal"))
  area_unit <- match.arg(area_unit)

  assertthat::assert_that(
    assertthat::is.number(area),
    is.finite(area),
    area >= 0
  )

  assertthat::assert_that(
    assertthat::is.number(tolerance),
    is.finite(tolerance),
    tolerance >= 0
  )

  if (!is.null(area_col)) {
    assertthat::assert_that(
      is.character(area_col),
      length(area_col) == 1,
      !is.na(area_col),
      nzchar(area_col)
    )
  }

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
      stop("`name` must be NULL or a non-empty character string.", call. = FALSE)
    }
  }

  if (sense != "equal" && tolerance > 0) {
    warning(
      "`tolerance` is only used when sense = 'equal'. It will be ignored.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  x <- .pa_clone_data(x)

  actions_txt <- NA_character_

  if (!is.null(actions)) {
    act_subset <- .pa_resolve_action_subset(x, subset = actions)

    if (!is.data.frame(act_subset) || nrow(act_subset) == 0) {
      stop("`actions` did not resolve to any valid actions.", call. = FALSE)
    }

    actions_txt <- .pa_subset_to_string(act_subset$id)
  }

  area_name <- if (is.null(name)) {
    if (is.na(actions_txt)) {
      paste0("area_", sense)
    } else {
      paste0("area_", sense, "_", gsub("\\|", "_", actions_txt))
    }
  } else {
    as.character(name)[1]
  }

  area_df <- data.frame(
    type = "area",
    sense = sense,
    value = as.numeric(area),
    tolerance = if (sense == "equal") as.numeric(tolerance) else 0,
    unit = area_unit,
    area_col = if (is.null(area_col)) NA_character_ else as.character(area_col)[1],
    actions = actions_txt,
    name = area_name,
    stringsAsFactors = FALSE
  )

  x <- .pa_store_area_constraints(x, area_df)

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
