#' @include internal.R
NULL

#' @title Add locked planning units to a problem
#'
#' @description
#' Define planning units that must be included in, or excluded from, the
#' optimization problem.
#'
#' This function updates the planning-unit table stored in the \code{Problem}
#' object by creating or replacing the logical columns \code{locked_in} and
#' \code{locked_out}. These columns are later used by the model builder when
#' translating the problem into optimization constraints.
#'
#' Lock information may be supplied either directly as logical vectors, as
#' vectors of planning-unit ids, or by referencing columns in the raw
#' planning-unit data originally passed to \code{\link{create_problem}}.
#'
#' @details
#' Use this function when whole planning units must be forced into or out of
#' the solution, regardless of which action may later be selected in them.
#'
#' Let \eqn{\mathcal{I}} denote the set of planning units and let
#' \eqn{w_i \in \{0,1\}} denote the binary variable indicating whether planning
#' unit \eqn{i \in \mathcal{I}} is selected by the model.
#'
#' This function defines two subsets:
#' \itemize{
#'   \item \eqn{\mathcal{I}^{in} \subseteq \mathcal{I}}, the planning units that
#'   must be included,
#'   \item \eqn{\mathcal{I}^{out} \subseteq \mathcal{I}}, the planning units
#'   that must be excluded.
#' }
#'
#' Conceptually, these sets correspond to the following conditions:
#' \itemize{
#'   \item if \eqn{i \in \mathcal{I}^{in}}, then \eqn{w_i = 1},
#'   \item if \eqn{i \in \mathcal{I}^{out}}, then \eqn{w_i = 0}.
#' }
#'
#' These constraints are not imposed immediately by this function; instead, they
#' are stored in the planning-unit table and enforced later when building the
#' optimization model.
#'
#' \strong{Philosophy}
#'
#' The role of \code{\link{create_problem}} is to construct and normalize the basic
#' inputs of the planning problem. Locking planning units is treated as a
#' separate modelling step so that users can define or revise selection
#' restrictions after the \code{Problem} object has already been created.
#'
#' In contrast, \code{\link{add_constraint_locked_actions}} is used to fix
#' specific feasible \code{(pu, action)} decisions rather than whole planning
#' units.
#'
#' \strong{Supported input formats}
#'
#' For both \code{locked_in} and \code{locked_out}, the function accepts:
#' \itemize{
#'   \item \code{NULL}, meaning that no planning units are locked on that side,
#'   \item a single character string, interpreted as a column name in the raw
#'   planning-unit data,
#'   \item a logical vector of length \code{nrow(pu)},
#'   \item a vector of planning-unit ids.
#' }
#'
#' When a column name is supplied, the referenced column is coerced to logical.
#' Numeric values are interpreted as non-zero = \code{TRUE}; character and
#' factor values are interpreted using common logical strings such as
#' \code{"true"}, \code{"t"}, \code{"1"}, \code{"yes"}, and \code{"y"}.
#' Missing values are treated as \code{FALSE}.
#'
#' \strong{Replacement behaviour}
#'
#' Each call to \code{add_constraint_locked_pu()} replaces any existing
#' \code{locked_in} and \code{locked_out} columns in the planning-unit table. In
#' other words, the function defines the complete current set of locked planning
#' units; it does not merge new values with previous ones.
#'
#' \strong{Consistency checks}
#'
#' The function checks that no planning unit is simultaneously assigned to both
#' \code{locked_in} and \code{locked_out}. If such conflicts are found, an error
#' is raised.
#'
#' @param x A \code{Problem} object created with \code{\link{create_problem}}.
#'
#' @param locked_in Optional locked-in specification. It may be \code{NULL}, a
#'   column name in the raw planning-unit data, a logical vector, or a vector of
#'   planning-unit ids.
#'
#' @param locked_out Optional locked-out specification. It may be \code{NULL}, a
#'   column name in the raw planning-unit data, a logical vector, or a vector of
#'   planning-unit ids.
#'
#' @return An updated \code{Problem} object in which the planning-unit table
#'   contains logical columns \code{locked_in} and \code{locked_out}.
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:5,
#'   cost = c(2, 3, 1, 4, 2),
#'   lock_col = c(TRUE, FALSE, FALSE, TRUE, FALSE),
#'   out_col = c(FALSE, FALSE, FALSE, FALSE, TRUE)
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
#' p <- create_problem(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' # 1) Lock by planning-unit ids
#' p1 <- add_constraint_locked_pu(
#'   x = p,
#'   locked_in = c(1, 3),
#'   locked_out = c(5)
#' )
#'
#' p1$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' # 2) Read lock information from raw PU data columns
#' p2 <- add_constraint_locked_pu(
#'   x = p,
#'   locked_in = "lock_col",
#'   locked_out = "out_col"
#' )
#'
#' p2$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' # 3) Use logical vectors
#' p3 <- add_constraint_locked_pu(
#'   x = p,
#'   locked_in = c(TRUE, FALSE, TRUE, FALSE, FALSE),
#'   locked_out = c(FALSE, FALSE, FALSE, TRUE, FALSE)
#' )
#'
#' p3$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' @seealso
#' \code{\link{create_problem}},
#' \code{\link{add_actions}},
#' \code{\link{add_constraint_locked_actions}}
#'
#' @export
add_constraint_locked_pu <- function(
    x,
    locked_in = NULL,
    locked_out = NULL
) {
  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)

  pu <- x$data$pu %||% NULL
  if (is.null(pu) || !inherits(pu, "data.frame") || nrow(pu) == 0L) {
    stop("x$data$pu is missing or empty.", call. = FALSE)
  }
  if (!("id" %in% names(pu))) {
    stop("x$data$pu must contain column 'id'.", call. = FALSE)
  }

  pu_raw <- x$data$pu_data_raw %||% NULL
  n_pu <- nrow(pu)
  pu_ids <- as.integer(pu$id)

  .as_logical_from_column <- function(colname, source_df, what) {
    if (is.null(source_df) || !inherits(source_df, "data.frame")) {
      stop(
        what, " was provided as a column name, but x$data$pu_data_raw is missing.",
        call. = FALSE
      )
    }

    colname <- as.character(colname)[1]
    if (is.na(colname) || !nzchar(colname)) {
      stop(what, " column name must be a non-empty string.", call. = FALSE)
    }
    if (!(colname %in% names(source_df))) {
      stop(
        what, " column '", colname, "' was not found in x$data$pu_data_raw.",
        call. = FALSE
      )
    }

    v <- source_df[[colname]]

    if (is.logical(v)) {
      out <- v
    } else if (is.numeric(v) || is.integer(v)) {
      out <- v != 0
    } else if (is.factor(v)) {
      v <- as.character(v)
      vv <- tolower(trimws(v))
      out <- vv %in% c("true", "t", "1", "yes", "y")
    } else if (is.character(v)) {
      vv <- tolower(trimws(v))
      out <- vv %in% c("true", "t", "1", "yes", "y")
    } else {
      stop(
        what, " column '", colname, "' could not be coerced to logical.",
        call. = FALSE
      )
    }

    out <- as.logical(out)
    out[is.na(out)] <- FALSE

    if (length(out) != n_pu) {
      stop(
        what, " column '", colname, "' has length ", length(out),
        " but expected ", n_pu, ".",
        call. = FALSE
      )
    }

    out
  }

  .resolve_lock_spec <- function(spec, what) {
    if (is.null(spec)) return(rep(FALSE, n_pu))

    # case 1: single character -> column name in pu_data_raw
    if (is.character(spec) && length(spec) == 1L && !spec %in% as.character(pu_ids)) {
      return(.as_logical_from_column(spec, pu_raw, what))
    }

    # case 2: logical vector
    if (is.logical(spec)) {
      if (length(spec) != n_pu) {
        stop(
          what, " logical vector must have length ", n_pu, ".",
          call. = FALSE
        )
      }
      spec <- as.logical(spec)
      spec[is.na(spec)] <- FALSE
      return(spec)
    }

    # case 3: PU ids
    ids <- suppressWarnings(as.integer(spec))
    if (length(ids) == 0L || all(is.na(ids))) {
      stop(
        what, " must be NULL, a column name, a logical vector, or a vector of PU ids.",
        call. = FALSE
      )
    }
    ids <- unique(ids[!is.na(ids)])

    bad <- ids[!ids %in% pu_ids]
    if (length(bad) > 0L) {
      stop(
        what, " contains PU ids not present in x$data$pu$id: ",
        paste(bad, collapse = ", "),
        call. = FALSE
      )
    }

    pu_ids %in% ids
  }

  new_in  <- .resolve_lock_spec(locked_in,  "locked_in")
  new_out <- .resolve_lock_spec(locked_out, "locked_out")

  if (any(new_in & new_out, na.rm = TRUE)) {
    bad_ids <- pu_ids[new_in & new_out]
    stop(
      "Some planning units are both locked_in and locked_out: ",
      paste(utils::head(bad_ids, 20), collapse = ", "),
      if (length(bad_ids) > 20) " ..." else "",
      call. = FALSE
    )
  }

  x$data$pu$locked_in <- as.logical(new_in)
  x$data$pu$locked_out <- as.logical(new_out)

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
