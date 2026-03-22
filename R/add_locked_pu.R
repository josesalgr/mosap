#' @include internal.R
NULL

#' Add locked planning units to a problem
#'
#' @description
#' Define locked-in and locked-out planning units for a `Problem` object.
#'
#' This function is intended to be used \emph{after} [inputData()]. It updates
#' the internal planning-unit table stored in `x$data$pu` by creating or
#' replacing the logical columns `locked_in` and `locked_out`.
#'
#' Lock information can be supplied in three ways:
#'
#' 1. As a column name in `x$data$pu_data_raw`,
#' 2. As a logical vector of length equal to the number of planning units,
#' 3. As a vector of planning-unit ids.
#'
#' In addition, `pu_status` can be used with Marxan-style status codes:
#' values `2` indicate locked-in planning units and values `3` indicate
#' locked-out planning units.
#'
#' @details
#' \strong{Philosophy.}
#' The role of [inputData()] is to construct and normalize the problem inputs.
#' Locking planning units is treated as a separate modeling step, so this
#' function allows users to define or update locked planning units explicitly
#' after the `Problem` object has been created.
#'
#' \strong{How inputs are interpreted.}
#'
#' For `locked_in` and `locked_out`, the function accepts:
#' \itemize{
#'   \item `NULL`: do not modify that side;
#'   \item a single character string: interpreted as a column name in
#'   `x$data$pu_data_raw`;
#'   \item a logical vector of length `nrow(x$data$pu)`;
#'   \item a vector of planning-unit ids.
#' }
#'
#' For `pu_status`, the function accepts:
#' \itemize{
#'   \item `NULL`: ignored;
#'   \item a single character string: interpreted as a column name in
#'   `x$data$pu_data_raw`;
#'   \item a vector of length `nrow(x$data$pu)` with status values.
#' }
#'
#' When `pu_status` is supplied, values `2` are interpreted as locked-in and
#' values `3` as locked-out. By default, explicit `locked_in` / `locked_out`
#' inputs take precedence over `pu_status`.
#'
#' @param x A `Problem` object created with [inputData()] or
#'   [inputDataSpatial()].
#' @param locked_in Optional locked-in specification. Can be `NULL`, a column
#'   name in `x$data$pu_data_raw`, a logical vector, or a vector of PU ids.
#' @param locked_out Optional locked-out specification. Can be `NULL`, a column
#'   name in `x$data$pu_data_raw`, a logical vector, or a vector of PU ids.
#' @param pu_status Optional status specification using Marxan-style codes.
#'   Can be `NULL`, a column name in `x$data$pu_data_raw`, or a vector of length
#'   `nrow(x$data$pu)`. Values `2` mean locked-in and values `3` mean locked-out.
#' @param overwrite Logical. If `TRUE`, replace existing `locked_in` and
#'   `locked_out` columns in `x$data$pu`. If `FALSE`, merge with existing values
#'   using logical OR.
#' @param status_overrides Logical. If `TRUE`, `pu_status` overrides any
#'   existing or explicitly supplied `locked_in` / `locked_out` assignments.
#'   If `FALSE` (default), explicit `locked_in` and `locked_out` take precedence.
#'
#' @return
#' The updated [problem-class] object with logical columns
#' `x$data$pu$locked_in` and `x$data$pu$locked_out`.
#'
#' @examples
#' \dontrun{
#' # Read lock columns from original PU data
#' p <- add_locked_pu(
#'   p,
#'   locked_in = "locked_in",
#'   locked_out = "locked_out"
#' )
#'
#' # Read Marxan-style status column
#' p <- add_locked_pu(
#'   p,
#'   pu_status = "status"
#' )
#'
#' # Lock by PU ids
#' p <- add_locked_pu(
#'   p,
#'   locked_in = c(1, 5, 8),
#'   locked_out = c(10, 12)
#' )
#'
#' # Lock using logical vectors
#' p <- add_locked_pu(
#'   p,
#'   locked_in = c(TRUE, FALSE, TRUE, FALSE),
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [inputData()], [add_actions()]
#'
#' @export
add_locked_pu <- function(
    x,
    locked_in = NULL,
    locked_out = NULL,
    pu_status = NULL,
    overwrite = TRUE,
    status_overrides = FALSE
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

  .as_status_vector <- function(z) {
    if (is.null(z)) return(NULL)

    if (is.character(z) && length(z) == 1L) {
      if (is.null(pu_raw) || !inherits(pu_raw, "data.frame")) {
        stop(
          "pu_status was provided as a column name, but x$data$pu_data_raw is missing.",
          call. = FALSE
        )
      }
      if (!(z %in% names(pu_raw))) {
        stop("pu_status column '", z, "' was not found in x$data$pu_data_raw.", call. = FALSE)
      }
      z <- pu_raw[[z]]
    }

    z <- as.integer(z)
    if (length(z) != n_pu) {
      stop(
        "pu_status must have length ", n_pu, ".",
        call. = FALSE
      )
    }

    z
  }

  .resolve_lock_spec <- function(spec, what) {
    if (is.null(spec)) return(NULL)

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

  lock_in_vec  <- .resolve_lock_spec(locked_in,  "locked_in")
  lock_out_vec <- .resolve_lock_spec(locked_out, "locked_out")
  status_vec   <- .as_status_vector(pu_status)

  status_in  <- if (!is.null(status_vec)) status_vec == 2L else NULL
  status_out <- if (!is.null(status_vec)) status_vec == 3L else NULL

  # current values if they already exist
  current_in <- if ("locked_in" %in% names(pu)) {
    as.logical(pu$locked_in)
  } else {
    rep(FALSE, n_pu)
  }
  current_out <- if ("locked_out" %in% names(pu)) {
    as.logical(pu$locked_out)
  } else {
    rep(FALSE, n_pu)
  }

  current_in[is.na(current_in)] <- FALSE
  current_out[is.na(current_out)] <- FALSE

  if (isTRUE(overwrite)) {
    new_in  <- rep(FALSE, n_pu)
    new_out <- rep(FALSE, n_pu)
  } else {
    new_in  <- current_in
    new_out <- current_out
  }

  if (!is.null(status_in) && isTRUE(status_overrides)) {
    new_in  <- status_in
    new_out <- status_out
  } else {
    if (!is.null(status_in)) {
      new_in  <- new_in  | status_in
      new_out <- new_out | status_out
    }
    if (!is.null(lock_in_vec)) {
      if (isTRUE(overwrite)) {
        new_in <- lock_in_vec
      } else {
        new_in <- new_in | lock_in_vec
      }
    }
    if (!is.null(lock_out_vec)) {
      if (isTRUE(overwrite)) {
        new_out <- lock_out_vec
      } else {
        new_out <- new_out | lock_out_vec
      }
    }
  }

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
