#' @include internal.R
NULL

#' Add locked action decisions to a planning problem
#'
#' @description
#' Fix feasible \code{(pu, action)} decisions to be selected
#' (\code{locked_in}) or not selected (\code{locked_out}).
#'
#' This function does not create new feasible action pairs. It only modifies
#' the \code{status} column of existing rows in \code{x$data$dist_actions}.
#'
#' @details
#' Internally, the following status codes are used:
#' \itemize{
#'   \item \code{0}: free
#'   \item \code{2}: locked-in
#'   \item \code{3}: locked-out
#' }
#'
#' The function validates that all requested locked pairs are already feasible.
#' Therefore, \code{add_actions()} should be called first.
#'
#' If a planning unit is already marked as \code{locked_out} in
#' \code{x$data$pu}, then all its feasible action pairs are forced to
#' \code{status = 3}. In that case, any overlapping \code{locked_in} request
#' will raise an error.
#'
#' Accepted formats for \code{locked_in} and \code{locked_out} are:
#' \itemize{
#'   \item \code{NULL}
#'   \item A \code{data.frame} with columns \code{pu} and \code{action}. An
#'   optional \code{feasible} column is supported as a filter.
#'   \item A named list with names equal to action ids. Each element can be:
#'   \itemize{
#'     \item an integer vector of PU ids, or
#'     \item an \code{sf} object defining a spatial zone for that action.
#'   }
#' }
#'
#' @param x A \code{Problem} object with actions already defined through
#'   \code{\link{add_actions}}.
#' @param locked_in Optional specification of feasible \code{(pu, action)} pairs
#'   that must be selected.
#' @param locked_out Optional specification of feasible \code{(pu, action)} pairs
#'   that must not be selected.
#' @param na_is_infeasible Logical. Only relevant when a specification is
#'   provided as a \code{data.frame} with a \code{feasible} column.
#'
#' @return The updated \code{Problem} object with modified
#'   \code{x$data$dist_actions$status}.
#'
#' @examples
#' \dontrun{
#' p <- add_actions(
#'   p,
#'   actions = data.frame(id = c("harvest", "restoration"))
#' )
#'
#' p <- add_locked_actions(
#'   p,
#'   locked_in = data.frame(pu = c(1, 2), action = c("harvest", "restoration")),
#'   locked_out = data.frame(pu = c(5), action = c("harvest"))
#' )
#' }
#'
#' @seealso \code{\link{add_actions}}, \code{\link{add_locked_pu}}
#' @export
add_locked_actions <- function(
    x,
    locked_in = NULL,
    locked_out = NULL,
    na_is_infeasible = TRUE
) {

  .as_int_id <- function(v, what) {
    if (is.factor(v)) v <- as.character(v)
    if (is.character(v)) {
      if (any(grepl("[^0-9\\-]", v))) {
        stop(what, " must be numeric/integer ids (got non-numeric strings).", call. = FALSE)
      }
      v <- as.integer(v)
    } else {
      v <- as.integer(v)
    }
    if (anyNA(v)) stop(what, " contains NA after coercion to integer.", call. = FALSE)
    v
  }

  .normalize_feasible_col <- function(df, what) {
    if (!("feasible" %in% names(df))) {
      df$feasible <- TRUE
      return(df)
    }

    f <- df$feasible

    if (is.logical(f)) {
      # keep
    } else if (is.numeric(f) || is.integer(f)) {
      f <- f != 0
    } else if (is.factor(f)) {
      f <- as.character(f)
    }

    if (is.character(f)) {
      w <- tolower(trimws(f))
      f <- w %in% c("true", "t", "1", "yes", "y")
    } else {
      f <- as.logical(f)
    }

    if (na_is_infeasible) f[is.na(f)] <- FALSE
    df$feasible <- as.logical(f)
    df
  }

  .spec_to_pairs <- function(spec, what, action_ids, pu_ids, pu_sf, as_int_id_fun) {
    if (is.null(spec)) return(NULL)

    if (inherits(spec, "data.frame")) {
      assertthat::assert_that(nrow(spec) > 0, msg = paste0(what, " is an empty data.frame."))

      if ("id" %in% names(spec) && !("action" %in% names(spec))) {
        names(spec)[names(spec) == "id"] <- "action"
      }
      assertthat::assert_that(
        assertthat::has_name(spec, "pu"),
        assertthat::has_name(spec, "action"),
        msg = paste0(what, " must have columns 'pu' and 'action'.")
      )

      spec$pu <- as_int_id_fun(spec$pu, paste0(what, "$pu"))
      spec$action <- as.character(spec$action)
      spec <- .normalize_feasible_col(spec, what)

      if (!all(spec$pu %in% pu_ids)) {
        bad <- unique(spec$pu[!spec$pu %in% pu_ids])
        stop(what, " contains PU ids not present in x: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      if (!all(spec$action %in% action_ids)) {
        bad <- unique(spec$action[!spec$action %in% action_ids])
        stop(what, " contains action ids not present in actions: ", paste(bad, collapse = ", "), call. = FALSE)
      }

      tmp <- spec[, c("pu", "action")]
      if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) {
        stop(what, " has duplicate (pu, action) rows. Please de-duplicate.", call. = FALSE)
      }

      out <- spec[spec$feasible, c("pu", "action"), drop = FALSE]
      return(out)
    }

    if (is.list(spec)) {
      if (is.null(names(spec)) || any(names(spec) == "")) {
        stop("If '", what, "' is a list, it must be a named list with names = action ids.", call. = FALSE)
      }
      if (!all(names(spec) %in% action_ids)) {
        bad <- setdiff(names(spec), action_ids)
        stop(what, " list contains unknown actions: ", paste(bad, collapse = ", "), call. = FALSE)
      }

      idx_first <- which(!vapply(spec, is.null, logical(1)))[1]
      first <- spec[[idx_first]]
      is_sf_list <- inherits(first, "sf")

      if (is_sf_list) {
        if (!requireNamespace("sf", quietly = TRUE)) {
          stop(what, " provided as sf layers requires the 'sf' package.", call. = FALSE)
        }
        if (is.null(pu_sf) || !inherits(pu_sf, "sf")) {
          stop("To use '", what, "' as sf layers, the problem object must contain x$data$pu_sf (sf planning unit geometry).", call. = FALSE)
        }
        if (!("id" %in% names(pu_sf))) {
          stop("x$data$pu_sf is missing an 'id' column.", call. = FALSE)
        }

        pu_sf2 <- pu_sf
        pu_sf2$id <- as_int_id_fun(pu_sf2$id, "x$data$pu_sf$id")

        out <- vector("list", length(spec))
        names(out) <- names(spec)

        for (a in names(spec)) {
          zone <- spec[[a]]
          if (is.null(zone)) {
            out[[a]] <- NULL
            next
          }
          if (!inherits(zone, "sf")) {
            stop(what, "[[", a, "]] must be an sf object.", call. = FALSE)
          }

          hits <- sf::st_intersects(pu_sf2, zone, sparse = TRUE)
          ids <- pu_sf2$id[lengths(hits) > 0]

          if (length(ids) == 0) {
            out[[a]] <- NULL
          } else {
            out[[a]] <- data.frame(
              pu = ids,
              action = a,
              stringsAsFactors = FALSE
            )
          }
        }

        out_df <- dplyr::bind_rows(out)
        if (nrow(out_df) == 0) return(out_df)
        out_df$pu <- as_int_id_fun(out_df$pu, paste0(what, "$pu"))
        out_df <- dplyr::distinct(out_df)
        return(out_df)
      }

      out <- vector("list", length(spec))
      names(out) <- names(spec)

      for (a in names(spec)) {
        ids <- spec[[a]]
        if (is.null(ids)) {
          out[[a]] <- NULL
          next
        }
        ids <- unique(as_int_id_fun(ids, paste0(what, "[['", a, "']]")))
        if (!all(ids %in% pu_ids)) {
          bad <- ids[!ids %in% pu_ids]
          stop(what, "[[", a, "]] contains PU ids not present in x: ", paste(bad, collapse = ", "), call. = FALSE)
        }
        out[[a]] <- data.frame(
          pu = ids,
          action = a,
          stringsAsFactors = FALSE
        )
      }

      out_df <- dplyr::bind_rows(out)
      if (nrow(out_df) == 0) return(out_df)
      out_df$pu <- as_int_id_fun(out_df$pu, paste0(what, "$pu"))
      out_df <- dplyr::distinct(out_df)
      return(out_df)
    }

    stop("Unsupported type for '", what, "'. Use NULL, data.frame, or a named list.", call. = FALSE)
  }

  .validate_lock_pairs_exist <- function(lock_pairs, dist_actions, what) {
    if (is.null(lock_pairs) || nrow(lock_pairs) == 0) return(invisible(TRUE))

    key_da <- paste(dist_actions$pu, dist_actions$action)
    key_lk <- paste(lock_pairs$pu, lock_pairs$action)

    miss <- !(key_lk %in% key_da)
    if (any(miss)) {
      bad <- lock_pairs[miss, , drop = FALSE]
      ex <- utils::head(paste0("(", bad$pu, ", ", bad$action, ")"), 8)
      stop(
        what, " contains pairs that are not feasible in x$data$dist_actions.\n",
        "Examples: ", paste(ex, collapse = ", "),
        if (nrow(bad) > 8) paste0(" ... (", nrow(bad), " total)") else "",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  stopifnot(inherits(x, "Problem"))

  if (is.null(x$data$dist_actions) || !inherits(x$data$dist_actions, "data.frame")) {
    stop("No action feasibility found. Run add_actions() before add_locked_actions().", call. = FALSE)
  }

  x <- .pa_clone_data(x)

  da <- x$data$dist_actions
  pu <- x$data$pu
  actions <- x$data$actions
  pu_sf <- x$data$pu_sf %||% NULL

  if (is.null(pu) || !inherits(pu, "data.frame") || nrow(pu) == 0) {
    stop("x$data$pu is missing or empty.", call. = FALSE)
  }
  if (is.null(actions) || !inherits(actions, "data.frame") || nrow(actions) == 0) {
    stop("x$data$actions is missing or empty.", call. = FALSE)
  }

  pu_ids <- .as_int_id(pu$id, "x$data$pu$id")
  action_ids <- as.character(actions$id)

  locked_in_pairs <- .spec_to_pairs(locked_in, "locked_in", action_ids, pu_ids, pu_sf, .as_int_id)
  locked_out_pairs <- .spec_to_pairs(locked_out, "locked_out", action_ids, pu_ids, pu_sf, .as_int_id)

  .validate_lock_pairs_exist(locked_in_pairs, da, "locked_in")
  .validate_lock_pairs_exist(locked_out_pairs, da, "locked_out")

  if (!is.null(locked_in_pairs) && !is.null(locked_out_pairs) &&
      nrow(locked_in_pairs) > 0 && nrow(locked_out_pairs) > 0) {
    key_li <- paste(locked_in_pairs$pu, locked_in_pairs$action)
    key_lo <- paste(locked_out_pairs$pu, locked_out_pairs$action)
    overlap <- intersect(key_li, key_lo)

    if (length(overlap) > 0) {
      ex <- utils::head(overlap, 8)
      stop(
        "Some (pu, action) pairs are simultaneously locked_in and locked_out.\n",
        "Examples: ", paste(ex, collapse = ", "),
        if (length(overlap) > 8) paste0(" ... (", length(overlap), " total)") else "",
        call. = FALSE
      )
    }
  }

  if (is.null(da$status)) {
    da$status <- 0L
  }
  da$status <- as.integer(da$status)
  da$status[is.na(da$status)] <- 0L

  if (!is.null(locked_in_pairs) && nrow(locked_in_pairs) > 0) {
    key_da <- paste(da$pu, da$action)
    key_li <- paste(locked_in_pairs$pu, locked_in_pairs$action)
    idx_li <- key_da %in% key_li
    da$status[idx_li] <- 2L
  }

  if (!is.null(locked_out_pairs) && nrow(locked_out_pairs) > 0) {
    key_da <- paste(da$pu, da$action)
    key_lo <- paste(locked_out_pairs$pu, locked_out_pairs$action)
    idx_lo <- key_da %in% key_lo
    da$status[idx_lo] <- 3L
  }

  # enforce PU locked_out at the end
  if ("locked_out" %in% names(pu)) {
    pu_locked_out <- as.logical(pu$locked_out)
    pu_locked_out[is.na(pu_locked_out)] <- FALSE

    locked_out_pus <- pu$id[pu_locked_out]

    if (length(locked_out_pus) > 0) {
      idx_pu_lo <- da$pu %in% locked_out_pus

      if (any(idx_pu_lo & da$status == 2L, na.rm = TRUE)) {
        bad <- da[idx_pu_lo & da$status == 2L, c("pu", "action"), drop = FALSE]
        ex <- utils::head(paste0("(", bad$pu, ", ", bad$action, ")"), 8)
        stop(
          "Some actions are locked_in inside planning units that are locked_out.\n",
          "Examples: ", paste(ex, collapse = ", "),
          if (nrow(bad) > 8) paste0(" ... (", nrow(bad), " total)") else "",
          call. = FALSE
        )
      }

      da$status[idx_pu_lo] <- 3L
    }
  }

  x$data$dist_actions <- da
  x
}
