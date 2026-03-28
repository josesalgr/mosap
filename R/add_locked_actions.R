#' @include internal.R
NULL

#' @title Add locked action decisions to a planning problem
#'
#' @description
#' Fix feasible planning unit--action decisions to be selected or excluded.
#'
#' This function modifies the status of existing feasible
#' \code{(pu, action)} pairs stored in \code{x$data$dist_actions}. It does not
#' create new feasible action pairs and therefore must be used only after
#' \code{\link{add_actions}} has been called.
#'
#' Locked decisions are represented through status codes:
#' \itemize{
#'   \item \code{0}: free decision,
#'   \item \code{2}: locked in,
#'   \item \code{3}: locked out.
#' }
#'
#' @details
#' Let \eqn{\mathcal{F} \subseteq \mathcal{P} \times \mathcal{A}} denote the set
#' of feasible planning unit--action pairs already defined in
#' \code{x$data$dist_actions}, where \eqn{\mathcal{P}} is the set of planning
#' units and \eqn{\mathcal{A}} is the set of actions.
#'
#' This function allows the user to define two subsets:
#' \itemize{
#'   \item \eqn{\mathcal{L}^{in} \subseteq \mathcal{F}}, the set of feasible
#'   pairs that must be selected,
#'   \item \eqn{\mathcal{L}^{out} \subseteq \mathcal{F}}, the set of feasible
#'   pairs that must not be selected.
#' }
#'
#' These sets are encoded by updating the \code{status} column of
#' \code{x$data$dist_actions}. The function validates that all requested
#' locked-in and locked-out pairs are already feasible. Therefore, it cannot be
#' used to introduce new planning unit--action combinations into the problem.
#'
#' In optimization terms, if \eqn{x_{ia}} denotes the decision variable
#' associated with planning unit \eqn{i} and action \eqn{a}, then:
#' \itemize{
#'   \item locked-in pairs conceptually impose \eqn{x_{ia} = 1},
#'   \item locked-out pairs conceptually impose \eqn{x_{ia} = 0}.
#' }
#'
#' The exact translation into solver-side constraints occurs later when the
#' model is built.
#'
#' \strong{Accepted formats}
#'
#' Both \code{locked_in} and \code{locked_out} accept the same formats:
#' \itemize{
#'   \item \code{NULL},
#'   \item a \code{data.frame} with columns \code{pu} and \code{action},
#'   optionally including a \code{feasible} column used as a filter,
#'   \item a named list whose names are action ids and whose elements are either
#'   vectors of planning unit ids or \code{sf} objects.
#' }
#'
#' If a \code{feasible} column is supplied in a \code{data.frame}, only rows
#' with \code{feasible = TRUE} are used. Missing values in \code{feasible} are
#' treated as \code{FALSE}.
#'
#' If an \code{sf} specification is supplied, the problem object must contain
#' \code{x$data$pu_sf}, and planning units are matched spatially using
#' \code{sf::st_intersects()}.
#'
#' \strong{Conflict checking}
#'
#' A given \code{(pu, action)} pair cannot be simultaneously requested in both
#' \code{locked_in} and \code{locked_out}. Such overlaps are rejected.
#'
#' In addition, if a planning unit is already marked as locked out at the
#' planning-unit level through \code{x$data$pu$locked_out}, then all feasible
#' actions in that planning unit are forced to \code{status = 3}. Any attempt to
#' lock in an action within such a planning unit raises an error.
#'
#' \strong{Order of precedence}
#'
#' User-supplied locked-in and locked-out action requests are first applied to
#' \code{x$data$dist_actions}. Afterwards, any planning-unit-level
#' \code{locked_out} flag stored in \code{x$data$pu} is enforced, overriding
#' action-level status and ensuring consistency with planning-unit exclusions.
#'
#' @param x A \code{Problem} object with action feasibility already defined via
#'   \code{\link{add_actions}}.
#'
#' @param locked_in Optional specification of feasible \code{(pu, action)} pairs
#'   that must be selected. It may be \code{NULL}, a \code{data.frame}, or a
#'   named list.
#'
#' @param locked_out Optional specification of feasible \code{(pu, action)}
#'   pairs that must not be selected. It may be \code{NULL}, a
#'   \code{data.frame}, or a named list.
#'
#' @return An updated \code{Problem} object in which
#'   \code{x$data$dist_actions$status} has been modified to reflect locked-in and
#'   locked-out decisions.
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:4,
#'   cost = c(2, 3, 1, 4)
#' )
#'
#' features <- data.frame(
#'   id = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3, 4, 4),
#'   feature = c("sp1", "sp2", "sp1", "sp2", "sp1", "sp2"),
#'   amount = c(1, 2, 1, 3, 2, 1)
#' )
#'
#' p <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_actions(
#'   x = p,
#'   actions = data.frame(id = c("conservation", "restoration")),
#'   cost = c(conservation = 3, restoration = 8)
#' )
#'
#' # Lock a few feasible decisions
#' p <- add_locked_actions(
#'   x = p,
#'   locked_in = data.frame(
#'     pu = c(1, 2),
#'     action = c("conservation", "restoration")
#'   ),
#'   locked_out = data.frame(
#'     pu = c(4),
#'     action = c("conservation")
#'   )
#' )
#'
#' p$data$dist_actions
#'
#' # Named-list interface
#' p2 <- add_locked_actions(
#'   x = p,
#'   locked_in = list(
#'     conservation = c(1, 3)
#'   ),
#'   locked_out = list(
#'     restoration = c(2)
#'   )
#' )
#'
#' p2$data$dist_actions
#'
#' @seealso
#' \code{\link{add_actions}},
#' \code{\link{add_locked_pu}}
#'
#' @export
add_locked_actions <- function(
    x,
    locked_in = NULL,
    locked_out = NULL
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

    # if (na_is_infeasible)
    f[is.na(f)] <- FALSE
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
