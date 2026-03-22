#' @include internal.R
#'
#' @title Add actions to a planning problem
#'
#' @description
#' Define the set of management actions, where they can be implemented
#' (feasibility), and their costs.
#'
#' The function stores the action catalogue in \code{x$data$actions} and the
#' feasible action distribution in \code{x$data$dist_actions}.
#'
#' Feasibility is controlled via \code{include} / \code{exclude}:
#' \itemize{
#'   \item If \code{include} is \code{NULL} and \code{feasible_default = TRUE},
#'   all \code{(pu, action)} pairs are feasible.
#'   \item If \code{include} is provided, only those \code{(pu, action)} pairs
#'   are feasible.
#'   \item If \code{exclude} is provided, those \code{(pu, action)} pairs are
#'   removed from the feasible set.
#' }
#'
#' @details
#' \strong{Action catalogue.}
#' The \code{actions} table must contain an \code{id} column with unique action
#' identifiers. Additional columns are preserved and stored in
#' \code{x$data$actions}. Users may optionally provide an \code{action_set}
#' column to define action groups.
#'
#' \strong{Accepted formats for \code{include} and \code{exclude}:}
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
#' \strong{Important distinction:}
#' \itemize{
#'   \item \code{include}/\code{exclude} control whether a \code{(pu, action)}
#'   pair exists in the model.
#'   \item Decision fixing must be applied later with \code{add_locked_actions()}.
#' }
#'
#' \strong{Spatial feasibility:} when these specifications are provided as
#' \code{sf} layers, planning units in \code{x$data$pu_sf} are matched to zones
#' via \code{sf::st_intersects()}.
#'
#' \strong{Costs:} costs can be specified as a scalar, a named numeric vector by
#' action id, or a \code{data.frame} providing costs by action
#' (\code{action, cost}) or by pair (\code{pu, action, cost}).
#'
#' Internally, feasible pairs are initialized with \code{status = 0} (free).
#' If \code{x$data$pu$locked_out} exists, then all action pairs in those planning
#' units are forced to \code{status = 3}.
#'
#' @param x A \code{Problem} object created with \code{\link{inputData}} or
#'   \code{\link{inputDataSpatial}}.
#' @param actions A \code{data.frame} defining the action catalogue. Must
#'   contain a unique \code{id} column. A column named \code{action} is also
#'   accepted and renamed to \code{id}.
#' @param include Optional feasibility specification. Only these
#'   \code{(pu, action)} pairs are feasible.
#' @param exclude Optional infeasibility specification. Removed from the feasible
#'   set.
#' @param cost Optional cost specification for feasible \code{(pu, action)}
#'   pairs.
#' @param feasible_default Logical. If \code{include} is \code{NULL}, should all
#'   actions be feasible in all planning units?
#' @param na_is_infeasible Logical. Only relevant when a specification is
#'   provided as a \code{data.frame} with a \code{feasible} column.
#' @param sort_actions Logical. If \code{TRUE}, sort \code{actions} by
#'   \code{id}.
#'
#' @return The updated \code{Problem} object with:
#' \itemize{
#'   \item \code{x$data$actions}: action catalogue including \code{internal_id},
#'   \item \code{x$data$dist_actions}: feasible \code{(pu, action)} pairs with
#'   columns \code{pu}, \code{action}, \code{cost}, \code{status},
#'   \code{internal_pu}, and \code{internal_action},
#'   \item \code{x$data$index$pu} and \code{x$data$index$action}: id-to-internal-id mappings.
#' }
#'
#' @export
add_actions <- function(
    x,
    actions,
    include = NULL,
    exclude = NULL,
    cost = NULL,
    feasible_default = TRUE,
    na_is_infeasible = TRUE,
    sort_actions = TRUE
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
          feasible_ids <- pu_sf2$id[lengths(hits) > 0]

          if (length(feasible_ids) == 0) {
            out[[a]] <- NULL
          } else {
            out[[a]] <- data.frame(
              pu = feasible_ids,
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

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a prioriactions Problem object")
  assertthat::assert_that(
    !is.null(x$data$pu),
    !is.null(x$data$features),
    !is.null(x$data$dist_features),
    msg = "x must be created with inputData()/inputDataSpatial()"
  )

  if (is.null(x$data$pu$internal_id)) {
    x$data$pu$internal_id <- seq_len(nrow(x$data$pu))
  }

  x <- .pa_clone_data(x)
  x$data$pu$id <- .as_int_id(x$data$pu$id, "x$data$pu$id")

  pu_ids <- x$data$pu$id
  pu_index <- stats::setNames(x$data$pu$internal_id, as.character(x$data$pu$id))

  # ---- actions catalog
  assertthat::assert_that(inherits(actions, "data.frame"), nrow(actions) > 0)

  if ("action" %in% names(actions) && !("id" %in% names(actions))) {
    warning("actions has column 'action'. Renaming it to 'id'.", call. = FALSE, immediate. = TRUE)
    names(actions)[names(actions) == "action"] <- "id"
  }

  assertthat::assert_that(assertthat::has_name(actions, "id"), assertthat::noNA(actions$id))
  actions$id <- as.character(actions$id)
  if (anyDuplicated(actions$id) != 0) {
    stop("actions$id must be unique.", call. = FALSE)
  }

  if (!("name" %in% names(actions))) {
    actions$name <- as.character(actions$id)
  } else {
    actions$name <- as.character(actions$name)
    if (anyNA(actions$name) || any(!nzchar(actions$name))) {
      stop("actions$name cannot contain NA or empty strings.", call. = FALSE)
    }
  }

  if ("action_set" %in% names(actions)) {
    actions$action_set <- as.character(actions$action_set)
    if (anyNA(actions$action_set) || any(!nzchar(actions$action_set))) {
      stop("actions$action_set cannot contain NA or empty strings.", call. = FALSE)
    }
  }

  if (isTRUE(sort_actions)) {
    actions <- actions[order(actions$id), , drop = FALSE]
  }

  if (!("internal_id" %in% names(actions))) {
    actions$internal_id <- seq_len(nrow(actions))
  } else {
    actions$internal_id <- as.integer(actions$internal_id)
    if (assertthat::anyNA(actions$internal_id)) {
      stop("actions$internal_id contains NA.", call. = FALSE)
    }
    if (anyDuplicated(actions$internal_id) != 0) {
      stop("actions$internal_id must be unique if provided.", call. = FALSE)
    }
  }

  action_ids <- actions$id
  action_index <- stats::setNames(actions$internal_id, actions$id)

  if (is.null(x$data$index) || !is.list(x$data$index)) x$data$index <- list()
  x$data$index$pu <- pu_index
  x$data$index$action <- action_index

  # ---- build feasible pairs
  pu_sf <- x$data$pu_sf
  include_pairs <- .spec_to_pairs(include, "include", action_ids, pu_ids, pu_sf, .as_int_id)
  exclude_pairs <- .spec_to_pairs(exclude, "exclude", action_ids, pu_ids, pu_sf, .as_int_id)

  if (is.null(include_pairs)) {
    if (!isTRUE(feasible_default)) {
      stop("No feasible (pu, action) pairs were created: include is NULL and feasible_default=FALSE.", call. = FALSE)
    }
    dist_actions <- base::expand.grid(
      pu = pu_ids,
      action = action_ids,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    dist_actions <- include_pairs
  }

  if (!is.null(exclude_pairs) && nrow(exclude_pairs) > 0) {
    key_da <- paste(dist_actions$pu, dist_actions$action)
    key_ex <- paste(exclude_pairs$pu, exclude_pairs$action)
    keep <- !(key_da %in% key_ex)
    dist_actions <- dist_actions[keep, , drop = FALSE]
  }

  if (nrow(dist_actions) == 0) {
    stop("No feasible (pu, action) pairs were created after applying include/exclude.", call. = FALSE)
  }

  dist_actions$pu <- .as_int_id(dist_actions$pu, "dist_actions$pu")
  dist_actions$action <- as.character(dist_actions$action)

  # ---- costs
  dist_actions$cost <- 1

  if (is.null(cost)) {

    # keep default

  } else if (is.numeric(cost) && length(cost) == 1) {

    dist_actions$cost <- as.numeric(cost)

  } else if (is.numeric(cost) && !is.null(names(cost))) {

    if (!all(names(cost) %in% action_ids)) {
      bad <- setdiff(names(cost), action_ids)
      stop("cost contains unknown actions: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    dist_actions$cost <- as.numeric(cost[dist_actions$action])

  } else if (inherits(cost, "data.frame")) {

    if ("id" %in% names(cost) && !("action" %in% names(cost))) {
      names(cost)[names(cost) == "id"] <- "action"
    }

    if (all(c("action", "cost") %in% names(cost)) && !("pu" %in% names(cost))) {

      cost$action <- as.character(cost$action)
      if (!all(cost$action %in% action_ids)) {
        bad <- unique(cost$action[!cost$action %in% action_ids])
        stop("cost contains unknown actions: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      if (nrow(dplyr::distinct(cost[, c("action")])) != nrow(cost)) {
        stop("cost (action,cost) must have unique action rows.", call. = FALSE)
      }

      m <- match(dist_actions$action, cost$action)
      dist_actions$cost <- cost$cost[m]

    } else if (all(c("pu", "action", "cost") %in% names(cost))) {

      cost$pu <- .as_int_id(cost$pu, "cost$pu")
      cost$action <- as.character(cost$action)

      if (!all(cost$pu %in% pu_ids)) stop("cost contains unknown pu ids.", call. = FALSE)
      if (!all(cost$action %in% action_ids)) stop("cost contains unknown actions.", call. = FALSE)

      tmp <- cost[, c("pu", "action")]
      if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) {
        stop("cost has duplicate (pu, action) rows.", call. = FALSE)
      }

      key_da <- paste(dist_actions$pu, dist_actions$action)
      key_c  <- paste(cost$pu, cost$action)
      m <- match(key_da, key_c)
      hit <- !is.na(m)
      dist_actions$cost[hit] <- cost$cost[m[hit]]

    } else {
      stop("Unsupported cost data.frame format. Use (action,cost) or (pu,action,cost).", call. = FALSE)
    }

  } else {
    stop("Unsupported type for 'cost'.", call. = FALSE)
  }

  assertthat::assert_that(is.numeric(dist_actions$cost))
  if (any(!is.finite(dist_actions$cost) | is.na(dist_actions$cost))) {
    stop("Some feasible (pu, action) pairs have missing/invalid costs after processing 'cost'.", call. = FALSE)
  }
  if (any(dist_actions$cost < 0, na.rm = TRUE)) {
    stop("Action costs must be non-negative.", call. = FALSE)
  }

  # ---- initialize status as free
  dist_actions$status <- 0L

  # ---- enforce PU locked_out
  if ("locked_out" %in% names(x$data$pu)) {
    pu_locked_out <- x$data$pu$locked_out
    pu_locked_out[is.na(pu_locked_out)] <- FALSE
    pu_locked_out <- as.logical(pu_locked_out)

    locked_out_pus <- x$data$pu$id[pu_locked_out]

    if (length(locked_out_pus) > 0) {
      idx_pu_lo <- dist_actions$pu %in% locked_out_pus
      dist_actions$status[idx_pu_lo] <- 3L
    }
  }

  # ---- add internal ids
  dist_actions$internal_pu <- unname(pu_index[as.character(dist_actions$pu)])
  dist_actions$internal_action <- unname(action_index[as.character(dist_actions$action)])

  dist_actions <- dist_actions[order(dist_actions$internal_pu, dist_actions$internal_action), , drop = FALSE]

  if (anyNA(dist_actions$internal_pu)) {
    stop("Internal error: could not map pu -> internal_pu.", call. = FALSE)
  }
  if (anyNA(dist_actions$internal_action)) {
    stop("Internal error: could not map action -> internal_action.", call. = FALSE)
  }

  x$data$actions <- actions
  x$data$dist_actions <- dist_actions

  x
}
