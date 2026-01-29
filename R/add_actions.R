#' @include internal.R
#'
#' @title Add actions to a planning problem
#'
#' @description
#' Define the set of actions, where they can be implemented (feasibility),
#' their costs, and optional lock status per (pu, action).
#' Populates `actions` and `dist_actions` inside the problem object.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param actions A `data.frame` with column `id` (unique action ids).
#'   For backwards compatibility, a column named `action` is also accepted
#'   and will be renamed to `id`. Optional columns (e.g., `name`, `type`) are kept.
#' @param where Optional feasibility specification (see docs).
#' @param cost Optional costs specification (see docs).
#' @param status Optional lock status for feasible (pu, action) pairs (0/2/3).
#' @param feasible_default Logical. If `where` is NULL, set all actions feasible in all PUs.
#' @param na_is_infeasible Logical. If `feasible` contains NA, treat as FALSE.
#' @param sort_actions Logical. If TRUE, order actions by `id` before assigning internal_id.
#'
#' @return The updated [data-class] object.
#' @export
add_actions <- function(
    x,
    actions,
    where = NULL,
    cost = NULL,
    status = NULL,
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

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a prioriactions Data object")
  assertthat::assert_that(!is.null(x$data$pu), !is.null(x$data$features), !is.null(x$data$dist_features),
                          msg = "x must be created with inputData()/inputDataSpatial()")

  # ---- ensure PU internal ids exist + index
  if (is.null(x$data$pu$internal_id)) {
    x$data$pu$internal_id <- seq_len(nrow(x$data$pu))
  }

  # enforce PU ids integer (consistent with inputData())
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
  if (anyDuplicated(actions$id) != 0) stop("actions$id must be unique.", call. = FALSE)

  # optional: keep columns, but standardize ordering and internal id
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

  # Store index (optional but very handy)
  if (is.null(x$data$index) || !is.list(x$data$index)) x$data$index <- list()
  x$data$index$pu <- pu_index
  x$data$index$action <- action_index

  # ---- build feasible pairs (dist_actions skeleton)
  dist_actions <- NULL

  if (is.null(where)) {

    if (!isTRUE(feasible_default)) {
      dist_actions <- data.frame(pu = integer(0), action = character(0), stringsAsFactors = FALSE)
    } else {
      dist_actions <- base::expand.grid(
        pu = pu_ids,
        action = action_ids,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    }

  } else if (inherits(where, "data.frame")) {

    assertthat::assert_that(assertthat::has_name(where, "pu"), nrow(where) > 0)

    if ("id" %in% names(where) && !("action" %in% names(where))) {
      names(where)[names(where) == "id"] <- "action"
    }
    assertthat::assert_that(assertthat::has_name(where, "action"))

    # normalize types
    where$pu <- .as_int_id(where$pu, "where$pu")
    where$action <- as.character(where$action)

    if (!("feasible" %in% names(where))) where$feasible <- TRUE
    where$feasible <- as.logical(where$feasible)
    if (na_is_infeasible) where$feasible[is.na(where$feasible)] <- FALSE

    if (!all(where$pu %in% pu_ids)) {
      bad <- unique(where$pu[!where$pu %in% pu_ids])
      stop("where contains PU ids not present in x: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    if (!all(where$action %in% action_ids)) {
      bad <- unique(where$action[!where$action %in% action_ids])
      stop("where contains action ids not present in actions: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    tmp <- where[, c("pu", "action")]
    if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) {
      stop("where has duplicate (pu, action) rows. Please de-duplicate.", call. = FALSE)
    }

    dist_actions <- where[where$feasible, c("pu", "action"), drop = FALSE]

  } else if (is.list(where)) {

    if (is.null(names(where)) || any(names(where) == "")) {
      stop("If 'where' is a list, it must be a named list with names = action ids.", call. = FALSE)
    }
    if (!all(names(where) %in% action_ids)) {
      bad <- setdiff(names(where), action_ids)
      stop("where list contains unknown actions: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    idx_first <- which(!vapply(where, is.null, logical(1)))[1]
    first <- where[[idx_first]]
    is_sf_list <- inherits(first, "sf")

    if (is_sf_list) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        stop("where provided as sf layers requires the 'sf' package.", call. = FALSE)
      }
      if (is.null(x$data$pu_sf) || !inherits(x$data$pu_sf, "sf")) {
        stop("To use 'where' as sf layers, the problem object must contain x$data$pu_sf (sf planning unit geometry).", call. = FALSE)
      }
      pu_sf <- x$data$pu_sf
      if (!("id" %in% names(pu_sf))) stop("x$data$pu_sf is missing an 'id' column.", call. = FALSE)

      # ensure pu_sf$id integer too (safe)
      pu_sf$id <- .as_int_id(pu_sf$id, "x$data$pu_sf$id")

      out <- vector("list", length(where))
      names(out) <- names(where)

      for (a in names(where)) {
        zone <- where[[a]]
        if (is.null(zone)) { out[[a]] <- NULL; next }
        if (!inherits(zone, "sf")) stop("where[[", a, "]] must be an sf object.", call. = FALSE)

        hits <- sf::st_intersects(pu_sf, zone, sparse = TRUE)
        feasible_ids <- pu_sf$id[lengths(hits) > 0]

        if (length(feasible_ids) == 0) {
          out[[a]] <- NULL
        } else {
          out[[a]] <- data.frame(pu = feasible_ids, action = a, stringsAsFactors = FALSE)
        }
      }

      dist_actions <- dplyr::bind_rows(out)

    } else {

      out <- vector("list", length(where))
      names(out) <- names(where)

      for (a in names(where)) {
        ids <- where[[a]]
        if (is.null(ids)) { out[[a]] <- NULL; next }
        ids <- unique(.as_int_id(ids, paste0("where[['", a, "']]")))
        if (!all(ids %in% pu_ids)) {
          bad <- ids[!ids %in% pu_ids]
          stop("where[[", a, "]] contains PU ids not present in x: ", paste(bad, collapse = ", "), call. = FALSE)
        }
        out[[a]] <- data.frame(pu = ids, action = a, stringsAsFactors = FALSE)
      }

      dist_actions <- dplyr::bind_rows(out)
    }

  } else {
    stop("Unsupported type for 'where'. Use NULL, data.frame, or a named list.", call. = FALSE)
  }

  if (nrow(dist_actions) == 0) {
    stop("No feasible (pu, action) pairs were created. Check 'where' / 'feasible_default'.", call. = FALSE)
  }

  # enforce pu integer in dist_actions (important!)
  dist_actions$pu <- .as_int_id(dist_actions$pu, "dist_actions$pu")

  # ---- costs (default 1)
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
      if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) stop("cost has duplicate (pu, action) rows.", call. = FALSE)

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

  # important: costs must be present (no NA) in dist_actions rows
  assertthat::assert_that(is.numeric(dist_actions$cost))
  if (any(!is.finite(dist_actions$cost) | is.na(dist_actions$cost))) {
    stop("Some feasible (pu, action) pairs have missing/invalid costs after processing 'cost'.", call. = FALSE)
  }
  if (any(dist_actions$cost < 0, na.rm = TRUE)) stop("Action costs must be non-negative.", call. = FALSE)

  # ---- status (0/2/3)
  dist_actions$status <- 0L

  if (!is.null(status)) {
    assertthat::assert_that(inherits(status, "data.frame"))
    if ("id" %in% names(status) && !("action" %in% names(status))) {
      names(status)[names(status) == "id"] <- "action"
    }
    assertthat::assert_that(all(c("pu", "action", "status") %in% names(status)))

    status$pu <- .as_int_id(status$pu, "status$pu")
    status$action <- as.character(status$action)
    status$status <- as.integer(status$status)

    if (!all(status$pu %in% pu_ids)) stop("status contains unknown pu ids.", call. = FALSE)
    if (!all(status$action %in% action_ids)) stop("status contains unknown actions.", call. = FALSE)
    if (!all(status$status %in% c(0L, 2L, 3L))) stop("status must be in {0,2,3}.", call. = FALSE)

    tmp <- status[, c("pu", "action")]
    if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) stop("status has duplicate (pu, action) rows.", call. = FALSE)

    key_da <- paste(dist_actions$pu, dist_actions$action)
    key_s  <- paste(status$pu, status$action)
    m <- match(key_da, key_s)
    hit <- !is.na(m)
    dist_actions$status[hit] <- status$status[m[hit]]
  }

  # ---- enforce PU locked_out: all actions in locked_out PUs become status=3
  if ("locked_out" %in% names(x$data$pu)) {
    locked_out_pus <- x$data$pu$id[which(isTRUE(x$data$pu$locked_out) | x$data$pu$locked_out == TRUE)]
    if (length(locked_out_pus) > 0) {
      idx_lo <- dist_actions$pu %in% locked_out_pus
      if (any(idx_lo)) {
        if (any(dist_actions$status[idx_lo] == 2L, na.rm = TRUE)) {
          warning("Some actions were locked-in inside PU(s) that are locked_out. Forcing those actions to locked_out (status=3).",
                  call. = FALSE, immediate. = TRUE)
        }
        dist_actions$status[idx_lo] <- 3L
      }
    }
  }

  # ---- add internal ids to dist_actions
  dist_actions$internal_pu <- unname(pu_index[as.character(dist_actions$pu)])
  dist_actions$internal_action <- unname(action_index[as.character(dist_actions$action)])

  # ---- (NEW) internal row id for each feasible (pu, action) pair
  # Some Rcpp routines expect this column (e.g., linking constraints).
  # Keep it stable and integer.
  dist_actions <- dist_actions[order(dist_actions$internal_pu, dist_actions$internal_action), , drop = FALSE]
  #dist_actions$internal_row <- seq_len(nrow(dist_actions))

  if (anyNA(dist_actions$internal_pu)) stop("Internal error: could not map pu -> internal_pu.", call. = FALSE)
  if (anyNA(dist_actions$internal_action)) stop("Internal error: could not map action -> internal_action.", call. = FALSE)

  # ---- store in object
  x$data$actions <- actions
  x$data$dist_actions <- dist_actions

  x
}
