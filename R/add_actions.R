#' @include internal.R
#'
#' @title Add management actions to a planning problem
#'
#' @description
#' Define the action catalogue, the set of feasible planning unit--action pairs,
#' and their implementation costs.
#'
#' This function adds two core components to a \code{Problem} object. First, it
#' stores the action catalogue in \code{x$data$actions}. Second, it creates the
#' feasible planning unit--action table in \code{x$data$dist_actions}, including
#' implementation costs, status codes, and internal indices used by the
#' optimization backend.
#'
#' Conceptually, if \eqn{\mathcal{P}} is the set of planning units and
#' \eqn{\mathcal{A}} is the set of actions, this function defines a feasible set
#' \eqn{\mathcal{F} \subseteq \mathcal{P} \times \mathcal{A}} together with a
#' non-negative cost function \eqn{c : \mathcal{F} \to \mathbb{R}_{\ge 0}}.
#'
#' @details
#' \strong{Action catalogue.}
#'
#' The \code{actions} argument must be a \code{data.frame} with a unique
#' \code{id} column identifying each action. If a column named \code{action} is
#' supplied instead, it is renamed internally to \code{id}. Additional columns
#' are preserved. If no \code{name} column is provided, action labels are taken
#' from \code{id}. If an \code{action_set} column is present, it is also
#' preserved and can later be used to refer to groups of actions.
#'
#' Actions are stored sorted by \code{id} to ensure reproducible internal
#' indexing.
#'
#' \strong{Feasible planning unit--action pairs.}
#'
#' Feasibility is controlled through \code{include_pairs} and
#' \code{exclude_pairs}.
#'
#' If \code{include_pairs = NULL}, all possible \code{(pu, action)} pairs are
#' initially considered feasible:
#' \deqn{
#' \mathcal{F} = \mathcal{P} \times \mathcal{A}.
#' }
#'
#' If \code{include_pairs} is supplied, only those pairs are retained in the
#' feasible set. If \code{exclude_pairs} is also supplied, matching pairs are
#' removed after applying \code{include_pairs}. Thus, in general:
#' \deqn{
#' \mathcal{F} =
#' \left(\mathcal{F}_{\mathrm{include}} \text{ or } \mathcal{P}\times\mathcal{A}\right)
#' \setminus
#' \mathcal{F}_{\mathrm{exclude}}.
#' }
#'
#' Both \code{include_pairs} and \code{exclude_pairs} can be specified as:
#' \itemize{
#'   \item \code{NULL},
#'   \item a \code{data.frame} with columns \code{pu} and \code{action},
#'   \item or a named list whose names are action ids.
#' }
#'
#' When supplied as a \code{data.frame}, the object must contain columns
#' \code{pu} and \code{action}. An optional logical-like column
#' \code{feasible} may also be provided; only rows with \code{feasible = TRUE}
#' are retained. Missing values in \code{feasible} are treated as
#' \code{FALSE}.
#'
#' When supplied as a named list, names must match action ids. Each element may
#' contain either:
#' \itemize{
#'   \item a vector of planning-unit ids, or
#'   \item an \code{sf} object defining the spatial zone where the action is
#'   feasible.
#' }
#'
#' In the spatial case, feasible planning units are identified using
#' \code{sf::st_intersects()} against \code{x$data$pu_sf}.
#'
#' \strong{Feasibility versus decision fixing.}
#'
#' This function only determines whether a \code{(pu, action)} pair exists in
#' the model. It does not force a feasible action to be selected or forbidden
#' beyond structural infeasibility. Fixed decisions should instead be imposed
#' later with \code{\link{add_locked_actions}}.
#'
#' \strong{Costs.}
#'
#' Costs can be supplied in several ways:
#' \itemize{
#'   \item If \code{cost = NULL}, all feasible pairs receive a default cost of
#'   \code{1}.
#'   \item If \code{cost} is a scalar, that value is assigned to all feasible
#'   pairs.
#'   \item If \code{cost} is a named numeric vector, names must match action ids
#'   and costs are assigned by action.
#'   \item If \code{cost} is a \code{data.frame}, it must define either:
#'   \itemize{
#'     \item action-level costs through columns \code{action} and \code{cost}, or
#'     \item pair-specific costs through columns \code{pu}, \code{action}, and
#'     \code{cost}.
#'   }
#' }
#'
#' In all cases, costs must be finite and non-negative.
#'
#' \strong{Status values.}
#'
#' Internally, all feasible pairs are initialized with \code{status = 0},
#' meaning that the decision is free. If \code{x$data$pu$locked_out} exists and
#' a planning unit is marked as locked out, then all feasible actions in that
#' planning unit are assigned \code{status = 3}. This preserves consistency with
#' planning-unit exclusions already stored in the problem.
#'
#' \strong{Replacement behaviour.}
#'
#' Calling \code{add_actions()} replaces any previous action catalogue and
#' feasible action table stored in the problem object.
#'
#' @param x A \code{Problem} object created with \code{\link{input_data}}.
#'
#' @param actions A \code{data.frame} defining the action catalogue. It must
#'   contain a unique \code{id} column. A column named \code{action} is also
#'   accepted and automatically renamed to \code{id}.
#'
#' @param include_pairs Optional specification of feasible \code{(pu, action)}
#'   pairs. It can be \code{NULL}, a \code{data.frame} with columns
#'   \code{pu} and \code{action} (optionally also \code{feasible}), or a named
#'   list whose names are action ids and whose elements are vectors of planning
#'   unit ids or \code{sf} objects.
#'
#' @param exclude_pairs Optional specification of infeasible \code{(pu, action)}
#'   pairs. It uses the same formats as \code{include_pairs} and removes
#'   matching pairs from the feasible set.
#'
#' @param cost Optional cost specification for feasible pairs. It may be
#'   \code{NULL}, a scalar numeric value, a named numeric vector indexed by
#'   action id, or a \code{data.frame} with columns \code{action, cost} or
#'   \code{pu, action, cost}.
#'
#' @return An updated \code{Problem} object with:
#' \describe{
#'   \item{\code{x$data$actions}}{The action catalogue, including a unique
#'   integer \code{internal_id} for each action.}
#'   \item{\code{x$data$dist_actions}}{The feasible planning unit--action table
#'   with columns \code{pu}, \code{action}, \code{cost}, \code{status},
#'   \code{internal_pu}, and \code{internal_action}.}
#'   \item{\code{x$data$index$pu}}{A mapping from user-supplied planning-unit ids
#'   to internal integer ids.}
#'   \item{\code{x$data$index$action}}{A mapping from action ids to internal
#'   integer ids.}
#' }
#'
#' @seealso
#' \code{\link{input_data}},
#' \code{\link{add_locked_actions}}
#'
#' @examples
#' # ------------------------------------------------------
#' # Minimal planning problem
#' # ------------------------------------------------------
#' pu <- data.frame(
#'   id = 1:4,
#'   cost = c(2, 3, 1, 4)
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
#' actions <- data.frame(
#'   id = c("conservation", "restoration"),
#'   name = c("Conservation", "Restoration")
#' )
#'
#' # Example 1: all actions feasible in all planning units
#' p1 <- add_actions(
#'   x = p,
#'   actions = actions,
#'   cost = c(conservation = 5, restoration = 12)
#' )
#'
#' print(p1)
#'
#' utils::head(p1$data$dist_actions)
#'
#' # Example 2: specify feasible pairs explicitly
#' include_df <- data.frame(
#'   pu = c(1, 2, 3, 4),
#'   action = c("conservation", "conservation", "restoration", "restoration")
#' )
#'
#' p2 <- add_actions(
#'   x = p,
#'   actions = actions,
#'   include_pairs = include_df,
#'   cost = 10
#' )
#'
#' p2$data$dist_actions
#'
#' # Example 3: remove selected pairs after full expansion
#' exclude_df <- data.frame(
#'   pu = c(2, 4),
#'   action = c("restoration", "conservation")
#' )
#'
#' p3 <- add_actions(
#'   x = p,
#'   actions = actions,
#'   exclude_pairs = exclude_df,
#'   cost = c(conservation = 3, restoration = 8)
#' )
#'
#' p3$data$dist_actions
#'
#' @export
add_actions <- function(
    x,
    actions,
    include_pairs = NULL,
    exclude_pairs = NULL,
    cost = NULL
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
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a paretoscape Problem object")
  assertthat::assert_that(
    !is.null(x$data$pu),
    !is.null(x$data$features),
    !is.null(x$data$dist_features),
    msg = "x must be created with input_data()"
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

  actions <- actions[order(actions$id), , drop = FALSE]

  if (!("internal_id" %in% names(actions))) {
    actions$internal_id <- seq_len(nrow(actions))
  } else {
    actions$internal_id <- as.integer(actions$internal_id)
    if (base::anyNA(actions$internal_id)) {
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
  include_df <- .spec_to_pairs(include_pairs, "include_pairs", action_ids, pu_ids, pu_sf, .as_int_id)
  exclude_df <- .spec_to_pairs(exclude_pairs, "exclude_pairs", action_ids, pu_ids, pu_sf, .as_int_id)

  if (is.null(include_df)) {
    dist_actions <- base::expand.grid(
      pu = pu_ids,
      action = action_ids,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    dist_actions <- include_df
  }

  if (!is.null(exclude_df) && nrow(exclude_df) > 0) {
    key_da <- paste(dist_actions$pu, dist_actions$action)
    key_ex <- paste(exclude_df$pu, exclude_df$action)
    keep <- !(key_da %in% key_ex)
    dist_actions <- dist_actions[keep, , drop = FALSE]
  }

  if (nrow(dist_actions) == 0) {
    stop("No feasible (pu, action) pairs were created after applying include_pairs/exclude_pairs.", call. = FALSE)
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
  # if ("locked_out" %in% names(x$data$pu)) {
  #   pu_locked_out <- x$data$pu$locked_out
  #   pu_locked_out[is.na(pu_locked_out)] <- FALSE
  #   pu_locked_out <- as.logical(pu_locked_out)
  #
  #   locked_out_pus <- x$data$pu$id[pu_locked_out]
  #
  #   if (length(locked_out_pus) > 0) {
  #     idx_pu_lo <- dist_actions$pu %in% locked_out_pus
  #     dist_actions$status[idx_pu_lo] <- 3L
  #   }
  # }

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
