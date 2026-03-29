#' @include internal.R
#'
#' @title Add action effects to a planning problem
#'
#' @description
#' Define the effects of management actions on features across planning units.
#'
#' Effects are stored in a canonical representation in
#' \code{x$data$dist_effects}, with one row per
#' \code{(pu, action, feature)} triple and two non-negative columns:
#' \itemize{
#'   \item \code{benefit}: the positive component of the effect,
#'   \item \code{loss}: the magnitude of the negative component of the effect.
#' }
#'
#' The net effect is therefore interpreted as
#' \deqn{
#' \Delta_{i a f} = \mathrm{benefit}_{i a f} - \mathrm{loss}_{i a f},
#' }
#' where \eqn{i} indexes planning units, \eqn{a} indexes actions, and
#' \eqn{f} indexes features.
#'
#' Under the semantics adopted by this package, each
#' \code{(pu, action, feature)} triple represents a single net effect.
#' Consequently, after validation and aggregation, a stored row cannot have both
#' \code{benefit > 0} and \code{loss > 0} at the same time.
#'
#' @details
#' This function provides a unified interface for specifying action effects from
#' several input formats while enforcing a single internal representation.
#' Regardless of how the user supplies the effects, the stored output always
#' follows the same canonical structure based on non-negative
#' \code{benefit}/\code{loss} components.
#'
#' Let \eqn{b_{if}} denote the baseline amount of feature \eqn{f} in planning
#' unit \eqn{i}, taken from \code{x$data$dist_features$amount}. Let
#' \eqn{\Delta_{i a f}} denote the net change caused by applying action
#' \eqn{a} in planning unit \eqn{i} to feature \eqn{f}. The canonical stored
#' representation is:
#'
#' \deqn{
#' \mathrm{benefit}_{i a f} = \max(\Delta_{i a f}, 0),
#' }
#'
#' \deqn{
#' \mathrm{loss}_{i a f} = \max(-\Delta_{i a f}, 0).
#' }
#'
#' Hence:
#' \itemize{
#'   \item if \eqn{\Delta_{i a f} > 0}, then \code{benefit > 0} and \code{loss = 0},
#'   \item if \eqn{\Delta_{i a f} < 0}, then \code{benefit = 0} and \code{loss > 0},
#'   \item if \eqn{\Delta_{i a f} = 0}, then both are zero.
#' }
#'
#' \strong{Why split effects into benefit and loss?}
#'
#' This representation avoids ambiguity in downstream optimization models. It
#' allows the package to support, for example, objectives that maximize
#' beneficial effects, minimize damages, impose no-net-loss conditions, or
#' combine both components differently in multi-objective formulations.
#'
#' \strong{Supported effect specifications}
#'
#' The \code{effects} argument may be provided in one of the following forms:
#'
#' \enumerate{
#'   \item \code{NULL}. An empty effects table is stored.
#'
#'   \item A \code{data.frame(action, feature, multiplier)}. In this case,
#'   effects are constructed by multiplying baseline feature amounts by the
#'   supplied multiplier:
#'   \deqn{
#'   \Delta_{i a f} = b_{if} \times m_{a f},
#'   }
#'   where \eqn{m_{a f}} is the multiplier associated with action \eqn{a} and
#'   feature \eqn{f}. This specification is expanded over all feasible
#'   \code{(pu, action)} pairs.
#'
#'   \item A \code{data.frame(pu, action, feature, ...)} giving explicit effects
#'   for individual triples. The table may contain:
#'   \itemize{
#'     \item \code{delta} or \code{effect}: interpreted as signed net changes,
#'     \item \code{after}: interpreted as after-action amounts if
#'     \code{effect_type = "after"},
#'     \item \code{benefit} and/or \code{loss}: explicit non-negative split
#'     components,
#'     \item legacy signed \code{benefit} without \code{loss}: interpreted as a
#'     signed net effect for backwards compatibility.
#'   }
#'
#'   \item A named list of \code{terra::SpatRaster} objects, one per action. In
#'   this case, names must match action ids, and each raster must contain one
#'   layer per feature. Raster values are aggregated to planning-unit level
#'   using \code{effect_aggregation}.
#' }
#'
#' \strong{Interpretation of \code{effect_type}}
#'
#' If \code{effect_type = "delta"}, supplied values are interpreted as net
#' changes directly.
#'
#' If \code{effect_type = "after"}, supplied values are interpreted as
#' after-action amounts and converted internally to net effects using:
#'
#' \deqn{
#' \Delta_{i a f} = \mathrm{after}_{i a f} - b_{if}.
#' }
#'
#' Missing baseline values are treated as zero.
#'
#' \strong{Feasibility and locked-out decisions}
#'
#' Effects are only retained for feasible \code{(pu, action)} pairs listed in
#' \code{x$data$dist_actions}. Thus, \code{add_actions()} must be called first.
#' Pairs marked as locked out (\code{status == 3}) are removed before storing
#' the final effects table.
#'
#' \strong{Duplicate rows and semantic validation}
#'
#' If multiple rows are supplied for the same \code{(pu, action, feature)}
#' triple, they are aggregated by summing \code{benefit} and \code{loss}
#' separately. The resulting triple must still respect the package semantics,
#' namely that both components cannot be strictly positive simultaneously.
#' Inputs violating this rule are rejected.
#'
#' \strong{Component selection}
#'
#' After canonicalization and validation, rows can be restricted to:
#' \itemize{
#'   \item \code{component = "any"}: keep all non-zero effects,
#'   \item \code{component = "benefit"}: keep only rows with \code{benefit > 0},
#'   \item \code{component = "loss"}: keep only rows with \code{loss > 0}.
#' }
#'
#' Rows with \code{benefit = 0} and \code{loss = 0} are always removed.
#'
#' \strong{Raster handling}
#'
#' When effects are supplied as rasters, they are automatically aligned to the
#' planning-unit raster or geometry when needed before extraction or zonal
#' aggregation.
#'
#' \strong{Stored output}
#'
#' The resulting table \code{x$data$dist_effects} contains user-facing ids,
#' internal integer ids, and optional labels for actions and features. Metadata
#' describing the stored representation and input interpretation are written to
#' \code{x$data$effects_meta}.
#'
#' @param x A \code{Problem} object created with \code{\link{input_data}}. It
#'   must already contain \code{x$data$dist_actions}; run
#'   \code{\link{add_actions}} first.
#'
#' @param effects Effect specification. One of:
#' \itemize{
#'   \item \code{NULL}, to store an empty effects table,
#'   \item a \code{data.frame(action, feature, multiplier)},
#'   \item a \code{data.frame(pu, action, feature, ...)} with explicit effects,
#'   \item a named list of \code{terra::SpatRaster} objects, one per action.
#' }
#'
#' @param effect_type Character string indicating how supplied effect values are
#'   interpreted. Must be one of:
#'   \itemize{
#'     \item \code{"delta"}: values represent signed net changes,
#'     \item \code{"after"}: values represent after-action amounts and are
#'     converted to net changes relative to baseline feature amounts.
#'   }
#'
#' @param effect_aggregation Character string giving the aggregation used when
#'   converting raster values to planning-unit level. Must be one of
#'   \code{"sum"} or \code{"mean"}.
#'
#' @param component Character string controlling which component of the
#'   canonical effects table is retained. Must be one of:
#'   \itemize{
#'     \item \code{"any"}: keep all non-zero rows,
#'     \item \code{"benefit"}: keep only rows with \code{benefit > 0},
#'     \item \code{"loss"}: keep only rows with \code{loss > 0}.
#'   }
#'
#' @return An updated \code{Problem} object with:
#' \describe{
#'   \item{\code{x$data$dist_effects}}{A canonical effects table with columns
#'   \code{pu}, \code{action}, \code{feature}, \code{benefit}, \code{loss},
#'   \code{internal_pu}, \code{internal_action}, \code{internal_feature}, and
#'   optional labels such as \code{feature_name} and \code{action_name}.}
#'   \item{\code{x$data$effects_meta}}{Metadata describing how effects were
#'   interpreted and stored.}
#' }
#'
#' @examples
#' # ------------------------------------------------------
#' # Minimal problem with actions
#' # ------------------------------------------------------
#' pu <- data.frame(
#'   id = 1:3,
#'   cost = c(1, 2, 3)
#' )
#'
#' features <- data.frame(
#'   id = 1:2,
#'   name = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3),
#'   feature = c(1, 2, 1, 2),
#'   amount = c(10, 5, 8, 4)
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
#' p <- add_actions(
#'   x = p,
#'   actions = actions,
#'   cost = c(conservation = 2, restoration = 4)
#' )
#'
#' print(p)
#'
#' # ------------------------------------------------------
#' # 1) Empty effects
#' # ------------------------------------------------------
#' p0 <- add_effects(p, effects = NULL)
#' p0$data$dist_effects
#'
#' # ------------------------------------------------------
#' # 2) Multipliers by action and feature
#' # delta = baseline amount * multiplier
#' # ------------------------------------------------------
#' mult <- data.frame(
#'   action = c("conservation", "conservation",
#'              "restoration", "restoration"),
#'   feature = c("sp1", "sp2", "sp1", "sp2"),
#'   multiplier = c(0.10, 0.00, -0.20, 0.25)
#' )
#'
#' p1 <- add_effects(
#'   x = p,
#'   effects = mult,
#'   effect_type = "delta"
#' )
#'
#' p1$data$dist_effects
#'
#' # ------------------------------------------------------
#' # 3) Explicit net effects using signed deltas
#' # ------------------------------------------------------
#' eff_delta <- data.frame(
#'   pu = c(1, 2, 3, 3),
#'   action = c("conservation", "restoration", "restoration", "conservation"),
#'   feature = c(1, 1, 2, 2),
#'   delta = c(2, -3, 1, 0.5)
#' )
#'
#' p2 <- add_effects(
#'   x = p,
#'   effects = eff_delta
#' )
#'
#' p2$data$dist_effects
#'
#' # ------------------------------------------------------
#' # 4) Explicit after-action amounts
#' # delta = after - baseline
#' # ------------------------------------------------------
#' eff_after <- data.frame(
#'   pu = c(1, 2, 3),
#'   action = c("conservation", "restoration", "restoration"),
#'   feature = c(1, 1, 2),
#'   after = c(12, 5, 6)
#' )
#'
#' p3 <- add_effects(
#'   x = p,
#'   effects = eff_after,
#'   effect_type = "after"
#' )
#'
#' p3$data$dist_effects
#'
#' # ------------------------------------------------------
#' # 5) Keep only beneficial effects
#' # ------------------------------------------------------
#' p4 <- add_effects(
#'   x = p,
#'   effects = eff_delta,
#'   component = "benefit"
#' )
#'
#' p4$data$dist_effects
#'
#' @seealso
#' \code{\link{add_actions}},
#' \code{\link{add_benefits}},
#' \code{\link{add_losses}}
#'
#' @export
add_effects <- function(
    x,
    effects = NULL,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean"),
    component = c("any", "benefit", "loss")
) {

  effect_type <- match.arg(effect_type)
  effect_aggregation <- match.arg(effect_aggregation)
  component <- match.arg(component)

  align_rasters <- TRUE
  na_to_zero <- TRUE

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a paretoscape Problem object")
  assertthat::assert_that(
    !is.null(x$data$pu), !is.null(x$data$features), !is.null(x$data$dist_features),
    msg = "x must be created with input_data()"
  )
  assertthat::assert_that(!is.null(x$data$dist_actions), msg = "No actions found. Run add_actions() first.")

  x <- .pa_clone_data(x)

  pu    <- x$data$pu
  feats <- x$data$features
  df    <- x$data$dist_features
  da    <- x$data$dist_actions
  acts  <- x$data$actions

  # required columns
  assertthat::assert_that(all(c("id", "internal_id") %in% names(pu)))
  assertthat::assert_that(all(c("id", "internal_id") %in% names(feats)))
  assertthat::assert_that(all(c("id") %in% names(acts)))
  assertthat::assert_that(all(c("pu", "feature", "amount") %in% names(df)))
  assertthat::assert_that(all(c("pu", "action", "cost") %in% names(da)))

  # defensive: enforce action internal_id
  if (!("internal_id" %in% names(acts))) {
    acts$internal_id <- seq_len(nrow(acts))
    x$data$actions <- acts
    acts <- x$data$actions
  }

  # ---- defensive coercions (consistent ids)
  pu$id    <- as.integer(pu$id)
  feats$id <- as.integer(feats$id)

  pu_ids     <- pu$id
  feat_ids   <- feats$id
  feat_names <- if ("name" %in% names(feats)) as.character(feats$name) else paste0("feature.", feat_ids)
  action_ids <- as.character(acts$id)

  # ---- helper: normalize feature column (id OR name)
  .normalize_feature <- function(feature_col, feats_df) {
    if (is.null(feature_col)) return(feature_col)

    if (is.factor(feature_col)) feature_col <- as.character(feature_col)

    if (is.character(feature_col)) {
      if (!("name" %in% names(feats_df))) {
        stop("features have no 'name' column, cannot match features by name.", call. = FALSE)
      }
      m <- match(feature_col, as.character(feats_df$name))
      bad <- unique(feature_col[is.na(m)])
      if (length(bad) > 0) {
        stop(
          "Unknown feature name(s) in effects$feature: ",
          paste0("'", bad, "'", collapse = ", "),
          ". Valid names are: ",
          paste0("'", as.character(feats_df$name), "'", collapse = ", "),
          call. = FALSE
        )
      }
      return(as.integer(feats_df$id[m]))
    }

    if (is.numeric(feature_col) || is.integer(feature_col)) {
      feature_col <- as.integer(feature_col)
      bad <- unique(feature_col[!feature_col %in% feats_df$id])
      if (length(bad) > 0) {
        stop(
          "Unknown feature id(s) in effects$feature: ",
          paste(bad, collapse = ", "),
          ". Valid ids are: ",
          paste(feats_df$id, collapse = ", "),
          call. = FALSE
        )
      }
      return(feature_col)
    }

    stop("effects$feature must be either numeric ids or character feature names.", call. = FALSE)
  }

  # ---- baseline lookup for (pu, feature) -> amount (missing treated as 0)
  df$pu      <- as.integer(df$pu)
  df$feature <- as.integer(df$feature)
  df$amount  <- as.numeric(df$amount)

  base_key <- paste(df$pu, df$feature, sep = "||")
  base_amt <- df$amount
  names(base_amt) <- base_key

  .baseline_amount <- function(pu_vec, feat_vec) {
    k <- paste(as.integer(pu_vec), as.integer(feat_vec), sep = "||")
    out <- unname(base_amt[k])
    out[is.na(out)] <- 0
    out
  }

  # ---- helper: split signed delta into benefit/loss (both >=0)
  .split_delta <- function(delta) {
    delta <- as.numeric(delta)
    delta[is.na(delta)] <- 0
    benefit <- pmax(delta, 0)
    loss    <- pmax(-delta, 0)
    list(benefit = benefit, loss = loss)
  }

  # ---- helper: validate split effects semantics
  .validate_split_effects <- function(tbl, context = "effects") {
    if (!("benefit" %in% names(tbl)) || !("loss" %in% names(tbl))) {
      stop("Internal error: .validate_split_effects() requires 'benefit' and 'loss' columns.", call. = FALSE)
    }

    tbl$benefit <- as.numeric(tbl$benefit)
    tbl$loss    <- as.numeric(tbl$loss)

    tbl$benefit[is.na(tbl$benefit)] <- 0
    tbl$loss[is.na(tbl$loss)] <- 0

    if (any(tbl$benefit < 0, na.rm = TRUE) || any(tbl$loss < 0, na.rm = TRUE)) {
      stop(
        context,
        ": 'benefit' and 'loss' must be non-negative.",
        call. = FALSE
      )
    }

    bad <- which(tbl$benefit > 0 & tbl$loss > 0)
    if (length(bad) > 0) {
      ex <- tbl[bad[1], intersect(c("pu", "action", "feature", "benefit", "loss"), names(tbl)), drop = FALSE]
      msg <- paste0(
        context,
        ": a single (pu, action, feature) effect cannot have both positive 'benefit' and positive 'loss'."
      )
      if (nrow(ex) == 1) {
        msg <- paste0(
          msg,
          " Example offending row -> pu=", ex$pu,
          ", action='", ex$action,
          "', feature=", ex$feature,
          ", benefit=", ex$benefit,
          ", loss=", ex$loss, "."
        )
      }
      stop(msg, call. = FALSE)
    }

    tbl
  }

  # always drop locked out actions
  if ("status" %in% names(da)) {
    da <- da[da$status != 3L, , drop = FALSE]
    if (nrow(da) == 0) stop("All (pu, action) pairs are locked_out (status=3).", call. = FALSE)
  }

  # helper: align raster to a template
  .align_to <- function(r, template) {
    if (!is.na(terra::crs(r)) && !is.na(terra::crs(template)) && terra::crs(r) != terra::crs(template)) {
      r <- terra::project(r, template)
    }
    if (!terra::compareGeom(r, template, stopOnError = FALSE)) {
      r <- terra::resample(r, template)
    }
    r
  }

  # helper: build effects from action rasters using zonal or extract
  .effects_from_rasters <- function(x, effects_list) {

    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Raster effects require the 'terra' package.", call. = FALSE)
    }

    has_pu_raster <- !is.null(x$data$pu_raster_id) && inherits(x$data$pu_raster_id, "SpatRaster")
    has_pu_sf     <- !is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")

    if (!has_pu_raster && !has_pu_sf) {
      stop(
        "To use raster effects, the object must contain either x$data$pu_raster_id (SpatRaster) or x$data$pu_sf (sf).",
        call. = FALSE
      )
    }

    if (is.null(names(effects_list)) || any(names(effects_list) == "")) {
      stop("If effects is a list of rasters, it must be a named list with names = action ids.", call. = FALSE)
    }
    if (!all(names(effects_list) %in% action_ids)) {
      bad <- setdiff(names(effects_list), action_ids)
      stop("effects list contains unknown action ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    # baseline dense matrix once for after->delta
    baseline_mat <- NULL
    if (identical(effect_type, "after")) {
      baseline_mat <- matrix(0, nrow = length(pu_ids), ncol = length(feat_ids))
      pu_pos <- match(df$pu, pu_ids)
      ft_pos <- match(df$feature, feat_ids)
      ok <- !(is.na(pu_pos) | is.na(ft_pos))
      if (any(ok)) {
        idx <- (ft_pos[ok] - 1L) * length(pu_ids) + pu_pos[ok]
        baseline_mat[idx] <- df$amount[ok]
      }
    }

    out_list <- vector("list", length(effects_list))
    k <- 0L

    for (a in names(effects_list)) {
      r <- effects_list[[a]]
      if (is.null(r)) next
      if (!inherits(r, "SpatRaster")) stop("effects[['", a, "']] must be a terra::SpatRaster.", call. = FALSE)

      if (terra::nlyr(r) != nrow(feats)) {
        stop(
          "effects[['", a, "']] has ", terra::nlyr(r), " layers but x$data$features has ", nrow(feats), " features. ",
          "Provide one layer per feature.",
          call. = FALSE
        )
      }

      try(names(r) <- feat_names, silent = TRUE)

      if (has_pu_raster) {
        z <- x$data$pu_raster_id
        r2 <- .align_to(r, z)
        zb <- terra::zonal(r2, z, fun = effect_aggregation, na.rm = TRUE)
        zb <- zb[match(pu_ids, zb[[1]]), , drop = FALSE]
        mat <- as.matrix(zb[, -1, drop = FALSE])
      } else {
        pu_sf <- x$data$pu_sf
        pu_v  <- terra::vect(pu_sf)
        fun <- switch(
          effect_aggregation,
          sum  = function(v) sum(v, na.rm = TRUE),
          mean = function(v) mean(v, na.rm = TRUE)
        )
        ex <- terra::extract(r, pu_v, fun = fun, na.rm = TRUE)
        ex <- ex[match(pu_ids, ex[[1]]), , drop = FALSE]
        mat <- as.matrix(ex[, -1, drop = FALSE])
      }

      # convert after->delta if needed (delta can be signed)
      if (identical(effect_type, "after")) {
        mat <- mat - baseline_mat
      }

      # flatten signed delta
      delta_vec <- as.vector(t(mat))
      delta_vec[is.na(delta_vec)] <- 0

      sp <- .split_delta(delta_vec)

      k <- k + 1L
      out_list[[k]] <- data.frame(
        pu      = rep(pu_ids, times = ncol(mat)),
        action  = rep(a,    times = length(pu_ids) * ncol(mat)),
        feature = rep(feat_ids, each = length(pu_ids)),
        benefit = sp$benefit,
        loss    = sp$loss,
        stringsAsFactors = FALSE
      )
    }

    out_list <- out_list[seq_len(k)]
    out <- dplyr::bind_rows(out_list)

    # keep only feasible (pu, action)
    out <- dplyr::inner_join(
      out,
      da[, c("pu", "action"), drop = FALSE],
      by = c("pu", "action")
    )

    out
  }

  # ---- compute effects depending on 'effects'
  if (is.list(effects) && !inherits(effects, "data.frame")) {

    base <- .effects_from_rasters(x, effects)

  } else if (is.null(effects)) {

    # recommended default: empty table
    base <- da[0, c("pu", "action"), drop = FALSE]
    base$feature <- integer(0)
    base$benefit <- numeric(0)
    base$loss <- numeric(0)

  } else if (inherits(effects, "data.frame")) {

    b <- effects
    if ("id" %in% names(b) && !("action" %in% names(b))) names(b)[names(b) == "id"] <- "action"
    if ("action" %in% names(b)) b$action <- as.character(b$action)
    if ("feature" %in% names(b)) b$feature <- .normalize_feature(b$feature, feats)

    # accept delta/effect as signed values
    if ("effect" %in% names(b) && !("delta" %in% names(b))) b$delta <- b$effect

    # Case A: (action, feature, multiplier) => signed delta = amount * multiplier
    if (all(c("action", "feature", "multiplier") %in% names(b)) &&
        !("pu" %in% names(b)) &&
        !any(c("delta","benefit","loss","after") %in% names(b))) {

      b$multiplier <- as.numeric(b$multiplier)

      assertthat::assert_that(assertthat::noNA(b$action), assertthat::noNA(b$feature), assertthat::noNA(b$multiplier))
      assertthat::assert_that(all(b$action %in% action_ids), msg = "Unknown action id(s) in effects.")
      assertthat::assert_that(all(b$feature %in% feat_ids), msg = "Unknown feature id(s) in effects.")

      df2 <- df[, c("pu", "feature", "amount"), drop = FALSE]
      tmp <- dplyr::inner_join(da[, c("pu","action"), drop = FALSE], df2, by = "pu", relationship = "many-to-many")
      if (nrow(tmp) == 0) stop("No (pu, action, feature) triples were created. Check dist_actions/dist_features.", call. = FALSE)

      tmp <- dplyr::left_join(tmp, b, by = c("action","feature"))
      tmp$multiplier[is.na(tmp$multiplier)] <- 0

      delta <- as.numeric(tmp$amount) * as.numeric(tmp$multiplier)
      sp <- .split_delta(delta)

      base <- tmp[, c("pu","action","feature")]
      base$benefit <- sp$benefit
      base$loss    <- sp$loss

    } else {

      # Case B: explicit rows (pu, action, feature, ...)
      assertthat::assert_that(all(c("pu","action","feature") %in% names(b)))

      b$pu <- as.integer(b$pu)
      b$feature <- as.integer(b$feature)

      assertthat::assert_that(assertthat::noNA(b$pu), assertthat::noNA(b$action), assertthat::noNA(b$feature))
      assertthat::assert_that(all(b$pu %in% pu_ids), msg = "Unknown pu id(s) in effects.")
      assertthat::assert_that(all(b$action %in% action_ids), msg = "Unknown action id(s) in effects.")
      assertthat::assert_that(all(b$feature %in% feat_ids), msg = "Unknown feature id(s) in effects.")

      tmp <- dplyr::inner_join(
        b,
        da[, c("pu","action"), drop = FALSE],
        by = c("pu","action")
      )
      if (nrow(tmp) == 0) stop("No rows in effects match feasible (pu, action) pairs.", call. = FALSE)

      # Input mode:
      # 1) already split benefit/loss (non-negative)
      # 2) signed delta (delta/effect/legacy benefit)
      has_any_split <- any(c("benefit","loss") %in% names(tmp))

      if (has_any_split && !("delta" %in% names(tmp)) && !("after" %in% names(tmp)) && !("effect" %in% names(tmp))) {
        # split-only path
        if (!("benefit" %in% names(tmp))) tmp$benefit <- 0
        if (!("loss" %in% names(tmp)))    tmp$loss <- 0

        tmp <- .validate_split_effects(
          tmp,
          context = "When providing explicit benefit/loss columns"
        )

        base <- tmp[, c("pu","action","feature","benefit","loss")]

      } else {
        # signed path (delta/effect/after or legacy benefit)
        if (!("delta" %in% names(tmp))) {
          if ("effect" %in% names(tmp)) tmp$delta <- tmp$effect
          else if ("after" %in% names(tmp)) tmp$delta <- tmp$after
          else if ("benefit" %in% names(tmp) && !("loss" %in% names(tmp))) tmp$delta <- tmp$benefit
          else stop("effects data.frame must include 'delta'/'effect'/'after' (signed) or 'benefit/loss' (non-negative).", call. = FALSE)
        }

        tmp$delta <- as.numeric(tmp$delta)
        tmp$delta[is.na(tmp$delta)] <- 0

        if (identical(effect_type, "after")) {
          base_amount <- .baseline_amount(tmp$pu, tmp$feature)
          tmp$delta <- tmp$delta - base_amount
        }

        sp <- .split_delta(tmp$delta)
        base <- tmp[, c("pu","action","feature")]
        base$benefit <- sp$benefit
        base$loss    <- sp$loss
      }
    }

  } else {
    stop("Unsupported type for 'effects'. Use NULL, a data.frame, or a named list of SpatRaster.", call. = FALSE)
  }

  # ---- aggregate duplicates and enforce one net effect per (pu, action, feature)
  if (nrow(base) > 0) {
    base <- stats::aggregate(
      cbind(benefit, loss) ~ pu + action + feature,
      data = base,
      FUN = sum
    )
  }

  # ---- cleanup / validation / filtering
  base$pu      <- as.integer(base$pu)
  base$feature <- as.integer(base$feature)
  base$benefit <- as.numeric(base$benefit)
  base$loss    <- as.numeric(base$loss)

  base$benefit[is.na(base$benefit)] <- 0
  base$loss[is.na(base$loss)] <- 0

  base <- .validate_split_effects(base, context = "Validated effects")

  if (identical(component, "benefit")) base <- base[base$benefit > 0, , drop = FALSE]
  if (identical(component, "loss"))    base <- base[base$loss > 0, , drop = FALSE]

  # always drop zero rows
  base <- base[!(base$benefit == 0 & base$loss == 0), , drop = FALSE]
  if (nrow(base) == 0) warning("All effect values are zero after filtering.", call. = FALSE, immediate. = TRUE)

  # ---- add internal ids
  pu_map    <- pu[, c("id", "internal_id")]
  feats_map <- feats[, c("id", "internal_id")]
  acts_map  <- x$data$actions[, c("id", "internal_id")]

  base$internal_pu      <- pu_map$internal_id[match(base$pu, pu_map$id)]
  base$internal_feature <- feats_map$internal_id[match(base$feature, feats_map$id)]
  base$internal_action  <- acts_map$internal_id[match(base$action, acts_map$id)]

  dist_effects <- base[, c("pu","action","feature","benefit","loss",
                           "internal_pu","internal_action","internal_feature"), drop = FALSE]

  dist_effects <- .pa_add_feature_labels(
    df = dist_effects,
    features_df = feats,
    feature_col = "feature",
    internal_feature_col = "internal_feature",
    out_col = "feature_name"
  )

  dist_effects <- .pa_add_action_labels(
    df = dist_effects,
    actions_df = acts,
    action_col = "action",
    internal_action_col = "internal_action",
    out_col = "action_name"
  )

  x$data$dist_effects <- dist_effects

  x$data$effects_meta <- list(
    stored_as = "benefit_loss",
    input_interpretation = effect_type,
    component = component
  )

  x
}

#' @title Add benefits
#'
#' @description
#' Convenience wrapper around \code{\link{add_effects}} that keeps only positive
#' effects, that is, rows with \code{benefit > 0}.
#'
#' This function is useful when the user wants to work only with beneficial
#' consequences of actions. Internally, it calls \code{add_effects()} with
#' \code{component = "benefit"} and stores the resulting canonical effects table
#' in \code{x$data$dist_effects}.
#'
#' For backwards compatibility, a mirror table containing only the benefit
#' component is also written to \code{x$data$dist_benefit}.
#'
#' @inheritParams add_effects
#' @param benefits Alias of \code{effects}, kept for backwards compatibility.
#'
#' @return An updated \code{Problem} object with:
#' \describe{
#'   \item{\code{x$data$dist_effects}}{The canonical filtered effects table,
#'   containing only rows with \code{benefit > 0}.}
#'   \item{\code{x$data$dist_benefit}}{A backwards-compatible table containing
#'   only the benefit component.}
#' }
#'
#' @examples
#' pu <- data.frame(id = 1:2, cost = c(1, 2))
#' features <- data.frame(id = 1, name = "sp1")
#' dist_features <- data.frame(pu = 1:2, feature = 1, amount = c(5, 10))
#'
#' p <- input_data(pu = pu, features = features, dist_features = dist_features)
#' p <- add_actions(p, data.frame(id = "restoration"))
#'
#' eff <- data.frame(
#'   pu = c(1, 2),
#'   action = c("restoration", "restoration"),
#'   feature = c(1, 1),
#'   delta = c(2, -1)
#' )
#'
#' p <- add_benefits(p, benefits = eff)
#' p$data$dist_benefit
#'
#' @seealso
#' \code{\link{add_effects}},
#' \code{\link{add_losses}}
#'
#' @export
add_benefits <- function(
    x,
    benefits = NULL,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean")
) {
  effects <- benefits

  x <- add_effects(
    x = x,
    effects = effects,
    effect_type = effect_type,
    effect_aggregation = effect_aggregation,
    component = "benefit"
  )

  # backward-compatible mirror: dist_benefit (ONLY benefit rows/col)
  if (!is.null(x$data$dist_effects) && inherits(x$data$dist_effects, "data.frame")) {
    db <- x$data$dist_effects
    if ("loss" %in% names(db)) db$loss <- NULL
    x$data$dist_benefit <- db
  } else {
    x$data$dist_benefit <- x$data$dist_effects
  }

  x
}

#' @title Add losses
#'
#' @description
#' Convenience wrapper around \code{\link{add_effects}} that keeps only negative
#' effects, represented by rows with \code{loss > 0}.
#'
#' This function is useful when the user wants to work only with the damaging
#' consequences of actions. Internally, it calls \code{add_effects()} with
#' \code{component = "loss"} and stores the resulting canonical effects table in
#' \code{x$data$dist_effects}.
#'
#' In addition, a mirror table containing only the loss component is stored in
#' \code{x$data$dist_loss}.
#'
#' @inheritParams add_effects
#' @param losses Alias of \code{effects}, used for symmetry with
#'   \code{add_benefits()}.
#'
#' @return An updated \code{Problem} object with:
#' \describe{
#'   \item{\code{x$data$dist_effects}}{The canonical filtered effects table,
#'   containing only rows with \code{loss > 0}.}
#'   \item{\code{x$data$dist_loss}}{A convenience table containing only the loss
#'   component.}
#'   \item{\code{x$data$losses_meta}}{Metadata for the stored loss table.}
#' }
#'
#' @examples
#' pu <- data.frame(id = 1:2, cost = c(1, 2))
#' features <- data.frame(id = 1, name = "sp1")
#' dist_features <- data.frame(pu = 1:2, feature = 1, amount = c(5, 10))
#'
#' p <- input_data(pu = pu, features = features, dist_features = dist_features)
#' p <- add_actions(p, data.frame(id = "harvest"))
#'
#' eff <- data.frame(
#'   pu = c(1, 2),
#'   action = c("harvest", "harvest"),
#'   feature = c(1, 1),
#'   delta = c(2, -4)
#' )
#'
#' p <- add_losses(p, losses = eff)
#' p$data$dist_loss
#'
#' @seealso
#' \code{\link{add_effects}},
#' \code{\link{add_benefits}}
#'
#' @export
add_losses <- function(
    x,
    losses = NULL,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean")
) {
  effects <- losses

  x <- add_effects(
    x = x,
    effects = effects,
    effect_type = effect_type,
    effect_aggregation = effect_aggregation,
    component = "loss"
  )

  if (!is.null(x$data$dist_effects) && inherits(x$data$dist_effects, "data.frame")) {
    dl <- x$data$dist_effects
    if ("benefit" %in% names(dl)) dl$benefit <- NULL
    x$data$dist_loss <- dl
    x$data$losses_meta <- list(
      stored_as = "loss",
      input_interpretation = x$data$effects_meta$input_interpretation
    )
  }

  x
}
