#' @include internal.R
#'
#' @title Add effects (benefit/loss) to a planning problem
#'
#' @description
#' Create an effects table (\code{dist_effects}) describing the marginal change caused by
#' each feasible (pu, action) pair on each feature, split into two **non-negative**
#' components:
#' \itemize{
#' \item \code{benefit} \eqn{\ge 0}: positive delta (improvement)
#' \item \code{loss} \eqn{\ge 0}: negative delta magnitude (damage), i.e. \code{loss = -min(delta,0)}
#' }
#'
#' Internally, the model can still work with signed effects (e.g., net effect = benefit - loss),
#' but \code{dist_effects} stores only non-negative columns to avoid ambiguity.
#'
#' If you provide "after-action" amounts, they are converted to signed deltas:
#' \deqn{delta = after - baseline}
#' where baseline is \code{x$data$dist_features$amount} (missing baseline treated as 0).
#' Then the delta is split into \code{benefit/loss}.
#'
#' The function supports:
#' \enumerate{
#' \item Tabular specifications (NULL / multipliers / explicit table), and
#' \item Spatial rasters per action (recommended for spatial workflows).
#' }
#'
#' It produces \code{x$data$dist_effects} with columns:
#' \code{pu, action, feature, benefit, loss, internal_pu, internal_action, internal_feature}.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#'
#' @param effects Specification of effects. One of:
#' \itemize{
#' \item NULL: store an empty table (recommended default; no info).
#' \item A data.frame(action, feature, multiplier):
#' signed \code{delta = amount * multiplier} (multiplier can be negative).
#' feature can be numeric ids OR character feature names (matching \code{x$data$features$name}).
#' \item A data.frame(pu, action, feature, ...):
#' with either:
#' \itemize{
#'   \item \code{delta} (signed), or alias \code{effect} (signed), or legacy \code{benefit} (signed), or
#'   \item \code{after} (after-action amount; use \code{effect_type="after"}), or
#'   \item already split columns \code{benefit} and/or \code{loss} (both non-negative).
#' }
#' \item A named list of terra::SpatRaster objects:
#' names = action ids; each raster has one layer per feature. Values are aggregated by PU.
#' If \code{effect_type="after"}, raster values are treated as after-action amounts and
#' converted to deltas; otherwise treated as signed deltas.
#' }
#'
#' @param effect_type character. Interpretation of provided values when \code{effects} is an
#' explicit table or a list of rasters:
#' \itemize{
#' \item "delta" (default): values represent signed deltas.
#' \item "after": values represent after-action amounts (converted to deltas).
#' }
#'
#' @param effect_aggregation Aggregation used to compute PU-level values from rasters
#' (default: "sum").
#' @param align_rasters Logical. If TRUE, attempt to align effect rasters to the PU raster
#' grid before zonal operations (default TRUE).
#'
#' @param keep_zero Logical. Keep rows with \code{benefit==0 AND loss==0}? Default FALSE (drops them).
#' @param drop_locked_out Logical. If TRUE, drop rows for (pu, action) with status == 3 in dist_actions.
#' @param na_to_zero Logical. If TRUE, treat missing values as 0.
#'
#' @param filter Character. Filter rows by non-zero component:
#' \itemize{
#' \item "any": keep all rows (default)
#' \item "benefit": keep only rows with \code{benefit > 0}
#' \item "loss": keep only rows with \code{loss > 0}
#' }
#'
#' @return The updated [data-class] object.
#' @export
add_effects <- function(
    x,
    effects = NULL,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean"),
    align_rasters = TRUE,
    keep_zero = FALSE,
    drop_locked_out = TRUE,
    na_to_zero = TRUE,
    filter = c("any", "benefit", "loss")
) {

  effect_type <- match.arg(effect_type)
  effect_aggregation <- match.arg(effect_aggregation)
  filter <- match.arg(filter)

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a prioriactions Data object")
  assertthat::assert_that(
    !is.null(x$data$pu), !is.null(x$data$features), !is.null(x$data$dist_features),
    msg = "x must be created with inputData()/inputDataSpatial()"
  )
  assertthat::assert_that(!is.null(x$data$dist_actions), msg = "No actions found. Run add_actions() first.")

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
    if (na_to_zero) delta[is.na(delta)] <- 0
    benefit <- pmax(delta, 0)
    loss    <- pmax(-delta, 0)
    list(benefit = benefit, loss = loss)
  }

  # drop locked out actions if requested
  if (drop_locked_out && "status" %in% names(da)) {
    da <- da[da$status != 3L, , drop = FALSE]
    if (nrow(da) == 0) stop("All (pu, action) pairs are locked_out (status=3).", call. = FALSE)
  }

  # helper: align raster to a template
  .align_to <- function(r, template) {
    if (!isTRUE(align_rasters)) return(r)
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
      if (na_to_zero) delta_vec[is.na(delta_vec)] <- 0

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
      tmp <- dplyr::inner_join(da[, c("pu","action"), drop = FALSE], df2, by = "pu")
      if (nrow(tmp) == 0) stop("No (pu, action, feature) triples were created. Check dist_actions/dist_features.", call. = FALSE)

      tmp <- dplyr::left_join(tmp, b, by = c("action","feature"))
      if (na_to_zero) tmp$multiplier[is.na(tmp$multiplier)] <- 0

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
      has_delta_like <- any(c("delta","effect","after") %in% names(tmp)) || ("benefit" %in% names(tmp) && !has_any_split)

      if (has_any_split && !("delta" %in% names(tmp)) && !("after" %in% names(tmp)) && !("effect" %in% names(tmp))) {
        # split-only path
        if (!("benefit" %in% names(tmp))) tmp$benefit <- 0
        if (!("loss" %in% names(tmp)))    tmp$loss <- 0

        tmp$benefit <- as.numeric(tmp$benefit)
        tmp$loss    <- as.numeric(tmp$loss)
        if (na_to_zero) {
          tmp$benefit[is.na(tmp$benefit)] <- 0
          tmp$loss[is.na(tmp$loss)] <- 0
        }
        if (any(tmp$benefit < 0, na.rm = TRUE) || any(tmp$loss < 0, na.rm = TRUE)) {
          stop("When providing benefit/loss columns, both must be non-negative.", call. = FALSE)
        }

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
        if (na_to_zero) tmp$delta[is.na(tmp$delta)] <- 0

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

  # ---- cleanup / filtering
  base$pu      <- as.integer(base$pu)
  base$feature <- as.integer(base$feature)
  base$benefit <- as.numeric(base$benefit)
  base$loss    <- as.numeric(base$loss)

  if (na_to_zero) {
    base$benefit[is.na(base$benefit)] <- 0
    base$loss[is.na(base$loss)] <- 0
  }

  if (identical(filter, "benefit")) base <- base[base$benefit > 0, , drop = FALSE]
  if (identical(filter, "loss"))    base <- base[base$loss > 0, , drop = FALSE]

  if (!keep_zero) base <- base[!(base$benefit == 0 & base$loss == 0), , drop = FALSE]
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

  x$data$dist_effects <- dist_effects
  x$data$effects_meta <- list(
    stored_as = "benefit_loss",
    input_interpretation = effect_type,
    filter = filter
  )

  x
}

#' @title Add benefits (positive deltas)
#' @description Convenience wrapper around add_effects() keeping only rows with benefit > 0.
#' @inheritParams add_effects
#' @param benefits Alias of `effects` for backwards compatibility.
#' @export
add_benefits <- function(
    x,
    benefits = NULL,
    ...,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean"),
    align_rasters = TRUE,
    keep_zero = FALSE,
    drop_locked_out = TRUE,
    na_to_zero = TRUE
) {
  effects <- benefits

  x <- add_effects(
    x = x,
    effects = effects,
    effect_type = effect_type,
    effect_aggregation = effect_aggregation,
    align_rasters = align_rasters,
    keep_zero = keep_zero,
    drop_locked_out = drop_locked_out,
    na_to_zero = na_to_zero,
    filter = "benefit",
    ...
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

#' @title Add losses (negative effects magnitude)
#'
#' @description
#' Convenience wrapper around [add_effects()] that keeps only rows with \code{loss > 0}.
#' Also writes \code{x$data$dist_loss} with column \code{loss}.
#'
#' @inheritParams add_effects
#' @return Updated [data-class] object.
#' @export
add_losses <- function(
    x,
    losses = NULL,
    ...,
    effect_type = c("delta", "after"),
    effect_aggregation = c("sum", "mean"),
    align_rasters = TRUE,
    keep_zero = FALSE,
    drop_locked_out = TRUE,
    na_to_zero = TRUE
) {
  effects <- losses

  x <- add_effects(
    x = x,
    effects = effects,
    effect_type = effect_type,
    effect_aggregation = effect_aggregation,
    align_rasters = align_rasters,
    keep_zero = keep_zero,
    drop_locked_out = drop_locked_out,
    na_to_zero = na_to_zero,
    filter = "loss",
    ...
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
