#' @include internal.R
#'
#' @title Add effects (benefit/loss) to a planning problem
#'
#' @description
#' Define the ecological (or any feature-based) effects of implementing actions in planning units.
#' Effects are stored in \code{x$data$dist_effects} as two non-negative components per
#' feasible \code{(pu, action, feature)} triple:
#' \itemize{
#'   \item \code{benefit} \eqn{\ge 0}: positive change (improvement),
#'   \item \code{loss} \eqn{\ge 0}: magnitude of negative change (damage), computed as
#'   \code{loss = -min(delta, 0)}.
#' }
#'
#' @details
#' Internally, effects may originate from signed values (deltas) or from after-action amounts.
#' Regardless of input, \code{dist_effects} always stores \code{benefit} and \code{loss} as
#' non-negative values to avoid ambiguity and to support models that separately account for
#' gains and damages.
#'
#' \strong{Baseline and after-action amounts.}
#' If \code{effect_type = "after"}, provided values are interpreted as after-action amounts and
#' converted to signed deltas using the baseline amounts in \code{x$data$dist_features$amount}:
#' \deqn{\mathrm{delta} = \mathrm{after} - \mathrm{baseline}.}
#' Missing baseline values are treated as 0.
#'
#' \strong{Supported effect specifications.}
#' \enumerate{
#'   \item \emph{NULL}: store an empty effects table (recommended default when effects are not available yet).
#'   \item \emph{Multipliers}: a \code{data.frame(action, feature, multiplier)} that applies a signed multiplier to
#'   baseline amounts: \eqn{\mathrm{delta} = \mathrm{amount} \times \mathrm{multiplier}}.
#'   \item \emph{Explicit rows}: a \code{data.frame(pu, action, feature, ...)} providing either signed deltas
#'   (\code{delta} or \code{effect}, or legacy signed \code{benefit}), after-action amounts (\code{after}),
#'   or already split non-negative \code{benefit}/\code{loss}.
#'   \item \emph{Rasters per action}: a named list of \code{terra::SpatRaster} objects (names are action ids),
#'   each with one layer per feature. Raster values are aggregated by planning unit using
#'   \code{effect_aggregation} and then interpreted as deltas or after-action amounts depending on \code{effect_type}.
#' }
#'
#' \strong{Feasibility and locks.}
#' Effects are retained only for feasible \code{(pu, action)} pairs present in \code{x$data$dist_actions}.
#' If \code{drop_locked_out = TRUE} and \code{x$data$dist_actions$status} exists, pairs with \code{status == 3}
#' are excluded before effects are processed.
#'
#' \strong{Filtering.}
#' You can keep only beneficial effects (\code{benefit > 0}) or only losses (\code{loss > 0}) using \code{filter}.
#' By default, rows with both \code{benefit == 0} and \code{loss == 0} are dropped unless \code{keep_zero = TRUE}.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#'   Must contain \code{x$data$dist_actions} (run \code{\link{add_actions}} first).
#' @param effects Effect specification. One of:
#' \itemize{
#'   \item \code{NULL}: store an empty effects table.
#'   \item \code{data.frame(action, feature, multiplier)}: apply signed multipliers to baseline amounts.
#'     \code{feature} may be feature ids or feature names (matching \code{x$data$features$name}).
#'   \item \code{data.frame(pu, action, feature, ...)}: explicit effects with one of:
#'     \itemize{
#'       \item \code{delta} (signed) or \code{effect} (signed),
#'       \item \code{after} (after-action amount; set \code{effect_type = "after"}),
#'       \item \code{benefit} and/or \code{loss} (both non-negative; missing component treated as 0),
#'       \item legacy signed \code{benefit} without \code{loss} (treated as signed delta).
#'     }
#'   \item A named list of \code{terra::SpatRaster} objects: names = action ids; one layer per feature.
#' }
#' @param effect_type Character. How to interpret provided values for explicit tables or raster lists:
#'   \itemize{
#'     \item \code{"delta"}: values represent signed deltas (default),
#'     \item \code{"after"}: values represent after-action amounts (converted to deltas using baseline).
#'   }
#' @param effect_aggregation Character. Aggregation used to compute PU-level values from rasters.
#'   One of \code{"sum"} or \code{"mean"}.
#' @param align_rasters Logical. If \code{TRUE}, attempt to align effect rasters to the PU raster grid
#'   before zonal operations (default \code{TRUE}).
#' @param keep_zero Logical. If \code{TRUE}, keep rows where \code{benefit == 0} and \code{loss == 0}.
#'   Default \code{FALSE}.
#' @param drop_locked_out Logical. If \code{TRUE}, drop rows for \code{(pu, action)} pairs with
#'   \code{status == 3} in \code{x$data$dist_actions} (if the column exists). Default \code{TRUE}.
#' @param na_to_zero Logical. If \code{TRUE}, treat missing values as 0 when computing benefit/loss.
#'   Default \code{TRUE}.
#' @param filter Character. Filter rows by non-zero component:
#'   \itemize{
#'     \item \code{"any"}: keep both benefit and loss rows (default),
#'     \item \code{"benefit"}: keep only rows with \code{benefit > 0},
#'     \item \code{"loss"}: keep only rows with \code{loss > 0}.
#'   }
#'
#' @return The updated \code{Data} object with \code{x$data$dist_effects} created/updated, and
#'   metadata stored in \code{x$data$effects_meta}.
#'
#' @examples
#' \dontrun{
#' # 1) Empty effects (default)
#' p <- add_effects(p, effects = NULL)
#'
#' # 2) Multipliers (action x feature): delta = amount * multiplier
#' mult <- data.frame(
#'   action = c("harvest", "harvest", "restoration"),
#'   feature = c("sp1", "sp2", "sp1"),      # feature names (requires x$data$features$name)
#'   multiplier = c(-0.2, -0.1, 0.3)
#' )
#' p <- add_effects(p, effects = mult, effect_type = "delta")
#'
#' # 3) Explicit deltas by (pu, action, feature)
#' eff <- data.frame(
#'   pu = c(1, 1, 2),
#'   action = c("harvest", "harvest", "restoration"),
#'   feature = c(1, 2, 1),
#'   delta = c(-0.5, 0.2, 1.0)
#' )
#' p <- add_effects(p, effects = eff)
#'
#' # 4) After-action amounts (converted to delta = after - baseline)
#' after_tbl <- transform(eff, after = delta) # example only; typically after is an absolute amount
#' p <- add_effects(p, effects = after_tbl, effect_type = "after")
#'
#' # 5) Raster effects per action (one layer per feature)
#' # effects_rasters <- list(harvest = r_harv, restoration = r_rest) # terra::SpatRaster
#' # p <- add_effects(p, effects = effects_rasters, effect_type = "delta", effect_aggregation = "sum")
#'
#' # Keep only beneficial effects
#' p <- add_effects(p, effects = eff, filter = "benefit")
#' }
#'
#' @seealso \code{\link{add_benefits}}, \code{\link{add_losses}}
#'
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

#' @title Add benefits (positive effects)
#'
#' @description
#' Convenience wrapper around \code{\link{add_effects}} that keeps only rows with
#' \code{benefit > 0} (i.e., positive changes). For backwards compatibility, the argument
#' \code{benefits} is an alias of \code{effects}.
#'
#' In addition to writing \code{x$data$dist_effects}, this function also writes a
#' backward-compatible table \code{x$data$dist_benefit} containing only the benefit component.
#'
#' @inheritParams add_effects
#' @param benefits Alias of \code{effects} for backwards compatibility.
#'
#' @return The updated \code{Data} object.
#'
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
#' Convenience wrapper around \code{\link{add_effects}} that keeps only rows with
#' \code{loss > 0} (i.e., the magnitude of negative changes). Also writes
#' \code{x$data$dist_loss} containing only the loss component.
#'
#' @inheritParams add_effects
#'
#' @return The updated \code{Data} object.
#'
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
