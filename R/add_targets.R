#' @include internal.R
NULL

# -------------------------------------------------------------------------
# Targets API for prioriactions (Data-based workflow)
# -------------------------------------------------------------------------

#' @title Add conservation targets (absolute)
#' @description Adds absolute conservation targets for each feature.
#' @inheritParams add_conservation_targets_relative
#' @param targets See **Targets format** in Details.
#' @param overwrite If TRUE, replace existing conservation targets for the same features.
#' @param label Optional label stored with the targets.
#' @return Updated `Data` object.
#' @export
add_conservation_targets_absolute <- function(x, targets, overwrite = FALSE, label = NULL) {
  stopifnot(inherits(x, "Data"))
  dt <- .pa_parse_targets(x, targets)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "conservation",
    sense        = "ge",
    target_unit  = "absolute",
    target_raw   = as.numeric(dt$target_raw),
    basis_total  = NA_real_,
    target_value = as.numeric(dt$target_raw),
    label        = if (is.null(label)) NA_character_ else as.character(label),
    created_at   = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  .pa_store_targets(x, out, overwrite = overwrite)
}


#' @title Add conservation targets (relative to baseline)
#'
#' @description
#' Adds relative conservation targets specified as proportions in [0,1] of the
#' total baseline representation of each feature in the study area.
#'
#' @param x A `Data` object.
#' @param targets Target specification (proportions).
#' @param overwrite Logical. If TRUE, replace existing conservation targets for the same features.
#' @param label Optional label.
#' @return Updated `Data` object.
#' @export
add_conservation_targets_relative <- function(x, targets, overwrite = FALSE, label = NULL) {
  stopifnot(inherits(x, "Data"))
  dt <- .pa_parse_targets(x, targets)

  rel <- as.numeric(dt$target_raw)
  if (any(rel < 0 | rel > 1, na.rm = TRUE)) {
    stop("Relative conservation targets must be between 0 and 1.", call. = FALSE)
  }

  basis <- .pa_feature_totals(x) # named by feature id
  basis_v <- basis[as.character(dt$feature)]
  basis_v[is.na(basis_v)] <- 0

  abs_target <- rel * as.numeric(basis_v)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "conservation",
    sense        = "ge",
    target_unit  = "relative_baseline",
    target_raw   = rel,
    basis_total  = as.numeric(basis_v),
    target_value = as.numeric(abs_target),
    label        = if (is.null(label)) NA_character_ else as.character(label),
    created_at   = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  .pa_store_targets(x, out, overwrite = overwrite)
}


#' @title Add recovery targets (absolute)
#'
#' @description Adds absolute recovery targets (absolute amount of recovery gain required).
#' @param x A `Data` object.
#' @param targets Target specification.
#' @param overwrite Logical. If TRUE, replace existing recovery targets for the same features.
#' @param label Optional label.
#' @return Updated `Data` object.
#' @export
add_recovery_targets_absolute <- function(x, targets, overwrite = FALSE, label = NULL) {
  stopifnot(inherits(x, "Data"))
  dt <- .pa_parse_targets(x, targets)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "recovery",
    sense        = "ge",
    target_unit  = "absolute",
    target_raw   = as.numeric(dt$target_raw),
    basis_total  = NA_real_,
    target_value = as.numeric(dt$target_raw),
    label        = if (is.null(label)) NA_character_ else as.character(label),
    created_at   = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  .pa_store_targets(x, out, overwrite = overwrite)
}


#' @title Add recovery targets (relative)
#'
#' @description
#' Adds relative recovery targets specified as proportions in [0,1] of a chosen basis.
#' By default, basis is the potential maximum improvement per feature from dist_benefit.
#'
#' @param x A `Data` object.
#' @param targets Target specification (proportions).
#' @param relative_basis "potential" (default) or "baseline".
#' @param overwrite Logical. If TRUE, replace existing recovery targets for the same features.
#' @param label Optional label.
#' @return Updated `Data` object.
#' @export
add_recovery_targets_relative <- function(x,
                                          targets,
                                          relative_basis = c("potential", "baseline"),
                                          overwrite = FALSE,
                                          label = NULL) {
  stopifnot(inherits(x, "Data"))
  relative_basis <- match.arg(relative_basis)
  dt <- .pa_parse_targets(x, targets)

  rel <- as.numeric(dt$target_raw)
  if (any(rel < 0 | rel > 1, na.rm = TRUE)) {
    stop("Relative recovery targets must be between 0 and 1.", call. = FALSE)
  }

  basis <- switch(
    relative_basis,
    potential = .pa_feature_potential(x),
    baseline  = .pa_feature_totals(x)
  )

  basis_v <- basis[as.character(dt$feature)]
  basis_v[is.na(basis_v)] <- 0
  abs_target <- rel * as.numeric(basis_v)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "recovery",
    sense        = "ge",
    target_unit  = paste0("relative_", relative_basis),
    target_raw   = rel,
    basis_total  = as.numeric(basis_v),
    target_value = as.numeric(abs_target),
    label        = if (is.null(label)) NA_character_ else as.character(label),
    created_at   = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  .pa_store_targets(x, out, overwrite = overwrite)
}

#' @title Add mixed total targets (absolute)
#'
#' @description
#' Adds **absolute mixed-total targets** per feature. A mixed-total target enforces that
#' baseline + action deltas together reach a single threshold:
#' \deqn{\sum_i z_{is} r_{is} + \sum_{i,a} x_{ia}\Delta_{ias} \ge T^{mix}_s}
#'
#' This is **different** from setting separate conservation + recovery targets for the same feature.
#' Therefore, mixed targets are **mutually exclusive** with conservation/recovery targets for the same feature.
#'
#' Internally, targets are stored in `x$data$targets` with `type="mixed_total"`.
#'
#' @param x A `Data` object.
#' @param targets Target specification (absolute). See Targets format in
#'   `add_conservation_targets_absolute()`.
#' @param overwrite Logical. If `TRUE`, replaces existing mixed_total targets for the same features
#'   (but still errors if conservation/recovery targets exist for those features).
#' @param label Optional label stored with targets.
#'
#' @return Updated `Data` object.
#' @export
add_mixed_targets_total_absolute <- function(x, targets, overwrite = FALSE, label = NULL) {
  stopifnot(inherits(x, "Data"))
  dt <- .pa_parse_targets(x, targets)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "mixed_total",
    sense        = "ge",
    target_unit  = "absolute",
    target_raw   = as.numeric(dt$target_raw),
    basis_total  = NA_real_,
    target_value = as.numeric(dt$target_raw),
    label        = if (is.null(label)) NA_character_ else as.character(label),
    created_at   = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  .pa_store_targets(x, out, overwrite = overwrite)
}

