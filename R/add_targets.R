#' @include internal.R
NULL

#' Targets API
#'
#' @description
#' Functions to define feature-level targets for a planning problem. Targets are stored in
#' \code{x$data$targets} and later translated into mathematical constraints when the optimization
#' model is built.
#'
#' The API supports three target types:
#' \describe{
#'   \item{\strong{Conservation}}{Ensure baseline representation of selected planning units reaches a threshold.}
#'   \item{\strong{Recovery}}{Ensure action-driven improvements (deltas) reach a threshold.}
#'   \item{\strong{Mixed total}}{Ensure baseline + action deltas together reach a single threshold.}
#' }
#'
#' Each target is stored as a row with at least:
#' \code{feature}, \code{type}, \code{sense}, \code{target_unit}, \code{target_raw}, \code{basis_total},
#' \code{target_value}, and optional metadata such as \code{label} and \code{created_at}.
#'
#' @details
#' \strong{Targets format.}
#' The \code{targets} argument can be provided in multiple equivalent ways (as implemented by
#' \code{.pa_parse_targets()}), typically including:
#' \itemize{
#'   \item a single numeric value recycled to all features,
#'   \item a numeric vector aligned to the feature order,
#'   \item a named numeric vector where names identify features,
#'   \item a \code{data.frame} with feature identifiers and target values.
#' }
#' Features may be identified by numeric \code{id} and/or by a feature name column if supported by
#' \code{.pa_parse_targets()}.
#'
#' \strong{Absolute vs relative targets.}
#' Absolute targets set \code{target_value} directly from the provided \code{targets}.
#' Relative targets treat \code{targets} as proportions in \eqn{[0,1]} and convert them to absolute
#' thresholds by multiplying by a feature-specific basis:
#' \itemize{
#'   \item Conservation relative targets use baseline totals (via \code{.pa_feature_totals()}).
#'   \item Recovery relative targets use either potential improvement (via \code{.pa_feature_potential()})
#'   or baseline totals, depending on \code{relative_basis}.
#'   \item Mixed-total relative targets use baseline totals (via \code{.pa_feature_totals()}).
#' }
#' The chosen basis is stored in \code{basis_total}, and the resulting absolute threshold is stored in
#' \code{target_value}.
#'
#' \strong{Mutual exclusivity for mixed-total targets.}
#' Mixed-total targets represent a single threshold on \emph{baseline + deltas} for a feature:
#' \deqn{\sum_i z_{is} r_{is} + \sum_{i,a} x_{ia}\Delta_{ias} \ge T^{mix}_s.}
#' Because this differs from enforcing separate conservation and recovery targets, mixed-total targets
#' are intended to be mutually exclusive with conservation/recovery targets for the same feature.
#' (Enforcement is handled by \code{.pa_store_targets()}).
#'
#' \strong{Overwrite behavior.}
#' When \code{overwrite=TRUE}, existing targets of the same type for the same feature(s) can be replaced.
#' When \code{overwrite=FALSE}, adding targets for features that already have targets of that type will
#' typically error (exact behavior is handled by \code{.pa_store_targets()}).
#'
#' @name targets
#' @keywords internal
NULL

#' @title Add conservation targets (absolute)
#'
#' @description
#' Adds absolute conservation targets per feature. The provided values are treated as absolute
#' thresholds in the same units as the feature amounts stored in \code{x$data$dist_features$amount}.
#'
#' @param x A [data-class] object.
#' @param targets Target specification. See \strong{Targets format} in \code{\link{targets}} details.
#' @param overwrite Logical. If \code{TRUE}, replace existing conservation targets for the same features.
#' @param label Optional character label stored with the targets (useful for reporting).
#'
#' @return Updated [data-class] object.
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
#' Adds relative conservation targets as proportions in \eqn{[0,1]} of the baseline total
#' representation of each feature in the study area. Baseline totals are computed from the
#' input data (via \code{.pa_feature_totals()}).
#'
#' @param x A [data-class] object.
#' @param targets Target specification (proportions in \eqn{[0,1]}).
#' @param overwrite Logical. If \code{TRUE}, replace existing conservation targets for the same features.
#' @param label Optional character label stored with the targets.
#'
#' @return Updated [data-class] object.
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


#'
#' @title Add recovery targets (absolute)
#'
#' @description
#' Adds absolute recovery targets per feature. Recovery targets represent thresholds on action-driven
#' improvements (deltas) rather than baseline representation. The provided values are treated as
#' absolute thresholds in the same units as the effect/benefit amounts used by the model builder.
#'
#' @param x A [data-class] object.
#' @param targets Target specification. See \strong{Targets format} in \code{\link{targets}} details.
#' @param overwrite Logical. If \code{TRUE}, replace existing recovery targets for the same features.
#' @param label Optional character label stored with the targets.
#'
#' @return Updated [data-class] object.
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


#'
#' @title Add recovery targets (relative)
#'
#' @description
#' Adds relative recovery targets as proportions in \eqn{[0,1]} of a per-feature basis.
#' By default, the basis is the \emph{potential} maximum improvement per feature derived from
#' available actions (via \code{.pa_feature_potential()}). Alternatively, \code{relative_basis="baseline"}
#' uses baseline totals (via \code{.pa_feature_totals()}).
#'
#' @param x A [data-class] object.
#' @param targets Target specification (proportions in \eqn{[0,1]}).
#' @param relative_basis Character. Basis used to convert relative targets to absolute thresholds:
#' \code{"potential"} (default) or \code{"baseline"}.
#' @param overwrite Logical. If \code{TRUE}, replace existing recovery targets for the same features.
#' @param label Optional character label stored with the targets.
#'
#' @return Updated [data-class] object.
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

#'
#' @title Add mixed total targets (absolute)
#'
#' @description
#' Adds absolute mixed-total targets per feature. A mixed-total target enforces that
#' baseline representation plus action-induced deltas jointly reach a single threshold:
#' \deqn{\sum_i z_{is} r_{is} + \sum_{i,a} x_{ia}\Delta_{ias} \ge T^{mix}_s.}
#'
#' @details
#' Mixed-total targets are conceptually different from specifying separate conservation and recovery
#' targets for the same feature. As such, mixed-total targets are intended to be mutually exclusive
#' with conservation/recovery targets for the same feature. Conflict checks are handled by
#' \code{.pa_store_targets()}.
#'
#' @param x A [data-class] object.
#' @param targets Target specification (absolute). See \strong{Targets format} in \code{\link{targets}} details.
#' @param overwrite Logical. If \code{TRUE}, replace existing mixed-total targets for the same features
#' (but still errors if conservation/recovery targets exist for those features).
#' @param label Optional character label stored with the targets.
#'
#' @return Updated [data-class] object.
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


#'
#' @title Add mixed total targets (relative to baseline)
#'
#' @description
#' Adds relative mixed-total targets as proportions in \eqn{[0,1]} of baseline totals per feature.
#' Baseline totals are computed via \code{.pa_feature_totals()} and stored in \code{basis_total}.
#'
#' @param x A [data-class] object.
#' @param targets Target specification (proportions in \eqn{[0,1]}).
#' @param overwrite Logical. If \code{TRUE}, replace existing mixed-total targets for the same features
#' (subject to mutual exclusivity rules enforced by \code{.pa_store_targets()}).
#' @param label Optional character label stored with the targets.
#'
#' @return Updated [data-class] object.
#' @export
add_mixed_targets_total_relative <- function(x, targets, overwrite = FALSE, label = NULL) {
  stopifnot(inherits(x, "Data"))
  dt <- .pa_parse_targets(x, targets)

  rel <- as.numeric(dt$target_raw)
  if (any(rel < 0 | rel > 1, na.rm = TRUE)) {
    stop("Relative mixed_total targets must be between 0 and 1.", call. = FALSE)
  }

  basis <- .pa_feature_totals(x) # named by feature id
  basis_v <- basis[as.character(dt$feature)]
  basis_v[is.na(basis_v)] <- 0

  abs_target <- rel * as.numeric(basis_v)

  out <- data.frame(
    feature      = as.numeric(dt$feature),
    type         = "mixed_total",
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

