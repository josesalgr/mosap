#' @export
#' @rdname inputData
methods::setGeneric(
  "inputData",
  signature = methods::signature("pu", "features", "dist_features"),
  function(
    pu,
    features,
    dist_features,
    boundary = NULL,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    standardGeneric("inputData")
  }
)

# =========================================================
# Internal: TABULAR implementation (pure tabular + legacy)
# =========================================================
.pa_inputData_tabular_impl <- function(pu, features, dist_features, boundary = NULL, ...) {

  dots <- list(...)
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # -------------------------
  # Detect legacy inputs
  # -------------------------
  has_legacy <- !is.null(dots$threats) || !is.null(dots$dist_threats) || !is.null(dots$sensitivity)

  format <- dots$format %||% "auto"
  if (!format %in% c("auto", "new", "legacy")) {
    stop("`format` must be one of: 'auto', 'new', 'legacy'.", call. = FALSE)
  }
  if (format == "new" && has_legacy) {
    stop("You provided legacy inputs (threats/dist_threats/sensitivity) but format='new'.", call. = FALSE)
  }
  if (format == "legacy" && (!is.data.frame(dots$threats) || !is.data.frame(dots$dist_threats))) {
    stop("format='legacy' requires `threats` and `dist_threats` (data.frame) in ...", call. = FALSE)
  }

  # helper: coerce ids to integer safely
  .as_int_id <- function(x, what) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      if (any(grepl("[^0-9\\-]", x))) {
        stop(what, " must be numeric/integer ids (got non-numeric strings).", call. = FALSE)
      }
      x <- as.integer(x)
    } else {
      x <- as.integer(x)
    }
    if (anyNA(x)) stop(what, " contains NA after coercion to integer.", call. = FALSE)
    x
  }

  # =========================
  # PU: validate + normalize
  # =========================
  assertthat::assert_that(
    inherits(pu, "data.frame"),
    assertthat::has_name(pu, "id"),
    nrow(pu) > 0,
    assertthat::noNA(pu$id)
  )

  pu$id <- .as_int_id(pu$id, "pu$id")
  if (anyDuplicated(pu$id) != 0) stop("pu$id must be unique.", call. = FALSE)

  # accept cost or monitoring_cost -> normalize to cost
  if ("cost" %in% names(pu)) {
    assertthat::assert_that(is.numeric(pu$cost), assertthat::noNA(pu$cost))
  } else if ("monitoring_cost" %in% names(pu)) {
    assertthat::assert_that(is.numeric(pu$monitoring_cost), assertthat::noNA(pu$monitoring_cost))
    pu$cost <- pu$monitoring_cost
  } else {
    stop("pu must contain either a 'cost' column or a 'monitoring_cost' column.", call. = FALSE)
  }

  # locks: accept locked_in/locked_out or status (Marxan style)
  has_locked_cols <- ("locked_in" %in% names(pu)) || ("locked_out" %in% names(pu))
  if (has_locked_cols) {
    if (!("locked_in" %in% names(pu))) pu$locked_in <- FALSE
    if (!("locked_out" %in% names(pu))) pu$locked_out <- FALSE
    pu$locked_in  <- as.logical(pu$locked_in)
    pu$locked_out <- as.logical(pu$locked_out)
  } else if ("status" %in% names(pu)) {
    pu$status <- as.integer(pu$status)
    pu$locked_in  <- pu$status == 2L
    pu$locked_out <- pu$status == 3L
  } else {
    pu$locked_in  <- FALSE
    pu$locked_out <- FALSE
  }

  if (any(pu$locked_in & pu$locked_out, na.rm = TRUE)) {
    stop("Some planning units are both locked_in and locked_out. Please fix pu input.", call. = FALSE)
  }

  pu <- pu[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]
  pu <- pu[order(pu$id), , drop = FALSE]

  # internal ids + lookup
  pu$internal_id <- seq_len(nrow(pu))
  pu_index <- stats::setNames(pu$internal_id, as.character(pu$id))

  # =========================
  # FEATURES: validate + normalize
  # =========================
  assertthat::assert_that(
    inherits(features, "data.frame"),
    assertthat::has_name(features, "id"),
    nrow(features) > 0,
    assertthat::noNA(features$id)
  )

  features$id <- .as_int_id(features$id, "features$id")
  if (anyDuplicated(features$id) != 0) stop("features$id must be unique.", call. = FALSE)

  if (!("name" %in% names(features))) {
    features$name <- paste0("feature.", seq_len(nrow(features)))
  } else {
    features$name <- as.character(features$name)
    assertthat::assert_that(assertthat::noNA(features$name))
    if (anyDuplicated(features$name) != 0) stop("features$name must be unique.", call. = FALSE)
  }

  # legacy-only: require targets
  if ((format == "legacy") || (format == "auto" && has_legacy)) {
    if (!("target_recovery" %in% names(features))) {
      stop("Legacy mode requires features$target_recovery.", call. = FALSE)
    }
    assertthat::assert_that(is.numeric(features$target_recovery), assertthat::noNA(features$target_recovery))
    if (!("target_conservation" %in% names(features))) features$target_conservation <- 0
    assertthat::assert_that(is.numeric(features$target_conservation), assertthat::noNA(features$target_conservation))
  }

  features <- features[, c("id", "name", setdiff(names(features), c("id", "name"))), drop = FALSE]
  features <- features[order(features$id), , drop = FALSE]

  features$internal_id <- seq_len(nrow(features))
  feature_index <- stats::setNames(features$internal_id, as.character(features$id))

  # =========================
  # DIST_FEATURES: validate + normalize
  # =========================
  assertthat::assert_that(
    inherits(dist_features, "data.frame"),
    assertthat::has_name(dist_features, "pu"),
    assertthat::has_name(dist_features, "feature"),
    assertthat::has_name(dist_features, "amount"),
    nrow(dist_features) > 0,
    assertthat::noNA(dist_features$pu),
    assertthat::noNA(dist_features$feature),
    assertthat::noNA(dist_features$amount),
    is.numeric(dist_features$amount),
    all(dist_features$amount >= 0)
  )

  dist_features$pu      <- .as_int_id(dist_features$pu, "dist_features$pu")
  dist_features$feature <- .as_int_id(dist_features$feature, "dist_features$feature")

  if (!all(dist_features$pu %in% pu$id)) {
    bad <- unique(dist_features$pu[!dist_features$pu %in% pu$id])
    stop("dist_features contains unknown PU ids: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!all(dist_features$feature %in% features$id)) {
    bad <- unique(dist_features$feature[!dist_features$feature %in% features$id])
    stop("dist_features contains unknown feature ids: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  dist_features <- dist_features[dist_features$amount != 0, , drop = FALSE]

  key <- paste(dist_features$pu, dist_features$feature, sep = "||")
  if (anyDuplicated(key) != 0) stop("There are duplicate (pu, feature) pairs in dist_features.", call. = FALSE)

  dist_features$internal_pu      <- unname(pu_index[as.character(dist_features$pu)])
  dist_features$internal_feature <- unname(feature_index[as.character(dist_features$feature)])

  dist_features <- dist_features[order(dist_features$internal_pu, dist_features$internal_feature), , drop = FALSE]
  dist_features$internal_row <- seq_len(nrow(dist_features))

  # =========================
  # BOUNDARY: validate + normalize
  # =========================
  assertthat::assert_that(inherits(boundary, c("NULL", "data.frame")))
  if (inherits(boundary, "data.frame")) {
    assertthat::assert_that(
      assertthat::has_name(boundary, "id1"),
      assertthat::has_name(boundary, "id2"),
      assertthat::has_name(boundary, "boundary"),
      assertthat::noNA(boundary$id1),
      assertthat::noNA(boundary$id2),
      assertthat::noNA(boundary$boundary),
      is.numeric(boundary$boundary)
    )

    boundary$id1 <- .as_int_id(boundary$id1, "boundary$id1")
    boundary$id2 <- .as_int_id(boundary$id2, "boundary$id2")
    boundary$boundary <- base::round(as.numeric(boundary$boundary), 3)

    if (!all(boundary$id1 %in% pu$id) || !all(boundary$id2 %in% pu$id)) {
      warning("boundary contains PU ids not present in pu; they will be removed.", call. = FALSE, immediate. = TRUE)
      keep <- boundary$id1 %in% pu$id & boundary$id2 %in% pu$id
      boundary <- boundary[keep, , drop = FALSE]
    }

    if (nrow(boundary) == 0) {
      boundary <- NULL
    } else {
      boundary$internal_id1 <- unname(pu_index[as.character(boundary$id1)])
      boundary$internal_id2 <- unname(pu_index[as.character(boundary$id2)])
    }
  }

  # =========================
  # rounding
  # =========================
  pu$cost <- base::round(pu$cost, 3)
  dist_features$amount <- base::round(dist_features$amount, 3)

  # =========================
  # useful warnings
  # =========================
  dif_pu <- setdiff(unique(pu$id), unique(dist_features$pu))
  if (length(dif_pu) != 0L) {
    warning(
      paste0("The following pu's do not contain features: ", paste(dif_pu, collapse = " ")),
      call. = FALSE, immediate. = TRUE
    )
  }

  dif_features <- setdiff(unique(features$id), unique(dist_features$feature))
  if (length(dif_features) != 0L) {
    warning(
      paste0("The following features are not represented in dist_features: ", paste(dif_features, collapse = " ")),
      call. = FALSE, immediate. = TRUE
    )
  }

  # =========================
  # LEGACY BLOCK (optional)
  # =========================
  threats <- NULL
  dist_threats <- NULL
  sensitivity <- NULL
  threat_index <- NULL

  if ((format == "legacy") || (format == "auto" && has_legacy)) {

    threats <- dots$threats
    dist_threats <- dots$dist_threats
    sensitivity <- dots$sensitivity %||% NULL

    if (!inherits(threats, "data.frame") || !inherits(dist_threats, "data.frame")) {
      stop("Legacy inputs require `threats` and `dist_threats` as data.frame (passed via ...).", call. = FALSE)
    }

    # ---- threats
    assertthat::assert_that(
      assertthat::has_name(threats, "id"),
      nrow(threats) > 0,
      assertthat::noNA(threats$id)
    )
    threats$id <- .as_int_id(threats$id, "threats$id")
    if (anyDuplicated(threats$id) != 0) stop("threats$id must be unique.", call. = FALSE)

    if (!("name" %in% names(threats))) threats$name <- paste0("threat.", seq_len(nrow(threats)))
    threats$name <- as.character(threats$name)
    if (anyDuplicated(threats$name) != 0) stop("threats$name must be unique.", call. = FALSE)

    if (!("blm_actions" %in% names(threats))) threats$blm_actions <- 0
    assertthat::assert_that(is.numeric(threats$blm_actions), all(threats$blm_actions >= 0))

    threats <- threats[order(threats$id), , drop = FALSE]
    threats$internal_id <- seq_len(nrow(threats))
    threat_index <- stats::setNames(threats$internal_id, as.character(threats$id))

    # ---- dist_threats
    assertthat::assert_that(
      assertthat::has_name(dist_threats, "pu"),
      assertthat::has_name(dist_threats, "threat"),
      assertthat::has_name(dist_threats, "amount"),
      assertthat::has_name(dist_threats, "action_cost"),
      nrow(dist_threats) > 0,
      assertthat::noNA(dist_threats$pu),
      assertthat::noNA(dist_threats$threat),
      assertthat::noNA(dist_threats$amount),
      assertthat::noNA(dist_threats$action_cost),
      is.numeric(dist_threats$amount),
      is.numeric(dist_threats$action_cost),
      all(dist_threats$amount >= 0)
    )

    dist_threats$pu     <- .as_int_id(dist_threats$pu, "dist_threats$pu")
    dist_threats$threat <- .as_int_id(dist_threats$threat, "dist_threats$threat")

    if (!all(dist_threats$pu %in% pu$id)) {
      bad <- unique(dist_threats$pu[!dist_threats$pu %in% pu$id])
      stop("dist_threats contains unknown PU ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    if (!all(dist_threats$threat %in% threats$id)) {
      bad <- unique(dist_threats$threat[!dist_threats$threat %in% threats$id])
      stop("dist_threats contains unknown threat ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    # status handling (optional)
    if ("status" %in% names(dist_threats)) {
      dist_threats$status <- as.integer(dist_threats$status)
      ok <- dist_threats$status %in% c(0L, 2L, 3L)
      if (!all(ok, na.rm = TRUE)) stop("dist_threats$status must be in {0,2,3}.", call. = FALSE)

      locked_out_pus <- pu$id[pu$locked_out]
      if (length(locked_out_pus)) {
        idx <- dist_threats$pu %in% locked_out_pus & dist_threats$status == 2L
        if (any(idx, na.rm = TRUE)) {
          warning("Some actions were locked-in inside locked-out PU(s); setting them to locked-out.", call. = FALSE, immediate. = TRUE)
          dist_threats$status[idx] <- 3L
        }
        idx2 <- dist_threats$pu %in% locked_out_pus
        dist_threats$status[idx2] <- 3L
      }
    } else {
      dist_threats$status <- 0L
    }

    dist_threats <- dist_threats[dist_threats$amount != 0, , drop = FALSE]
    key_t <- paste(dist_threats$pu, dist_threats$threat, sep = "||")
    if (anyDuplicated(key_t) != 0) stop("There are duplicate (pu, threat) pairs in dist_threats.", call. = FALSE)

    dist_threats$internal_pu <- unname(pu_index[as.character(dist_threats$pu)])
    dist_threats$internal_threat <- unname(threat_index[as.character(dist_threats$threat)])

    # ---- sensitivity
    if (is.null(sensitivity)) {
      sensitivity <- base::expand.grid(feature = features$id, threat = threats$id)
    } else {
      assertthat::assert_that(
        inherits(sensitivity, "data.frame"),
        assertthat::has_name(sensitivity, "feature"),
        assertthat::has_name(sensitivity, "threat"),
        nrow(sensitivity) > 0,
        assertthat::noNA(sensitivity$feature),
        assertthat::noNA(sensitivity$threat)
      )
    }

    sensitivity$feature <- .as_int_id(sensitivity$feature, "sensitivity$feature")
    sensitivity$threat  <- .as_int_id(sensitivity$threat,  "sensitivity$threat")

    sensitivity <- sensitivity[sensitivity$feature %in% features$id & sensitivity$threat %in% threats$id, , drop = FALSE]
    if (nrow(sensitivity) == 0) stop("After filtering, sensitivity has 0 valid rows.", call. = FALSE)

    if (!("delta1" %in% names(sensitivity))) sensitivity$delta1 <- 0
    if (!("delta2" %in% names(sensitivity))) sensitivity$delta2 <- NA
    if (!("delta3" %in% names(sensitivity))) sensitivity$delta3 <- 0
    if (!("delta4" %in% names(sensitivity))) sensitivity$delta4 <- 1

    sensitivity$delta1[is.na(sensitivity$delta1)] <- 0
    sensitivity$delta3[is.na(sensitivity$delta3)] <- 0
    sensitivity$delta4[is.na(sensitivity$delta4)] <- 1

    max_int <- stats::aggregate(dist_threats$amount, by = list(threat = dist_threats$threat), FUN = max)
    names(max_int)[2] <- "max_amount"
    idx_map <- match(sensitivity$threat, max_int$threat)
    fill_vals <- max_int$max_amount[idx_map]
    sensitivity$delta2[is.na(sensitivity$delta2)] <- fill_vals[is.na(sensitivity$delta2)]

    if (!all(sensitivity$delta2 > sensitivity$delta1)) stop("Each delta2 must be > delta1.", call. = FALSE)
    if (!all(sensitivity$delta4 > sensitivity$delta3)) stop("Each delta4 must be > delta3.", call. = FALSE)

    sensitivity$internal_feature <- unname(feature_index[as.character(sensitivity$feature)])
    sensitivity$internal_threat  <- unname(threat_index[as.character(sensitivity$threat)])

    threats$blm_actions <- base::round(threats$blm_actions, 3)
    dist_threats$amount <- base::round(dist_threats$amount, 3)
    dist_threats$action_cost <- base::round(dist_threats$action_cost, 3)
    sensitivity$delta1 <- base::round(sensitivity$delta1, 3)
    sensitivity$delta2 <- base::round(sensitivity$delta2, 3)
    sensitivity$delta3 <- base::round(sensitivity$delta3, 3)
    sensitivity$delta4 <- base::round(sensitivity$delta4, 3)

    if (isTRUE(dots$warn_legacy %||% TRUE)) {
      if (requireNamespace("lifecycle", quietly = TRUE)) {
        lifecycle::deprecate_warn(
          when = "1.0.1",
          what = "inputData()",
          with = "add_actions()",
          details = paste(
            "Legacy inputs detected (threats/dist_threats/sensitivity).",
            "New workflow example:",
            "inputData(...) %>% add_actions(...) %>% add_effects(...) %>% solve()"
          )
        )
      } else {
        warning(
          "Legacy inputs detected (threats/dist_threats/sensitivity). Consider migrating to the new format.",
          call. = FALSE, immediate. = TRUE
        )
      }
    }
  }

  # =========================
  # build Data object (FIXED: assign to x and return it)
  # =========================
  x <- pproto(
    NULL, Data,
    data = list(
      pu = pu,
      features = features,
      dist_features = dist_features,
      boundary = boundary,

      # legacy (optional)
      threats = threats,
      dist_threats = dist_threats,
      sensitivity = sensitivity,

      index = list(
        pu = pu_index,
        feature = feature_index,
        threat = threat_index,
        feature_name_to_id = stats::setNames(features$id, features$name)
      ),

      meta = list(
        input_format = if ((format == "legacy") || (format == "auto" && has_legacy)) "legacy" else "new",
        dist_features_meaning = "baseline_amount",
        dist_benefit_meaning  = "delta_by_default"
      ),

      # new workflow placeholders
      actions = NULL,
      dist_actions = NULL,
      dist_benefit = NULL,
      locked_actions = NULL,
      targets = NULL,
      objective = NULL,
      decisions = NULL,
      solver = NULL
    )
  )

  x
}

# =========================================================
# Method: TABULAR inputs
# =========================================================
#' @export
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "data.frame", features = "data.frame", dist_features = "data.frame"),
  function(
    pu,
    features,
    dist_features,
    boundary = NULL,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    # ignore spatial-only args in tabular mode
    .pa_inputData_tabular_impl(
      pu = pu,
      features = features,
      dist_features = dist_features,
      boundary = boundary,
      ...
    )
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features missing)
# =========================================================
#' @export
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "missing"),
  function(
    pu,
    features,
    dist_features,
    boundary = "auto",
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    cost_aggregation <- match.arg(cost_aggregation)

    if (is.null(cost)) {
      stop(
        "Spatial mode: `dist_features` is missing, so you must provide `cost` ",
        "(either a PU column name for vector PUs, or a raster/file path).",
        call. = FALSE
      )
    }
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Spatial mode requires the 'terra' package. Please install it.", call. = FALSE)
    }
    if (!(is.null(boundary) || is.character(boundary) || is.data.frame(boundary))) {
      stop("boundary must be NULL, 'auto', or a data.frame.", call. = FALSE)
    }

    fun_cost <- .fun_from_name(cost_aggregation)

    # ---- read pu
    pu_r <- NULL
    pu_v <- NULL

    if (inherits(pu, "SpatRaster") || .is_raster_path(pu)) {
      pu_r <- .read_rast(pu)
      pu_r <- terra::round(pu_r)
      pu_v <- terra::as.polygons(pu_r, dissolve = TRUE, values = TRUE, na.rm = TRUE)
      names(pu_v) <- pu_id_col
    } else {
      pu_v <- .read_vect(pu)

      if (!(pu_id_col %in% names(pu_v))) {
        if (identical(pu_id_col, "id")) {
          warning(
            "Planning unit layer has no 'id' column. Creating sequential ids (1..n). ",
            "If you want to use an existing field, set pu_id_col to its name.",
            call. = FALSE, immediate. = TRUE
          )
          pu_v$id <- seq_len(nrow(pu_v))
          pu_id_col <- "id"
        } else {
          stop(
            "Planning unit vector is missing the id column: '", pu_id_col, "'. ",
            "Either provide that column or set pu_id_col to an existing column name.",
            call. = FALSE
          )
        }
      }
    }

    # base pu table
    pu_df <- terra::as.data.frame(pu_v)
    names(pu_df)[names(pu_df) == pu_id_col] <- "id"
    pu_df$id <- as.integer(round(pu_df$id))
    pu_df <- pu_df[order(pu_df$id), , drop = FALSE]
    if (!("id" %in% names(pu_df))) stop("Could not find planning unit ids.", call. = FALSE)

    # ---- cost
    if (is.character(cost) && (cost %in% names(pu_df))) {
      pu_df$cost <- pu_df[[cost]]
    } else {
      cost_r <- .read_rast(cost)
      if (is.null(cost_r)) {
        stop("You must provide 'cost' either as a column name in pu (vector) or as a raster/file path.", call. = FALSE)
      }
      cost_ex <- terra::extract(cost_r, pu_v, fun = fun_cost, na.rm = TRUE)
      cost_ex <- cost_ex[match(pu_df$id, cost_ex[[1]]), , drop = FALSE]
      pu_df$cost <- cost_ex[[2]]
    }

    # ---- locks (prefer columns if present)
    pu_df$locked_in  <- FALSE
    pu_df$locked_out <- FALSE

    if (locked_in_col %in% names(pu_df)) {
      pu_df$locked_in <- as.logical(pu_df[[locked_in_col]])
    }
    if (locked_out_col %in% names(pu_df)) {
      pu_df$locked_out <- as.logical(pu_df[[locked_out_col]])
    }

    # optional pu_status (0/2/3) only fills missing locks
    if (!is.null(pu_status) && !(locked_in_col %in% names(pu_df)) && !(locked_out_col %in% names(pu_df))) {
      if (is.character(pu_status) && (pu_status %in% names(pu_df))) {
        st <- pu_df[[pu_status]]
      } else {
        st_r <- .read_rast(pu_status)
        if (is.null(st_r)) stop("pu_status must be a column name or a raster/file path.", call. = FALSE)
        st_ex <- terra::extract(st_r, pu_v, fun = terra::modal, na.rm = TRUE)
        st <- st_ex[[2]]
      }
      pu_df$locked_in  <- st == 2
      pu_df$locked_out <- st == 3
    }

    if (any(pu_df$locked_in & pu_df$locked_out, na.rm = TRUE)) {
      stop("Some planning units are both locked_in and locked_out. Please fix your inputs.", call. = FALSE)
    }

    pu_df <- pu_df[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]

    # ---- features + dist_features (sum by PU)
    feat_r <- .read_rast(features)
    if (is.null(feat_r)) {
      stop(
        "Spatial mode: `features` must be a terra::SpatRaster (or raster file path) with one layer per feature.\n",
        "If you intended tabular mode, provide `dist_features` as a data.frame.",
        call. = FALSE
      )
    }

    feat_names <- names(feat_r)
    if (is.null(feat_names) || any(feat_names == "")) {
      feat_names <- paste0("feature.", seq_len(terra::nlyr(feat_r)))
    }

    features_df <- data.frame(
      id = seq_len(terra::nlyr(feat_r)),
      name = feat_names,
      stringsAsFactors = FALSE
    )

    feat_ex <- terra::extract(feat_r, pu_v, fun = sum, na.rm = TRUE)
    feat_ex <- feat_ex[match(pu_df$id, feat_ex[[1]]), , drop = FALSE]
    feat_mat <- as.matrix(feat_ex[, -1, drop = FALSE])

    dist_features_df <- data.frame(
      pu      = rep(pu_df$id, times = terra::nlyr(feat_r)),
      feature = rep(features_df$id, each = nrow(pu_df)),
      amount  = as.vector(t(feat_mat)),
      stringsAsFactors = FALSE
    )
    dist_features_df <- dist_features_df[!is.na(dist_features_df$amount) & dist_features_df$amount > 0, , drop = FALSE]

    # ---- boundary (optional)
    boundary_df <- NULL
    if (is.data.frame(boundary)) {
      boundary_df <- boundary
    } else if (is.character(boundary) && identical(boundary, "auto")) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        stop(
          "Automatic boundary derivation requires the 'sf' package. Please install it or provide boundary as a data.frame.",
          call. = FALSE
        )
      }

      pu_sf <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)
      if (is.null(pu_sf)) {
        stop("Could not convert planning units to sf for boundary derivation. Provide boundary as a data.frame.", call. = FALSE)
      }

      nb <- sf::st_touches(pu_sf)
      id_vec <- pu_df$id

      id1 <- integer(0)
      id2 <- integer(0)

      for (i in seq_along(nb)) {
        if (length(nb[[i]]) == 0) next
        j <- nb[[i]]
        keep <- j > i
        if (any(keep)) {
          id1 <- c(id1, rep(id_vec[i], sum(keep)))
          id2 <- c(id2, id_vec[j[keep]])
        }
      }

      boundary_df <- data.frame(
        id1 = id1,
        id2 = id2,
        boundary = 1,
        stringsAsFactors = FALSE
      )
    }

    # ---- build via tabular impl (no recursion / no double dispatch)
    x <- .pa_inputData_tabular_impl(
      pu = pu_df,
      features = features_df,
      dist_features = dist_features_df,
      boundary = boundary_df,
      ...
    )

    # ---- store spatial artifacts (optional)
    if (!is.null(pu_r)) x$data$pu_raster_id <- pu_r

    if (requireNamespace("sf", quietly = TRUE)) {
      pu_sf_store <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)
      if (!is.null(pu_sf_store)) {

        if (pu_id_col %in% names(pu_sf_store)) {
          names(pu_sf_store)[names(pu_sf_store) == pu_id_col] <- "id"
        }
        if (!("id" %in% names(pu_sf_store))) {
          pu_sf_store$id <- seq_len(nrow(pu_sf_store))
        }

        pu_sf_store <- pu_sf_store[, "id", drop = FALSE]
        ord <- match(x$data$pu$id, pu_sf_store$id)

        if (any(is.na(ord))) {
          warning(
            "Could not safely match PU geometry to PU ids; 'pu_sf' will not be stored in the problem object.",
            call. = FALSE, immediate. = TRUE
          )
        } else {
          x$data$pu_sf <- pu_sf_store[ord, , drop = FALSE]
        }
      }
    }

    x
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features explicitly NULL)
# =========================================================
#' @export
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "NULL"),
  function(
    pu,
    features,
    dist_features,
    boundary = "auto",
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    methods::selectMethod("inputData", signature = c("ANY", "ANY", "missing"))(
      pu = pu,
      features = features,
      boundary = boundary,
      cost = cost,
      pu_id_col = pu_id_col,
      locked_in_col = locked_in_col,
      locked_out_col = locked_out_col,
      pu_status = pu_status,
      cost_aggregation = cost_aggregation,
      ...
    )
  }
)
