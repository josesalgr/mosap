#' @include internal.R
#'
#' @title Build optimization model from Data
#'
#' @description
#' Materializes (builds) the optimization model using the current state of the `Data` object:
#' prepared data tables, stored objective settings, and stored constraints (e.g., targets).
#'
#' The configuration (objective type, budget, blm, etc.) is expected to be stored in
#' `x$data$model_args` by other functions (e.g., `set_objective_*()`, `add_budget()`, etc.).
#'
#' @param x Data object (class "Data") created with inputData()/inputDataSpatial().
#'
#' @return Updated `Data` object with model pointer and model snapshot.
.pa_build_model <- function(x) {

  stopifnot(inherits(x, "Data"))
  stopifnot(!is.null(x$data$pu))
  stopifnot(!is.null(x$data$dist_features))

  # ------------------------------------------------------------
  # 0) Detect input format
  # ------------------------------------------------------------
  input_format <- x$data$meta$input_format %||% "new"

  # ------------------------------------------------------------
  # 0.1) Objective must be configured in NEW pipeline
  #      (legacy can default to minimizeCosts)
  # ------------------------------------------------------------
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  if (is.null(x$data$model_args$model_type)) {
    if (identical(input_format, "legacy")) {
      x$data$model_args$model_type <- "minimizeCosts"
    } else {
      stop(
        "No objective configured. Use add_objective_*() (new pipeline) or call problem() (legacy pipeline).",
        call. = FALSE
      )
    }
  }

  # keep these for later constraints/printing
  if (is.null(x$data$model_args$budget))   x$data$model_args$budget   <- 0
  if (is.null(x$data$model_args$blm))      x$data$model_args$blm      <- 0
  if (is.null(x$data$model_args$curve))    x$data$model_args$curve    <- 1L
  if (is.null(x$data$model_args$segments)) x$data$model_args$segments <- 3L

  # ------------------------------------------------------------
  # 0.2) LEGACY ADAPTERS (only if legacy format)
  # ------------------------------------------------------------
  if (identical(input_format, "legacy")) {

    has_threats_data <- !is.null(x$data$dist_threats) &&
      inherits(x$data$dist_threats, "data.frame") &&
      nrow(x$data$dist_threats) > 0

    has_user_actions <- !is.null(x$data$actions) &&
      inherits(x$data$actions, "data.frame") &&
      nrow(x$data$actions) > 0

    has_user_dist_actions <- !is.null(x$data$dist_actions) &&
      inherits(x$data$dist_actions, "data.frame") &&
      nrow(x$data$dist_actions) > 0

    # ---- (A) Default actions from threats ONLY if user didn't define actions
    if (has_threats_data && !(has_user_actions || has_user_dist_actions)) {
      if (!exists(".pa_add_actions_default_from_threats", mode = "function")) {
        stop(
          "Legacy input detected (threats/dist_threats), but .pa_add_actions_default_from_threats() is not implemented.",
          call. = FALSE
        )
      }

      be  <- as.numeric(x$data$model_args$benefit_exponent %||% x$data$model_args$curve %||% 1)[1]
      seg <- as.integer(x$data$model_args$curve_segments %||% x$data$model_args$segments %||% 3L)[1]

      x <- .pa_add_actions_default_from_threats(
        x,
        benefit_exponent = be,
        curve_segments   = seg
      )
    }

    # ---- (B) Default targets from features ONLY if needed by objective
    mtype_now <- x$data$model_args$model_type %||% "minimizeCosts"

    need_targets <- identical(mtype_now, "minimizeCosts") &&
      (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0)

    has_legacy_targets_in_features <- !is.null(x$data$features) &&
      inherits(x$data$features, "data.frame") &&
      ("target_recovery" %in% names(x$data$features) || "target_conservation" %in% names(x$data$features))

    if (need_targets && has_legacy_targets_in_features) {
      if (!exists(".pa_targets_from_features_legacy", mode = "function")) {
        stop(
          "model_type='minimizeCosts' requires targets. Legacy targets exist in features, but .pa_targets_from_features_legacy() is not implemented.",
          call. = FALSE
        )
      }
      x <- .pa_targets_from_features_legacy(x)
    }

    # if still legacy + threats but no actions, warn once (after trying adapter)
    has_actions_data <- !is.null(x$data$dist_actions) &&
      inherits(x$data$dist_actions, "data.frame") &&
      nrow(x$data$dist_actions) > 0

    if (has_threats_data && !has_actions_data) {
      warning(
        "Legacy threats detected but no actions were materialized. Model will be built without actions (likely unintended).",
        call. = FALSE, immediate. = TRUE
      )
    }
  }

  # ------------------------------------------------------------
  # 1) Ensure tables exist (allow "no actions" case)
  # ------------------------------------------------------------
  if (is.null(x$data$dist_actions)) {
    x$data$dist_actions <- data.frame(
      pu = character(0),
      action = character(0),
      cost = numeric(0),
      status = integer(0),
      internal_pu = integer(0),
      internal_action = integer(0),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(x$data$actions)) {
    x$data$actions <- data.frame(
      id = character(0),
      internal_id = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # ------------------------------------------------------------
  # 1.1) NEW pipeline validation (post-adapter)
  # ------------------------------------------------------------
  mtype_check <- x$data$model_args$model_type %||% "minimizeCosts"
  if (!identical(input_format, "legacy") && identical(mtype_check, "minimizeCosts")) {
    if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
      stop(
        "model_type='minimizeCosts' requires targets, but x$data$targets is empty. ",
        "Run add_target_*() before solve().",
        call. = FALSE
      )
    }
  }

  # ------------------------------------------------------------
  # 2) Prepare model-ready tables (feasibility filter)
  # ------------------------------------------------------------
  da <- x$data$dist_actions

  if (inherits(da, "data.frame") && nrow(da) > 0) {

    # Drop locked-out (status == 3)
    if ("status" %in% names(da)) {
      da <- da[da$status != 3L, , drop = FALSE]
    }

    # Drop rows without internal ids (defensive)
    if (all(c("internal_pu", "internal_action") %in% names(da))) {
      da <- da[!is.na(da$internal_pu) & !is.na(da$internal_action), , drop = FALSE]
    }

    # Drop rows with missing/non-finite costs
    if ("cost" %in% names(da)) {
      da$cost <- as.numeric(da$cost)
      da <- da[is.finite(da$cost) & !is.na(da$cost), , drop = FALSE]
    }

    # type normalize for joins
    if ("pu" %in% names(da)) da$pu <- as.integer(da$pu)
    if ("action" %in% names(da)) da$action <- as.character(da$action)
  }

  # asegurar orden determinista (esto es importantísimo)
  da <- da[order(da$internal_pu, da$internal_action), , drop = FALSE]

  # internal_row para el MODELO (fila 1..n del dist_actions_model)
  da$internal_row <- seq_len(nrow(da))

  has_actions_model <- inherits(da, "data.frame") && nrow(da) > 0

  x$data$dist_actions_model <- da

  # ---- EFFECTS model-ready (new source of truth)
  de <- x$data$dist_effects
  if (!is.null(de) && inherits(de, "data.frame") && nrow(de) > 0 && nrow(da) > 0) {
    if ("pu" %in% names(de)) de$pu <- as.integer(de$pu)
    if ("action" %in% names(de)) de$action <- as.character(de$action)

    de <- dplyr::inner_join(
      de,
      da[, c("pu", "action"), drop = FALSE],
      by = c("pu", "action")
    )
  }
  x$data$dist_effects_model <- de

  # ---- OPTIONAL COMPAT: keep dist_benefit_model derived from effects if available
  db <- NULL
  if (!is.null(de) && inherits(de, "data.frame") && nrow(de) > 0 && "benefit" %in% names(de)) {
    db <- de
    db$benefit <- as.numeric(db$benefit)
  }
  x$data$dist_benefit_model <- db

  # profit model-ready (if present)
  dp <- x$data$dist_profit
  if (!is.null(dp) && inherits(dp, "data.frame") && nrow(dp) > 0 && nrow(da) > 0) {
    if ("pu" %in% names(dp)) dp$pu <- as.integer(dp$pu)
    if ("action" %in% names(dp)) dp$action <- as.character(dp$action)

    dp <- dplyr::inner_join(
      dp,
      da[, c("pu", "action"), drop = FALSE],
      by = c("pu", "action")
    )
  }
  x$data$dist_profit_model <- dp

  # ------------------------------------------------------------
  # 2.5) Early validation: objectives that require actions/effects
  # ------------------------------------------------------------
  args  <- x$data$model_args %||% list()
  mtype <- args$model_type %||% "minimizeCosts"
  oargs <- args$objective_args %||% list()  # (por si quieres usarlo en mensajes)

  .pa_fail <- function(...) stop(paste0(...), call. = FALSE)

  # Helpers robustos
  .has_df_rows <- function(df) !is.null(df) && inherits(df, "data.frame") && nrow(df) > 0

  needs_actions <- mtype %in% c("maximizeBenefits", "minimizeLosses", "maximizeProfit", "maximizeNetProfit")

  if (needs_actions && !isTRUE(has_actions_model)) {
    .pa_fail(
      "Objective '", mtype, "' requires actions, but no actions are available.\n",
      "You likely forgot to run add_actions() (and to provide feasible dist_actions).\n",
      "If you intended a reserve-selection problem with no actions, use set_objective_min_cost() ",
      "or set_objective_max_representation()."
    )
  }

  if (identical(mtype, "maximizeBenefits")) {
    if (!.has_df_rows(x$data$dist_effects_model)) {
      .pa_fail(
        "Objective 'maximizeBenefits' requires effects/benefits, but dist_effects is empty.\n",
        "Run add_benefits() / add_effects() after add_actions()."
      )
    }
    bcol <- as.character(oargs$benefit_col %||% "benefit")[1]
    if (!(bcol %in% names(x$data$dist_effects_model))) {
      .pa_fail(
        "Objective 'maximizeBenefits' requires column '", bcol, "' in dist_effects.\n",
        "Check add_benefits()/add_effects() output or set benefit_col in objective_args."
      )
    }
  }

  if (identical(mtype, "minimizeLosses")) {
    if (!.has_df_rows(x$data$dist_effects_model)) {
      .pa_fail(
        "Objective 'minimizeLosses' requires effects/losses, but dist_effects is empty.\n",
        "Run add_losses() / add_effects() after add_actions()."
      )
    }
    lcol <- as.character(oargs$loss_col %||% "loss")[1]
    if (!(lcol %in% names(x$data$dist_effects_model))) {
      .pa_fail(
        "Objective 'minimizeLosses' requires column '", lcol, "' in dist_effects.\n",
        "Check add_losses()/add_effects() output or set loss_col in objective_args."
      )
    }
  }

  if (mtype %in% c("maximizeProfit", "maximizeNetProfit")) {
    if (!.has_df_rows(x$data$dist_profit_model)) {
      .pa_fail(
        "Objective '", mtype, "' requires dist_profit, but dist_profit is empty.\n",
        "Run add_profit() after add_actions()."
      )
    }
    pcol <- as.character(oargs$profit_col %||% "profit")[1]
    if (!(pcol %in% names(x$data$dist_profit_model))) {
      .pa_fail(
        "Objective '", mtype, "' requires column '", pcol, "' in dist_profit.\n",
        "Check add_profit() output or set profit_col in objective_args."
      )
    }
  }

  # ------------------------------------------------------------
  # 3) Build optimization problem pointer (C++ side)
  # ------------------------------------------------------------
  op <- rcpp_new_optimization_problem()

  # ------------------------------------------------------------
  # 4) Add base variables (w, x, z)
  # ------------------------------------------------------------
  idx <- rcpp_add_base_variables(
    op,
    pu_data            = x$data$pu,
    dist_actions_data  = x$data$dist_actions_model,
    dist_features_data = x$data$dist_features
  )

  # ------------------------------------------------------------
  # 5) Minimal structural constraints
  # ------------------------------------------------------------
  if (nrow(x$data$dist_actions_model) > 0) {
    rcpp_add_linking_x_le_w(op, x$data$dist_actions_model)
  }
  if (exists("rcpp_add_linking_z_le_w", mode = "function")) {
    rcpp_add_linking_z_le_w(op, x$data$dist_features)
  }
  if (exists("rcpp_add_pu_locks", mode = "function")) {
    rcpp_add_pu_locks(op, x$data$pu)
  }
  if (nrow(x$data$dist_actions_model) > 0 && exists("rcpp_add_action_locks", mode = "function")) {
    rcpp_add_action_locks(op, x$data$dist_actions_model)
  }

  # ------------------------------------------------------------
  # 6) Objective (from model_args)
  # ------------------------------------------------------------
  args  <- x$data$model_args %||% list()
  mtype <- args$model_type %||% "minimizeCosts"
  oargs <- args$objective_args %||% list()

  if (identical(mtype, "minimizeCosts")) {

    if (!exists("rcpp_set_objective_min_cost", mode = "function")) {
      stop("Missing rcpp_set_objective_min_cost() in the package.", call. = FALSE)
    }

    rcpp_set_objective_min_cost(
      op,
      pu_data = x$data$pu,
      dist_actions_data = x$data$dist_actions_model,
      include_pu_cost = isTRUE(oargs$include_pu_cost %||% TRUE),
      include_action_cost = isTRUE(oargs$include_action_cost %||% TRUE)
    )

    objective_id <- "min_cost"
    modelsense   <- "min"

  } else if (identical(mtype, "maximizeBenefits")) {

    if (!exists("rcpp_set_objective_max_benefit", mode = "function")) {
      stop("Missing rcpp_set_objective_max_benefit() in the package.", call. = FALSE)
    }

    if (is.null(x$data$dist_effects_model) || nrow(x$data$dist_effects_model) == 0) {
      stop("maximizeBenefits requires dist_effects (with a 'benefit' column). Run add_effects()/add_benefits() first.", call. = FALSE)
    }

    bcol <- as.character(oargs$benefit_col %||% "benefit")[1]
    if (!(bcol %in% names(x$data$dist_effects_model))) {
      stop("maximizeBenefits: column '", bcol, "' not found in dist_effects.", call. = FALSE)
    }

    rcpp_set_objective_max_benefit(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_benefit_data = x$data$dist_effects_model,
      benefit_col = bcol
    )

    objective_id <- "max_benefit"
    modelsense   <- "max"

  } else if (identical(mtype, "minimizeLosses")) {

    if (!exists("rcpp_set_objective_min_loss", mode = "function")) {
      stop("Missing rcpp_set_objective_min_loss() in the package.", call. = FALSE)
    }

    if (is.null(x$data$dist_effects_model) || nrow(x$data$dist_effects_model) == 0) {
      stop("minimizeLosses requires dist_effects (with a 'loss' column). Run add_losses()/add_effects() first.", call. = FALSE)
    }

    lcol <- as.character(oargs$loss_col %||% "loss")[1]
    if (!(lcol %in% names(x$data$dist_effects_model))) {
      stop("minimizeLosses: column '", lcol, "' not found in dist_effects.", call. = FALSE)
    }

    rcpp_set_objective_min_loss(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_effects_data = x$data$dist_effects_model,
      loss_col = lcol
    )

    objective_id <- "min_loss"
    modelsense   <- "min"

  } else if (identical(mtype, "maximizeProfit")) {

    if (!exists("rcpp_set_objective_max_profit", mode = "function")) {
      stop("Missing rcpp_set_objective_max_profit() in the package.", call. = FALSE)
    }

    if (is.null(x$data$dist_profit_model) || nrow(x$data$dist_profit_model) == 0) {
      stop("maximizeProfit requires dist_profit. Run add_profit() first.", call. = FALSE)
    }

    rcpp_set_objective_max_profit(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_profit_data  = x$data$dist_profit_model,
      profit_col = oargs$profit_col %||% "profit"
    )

    objective_id <- "max_profit"
    modelsense   <- "max"

  } else if (identical(mtype, "maximizeNetProfit")) {

    if (!exists("rcpp_set_objective_max_net_profit", mode = "function")) {
      stop("Missing rcpp_set_objective_max_net_profit() in the package.", call. = FALSE)
    }

    if (is.null(x$data$dist_profit_model) || nrow(x$data$dist_profit_model) == 0) {
      stop("maximizeNetProfit requires dist_profit. Run add_profit() first.", call. = FALSE)
    }

    rcpp_set_objective_max_net_profit(
      op,
      pu_data = x$data$pu,
      dist_actions_data = x$data$dist_actions_model,
      dist_profit_data  = x$data$dist_profit_model,
      profit_col = oargs$profit_col %||% "profit",
      include_pu_cost = isTRUE(oargs$include_pu_cost %||% TRUE),
      include_action_cost = isTRUE(oargs$include_action_cost %||% TRUE)
    )

    objective_id <- "max_net_profit"
    modelsense   <- "max"

  } else if (identical(mtype, "maximizeRepresentation")) {

    if (!exists("rcpp_set_objective_max_representation", mode = "function")) {
      stop("Missing rcpp_set_objective_max_representation() in the package.", call. = FALSE)
    }

    # En este objetivo, solo necesitamos dist_features (y z), NO dist_actions ni dist_effects
    if (is.null(x$data$dist_features) || nrow(x$data$dist_features) == 0) {
      stop("maximizeRepresentation requires dist_features.", call. = FALSE)
    }

    acol <- as.character(oargs$amount_col %||% "amount")[1]
    if (!(acol %in% names(x$data$dist_features))) {
      stop("maximizeRepresentation: column '", acol, "' not found in dist_features.", call. = FALSE)
    }

    rcpp_set_objective_max_representation(
      op,
      dist_features_data = x$data$dist_features,
      amount_col = acol
    )

    objective_id <- "max_representation"
    modelsense   <- "max"

  } else {
    stop("Unknown model_type in x$data$model_args$model_type: ", mtype, call. = FALSE)
  }

  # ------------------------------------------------------------
  # 7) Store pointer + index
  # ------------------------------------------------------------
  x$data$model_ptr   <- op
  x$data$model_index <- idx

  # ------------------------------------------------------------
  # 8) Apply stored targets (if any)
  # ------------------------------------------------------------
  if (!is.null(x$data$targets) &&
      inherits(x$data$targets, "data.frame") &&
      nrow(x$data$targets) > 0) {
    x <- .pa_apply_targets_if_present(x, allow_multiple_rows_per_feature = TRUE)
  }

  # ------------------------------------------------------------
  # 9) Store resolved objective metadata + snapshot
  # ------------------------------------------------------------
  x$data$model_args$modelsense   <- modelsense
  x$data$model_args$objective_id <- objective_id

  x <- .pa_refresh_model_snapshot(x)

  x$data$has_model <- TRUE
  x
}
