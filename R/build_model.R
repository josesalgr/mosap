#' @include internal.R
#'
#' @title Build optimization model from Data
#'
#' @description
#' Materializes (builds) the optimization model using the current state of the `Data` object:
#' prepared data tables, stored objective settings, and stored constraints (e.g., targets).
#'
#' @param x Data object (class "Data") created with inputData()/inputDataSpatial().
#'
#' @return Updated `Data` object with model pointer and model snapshot.
#' @keywords internal
.pa_build_model <- function(x) {

  stopifnot(inherits(x, "Data"))
  stopifnot(!is.null(x$data$pu))
  stopifnot(!is.null(x$data$dist_features))

  # ------------------------------------------------------------
  # local helpers (keep minimal here; you can move to internal.R)
  # ------------------------------------------------------------
  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)
  .pa_has_rows <- function(df) !is.null(df) && inherits(df, "data.frame") && nrow(df) > 0

  .pa_log_init <- function(x) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$build_log <- x$data$meta$build_log %||% list(
      dropped_status3 = 0L,
      dropped_missing_internal = 0L,
      dropped_bad_cost = 0L,
      filtered_effects_rows = 0L,
      filtered_profit_rows = 0L
    )
    x
  }
  .pa_log_add <- function(x, key, n) {
    x$data$meta$build_log[[key]] <- as.integer(x$data$meta$build_log[[key]] %||% 0L) + as.integer(n)
    x
  }
  .pa_emit_build_warnings <- function(x) {
    log <- x$data$meta$build_log %||% list()
    msgs <- character(0)

    if (isTRUE(log$dropped_status3 > 0L)) {
      msgs <- c(msgs, paste0("Dropped ", log$dropped_status3, " dist_actions rows with status==3 (locked-out)."))
    }
    if (isTRUE(log$dropped_missing_internal > 0L)) {
      msgs <- c(msgs, paste0("Dropped ", log$dropped_missing_internal, " dist_actions rows with missing internal_pu/internal_action."))
    }
    if (isTRUE(log$dropped_bad_cost > 0L)) {
      msgs <- c(msgs, paste0("Dropped ", log$dropped_bad_cost, " dist_actions rows with missing/non-finite cost."))
    }
    if (isTRUE(log$filtered_effects_rows > 0L)) {
      msgs <- c(msgs, paste0("Filtered ", log$filtered_effects_rows, " dist_effects rows not matching feasible dist_actions."))
    }
    if (isTRUE(log$filtered_profit_rows > 0L)) {
      msgs <- c(msgs, paste0("Filtered ", log$filtered_profit_rows, " dist_profit rows not matching feasible dist_actions."))
    }

    if (length(msgs) > 0) {
      warning(paste(msgs, collapse = "\n"), call. = FALSE, immediate. = TRUE)
    }
    x
  }

  .pa_check_single_objective <- function(args) {
    if (isTRUE(args$mo_mode)) return(invisible(TRUE))

    if (is.null(args)) return(invisible(TRUE))

    if (!is.null(args$objectives)) {
      nobj <- if (is.data.frame(args$objectives)) nrow(args$objectives) else length(args$objectives)
      if (is.finite(nobj) && nobj > 1) {
        .pa_abort(
          "Multiple objectives detected (", nobj, "). prioriactions builds a single-objective MILP.\n",
          "Use a multiobjective wrapper (e.g., prioriactionsMO) for epsilon-constraint/AUGMECON/interactive methods."
        )
      }
    }

    if (is.vector(args$model_type) && length(args$model_type) > 1) {
      .pa_abort(
        "Multiple model_type values detected: ",
        paste(args$model_type, collapse = ", "),
        ". Choose exactly one objective."
      )
    }

    invisible(TRUE)
  }

  # ------------------------------------------------------------
  # init build log and args
  # ------------------------------------------------------------
  x <- .pa_log_init(x)

  input_format <- x$data$meta$input_format %||% "new"

  if (is.null(x$data$model_args)) x$data$model_args <- list()
  .pa_check_single_objective(x$data$model_args)

  # objective required for new pipeline (legacy can default)
  if (is.null(x$data$model_args$model_type)) {
    if (identical(input_format, "legacy")) {
      x$data$model_args$model_type <- "minimizeCosts"
    } else {
      .pa_abort(
        "No objective configured. Use add_objective_*() (new pipeline) or call problem() (legacy pipeline)."
      )
    }
  }

  # defaults for later printing / adapters
  if (is.null(x$data$model_args$budget))   x$data$model_args$budget   <- 0
  if (is.null(x$data$model_args$blm))      x$data$model_args$blm      <- 0
  if (is.null(x$data$model_args$curve))    x$data$model_args$curve    <- 1L
  if (is.null(x$data$model_args$segments)) x$data$model_args$segments <- 3L

  # ---- NEW: initialize + set needs flags (single-objective safe, MO-safe)
  x <- .pa_build_model_set_needs_from_objective(x)

  # ------------------------------------------------------------
  # legacy adapters
  # ------------------------------------------------------------
  if (identical(input_format, "legacy")) {
    x <- .pa_build_model_apply_legacy_adapters(x)
  }

  # ------------------------------------------------------------
  # ensure tables exist (allow "no actions" case)
  # ------------------------------------------------------------
  x <- .pa_build_model_ensure_tables(x)

  # ------------------------------------------------------------
  # post-adapter validation
  # ------------------------------------------------------------
  x <- .pa_build_model_validate_pipeline_state(x, input_format = input_format)

  # ------------------------------------------------------------
  # prepare model-ready tables (filters + joins)
  # ------------------------------------------------------------
  x <- .pa_build_model_prepare_tables(x)

  # ------------------------------------------------------------
  # early validation: objective dependencies
  # ------------------------------------------------------------
  x <- .pa_build_model_validate_objective_requirements(x)

  # ------------------------------------------------------------
  # build C++ model pointer + base vars + minimal constraints
  # ------------------------------------------------------------
  x <- .pa_build_model_build_cpp_core(x)

  # ------------------------------------------------------------
  # prepare auxiliary variables/constraints required by needs
  # ------------------------------------------------------------
  x <- .pa_build_model_prepare_needs_cpp(x)

  # ------------------------------------------------------------
  # objective (C++ side)
  # ------------------------------------------------------------
  x <- .pa_build_model_set_objective_cpp(x)

  # ------------------------------------------------------------
  # apply stored targets/constraints
  # ------------------------------------------------------------
  x <- .pa_build_model_apply_constraints(x)

  # ------------------------------------------------------------
  # store metadata + snapshot + warnings
  # ------------------------------------------------------------
  x <- .pa_build_model_finalize(x)
  x <- .pa_emit_build_warnings(x)

  x$data$has_model <- TRUE
  x
}

# -------------------------------------------------------------------------
# Helpers (recommended to live in internal.R, but ok here as noRd)
# -------------------------------------------------------------------------

.pa_build_model_apply_legacy_adapters <- function(x) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)

  has_threats_data <- !is.null(x$data$dist_threats) &&
    inherits(x$data$dist_threats, "data.frame") &&
    nrow(x$data$dist_threats) > 0

  has_user_actions <- !is.null(x$data$actions) &&
    inherits(x$data$actions, "data.frame") &&
    nrow(x$data$actions) > 0

  has_user_dist_actions <- !is.null(x$data$dist_actions) &&
    inherits(x$data$dist_actions, "data.frame") &&
    nrow(x$data$dist_actions) > 0

  # (A) Default actions from threats ONLY if user didn't define actions
  if (has_threats_data && !(has_user_actions || has_user_dist_actions)) {
    if (!exists(".pa_add_actions_default_from_threats", mode = "function")) {
      .pa_abort(
        "Legacy input detected (threats/dist_threats), but .pa_add_actions_default_from_threats() is not implemented."
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

  # (B) Default targets from features whenever legacy targets exist and targets table is empty
  targets_empty <- is.null(x$data$targets) ||
    !inherits(x$data$targets, "data.frame") ||
    nrow(x$data$targets) == 0

  has_legacy_targets_in_features <- !is.null(x$data$features) &&
    inherits(x$data$features, "data.frame") &&
    ("target_recovery" %in% names(x$data$features) || "target_conservation" %in% names(x$data$features))

  if (targets_empty && has_legacy_targets_in_features) {
    if (!exists(".pa_targets_from_features_legacy", mode = "function")) {
      .pa_abort(
        "Legacy targets exist in features, but .pa_targets_from_features_legacy() is not implemented."
      )
    }
    x <- .pa_targets_from_features_legacy(x)
  }


  # Warn once if threats exist but still no actions
  has_actions_data <- !is.null(x$data$dist_actions) &&
    inherits(x$data$dist_actions, "data.frame") &&
    nrow(x$data$dist_actions) > 0

  if (has_threats_data && !has_actions_data) {
    warning(
      "Legacy threats detected but no actions were materialized. Model will be built without actions (likely unintended).",
      call. = FALSE, immediate. = TRUE
    )
  }

  x
}

.pa_build_model_ensure_tables <- function(x) {

  if (is.null(x$data$dist_actions)) {
    x$data$dist_actions <- data.frame(
      pu = integer(0),
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
  x
}

.pa_build_model_validate_pipeline_state <- function(x, input_format) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)

  mtype_check <- x$data$model_args$model_type %||% "minimizeCosts"

  if (!identical(input_format, "legacy") && identical(mtype_check, "minimizeCosts")) {
    if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
      .pa_abort(
        "model_type='minimizeCosts' requires targets, but x$data$targets is empty.\n",
        "Run add_target_*() before solve()."
      )
    }
  }

  x
}

.pa_build_model_prepare_tables <- function(x) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)
  .has_rows <- function(df) !is.null(df) && inherits(df, "data.frame") && nrow(df) > 0

  da <- x$data$dist_actions
  if (is.null(da)) da <- data.frame()

  if (!inherits(da, "data.frame")) {
    .pa_abort("dist_actions must be a data.frame (or NULL).")
  }

  n0 <- nrow(da)

  if (n0 > 0) {

    # status filter
    if ("status" %in% names(da)) {
      keep <- da$status != 3L
      dropped <- sum(!keep, na.rm = TRUE)
      if (dropped > 0) x <- .pa_log_add(x, "dropped_status3", dropped)
      da <- da[keep, , drop = FALSE]
    }

    # internal ids
    if (all(c("internal_pu", "internal_action") %in% names(da))) {
      keep <- !is.na(da$internal_pu) & !is.na(da$internal_action)
      dropped <- sum(!keep)
      if (dropped > 0) x <- .pa_log_add(x, "dropped_missing_internal", dropped)
      da <- da[keep, , drop = FALSE]
    }

    # costs
    if ("cost" %in% names(da)) {
      da$cost <- as.numeric(da$cost)
      keep <- is.finite(da$cost) & !is.na(da$cost)
      dropped <- sum(!keep)
      if (dropped > 0) x <- .pa_log_add(x, "dropped_bad_cost", dropped)
      da <- da[keep, , drop = FALSE]
    }

    # normalize types
    if ("pu" %in% names(da)) da$pu <- as.integer(da$pu)
    if ("action" %in% names(da)) da$action <- as.character(da$action)
    if ("internal_pu" %in% names(da)) da$internal_pu <- as.integer(da$internal_pu)
    if ("internal_action" %in% names(da)) da$internal_action <- as.integer(da$internal_action)

    # deterministic order
    if (all(c("internal_pu","internal_action") %in% names(da)) && nrow(da) > 0) {
      da <- da[order(da$internal_pu, da$internal_action), , drop = FALSE]
    }
  }

  # internal_row
  da$internal_row <- seq_len(nrow(da))

  # validate internal id ranges (if present)
  if (.has_rows(da) && all(c("internal_pu","internal_action") %in% names(da))) {
    n_pu <- nrow(x$data$pu)
    if (any(da$internal_pu < 1L | da$internal_pu > n_pu, na.rm = TRUE)) {
      .pa_abort("dist_actions contains internal_pu indices out of range (1..n_pu).")
    }
    if (!is.null(x$data$actions) && inherits(x$data$actions, "data.frame") && nrow(x$data$actions) > 0) {
      n_a <- nrow(x$data$actions)
      if (any(da$internal_action < 1L | da$internal_action > n_a, na.rm = TRUE)) {
        .pa_abort("dist_actions contains internal_action indices out of range (1..n_actions).")
      }
    }
  }

  # duplicates check (pu, action)
  if (.has_rows(da) && all(c("pu","action") %in% names(da))) {
    key <- paste(da$pu, da$action, sep = "||")
    if (anyDuplicated(key)) {
      .pa_abort(
        "dist_actions has duplicated (pu, action) pairs. Please aggregate/resolve duplicates before building the model."
      )
    }
  }

  x$data$dist_actions_model <- da

  # dist_effects model-ready
  de <- x$data$dist_effects
  if (.has_rows(de) && .has_rows(da)) {

    n_before <- nrow(de)

    # tipos base
    if ("pu" %in% names(de))      de$pu      <- as.integer(de$pu)
    if ("action" %in% names(de))  de$action  <- as.character(de$action)
    if ("feature" %in% names(de)) de$feature <- as.integer(de$feature)

    # filtra a (pu, action) factibles
    de <- dplyr::inner_join(
      de,
      da[, c("pu", "action"), drop = FALSE],
      by = c("pu", "action")
    )

    n_after <- nrow(de)
    dropped <- n_before - n_after
    if (dropped > 0) x <- .pa_log_add(x, "filtered_effects_rows", dropped)

    # --- NEW: asegurar columnas internas (legacy-safe)
    pu_map <- x$data$pu[, c("id", "internal_id")]
    act_map <- x$data$actions[, c("id", "internal_id")]
    feat_map <- x$data$features[, c("id", "internal_id")]

    # internal_pu
    if (!("internal_pu" %in% names(de))) {
      de$internal_pu <- pu_map$internal_id[match(de$pu, pu_map$id)]
    } else {
      de$internal_pu <- as.integer(de$internal_pu)
    }

    # internal_action
    if (!("internal_action" %in% names(de))) {
      de$internal_action <- act_map$internal_id[match(de$action, act_map$id)]
    } else {
      de$internal_action <- as.integer(de$internal_action)
    }

    # internal_feature
    if (!("internal_feature" %in% names(de))) {
      # ojo: algunas tablas legacy pueden usar "feature" como id
      if (!("feature" %in% names(de))) {
        .pa_abort("dist_effects must contain column 'feature' (feature id) to derive internal_feature in legacy mode.")
      }
      de$internal_feature <- feat_map$internal_id[match(de$feature, feat_map$id)]
    } else {
      de$internal_feature <- as.integer(de$internal_feature)
    }

    # chequeos (si hay NA, es que hay ids que no matchean con pu/actions/features)
    if (anyNA(de$internal_pu)) {
      .pa_abort("dist_effects has pu ids not found in x$data$pu$id (cannot derive internal_pu).")
    }
    if (anyNA(de$internal_action)) {
      .pa_abort("dist_effects has action ids not found in x$data$actions$id (cannot derive internal_action).")
    }
    if (anyNA(de$internal_feature)) {
      .pa_abort("dist_effects has feature ids not found in x$data$features$id (cannot derive internal_feature).")
    }
  }
  x$data$dist_effects_model <- de



  # dist_benefit_model derived (now has internal_* too)
  db <- NULL
  if (.has_rows(de) && "benefit" %in% names(de)) {
    db <- de
    db$benefit <- as.numeric(db$benefit)
  }
  x$data$dist_benefit_model <- db

  # dist_profit model-ready (MUST carry internal_pu/internal_action)
  # dist_profit model-ready (MUST carry internal_pu/internal_action)
  dp <- x$data$dist_profit
  if (.has_rows(dp) && .has_rows(da)) {

    n_before <- nrow(dp)

    # tipos base
    if ("pu" %in% names(dp)) dp$pu <- as.integer(dp$pu)
    if ("action" %in% names(dp)) dp$action <- as.character(dp$action)

    # filtra a (pu, action) factibles
    dp <- dplyr::inner_join(
      dp,
      da[, c("pu", "action"), drop = FALSE],
      by = c("pu", "action")
    )

    n_after <- nrow(dp)
    dropped <- n_before - n_after
    if (dropped > 0) x <- .pa_log_add(x, "filtered_profit_rows", dropped)

    # --- NEW: asegurar columnas internas (igual que en dist_effects)
    pu_map  <- x$data$pu[, c("id", "internal_id")]
    act_map <- x$data$actions[, c("id", "internal_id")]

    pu_map$id  <- as.integer(pu_map$id)
    act_map$id <- as.character(act_map$id)

    if (!("internal_pu" %in% names(dp))) {
      dp$internal_pu <- pu_map$internal_id[match(dp$pu, pu_map$id)]
    } else {
      dp$internal_pu <- as.integer(dp$internal_pu)
    }

    if (!("internal_action" %in% names(dp))) {
      dp$internal_action <- act_map$internal_id[match(dp$action, act_map$id)]
    } else {
      dp$internal_action <- as.integer(dp$internal_action)
    }

    if (anyNA(dp$internal_pu)) {
      .pa_abort("dist_profit has pu ids not found in x$data$pu$id (cannot derive internal_pu).")
    }
    if (anyNA(dp$internal_action)) {
      .pa_abort("dist_profit has action ids not found in x$data$actions$id (cannot derive internal_action).")
    }

    # opcional: orden determinista
    dp <- dp[order(dp$internal_pu, dp$internal_action), , drop = FALSE]
  }

  x$data$dist_profit_model <- dp


  x
}

.pa_build_model_validate_objective_requirements <- function(x) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)
  .has_rows <- function(df) !is.null(df) && inherits(df, "data.frame") && nrow(df) > 0

  args  <- x$data$model_args %||% list()
  mtype <- args$model_type %||% "minimizeCosts"
  oargs <- args$objective_args %||% list()

  has_actions_model <- .has_rows(x$data$dist_actions_model)

  needs_actions <- mtype %in% c("maximizeBenefits", "minimizeLosses", "maximizeProfit", "maximizeNetProfit",
                                "minimizeActionFragmentation", "minimizeInterventionFragmentation")

  if (needs_actions && !isTRUE(has_actions_model)) {
    .pa_abort(
      "Objective '", mtype, "' requires actions, but no actions are available.\n",
      "Run add_actions() (and ensure feasible dist_actions rows exist) before solve()."
    )
  }

  if (identical(mtype, "maximizeBenefits")) {
    if (!.has_rows(x$data$dist_effects_model)) {
      .pa_abort(
        "Objective 'maximizeBenefits' requires effects/benefits, but dist_effects is empty.\n",
        "Run add_benefits() / add_effects() after add_actions()."
      )
    }
    bcol <- as.character(oargs$benefit_col %||% "benefit")[1]
    if (!(bcol %in% names(x$data$dist_effects_model))) {
      .pa_abort(
        "Objective 'maximizeBenefits' requires column '", bcol, "' in dist_effects."
      )
    }
  }

  if (identical(mtype, "minimizeLosses")) {
    if (!.has_rows(x$data$dist_effects_model)) {
      .pa_abort(
        "Objective 'minimizeLosses' requires effects/losses, but dist_effects is empty.\n",
        "Run add_losses() / add_effects() after add_actions()."
      )
    }
    lcol <- as.character(oargs$loss_col %||% "loss")[1]
    if (!(lcol %in% names(x$data$dist_effects_model))) {
      .pa_abort(
        "Objective 'minimizeLosses' requires column '", lcol, "' in dist_effects."
      )
    }
  }

  if (mtype %in% c("maximizeProfit", "maximizeNetProfit")) {
    if (!.has_rows(x$data$dist_profit_model)) {
      .pa_abort(
        "Objective '", mtype, "' requires dist_profit, but dist_profit is empty.\n",
        "Run add_profit() after add_actions()."
      )
    }
    pcol <- as.character(oargs$profit_col %||% "profit")[1]
    if (!(pcol %in% names(x$data$dist_profit_model))) {
      .pa_abort(
        "Objective '", mtype, "' requires column '", pcol, "' in dist_profit."
      )
    }
  }

  # fragmentation objectives: require spatial relation
  if (mtype %in% c("minimizeFragmentation", "minimizeActionFragmentation", "minimizeInterventionFragmentation")) {

    rel_name <- as.character(oargs$relation_name %||% "boundary")[1]
    rels <- x$data$spatial_relations

    if (is.null(rels) || !is.list(rels) || is.null(rels[[rel_name]])) {
      .pa_abort(
        "Objective '", mtype, "' requires a spatial relation named '", rel_name, "'.\n",
        "But x$data$spatial_relations[['", rel_name, "']] is missing."
      )
    }

    rel <- rels[[rel_name]]
    if (!inherits(rel, "data.frame") || nrow(rel) == 0) {
      .pa_abort("Spatial relation '", rel_name, "' exists but is empty or not a data.frame.")
    }

    need_cols <- c("internal_pu1", "internal_pu2", "weight")
    if (!all(need_cols %in% names(rel))) {
      .pa_abort(
        "Spatial relation '", rel_name, "' must contain columns: ",
        paste(need_cols, collapse = ", "), "."
      )
    }

    n_pu <- nrow(x$data$pu)
    if (anyNA(rel$internal_pu1) || anyNA(rel$internal_pu2)) {
      .pa_abort("Spatial relation '", rel_name, "' contains NA in internal_pu1/internal_pu2.")
    }
    if (any(rel$internal_pu1 < 1L | rel$internal_pu1 > n_pu) ||
        any(rel$internal_pu2 < 1L | rel$internal_pu2 > n_pu)) {
      .pa_abort("Spatial relation '", rel_name, "' has internal PU indices out of range (1..n_pu).")
    }

    ww <- as.numeric(rel$weight)
    if (any(!is.finite(ww)) || any(ww < 0)) {
      .pa_abort("Spatial relation '", rel_name, "' has non-finite or negative weights.")
    }

    if (identical(mtype, "minimizeActionFragmentation")) {
      if (is.null(x$data$actions) || !inherits(x$data$actions, "data.frame") || nrow(x$data$actions) == 0) {
        .pa_abort("Objective 'minimizeActionFragmentation' requires x$data$actions to exist (non-empty).")
      }
      if (!is.null(oargs$actions_to_use)) {
        a <- as.integer(oargs$actions_to_use)
        if (anyNA(a)) .pa_abort("actions_to_use contains NA.")
        if (any(a < 1L | a > nrow(x$data$actions))) {
          .pa_abort("actions_to_use has internal_action ids out of range (1..n_actions).")
        }
      }
      if (!is.null(oargs$action_weights)) {
        aw <- as.numeric(oargs$action_weights)
        if (any(!is.finite(aw)) || any(aw < 0)) {
          .pa_abort("action_weights must be finite and >= 0.")
        }
      }
    }
  }

  x
}

.pa_build_model_build_cpp_core <- function(x) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)

  if (!exists("rcpp_new_optimization_problem", mode = "function")) {
    .pa_abort("Missing rcpp_new_optimization_problem() in the package.")
  }
  if (!exists("rcpp_add_base_variables", mode = "function")) {
    .pa_abort("Missing rcpp_add_base_variables() in the package.")
  }

  # ---- NEW: read needs flags (default-safe)
  args  <- x$data$model_args %||% list()
  needs <- args$needs %||% list()
  need_z <- isTRUE(needs$z)

  op <- rcpp_new_optimization_problem()

  # registry placeholder for future MO updates (constraint/objective IDs)
  x$data$model_registry <- list(
    cons = list(),
    vars = list(),
    obj_templates = list(),
    objective = list()
  )

  idx <- rcpp_add_base_variables(
    op,
    pu_data            = x$data$pu,
    dist_actions_data  = x$data$dist_actions_model,
    dist_features_data = x$data$dist_features,
    add_z              = need_z            # <- NEW
  )

  # structural constraints
  if (!is.null(x$data$dist_actions_model) && nrow(x$data$dist_actions_model) > 0) {
    res_locks <- rcpp_add_linking_x_le_w(op, x$data$dist_actions_model)
    x$data$model_registry$cons$x_le_w <- res_locks
  }

  # ---- NEW: only add z <= w if z exists
  if (isTRUE(need_z) && exists("rcpp_add_linking_z_le_w", mode = "function")) {
    res_locks <- rcpp_add_linking_z_le_w(op, x$data$dist_features)
    x$data$model_registry$cons$z_le_w <- res_locks
  }

  if (exists("rcpp_add_pu_locks", mode = "function")) {
    res_locks <- rcpp_add_pu_locks(op, x$data$pu)
    x$data$model_registry$cons$pu_locks <- res_locks
  }

  if (!is.null(x$data$dist_actions_model) && nrow(x$data$dist_actions_model) > 0 &&
      exists("rcpp_add_action_locks", mode = "function")) {

    res_locks <- rcpp_add_action_locks(op, x$data$dist_actions_model)
    x$data$model_registry$cons$action_locks <- res_locks
  }

  x$data$model_ptr   <- op
  x$data$model_index <- idx

  x
}


.pa_build_model_set_objective_cpp <- function(x) {

  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)

  .pa_prepare_relation_model <- function(rel) {
    rel <- rel[, c(
      "internal_pu1","internal_pu2","weight",
      intersect(names(rel), c("distance","source","relation_name"))
    ), drop = FALSE]
    rel$internal_pu1 <- as.integer(rel$internal_pu1)
    rel$internal_pu2 <- as.integer(rel$internal_pu2)
    rel$weight <- as.numeric(rel$weight)
    rel <- rel[order(rel$internal_pu1, rel$internal_pu2), , drop = FALSE]
    rel$internal_edge <- seq_len(nrow(rel))
    rel
  }

  op <- x$data$model_ptr
  args  <- x$data$model_args %||% list()
  mtype <- args$model_type %||% "minimizeCosts"
  oargs <- args$objective_args %||% list()

  if (identical(mtype, "minimizeCosts")) {

    if (!exists("rcpp_set_objective_min_cost", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_min_cost() in the package.")
    }

    res <- rcpp_set_objective_min_cost(
      op,
      pu_data = x$data$pu,
      dist_actions_data = x$data$dist_actions_model,
      include_pu_cost = isTRUE(oargs$include_pu_cost %||% TRUE),
      include_action_cost = isTRUE(oargs$include_action_cost %||% TRUE)
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "min_cost"
    modelsense   <- "min"

  } else if (identical(mtype, "maximizeBenefits")) {

    if (!exists("rcpp_set_objective_max_benefit", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_max_benefit() in the package.")
    }

    bcol <- as.character(oargs$benefit_col %||% "benefit")[1]
    res <- rcpp_set_objective_max_benefit(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_benefit_data = x$data$dist_benefit_model,
      benefit_col = bcol
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "max_benefit"
    modelsense   <- "max"

  } else if (identical(mtype, "minimizeLosses")) {

    if (!exists("rcpp_set_objective_min_loss", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_min_loss() in the package.")
    }

    lcol <- as.character(oargs$loss_col %||% "loss")[1]
    res <- rcpp_set_objective_min_loss(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_effects_data = x$data$dist_effects_model,
      loss_col = lcol
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "min_loss"
    modelsense   <- "min"

  } else if (identical(mtype, "maximizeProfit")) {

    if (!exists("rcpp_set_objective_max_profit", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_max_profit() in the package.")
    }

    res <- rcpp_set_objective_max_profit(
      op,
      dist_actions_data = x$data$dist_actions_model,
      dist_profit_data  = x$data$dist_profit_model,
      profit_col = as.character(oargs$profit_col %||% "profit")[1]
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "max_profit"
    modelsense   <- "max"

  } else if (identical(mtype, "maximizeNetProfit")) {

    if (!exists("rcpp_set_objective_max_net_profit", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_max_net_profit() in the package.")
    }

    res <- rcpp_set_objective_max_net_profit(
      op,
      pu_data = x$data$pu,
      dist_actions_data = x$data$dist_actions_model,
      dist_profit_data  = x$data$dist_profit_model,
      profit_col = as.character(oargs$profit_col %||% "profit")[1],
      include_pu_cost = isTRUE(oargs$include_pu_cost %||% TRUE),
      include_action_cost = isTRUE(oargs$include_action_cost %||% TRUE)
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "max_net_profit"
    modelsense   <- "max"

  } else if (identical(mtype, "maximizeRepresentation")) {

    if (!exists("rcpp_set_objective_max_representation", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_max_representation() in the package.")
    }

    acol <- as.character(oargs$amount_col %||% "amount")[1]

    # ---- NEW: subset features (ids -> internal_feature)
    feats <- oargs$features %||% NULL
    feats_internal <- integer(0)

    if (!is.null(feats)) {
      if (is.null(x$data$features) || !inherits(x$data$features, "data.frame") || nrow(x$data$features) == 0) {
        .pa_abort("maximizeRepresentation: x$data$features is missing/empty; cannot map `features` subset.")
      }
      # map feature ids to internal_id (robusto a id numeric/char)
      m <- match(feats, x$data$features$id)
      if (anyNA(m)) {
        .pa_abort(
          "maximizeRepresentation: some `features` were not found in x$data$features$id: ",
          paste(feats[is.na(m)], collapse = ", ")
        )
      }
      feats_internal <- as.integer(x$data$features$internal_id[m])
    }

    res <- rcpp_set_objective_max_representation(
      op,
      dist_features_data = x$data$dist_features,
      amount_col = acol,
      features_to_use = feats_internal,           # NEW
      internal_feature_col = "internal_feature"   # NEW (si quieres hardcodear, puedes omitirlo)
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "max_representation"
    modelsense   <- "max"


  } else if (identical(mtype, "minimizeFragmentation")) {

    if (!exists("rcpp_set_objective_min_fragmentation", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_min_fragmentation() in the package.")
    }

    rel_name <- as.character(oargs$relation_name %||% "boundary")[1]
    rel <- x$data$spatial_relations[[rel_name]]
    rel_model <- x$data$spatial_relations_model[[rel_name]] %||% .pa_prepare_relation_model(x$data$spatial_relations[[rel_name]])
    x$data$spatial_relations_model[[rel_name]] <- rel_model


    res <- rcpp_set_objective_min_fragmentation(
      op,
      pu_data = x$data$pu,
      relation_data = rel_model,
      weight_multiplier = as.numeric(oargs$weight_multiplier %||% 1)[1]
    )

    x$data$model_registry$objective$cpp <- res
    objective_id <- "min_fragmentation"
    modelsense   <- "min"

  } else if (identical(mtype, "minimizeActionFragmentation")) {

    if (!exists("rcpp_set_objective_min_fragmentation_actions_by_action", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_min_fragmentation_actions_by_action() in the package.")
    }

    rel_name <- as.character(oargs$relation_name %||% "boundary")[1]
    rel <- x$data$spatial_relations[[rel_name]]
    rel_model <- .pa_prepare_relation_model(rel)

    x$data$spatial_relations_model <- x$data$spatial_relations_model %||% list()
    x$data$spatial_relations_model[[rel_name]] <- rel_model

    aw_vec <- .pa_action_weights_vector(
      actions_df = x$data$actions,
      action_weights = oargs$action_weights %||% NULL,
      subset_actions = oargs$actions_to_use %||% oargs$actions %||% NULL
    )

    subset_actions <- oargs$actions_to_use %||% oargs$actions %||% NULL

    aw_vec <- .pa_action_weights_vector(
      actions_df = x$data$actions,
      action_weights = oargs$action_weights %||% NULL,
      subset_actions = subset_actions
    )

    res <- rcpp_set_objective_min_fragmentation_actions_by_action(
      op,
      dist_actions_data = x$data$dist_actions_model,
      relation_data = rel_model,
      actions_to_use = NULL,          # <- clave: evitar mismatch
      action_weights = aw_vec,        # <- largo n_actions, ya incluye ceros fuera del subset
      weight_multiplier = as.numeric(oargs$weight_multiplier %||% 1)[1]
    )

    objective_id <- "min_action_fragmentation"
    modelsense   <- "min"
    x$data$model_registry$objective$cpp <- res


  } else if (identical(mtype, "minimizeInterventionFragmentation")) {

    if (!exists("rcpp_set_objective_min_fragmentation_interventions", mode = "function")) {
      .pa_abort("Missing rcpp_set_objective_min_fragmentation_interventions() in the package.")
    }

    rel_name <- as.character(oargs$relation_name %||% "boundary")[1]
    rel <- x$data$spatial_relations[[rel_name]]
    rel_model <- .pa_prepare_relation_model(rel)

    x$data$spatial_relations_model <- x$data$spatial_relations_model %||% list()
    x$data$spatial_relations_model[[rel_name]] <- rel_model

    res <- rcpp_set_objective_min_fragmentation_interventions(
      op,
      dist_actions_data = x$data$dist_actions_model,
      relation_data = rel_model,
      weight_multiplier = as.numeric(oargs$weight_multiplier %||% 1)[1]
    )

    objective_id <- "min_intervention_fragmentation"
    modelsense   <- "min"
    x$data$model_registry$objective$cpp <- res


  } else {
    .pa_abort("Unknown model_type in x$data$model_args$model_type: ", mtype)
  }

  x$data$model_args$modelsense   <- modelsense
  x$data$model_args$objective_id <- objective_id

  # future: store objective handle/id returned by C++ if you implement it
  x$data$model_registry$objective <- list(
    type = mtype,
    id = objective_id
  )

  x
}

.pa_build_model_apply_constraints <- function(x) {

  # targets...
  if (!is.null(x$data$targets) &&
      inherits(x$data$targets, "data.frame") &&
      nrow(x$data$targets) > 0) {

    if (!exists(".pa_apply_targets_if_present", mode = "function")) {
      stop(".pa_apply_targets_if_present() is missing.", call. = FALSE)
    }

    x <- .pa_apply_targets_if_present(x, allow_multiple_rows_per_feature = TRUE)
  }

  # NEW: per-PU action max constraint
  if (exists(".pa_apply_action_max_per_pu_if_present", mode = "function")) {
    x <- .pa_apply_action_max_per_pu_if_present(x)
  }

  x
}


.pa_build_model_finalize <- function(x) {

  if (!exists(".pa_refresh_model_snapshot", mode = "function")) {
    stop(".pa_refresh_model_snapshot() is missing.", call. = FALSE)
  }

  x <- .pa_refresh_model_snapshot(x)
  x
}
