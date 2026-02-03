#' @title Solve optimization model
#'
#' @description
#' Solves a model defined by a \code{Data} object. Solver configuration is read from
#' \code{x$data$solve_args} (typically set via \code{set_solver()} / \code{set_solver_*()}).
#'
#' @param x A \code{Data} object created with \code{inputData()} or \code{inputDataSpatial()}.
#' @param ... Optional legacy solver arguments (deprecated).
#'
#' @return A \code{Solution} object.
#' @export
solve <- function(x, ...) {
  UseMethod("solve")
}

#' @export
solve.Data <- function(x, ...) {

  assertthat::assert_that(inherits(x, "Data"))

  # ---- gather stored solve args (defaults + stored)
  dots <- list(...)

  legacy_keys <- c(
    "solver", "gap_limit", "time_limit", "solution_limit", "cores", "verbose",
    "name_output_file", "output_file", "solver_params"
  )

  unknown <- setdiff(names(dots), legacy_keys)
  if (length(unknown) > 0) {
    stop("Unknown argument(s) in solve(): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  used_legacy_args <- length(intersect(names(dots), legacy_keys)) > 0

  # consideramos “ya configurado” si solve_args existe y tiene al menos un campo no nulo
  has_stored_args <- !is.null(x$data$solve_args) &&
    length(x$data$solve_args) > 0 &&
    any(!vapply(x$data$solve_args, is.null, logical(1)))

  if (used_legacy_args && has_stored_args) {
    stop(
      "Solver options were provided twice: you already configured them via add_*_solver()/set_solver(), ",
      "and you also passed parameters to solve(). ",
      "Please remove arguments from solve() and keep only add_*_solver()/set_solver().",
      call. = FALSE
    )
  }

  # si NO hay configuración previa, aceptamos legacy args pero avisamos deprecación
  if (used_legacy_args && requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      when = "1.0.1",
      what = "solve()",
      with = "set_solver()",
      details = paste(
        "Legacy argument detected in the solve() function.",
        "New workflow example:",
        "inputData(...) %>% add_actions(...) %>% add_effects(...) %>% set_solver() %>% solve()"
      )
    )
  }

  sa <- do.call(.pa_get_solve_args, c(list(x = x), dots))

  solver         <- sa$solver %||% "auto"
  gap_limit      <- sa$gap_limit
  time_limit     <- sa$time_limit
  solution_limit <- sa$solution_limit
  cores          <- sa$cores
  verbose        <- sa$verbose
  name_output_file <- sa$name_output_file
  output_file    <- sa$output_file
  solver_params_user <- sa$solver_params %||% list()

  # ---- autodetect solver if requested
  available_gurobi   <- available_to_solve("gurobi")
  available_cplex    <- available_to_solve("cplex")
  available_symphony <- available_to_solve("symphony")
  available_cbc      <- available_to_solve("cbc")

  if (identical(solver, "auto") || identical(solver, "") || is.null(solver)) {
    if (requireNamespace("Rcplex", quietly = TRUE) && available_cplex) {
      solver <- "cplex"
    } else if (requireNamespace("gurobi", quietly = TRUE) && available_gurobi) {
      solver <- "gurobi"
    } else if (requireNamespace("rcbc", quietly = TRUE) && available_cbc) {
      solver <- "cbc"
    } else if (requireNamespace("Rsymphony", quietly = TRUE) && available_symphony) {
      solver <- "symphony"
    } else {
      stop("No optimization problem solvers available on this system.", call. = FALSE)
    }
  } else {
    if (!solver %in% c("gurobi", "cbc", "symphony", "cplex")) {
      stop("Solver not supported: ", solver, call. = FALSE)
    }
    if (identical(solver, "gurobi") && (!requireNamespace("gurobi", quietly = TRUE) || !available_gurobi)) {
      stop("Gurobi solver not available (package/license not found).", call. = FALSE)
    }
    if (identical(solver, "cbc") && (!requireNamespace("rcbc", quietly = TRUE) || !available_cbc)) {
      stop("CBC solver not available (rcbc not installed or CBC not available).", call. = FALSE)
    }
    if (identical(solver, "symphony") && (!requireNamespace("Rsymphony", quietly = TRUE) || !available_symphony)) {
      stop("SYMPHONY solver not available (Rsymphony not installed).", call. = FALSE)
    }
    if (identical(solver, "cplex") && (!requireNamespace("Rcplex", quietly = TRUE) || !available_cplex)) {
      stop("CPLEX solver not available (Rcplex not installed/licensed).", call. = FALSE)
    }
  }

  # ------------------------------------------------------------
  # Legacy + problem() preflight checks/adapters
  # ------------------------------------------------------------
  input_format <- x$data$meta$input_format %||% "new"
  has_model_now <- !is.null(x$data$model_ptr) && isTRUE(x$data$has_model)

  if (identical(input_format, "legacy") && has_model_now) {

    # If the user ran problem() too early (before actions were created),
    # we cannot fix it here (would require adding variables). Force rebuild.
    has_threats_data <- !is.null(x$data$dist_threats) && inherits(x$data$dist_threats, "data.frame") &&
      nrow(x$data$dist_threats) > 0

    # problem() stores model-ready tables; if they're missing, user likely modified after build
    dist_actions_in_model <- x$data$dist_actions_model
    dist_actions_has_rows <- !is.null(dist_actions_in_model) && inherits(dist_actions_in_model, "data.frame") &&
      nrow(dist_actions_in_model) > 0

    if (has_threats_data && !dist_actions_has_rows) {
      stop(
        "Legacy input detected and a model has already been built via problem(), ",
        "but no actions are present in the built model.\n",
        "This cannot be fixed inside solve() because actions require variables.\n",
        "Fix: build actions BEFORE problem(), e.g.\n",
        "  inputData(..., threats=..., dist_threats=...) %>% add_actions() %>% problem() %>% add_*_solver() %>% solve()\n",
        "or simply avoid calling problem() until after actions/targets are configured.",
        call. = FALSE
      )
    }

    # If targets were not configured but legacy features have target_* columns,
    # we CAN derive targets and apply them to the existing model (adds constraints).
    mt <- x$data$model_args$model_type %||% NA_character_

    need_targets <- identical(mt, "minimizeCosts") &&
      (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0)

    has_legacy_targets_in_features <- !is.null(x$data$features) &&
      inherits(x$data$features, "data.frame") &&
      ("target_recovery" %in% names(x$data$features) || "target_conservation" %in% names(x$data$features))

    if (need_targets && has_legacy_targets_in_features) {
      if (!exists(".pa_targets_from_features_legacy", mode = "function")) {
        stop(
          "model_type='minimizeCosts' requires targets. Legacy targets exist in features, ",
          "but .pa_targets_from_features_legacy() is not implemented.",
          call. = FALSE
        )
      }
      x <- .pa_targets_from_features_legacy(x)

      if (exists(".pa_apply_targets_if_present", mode = "function")) {
        x <- .pa_apply_targets_if_present(x, allow_multiple_rows_per_feature = TRUE)
      } else {
        stop("Internal error: .pa_apply_targets_if_present() not found.", call. = FALSE)
      }
    }

    # Objective-specific checks (legacy + problem)
    if (identical(mt, "maximizeBenefits")) {
      if (is.null(x$data$dist_benefit) || !inherits(x$data$dist_benefit, "data.frame") || nrow(x$data$dist_benefit) == 0) {
        stop(
          "Legacy + problem(): model_type='maximizeBenefits' requires dist_benefit. ",
          "Run add_benefits()/add_effects() (or equivalent) BEFORE problem().",
          call. = FALSE
        )
      }
    }

    if (identical(mt, "minimizeCosts")) {
      if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
        stop(
          "Legacy + problem(): model_type='minimizeCosts' requires targets. ",
          "Use add_target_*() before problem(), or provide features$target_recovery so targets can be derived.",
          call. = FALSE
        )
      }
    }
  }

  used_problem <- isTRUE(x$data$meta$used_problem) ||
    (!is.null(x$data$model_args$configured_by) && identical(x$data$model_args$configured_by, "problem()")) ||
    (!is.null(x$data$model_spec$configured_by) && identical(x$data$model_spec$configured_by, "problem()"))

  # si hay model_spec pero no model_args, sincroniza (robusto)
  has_spec <- !is.null(x$data$model_spec) && is.list(x$data$model_spec) && length(x$data$model_spec) > 0
  has_args <- !is.null(x$data$model_args) && is.list(x$data$model_args) && length(x$data$model_args) > 0
  if (has_spec && !has_args) x$data$model_args <- x$data$model_spec

  if (used_problem) {

    # 1) objective default (si por alguna razón quedó vacío)
    if (is.null(x$data$model_args$model_type)) {
      # problem() siempre debería setearlo, pero defensivo:
      x$data$model_args$model_type <- "minimizeCosts"
    }

    # 2) si maximizeBenefits, asegúrate de que el usuario tenga beneficios/effects
    if (identical(x$data$model_args$model_type, "maximizeBenefits")) {
      # builder ya hará stop si falta dist_effects_model, pero aquí puedes mejorar mensaje:
      has_effects <- !is.null(x$data$dist_effects) && inherits(x$data$dist_effects, "data.frame") && nrow(x$data$dist_effects) > 0
      has_benefit <- has_effects && ("benefit" %in% names(x$data$dist_effects))
      if (!has_benefit) {
        stop(
          "problem(model_type='maximizeBenefits') was used, but no benefits were provided.\n",
          "Please run add_effects()/add_benefits() to create dist_effects with a 'benefit' column, ",
          "or switch to problem(model_type='minimizeCosts').",
          call. = FALSE
        )
      }
    }

    # 3) si minimizeCosts, targets: si no hay targets y es legacy, builder intentará adapter
    # aquí solo damos warning si NO hay posibilidad de targets legacy.
    if (identical(x$data$model_args$model_type, "minimizeCosts")) {
      has_targets <- !is.null(x$data$targets) && inherits(x$data$targets, "data.frame") && nrow(x$data$targets) > 0
      has_legacy_targets_in_features <- !is.null(x$data$features) &&
        inherits(x$data$features, "data.frame") &&
        (("target_recovery" %in% names(x$data$features)) || ("target_conservation" %in% names(x$data$features)))

      if (!has_targets && !has_legacy_targets_in_features) {
        # no detengas necesariamente (depende si tu min_cost requiere targets sí o sí),
        # pero al menos guía al usuario:
        warning(
          "problem(model_type='minimizeCosts') was used but no targets were found.\n",
          "Add targets with add_target_*() (new pipeline) or provide legacy target_* columns in features.",
          call. = FALSE, immediate. = TRUE
        )
      }
    }
  }

  # ---- ensure model is built
  if (is.null(x$data$model_ptr) || !isTRUE(x$data$has_model) || isTRUE(x$data$meta$model_dirty)) {
    x <- .pa_build_model(x)
    x$data$meta$model_dirty <- FALSE
  }

  # ---- model list from pointer
  model <- .pa_model_from_ptr(
    x$data$model_ptr,
    args = x$data$model_args %||% list(),
    drop_triplets = TRUE
  )

  # ---- APPLY SUPERSET RUNTIME UPDATES (solver-agnostic)
  model <- .pa_apply_runtime_updates_to_model(model, x)

  # ---- pack args into Solution metadata
  solve_args <- list(
    solver = solver,
    gap = gap_limit,
    timelimit = time_limit,
    cores = cores,
    verbose = verbose,
    solution_limit = solution_limit,
    name_output_file = name_output_file,
    output_file = output_file,
    solver_params = solver_params_user
  )

  # ------------------------------------------------------------
  # 1) Call solver -> return unified payload:
  #    objval, solvec, gap, status_code, runtime
  # ------------------------------------------------------------
  objval <- NA_real_
  solvec <- numeric(0)
  gap    <- NA_real_
  status_code <- 999L
  runtime <- NA_real_

  if (solver == "gurobi") {

    model$sense <- replace(model$sense, model$sense == "==", "=")
    model$lb <- model$bounds$lower$val
    model$ub <- model$bounds$upper$val

    params <- list(
      Threads = cores,
      LogToConsole = as.integer(verbose),
      NodefileStart = 0.5,
      MIPGap = gap_limit,
      TimeLimit = time_limit
    )
    if (isTRUE(output_file)) params$LogFile <- paste0(name_output_file, "_log.txt")
    if (isTRUE(solution_limit)) params$SolutionLimit <- 1

    if (!is.null(model$args$curve) && !is.null(model$args$segments) && model$args$curve != 1) {
      params$FuncPieces <- 1
      params$FuncPieceLength <- round(1 / as.numeric(model$args$segments), digits = 1)
    }

    params <- modifyList(params, solver_params_user)

    sol <- gurobi::gurobi(model, params)

    status_code <- dplyr::case_when(
      sol$status == "OPTIMAL" ~ 0L,
      sol$status %in% c("INF_OR_UNBD", "INFEASIBLE", "UNBOUNDED") ~ 1L,
      (sol$status == "TIME_LIMIT" && !is.null(sol$objval)) ~ 2L,
      (sol$status == "TIME_LIMIT" && is.null(sol$objval)) ~ 3L,
      sol$status == "SOLUTION_LIMIT" ~ 4L,
      TRUE ~ 999L
    )

    objval  <- sol$objval %||% NA_real_
    solvec  <- sol$x %||% numeric(0)
    gap     <- sol$mipgap %||% NA_real_
    runtime <- sol$runtime %||% NA_real_

  } else if (solver == "cbc") {

    # build row bounds from sense + rhs
    row_lb <- rep(-Inf, length(model$rhs))
    row_ub <- rep( Inf, length(model$rhs))

    ii_le <- which(model$sense == "<=")
    ii_ge <- which(model$sense == ">=")
    ii_eq <- which(model$sense == "==" | model$sense == "=")

    row_ub[ii_le] <- model$rhs[ii_le]
    row_lb[ii_ge] <- model$rhs[ii_ge]
    row_lb[ii_eq] <- model$rhs[ii_eq]
    row_ub[ii_eq] <- model$rhs[ii_eq]

    cbc_args <- list(
      threads = cores,
      log = as.integer(verbose),
      verbose = 15,
      ratio = gap_limit,
      sec = time_limit,
      timem = "elapsed",
      heuristicsOnOff = "on"
    )
    cbc_args <- modifyList(cbc_args, solver_params_user)

    rt <- system.time({
      sol_cbc <- rcbc::cbc_solve(
        obj = model$obj,
        mat = model$A,
        is_integer = ifelse(model$vtype == "B", TRUE, FALSE),
        row_ub = row_ub,
        row_lb = row_lb,
        col_lb = model$bounds$lower$val,
        col_ub = model$bounds$upper$val,
        max = ifelse(model$modelsense == "min", FALSE, TRUE),
        cbc_args = cbc_args
      )
    })

    runtime <- rt[[3]]

    status_cbc <- rcbc::solution_status(sol_cbc)
    status_code <- dplyr::case_when(
      status_cbc == "optimal" ~ 0L,
      status_cbc == "infeasible" ~ 1L,
      (status_cbc == "timelimit" && !is.null(sol_cbc$objective_value)) ~ 2L,
      (status_cbc == "timelimit" && is.null(sol_cbc$objective_value)) ~ 3L,
      TRUE ~ 999L
    )

    objval <- sol_cbc$objective_value %||% NA_real_
    solvec <- sol_cbc$column_solution %||% numeric(0)
    gap    <- if (isTRUE(status_code == 0L)) gap_limit else NA_real_


  } else if (solver == "cplex") {

    model$lb <- model$bounds$lower$val
    model$ub <- model$bounds$upper$val

    sense <- model$sense
    sense[sense == ">="] <- "G"
    sense[sense == "=="] <- "E"
    sense[sense == "<="] <- "L"

    params <- list(
      trace = as.integer(verbose),
      epgap = gap_limit,
      tilim = time_limit
    )
    params <- modifyList(params, solver_params_user)

    if (any(solution_limit, output_file)) {
      warning(
        "Options not available with cplex solver via this interface: solution_limit, output_file",
        call. = FALSE, immediate. = TRUE
      )
    }

    rt <- system.time({
      sol_cplex <- Rcplex::Rcplex(
        cvec = model$obj,
        Amat = model$A,
        bvec = model$rhs,
        lb = model$lb,
        ub = model$ub,
        objsense = model$modelsense,
        sense = sense,
        vtype = model$vtype,
        control = params
      )
    })

    runtime <- rt[[3]]

    status_code <- dplyr::case_when(
      sol_cplex$status %in% c(101L, 1L) ~ 0L,
      sol_cplex$status %in% c(2L, 3L, 103L, 118L) ~ 1L,
      sol_cplex$status == 107L ~ 2L,
      sol_cplex$status == 108L ~ 3L,
      sol_cplex$status == 232L ~ 4L,
      TRUE ~ 999L
    )

    objval <- sol_cplex$obj %||% NA_real_
    solvec <- sol_cplex$xopt %||% numeric(0)
    gap    <- 0

  } else if (solver == "symphony") {

    if (isTRUE(output_file)) {
      warning("It is not possible to export a solver log using symphony solver.", call. = FALSE, immediate. = TRUE)
    }

    max_flag <- ifelse(model$modelsense == "min", FALSE, TRUE)
    verbosity <- as.integer(verbose) - 2

    rt <- system.time({
      sol_sym <- Rsymphony::Rsymphony_solve_LP(
        obj = model$obj,
        mat = model$A,
        dir = model$sense,
        rhs = model$rhs,
        bounds = model$bounds,
        types = model$vtype,
        max = max_flag,
        gap_limit = gap_limit,
        time_limit = time_limit,
        verbosity = verbosity,
        first_feasible = solution_limit
      )
    })

    runtime <- rt[[3]]

    status_code <- dplyr::case_when(
      sol_sym$status %in% c(0L, 231L) ~ 0L,
      sol_sym$status %in% c(226L, 237L) ~ 1L,
      sol_sym$status %in% c(235L, 228L) ~ 2L,
      sol_sym$status == 232L ~ 4L,
      TRUE ~ 999L
    )

    objval <- sol_sym$objval %||% NA_real_
    solvec <- sol_sym$solution %||% numeric(0)
    gap    <- if (isTRUE(status_code == 0L)) gap_limit else NA_real_

  } else {
    stop("Internal error: unknown solver '", solver, "'.", call. = FALSE)
  }

  # ------------------------------------------------------------
  # Hard fail if solver returned no usable solution
  # ------------------------------------------------------------
  if (length(solvec) == 0L || all(is.na(solvec))) {
    msg <- paste0(
      "Solver returned an empty solution vector.\n",
      "solver: ", solver, "\n",
      "status_code: ", status_code, "\n",
      "objval: ", as.character(objval), "\n",
      "Possible causes: infeasible model, time limit before first feasible, or solver error.\n",
      "Tip: try increasing time_limit, relaxing constraints, or inspect solver logs (if available)."
    )
    stop(msg, call. = FALSE)
  }

  # Ensure solution length is consistent with model offsets
  n_pu_chk <- as.integer(model$n_pu %||% 0L)
  n_x_chk  <- as.integer(model$n_x  %||% 0L)
  n_z_chk  <- as.integer(model$n_z  %||% 0L)
  w0_chk <- as.integer(model$w_offset %||% 0L)
  x0_chk <- as.integer(model$x_offset %||% 0L)
  z0_chk <- as.integer(model$z_offset %||% 0L)

  needed_len <- max(
    w0_chk + n_pu_chk,
    x0_chk + n_x_chk,
    z0_chk + n_z_chk,
    0L
  )

  if (length(solvec) < needed_len) {
    stop(
      "Solver returned a solution vector of length ", length(solvec),
      " but the model requires at least ", needed_len, " variables ",
      "(based on offsets + variable counts). ",
      "This indicates a mismatch between the built model and the decoded metadata.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # 2) Minimal decoding using offsets (0-based in C++; +1 in R)
  # ------------------------------------------------------------
  n_pu <- as.integer(model$n_pu %||% 0L)
  n_x  <- as.integer(model$n_x  %||% 0L)
  n_z  <- as.integer(model$n_z  %||% 0L)

  w0 <- as.integer(model$w_offset %||% 0L)
  x0 <- as.integer(model$x_offset %||% 0L)
  z0 <- as.integer(model$z_offset %||% 0L)

  sol_monitoring <- if (n_pu > 0L && length(solvec) >= (w0 + n_pu)) {
    base::round(solvec[(w0 + 1L):(w0 + n_pu)])
  } else numeric(0)

  sol_actions <- if (n_x > 0L && length(solvec) >= (x0 + n_x)) {
    base::round(solvec[(x0 + 1L):(x0 + n_x)])
  } else numeric(0)

  sol_conservation <- if (n_z > 0L && length(solvec) >= (z0 + n_z)) {
    solvec[(z0 + 1L):(z0 + n_z)]
  } else numeric(0)

  # ------------------------------------------------------------
  # 3) Build human-readable tables (you implement this helper)
  # ------------------------------------------------------------
  tables <- .pa_extract_solution_tables(x, solvec)

  x$data$runtime_updates <- NULL

  # ------------------------------------------------------------
  # 4) Construct Solution ONCE
  # ------------------------------------------------------------
  s <- pproto(
    NULL, Solution,
    data = list(
      objval = objval,
      sol = solvec,
      gap = gap,
      status = as.integer(status_code),
      runtime = runtime,
      args = solve_args,
      sol_monitoring = sol_monitoring,
      sol_actions = sol_actions,
      sol_conservation = sol_conservation,
      tables = tables
    ),
    Data = x
  )

  s
}
