#' This is an exact copy of the prioritizr code ------------
#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the `...`.
#'
#' @param ... arguments that are not used.
#'
#' @return `logical` indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  "unused arguments"
}

#' Verify if assertion is met
#'
#' Verify if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it throws warnings and not errors.
#'
#' @param x `logical` condition.
#'
#' @return `logical` if assertion is met and a `warning` if it is not.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res)) {
    return(TRUE)
  }
  warning(res, immediate. = TRUE)
  FALSE
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x `object`.
#'
#' @return `character` object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
#' @noRd
repr_atomic <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0) {
    description <- paste0(" ", description)
  }
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' Create a new `pproto` object
#'
#' Construct a new object with `pproto`. This object system is inspired
#' from the `ggproto` system used in the `ggplot2` package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit `pproto` object to inherit from. If `NULL`, don"t
#'   inherit from any object.
#'
#' @param ... A list of members to add to the new `pproto` object.
#'
#' @examples
#' Adder <- pproto("Adder",
#'   x = 0,
#'   add = function(self, n) {
#'     self$x <- self$x + n
#'     self$x
#'   }
#' )
#'
#' Adder$add(10)
#' Adder$add(10)
#'
#' Abacus <- pproto("Abacus", Adder,
#'   subtract = function(self, n) {
#'     self$x <- self$x - n
#'     self$x
#'   }
#' )
#' Abacus$add(10)
#' Abacus$subtract(10)
#' @noRd
pproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  assertthat::assert_that(
    assertthat::is.string(`_class`) || is.null(`_class`),
    inherits(`_inherit`, "pproto") || is.null(`_inherit`)
  )
  # copy objects from one proto to another proto
  assign_fields <- function(p1, p2) {
    if (!inherits(p2, "proto")) {
      return()
    }
    for (i in p2$ls()) {
      if (inherits(p2[[i]], "proto")) {
        p1[[i]] <- proto::proto()
        class(p1[[i]]) <- class(p2[[i]])
        assign_fields(p1[[i]], p2[[i]])
      } else {
        p1[[i]] <- p2[[i]]
      }
    }
    assign_fields(p1, p2$.super)
  }
  # create new proto
  p <- proto::proto()
  if (!is.null(`_inherit`)) {
    # assign inherited members
    assign_fields(p, `_inherit`)
    # assign inherited classes
    class(p) <- class(`_inherit`)
  } else {
    # assign pproto class
    class(p) <- c("pproto", class(p))
  }
  # assign members to new proto
  assign_fields(p, proto::proto(...))
  # assign new class if specified
  if (!is.null(`_class`)) {
    class(p) <- c(`_class`, class(p))
  }
  # return value
  p
}


#' Get status
#'
#' Construct a new object with `pproto`. This object system is inspired
#' from the `ggproto` system used in the `ggplot2` package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit `pproto` object to inherit from. If `NULL`, don"t
#'   inherit from any object.
#'
#' @param ... A list of members to add to the new `pproto` object.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' ## get status of solution
#' getStatus(s)
#'

#' @noRd
getStatus <- function(x) {
  # x puede ser Solution o list con $data
  d <- if (!is.null(x$data)) x$data else x

  # acepta varias convenciones
  code <- d$status_code %||% d$status %||% NA_integer_
  code <- as.integer(code)[1]

  if (is.na(code)) return("unknown")

  switch(as.character(code),
         "0"   = "optimal",
         "1"   = "infeasible_or_unbounded",
         "2"   = "time_limit_feasible",
         "3"   = "time_limit_no_solution",
         "4"   = "solution_limit",
         "999" = "unknown",
         paste0("unknown(", code, ")")
  )
}


#' Check if solvers are working
#'
#' Provides the status of solver. Being TRUE if it's working fine and FALSE in otherwise.
#'
#' @param package `character` object. Posible values: "gurobi", "cplex", and "symphony".
#'
#' @examples
#' available_to_solve("cplex")
#'

#' @noRd
available_to_solve <- function(package = ""){

  # define primitive data
  nPlants     <- 1
  nWarehouses <- 1
  # Warehouse demand in thousands of units
  Demand      <- c(10)
  # Plant capacity in thousands of units
  Capacity    <- c(20)
  # Fixed costs for each plant
  FixedCosts  <- c(100)
  # Transportation costs per thousand units
  TransCosts  <- c(100)

  flowidx <- function(w, p) {nPlants * (w-1) + p}

   # Build model
  model <- list()
  model$modelname <- 'facility'
  model$modelsense <- 'min'

  # initialize data for variables
  model$lb       <- 0
  model$ub       <- c(rep(1, nPlants),   rep(Inf, nPlants * nWarehouses))
  model$vtype    <- c(rep('B', nPlants), rep('C', nPlants * nWarehouses))
  model$obj      <- c(FixedCosts, TransCosts)
  model$varnames <- c(paste0(rep('Open',nPlants),1:nPlants),
                      sprintf('Trans%d,%d',
                              c(mapply(rep,1:nWarehouses,nPlants)),
                              1:nPlants))

  # build production constraint matrix
  A1 <- Matrix::spMatrix(nPlants, nPlants, i = c(1:nPlants), j = (1:nPlants), x = -Capacity)
  A2 <- Matrix::spMatrix(nPlants, nPlants * nWarehouses,
                 i = c(mapply(rep, 1:nPlants, nWarehouses)),
                 j = mapply(flowidx,1:nWarehouses,c(mapply(rep,1:nPlants,nWarehouses))),
                 x = rep(1, nWarehouses * nPlants))
  A3 <- Matrix::spMatrix(nWarehouses, nPlants)
  A4 <- Matrix::spMatrix(nWarehouses, nPlants * nWarehouses,
                 i = c(mapply(rep, 1:nWarehouses, nPlants)),
                 j = mapply(flowidx,c(mapply(rep,1:nWarehouses,nPlants)),1:nPlants),
                 x = rep(1, nPlants * nWarehouses))
  model$A           <- rbind(cbind(A1, A2), cbind(A3, A4))
  model$rhs         <- c(rep(0, nPlants),   Demand)
  model$sense       <- c(rep('<=', nPlants), rep('==', nWarehouses))
  model$constrnames <- c(sprintf('Capacity%d',1:nPlants),
                         sprintf('Demand%d',1:nWarehouses))


  if(package == "gurobi"){
    # set parameters
    params <- list()
    params$TimeLimit <- 0.01
    params$LogToConsole <- 0
    model$sense <- replace(model$sense, model$sense == "==", "=")

    sol <- invisible(try(gurobi::gurobi(model, params), silent = TRUE))
  }
  else if(package == "cbc"){
    # set parameters
    cbc_args <- list()
    cbc_args$sec <- "0.01"
    cbc_args$log <- "0"

    constraints_minus_equal <- which(model$sense != "<=")
    constraints_plus_equal <- which(model$sense == "<=")
    row_ub <- model$rhs
    row_ub[constraints_minus_equal] <- Inf
    row_lb <- model$rhs
    row_lb[constraints_plus_equal] <- -Inf

    sol <- invisible(try(rcbc::cbc_solve(obj = model$obj,
                                         mat = model$A,
                                         is_integer = ifelse(model$vtype == "B", TRUE, FALSE),
                                         row_ub = row_ub,
                                         row_lb = row_lb,
                                         col_lb = rep(0, length(model$vtype)),
                                         col_ub = model$ub,
                                         max = ifelse(model$modelsense == "min", FALSE, TRUE),
                                         cbc_args = cbc_args), silent = TRUE))
  }
  else if(package == "cplex"){
    model$sense[model$sense == ">="] <- "G"
    model$sense[model$sense == "=="] <- "E"
    model$sense[model$sense == "<="] <- "L"

    # set parameters
    params <- list()
    params$tilim <- 0.01
    params$trace <- 0

    sol <- try(Rcplex::Rcplex(cvec = model$obj,
                              Amat = model$A,
                              bvec = model$rhs,
                              lb = model$lb,
                              ub = model$ub,
                              objsense = model$modelsense,
                              sense = model$sense,
                              vtype = model$vtype,
                              control = params),
               silent = TRUE)
  }
  else if(package == "symphony"){
    model$mat <- model$A
    model$dir <- model$sense
    model$max <- ifelse(model$modelsense == "min", FALSE, TRUE)
    model$types <- model$vtype

    sol <- invisible(try(Rsymphony::Rsymphony_solve_LP(model$obj,
                                             model$mat,
                                             model$dir,
                                             model$rhs,
                                             model$bounds,
                                             model$types,
                                             model$max,
                                             gap_limit = 100,
                                             time_limit = 0.01),
               silent = TRUE))

  }
  if(inherits(sol, "try-error")){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}



# -------------------------------------------------------------------------
# Internal helpers TARGETS
# -------------------------------------------------------------------------

.pa_parse_targets <- function(x, targets) {
  feats <- x$data$features
  if (is.null(feats) || nrow(feats) == 0) stop("x$data$features is missing/empty.", call. = FALSE)
  if (!("id" %in% names(feats))) stop("x$data$features must contain column 'id'.", call. = FALSE)

  feat_ids <- as.numeric(feats$id)
  feat_names <- if ("name" %in% names(feats)) as.character(feats$name) else rep(NA_character_, length(feat_ids))

  # helper: map a feature identifier (id or name) to numeric id
  .map_feature <- function(v) {
    if (is.factor(v)) v <- as.character(v)

    if (is.numeric(v) || is.integer(v)) {
      v <- as.numeric(v)
      bad <- v[!v %in% feat_ids]
      if (length(bad) > 0) stop("Unknown feature id(s): ", paste(unique(bad), collapse = ", "), call. = FALSE)
      return(v)
    }

    if (is.character(v)) {
      # if looks numeric, try as id first
      sup <- suppressWarnings(as.numeric(v))
      is_numlike <- !is.na(sup) & nzchar(v)
      out <- rep(NA_real_, length(v))

      if (any(is_numlike)) {
        vv <- sup[is_numlike]
        bad <- vv[!vv %in% feat_ids]
        if (length(bad) == 0) out[is_numlike] <- vv
      }

      # map remaining by name
      need_name <- is.na(out)
      if (any(need_name)) {
        if (!("name" %in% names(feats))) {
          stop("Targets reference feature names, but x$data$features has no 'name' column.", call. = FALSE)
        }
        m <- match(v[need_name], feat_names)
        bad <- v[need_name][is.na(m)]
        if (length(bad) > 0) {
          stop(
            "Unknown feature name(s): ", paste0("'", unique(bad), "'", collapse = ", "),
            ". Valid names include: ", paste0("'", head(feat_names, 10), "'", collapse = ", "),
            if (length(feat_names) > 10) " ...",
            call. = FALSE
          )
        }
        out[need_name] <- feat_ids[m]
      }

      return(as.numeric(out))
    }

    stop("Unsupported feature identifier type in targets.", call. = FALSE)
  }

  # 1) data.frame(feature, target)
  if (inherits(targets, "data.frame")) {
    if (!all(c("feature", "target") %in% names(targets))) {
      stop("If targets is a data.frame it must contain columns 'feature' and 'target'.", call. = FALSE)
    }
    f <- .map_feature(targets$feature)
    t <- as.numeric(targets$target)
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)

    dt <- data.frame(feature = f, target_raw = t, stringsAsFactors = FALSE)
    dt <- dt[order(dt$feature), , drop = FALSE]
    if (anyDuplicated(dt$feature)) stop("Duplicate targets for the same feature. Please de-duplicate.", call. = FALSE)
    return(dt)
  }

  # 2) matrix (one column) with rownames
  if (is.matrix(targets)) {
    if (ncol(targets) != 1) stop("If targets is a matrix it must have exactly 1 column.", call. = FALSE)
    t <- as.numeric(targets[, 1])
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)

    rn <- rownames(targets)
    if (!is.null(rn) && length(rn) == length(t)) {
      f <- .map_feature(rn)
      dt <- data.frame(feature = f, target_raw = t, stringsAsFactors = FALSE)
      dt <- dt[order(dt$feature), , drop = FALSE]
      if (anyDuplicated(dt$feature)) stop("Duplicate targets for the same feature. Please de-duplicate.", call. = FALSE)
      return(dt)
    }

    # no rownames: assume feature order
    if (length(t) != length(feat_ids)) stop("Matrix targets without rownames must have nrow = number of features.", call. = FALSE)
    return(data.frame(feature = feat_ids, target_raw = t, stringsAsFactors = FALSE))
  }

  # 3) scalar numeric -> recycle
  if (is.numeric(targets) && length(targets) == 1) {
    t <- as.numeric(targets)
    if (is.na(t)) stop("Target is NA.", call. = FALSE)
    return(data.frame(feature = feat_ids, target_raw = rep(t, length(feat_ids)), stringsAsFactors = FALSE))
  }

  # 4) numeric vector
  if (is.numeric(targets) && length(targets) > 1) {
    t <- as.numeric(targets)
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)

    nm <- names(targets)
    if (!is.null(nm) && any(nzchar(nm))) {
      f <- .map_feature(nm)
      dt <- data.frame(feature = f, target_raw = t, stringsAsFactors = FALSE)
      dt <- dt[order(dt$feature), , drop = FALSE]
      if (anyDuplicated(dt$feature)) stop("Duplicate targets for the same feature name/id.", call. = FALSE)
      return(dt)
    }

    # unnamed: assume feature order
    if (length(t) != length(feat_ids)) {
      stop("Un-named numeric targets must have length equal to number of features.", call. = FALSE)
    }
    return(data.frame(feature = feat_ids, target_raw = t, stringsAsFactors = FALSE))
  }

  # 5) character -> parse "feature=target" or "target"?
  if (is.character(targets)) {
    # allow a single proportion/number as character
    if (length(targets) == 1 && grepl("^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*$", targets)) {
      t <- as.numeric(targets)
      return(data.frame(feature = feat_ids, target_raw = rep(t, length(feat_ids)), stringsAsFactors = FALSE))
    }

    # parse "feat=val" pairs
    if (any(grepl("=", targets, fixed = TRUE))) {
      parts <- strsplit(targets, "=", fixed = TRUE)
      feat_part <- vapply(parts, `[[`, character(1), 1)
      val_part  <- vapply(parts, `[[`, character(1), 2)
      f <- .map_feature(trimws(feat_part))
      t <- as.numeric(trimws(val_part))
      if (any(is.na(t))) stop("Could not parse numeric target values from character input.", call. = FALSE)
      dt <- data.frame(feature = f, target_raw = t, stringsAsFactors = FALSE)
      dt <- dt[order(dt$feature), , drop = FALSE]
      if (anyDuplicated(dt$feature)) stop("Duplicate targets for the same feature.", call. = FALSE)
      return(dt)
    }

    stop("Unsupported character targets format. Use 'feature=target' pairs or a single numeric string.", call. = FALSE)
  }

  stop("Unsupported targets format.", call. = FALSE)
}

.pa_store_targets <- function(x, targets_df, overwrite = FALSE) {
  stopifnot(inherits(x, "Data"))
  stopifnot(inherits(targets_df, "data.frame"))
  stopifnot(all(c("feature","type","target_value") %in% names(targets_df)))

  # normalize minimal fields (defensive)
  targets_df$feature <- as.integer(targets_df$feature)
  targets_df$type    <- as.character(targets_df$type)

  valid_types <- c("conservation", "recovery", "mixed_total")
  bad_type <- setdiff(unique(targets_df$type), valid_types)
  if (length(bad_type) > 0) {
    stop("Unknown target types: ", paste(bad_type, collapse = ", "), call. = FALSE)
  }

  if (is.null(x$data$targets)) {
    x$data$targets <- targets_df
    return(x)
  }

  old <- x$data$targets
  old$feature <- as.integer(old$feature)
  old$type    <- as.character(old$type)

  # ---- STRICT RULE: mixed_total cannot coexist with conservation/recovery for same feature
  old_mix <- unique(old$feature[old$type == "mixed_total"])
  new_mix <- unique(targets_df$feature[targets_df$type == "mixed_total"])

  old_nonmix <- unique(old$feature[old$type %in% c("conservation","recovery")])
  new_nonmix <- unique(targets_df$feature[targets_df$type %in% c("conservation","recovery")])

  # conflicts:
  # (A) adding mixed where there is already conservation/recovery
  conflict_A <- intersect(new_mix, old_nonmix)
  # (B) adding conservation/recovery where there is already mixed
  conflict_B <- intersect(new_nonmix, old_mix)
  # (C) within the same incoming batch: mixed + (conservation/recovery) for same feature
  conflict_C <- intersect(new_mix, new_nonmix)

  conflicts <- sort(unique(c(conflict_A, conflict_B, conflict_C)))
  if (length(conflicts) > 0) {
    ex <- paste(head(conflicts, 8), collapse = ", ")
    stop(
      "Incompatible targets detected. Features cannot have 'mixed_total' together with ",
      "'conservation' and/or 'recovery'. Conflicting feature ids: ", ex,
      if (length(conflicts) > 8) paste0(" ... (", length(conflicts), " total)") else "",
      call. = FALSE
    )
  }

  # key: feature + type (your existing overwrite behavior)
  key_old <- paste0(old$feature, "||", old$type)
  key_new <- paste0(targets_df$feature, "||", targets_df$type)

  overlap <- intersect(key_old, key_new)
  if (length(overlap) > 0) {
    if (!isTRUE(overwrite)) {
      k <- overlap[1]
      stop(
        "Targets already exist for some (feature,type) pairs. Example key: ", k,
        ". Use overwrite=TRUE to replace.",
        call. = FALSE
      )
    }
    keep <- !(key_old %in% overlap)
    old <- old[keep, , drop = FALSE]
  }

  x$data$targets <- rbind(old, targets_df)

  # If model already exists, apply targets immediately and refresh snapshot
  if (!is.null(x$data$model_ptr)) {
    if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}

.pa_feature_totals <- function(x) {
  df <- x$data$dist_features
  if (is.null(df) || nrow(df) == 0) {
    stop("Need x$data$dist_features to compute baseline totals.", call. = FALSE)
  }
  stopifnot(all(c("feature","amount") %in% names(df)))
  f <- as.numeric(df$feature)
  a <- as.numeric(df$amount)
  a[!is.finite(a) | is.na(a)] <- 0
  out <- tapply(a, f, sum, na.rm = TRUE)
  out[is.na(out)] <- 0
  out
}


.pa_feature_potential <- function(x, value_col = "benefit") {
  de <- x$data$dist_effects_model %||% x$data$dist_effects
  if (is.null(de) || nrow(de) == 0) {
    stop("Need dist_effects to compute potential. Run add_effects()/add_benefits() first.", call. = FALSE)
  }
  stopifnot(all(c("feature", value_col) %in% names(de)))

  f <- as.numeric(de$feature)
  v <- as.numeric(de[[value_col]])

  # potential is sum of positive contributions only
  v_pos <- pmax(v, 0)
  v_pos[!is.finite(v_pos) | is.na(v_pos)] <- 0

  out <- tapply(v_pos, f, sum, na.rm = TRUE)
  out[is.na(out)] <- 0
  out
}

# -----------------------------------------------------------------------------
# Apply targets to the active C++ model (helper)
# -----------------------------------------------------------------------------
# This helper is meant to be called INSIDE problem() after:
#   - base variables have been added
#   - structural constraints have been added
#   - objective has been set (optional)
#
# It will:
#   - read x$data$targets (data.frame)
#   - decide per-feature whether we have conservation-only, recovery-only, or mixed
#   - call the corresponding Rcpp function(s)
#   - store a lightweight "applied_targets" summary in x$data$model_args
#
# Assumptions (consistent with your design):
#   - x$data$model_ptr exists and is an XPtr<OptimizationProblem>
#   - target_value is already ABSOLUTE in x$data$targets
#   - conservation-only uses z variables + dist_features amounts
#   - recovery-only uses x variables + dist_benefit deltas (can be + or -)
#   - mixed (option 1) uses total = (conservation baseline) + (action deltas),
#     with target = Tc + Tr if both targets exist for a feature.
# -----------------------------------------------------------------------------
# helper: get model-ready benefit table from dist_effects
.get_dist_benefit_model_from_effects <- function(x, benefit_col = "benefit") {
  de <- x$data$dist_effects_model %||% x$data$dist_effects
  if (is.null(de) || nrow(de) == 0) return(NULL)
  if (!benefit_col %in% names(de)) {
    stop("dist_effects is missing column '", benefit_col, "'.", call. = FALSE)
  }
  out <- de
  # ensure the expected column exists for old C++: 'benefit'
  if (!("benefit" %in% names(out))) out$benefit <- as.numeric(out[[benefit_col]])
  else out$benefit <- as.numeric(out$benefit)
  out
}

.pa_apply_targets_if_present <- function(x,
                                         allow_multiple_rows_per_feature = FALSE) {

  stopifnot(inherits(x, "Data"))

  if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
    return(invisible(x))
  }
  if (is.null(x$data$model_ptr)) {
    stop("No active model pointer found in x$data$model_ptr. Run problem() before applying targets.", call. = FALSE)
  }

  t <- x$data$targets
  req <- c("feature", "type", "target_value")
  miss <- setdiff(req, names(t))
  if (length(miss) > 0) {
    stop("x$data$targets is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  t$feature <- as.integer(t$feature)
  t$type <- as.character(t$type)
  t$target_value <- as.numeric(t$target_value)

  bad_type <- setdiff(unique(t$type), c("conservation", "recovery", "mixed_total"))
  if (length(bad_type) > 0) {
    stop("Unknown target types in x$data$targets: ", paste(bad_type, collapse = ", "), call. = FALSE)
  }

  # ---- STRICT RULE again (defensive)
  feats_mix    <- sort(unique(t$feature[t$type == "mixed_total"]))
  feats_nonmix <- sort(unique(t$feature[t$type %in% c("conservation","recovery")]))
  conflict <- intersect(feats_mix, feats_nonmix)
  if (length(conflict) > 0) {
    ex <- paste(head(conflict, 8), collapse = ", ")
    stop(
      "Incompatible targets: features cannot have 'mixed_total' together with 'conservation'/'recovery'. ",
      "Conflicts: ", ex,
      call. = FALSE
    )
  }

  if (!allow_multiple_rows_per_feature) {
    dup <- t[duplicated(t[, c("feature", "type")]), c("feature", "type"), drop = FALSE]
    if (nrow(dup) > 0) {
      stop(
        "Multiple target rows found for the same (feature, type). ",
        "Set allow_multiple_rows_per_feature=TRUE or collapse them before storing.",
        call. = FALSE
      )
    }
  }

  if (allow_multiple_rows_per_feature) {
    t <- stats::aggregate(target_value ~ feature + type, data = t, FUN = sum)
  }

  op <- x$data$model_ptr

  .mk_targets_df <- function(features, values, colname) {
    df <- data.frame(
      internal_id = as.integer(features),
      stringsAsFactors = FALSE
    )
    df[[colname]] <- as.numeric(values)
    df
  }

  # split by type
  tc <- t[t$type == "conservation", c("feature", "target_value"), drop = FALSE]
  tr <- t[t$type == "recovery",      c("feature", "target_value"), drop = FALSE]
  tm <- t[t$type == "mixed_total",   c("feature", "target_value"), drop = FALSE]

  # Read exponent config (legacy soft objective)
  args <- x$data$model_args %||% list()
  benefit_exponent <- as.numeric(args$benefit_exponent %||% 1)
  curve_segments   <- as.integer(args$curve_segments %||% 3L)

  # ------------------------------------------------------------
  # NEW: helper flags
  # ------------------------------------------------------------
  has_actions <- !is.null(x$data$dist_actions_model) && inherits(x$data$dist_actions_model, "data.frame") &&
    nrow(x$data$dist_actions_model) > 0

  has_effects <- !is.null(x$data$dist_effects_model) && inherits(x$data$dist_effects_model, "data.frame") &&
    nrow(x$data$dist_effects_model) > 0

  # ------------------------------------------------------------
  # conservation
  # ------------------------------------------------------------
  if (nrow(tc) > 0) {

    # NEW: exclude conservation z_is when there exist improving actions for same (pu,feature)
    # Only if the function exists and we actually have actions+effects.
    if (exists("rcpp_add_exclude_conservation_when_actions", mode = "function") && has_actions && has_effects) {

      dbm_for_excl <- .get_dist_benefit_model_from_effects(x, benefit_col = "benefit")

      # dbm can be empty if benefit col isn't available; in that case, we skip the exclusion.
      if (!is.null(dbm_for_excl) && nrow(dbm_for_excl) > 0) {
        rcpp_add_exclude_conservation_when_actions(
          op,
          dist_features_data = x$data$dist_features,
          dist_actions_data  = x$data$dist_actions_model,
          dist_effects_data  = dbm_for_excl,
          benefit_col_sexp   = "benefit",
          tol                = 1e-12
        )
      }
    }

    if (!exists("rcpp_add_target_conservation", mode = "function")) {
      stop("Missing rcpp_add_target_conservation() in the package.", call. = FALSE)
    }
    rcpp_add_target_conservation(
      op,
      features_data      = .mk_targets_df(tc$feature, tc$target_value, "target_conservation"),
      dist_features_data = x$data$dist_features,
      target_col         = "target_conservation"
    )
  }

  # ------------------------------------------------------------
  # recovery
  # ------------------------------------------------------------
  if (nrow(tr) > 0) {

    dbm <- .get_dist_benefit_model_from_effects(x, benefit_col = "benefit")
    if (is.null(dbm) || nrow(dbm) == 0) {
      stop("Recovery targets present but no benefit column available in dist_effects.", call. = FALSE)
    }

    if (is.finite(benefit_exponent) && benefit_exponent > 1 + 1e-12) {
      if (!exists("rcpp_add_target_recovery_power", mode = "function")) {
        stop(
          "benefit_exponent > 1 requested, but rcpp_add_target_recovery_power() is missing in the package.",
          call. = FALSE
        )
      }

      rcpp_add_target_recovery_power(
        op,
        features_data     = .mk_targets_df(tr$feature, tr$target_value, "target_recovery"),
        dist_actions_data = x$data$dist_actions_model,
        dist_benefit_data = dbm,
        exponent          = benefit_exponent,
        segments          = curve_segments,
        target_col_sexp   = "target_recovery"
      )

    } else {
      if (!exists("rcpp_add_target_recovery", mode = "function")) {
        stop("Missing rcpp_add_target_recovery() in the package.", call. = FALSE)
      }
      rcpp_add_target_recovery(
        op,
        features_data     = .mk_targets_df(tr$feature, tr$target_value, "target_recovery"),
        dist_actions_data = x$data$dist_actions_model,
        dist_benefit_data = dbm,
        target_col        = "target_recovery"
      )
    }
  }

  # ------------------------------------------------------------
  # mixed_total
  # ------------------------------------------------------------
  if (nrow(tm) > 0) {
    if (!exists("rcpp_add_target_mixed_total", mode = "function")) {
      stop("Missing rcpp_add_target_mixed_total() in the package.", call. = FALSE)
    }

    dbm <- .get_dist_benefit_model_from_effects(x, benefit_col = "benefit")
    if (is.null(dbm) || nrow(dbm) == 0) {
      stop("Mixed targets present but no benefit column available in dist_effects.", call. = FALSE)
    }

    rcpp_add_target_mixed_total(
      op,
      dist_features_data = x$data$dist_features,
      dist_benefit_data  = dbm,
      dist_actions_data  = x$data$dist_actions_model,
      targets_df         = .mk_targets_df(tm$feature, tm$target_value)
    )
  }

  x$data$model_args$targets_applied <- TRUE
  x$data$model_args$targets_counts <- list(
    conservation = nrow(tc),
    recovery     = nrow(tr),
    mixed_total  = nrow(tm)
  )

  invisible(x)
}


.pa_refresh_model_snapshot <- function(x, drop_triplets = TRUE) {
  stopifnot(inherits(x, "Data"))

  if (is.null(x$data$model_ptr)) return(x)

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required to build sparse constraint matrix.", call. = FALSE)
  }

  model_list <- rcpp_optimization_problem_as_list(x$data$model_ptr)

  # Build sparse A defensively (A_i/A_j are 0-based in C++ -> +1 in R)
  if (all(c("A_i", "A_j", "A_x") %in% names(model_list)) &&
      length(model_list$A_i) == length(model_list$A_j) &&
      length(model_list$A_i) == length(model_list$A_x)) {

    model_list$A <- Matrix::sparseMatrix(
      i = as.integer(model_list$A_i) + 1L,
      j = as.integer(model_list$A_j) + 1L,
      x = as.numeric(model_list$A_x)
    )

    if (isTRUE(drop_triplets)) {
      model_list$A_i <- NULL
      model_list$A_j <- NULL
      model_list$A_x <- NULL
    }

  } else {
    # fallback: empty sparse matrix if missing triplets
    n_con <- if (!is.null(model_list$rhs)) length(model_list$rhs) else 0L
    n_var <- if (!is.null(model_list$obj)) length(model_list$obj) else 0L
    model_list$A <- Matrix::sparseMatrix(
      i = integer(0), j = integer(0), x = numeric(0),
      dims = c(n_con, n_var)
    )
  }

  # Keep args attached if you use them downstream (optional but handy)
  model_list$args <- x$data$model_args %||% list()

  x$data$model_list <- model_list

  # Optional: store dims for fast printing
  x$data$model_args <- x$data$model_args %||% list()
  x$data$model_args$n_constraints <- if (!is.null(model_list$rhs)) length(model_list$rhs) else NA_integer_
  x$data$model_args$n_variables   <- if (!is.null(model_list$obj)) length(model_list$obj) else NA_integer_
  x$data$model_args$nnz           <- Matrix::nnzero(model_list$A)

  x
}


.pa_model_from_ptr <- function(op, args = list(), drop_triplets = TRUE) {

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required to build sparse constraint matrix.", call. = FALSE)
  }

  model_list <- rcpp_optimization_problem_as_list(op)

  # n_con / n_var siempre desde rhs/obj
  n_con <- if (!is.null(model_list$rhs)) length(model_list$rhs) else 0L
  n_var <- if (!is.null(model_list$obj)) length(model_list$obj) else 0L

  # build sparse A defensively (A_i/A_j are 0-based -> +1)
  if (all(c("A_i", "A_j", "A_x") %in% names(model_list)) &&
      length(model_list$A_i) == length(model_list$A_j) &&
      length(model_list$A_i) == length(model_list$A_x) &&
      length(model_list$A_i) > 0L) {

    model_list$A <- Matrix::sparseMatrix(
      i = as.integer(model_list$A_i) + 1L,
      j = as.integer(model_list$A_j) + 1L,
      x = as.numeric(model_list$A_x),
      dims = c(n_con, n_var)   # <<< CRÍTICO: fija dimensiones reales
    )

    if (isTRUE(drop_triplets)) {
      model_list$A_i <- NULL
      model_list$A_j <- NULL
      model_list$A_x <- NULL
    }

  } else {

    model_list$A <- Matrix::sparseMatrix(
      i = integer(0), j = integer(0), x = numeric(0),
      dims = c(n_con, n_var)
    )
  }

  # attach model args (needed for curve/segments and any other solver params)
  model_list$args <- args %||% list()

  # registry: ya viene desde C++ como data.frame en model_list$registry.
  # No lo toques aquí; solo asegúrate de no borrarlo por error.
  # (si quisieras, podrías validar que exista)
  # if (is.null(model_list$registry)) model_list$registry <- data.frame()

  model_list
}



# -------------------------------------------------------------------------
# Internal helpers CLASS
# -------------------------------------------------------------------------

.pa_cli_theme <- function() {
  list(
    .h     = list("font-weight" = "bold", color = "#569746"),
    .cls   = list("font-weight" = "bold", color = "blue"),
    .code  = list(color = "green"),
    .muted = list(color = "grey60")
  )
}

.pa_cli_box_chars <- function() {
  use_unicode <- l10n_info()[["UTF-8"]]
  if (isTRUE(use_unicode)) {
    list(
      j = "\u251C", # ├
      l = "\u2514", # └
      v = "\u2502", # │
      b = "\u2500"  # ─
    )
  } else {
    list(
      j = "+",
      l = "\\",
      v = "|",
      b = "-"
    )
  }
}

.pa_safe_range <- function(x, digits = 5) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) return(NULL)
  base::round(range(x), digits)
}

.pa_repr_atomic <- function(x, label = NULL, max_items = 6) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    if (is.null(label)) return("{.muted (none)}")
    return(paste0("{.muted (0 ", label, ")}"))
  }
  if (length(x) <= max_items) {
    paste0(paste0('"', x, '"', collapse = ", "))
  } else {
    head <- x[seq_len(max_items)]
    paste0(
      paste0('"', head, '"', collapse = ", "),
      ", {.muted ... (", length(x), " total)}"
    )
  }
}

.pa_get_cost_vec <- function(pu) {
  if (is.null(pu) || nrow(pu) == 0) return(numeric(0))
  if ("cost" %in% names(pu)) return(as.numeric(pu$cost))
  if ("monitoring_cost" %in% names(pu)) return(as.numeric(pu$monitoring_cost))
  numeric(0)
}

.pa_get_action_cost_vec <- function(self) {
  # new field (preferred)
  if (!is.null(self$data$dist_actions) &&
      inherits(self$data$dist_actions, "data.frame") &&
      nrow(self$data$dist_actions) > 0 &&
      "cost" %in% names(self$data$dist_actions)) {
    return(as.numeric(self$data$dist_actions$cost))
  }
  # legacy field (older prioriactions)
  if (!is.null(self$data$dist_threats) &&
      inherits(self$data$dist_threats, "data.frame") &&
      nrow(self$data$dist_threats) > 0 &&
      "action_cost" %in% names(self$data$dist_threats)) {
    return(as.numeric(self$data$dist_threats$action_cost))
  }
  numeric(0)
}

.pa_nrow0 <- function(x) {
  if (is.null(x)) return(0L)
  if (!inherits(x, "data.frame")) return(0L)
  nrow(x)
}

.pa_has_model <- function(self) {
  !is.null(self$data$model_list) && is.list(self$data$model_list)
}

.pa_model_dims <- function(self) {
  # Returns list(n_con, n_var, nnz) safely
  ml <- self$data$model_list

  # If model_list doesn't exist yet, return zeros
  if (is.null(ml) || !is.list(ml)) {
    return(list(n_con = 0L, n_var = 0L, nnz = 0L))
  }

  # Best effort: support both A (dgCMatrix) and triplet (A_i/A_j/A_x)
  n_con <- 0L
  n_var <- 0L
  nnz   <- 0L

  if (!is.null(ml$A) && inherits(ml$A, "Matrix")) {
    n_con <- nrow(ml$A)
    n_var <- ncol(ml$A)
    nnz   <- length(ml$A@x)
    return(list(n_con = as.integer(n_con), n_var = as.integer(n_var), nnz = as.integer(nnz)))
  }

  if (!is.null(ml$A_i) && !is.null(ml$A_j)) {
    # We may not know nrow/ncol if not stored; infer n_con/n_var if present
    nnz <- length(ml$A_i)
    if (!is.null(ml$rhs)) n_con <- length(ml$rhs)
    if (!is.null(ml$obj)) n_var <- length(ml$obj)
    return(list(n_con = as.integer(n_con), n_var = as.integer(n_var), nnz = as.integer(nnz)))
  }

  # Fallback from rhs/obj if present
  if (!is.null(ml$rhs)) n_con <- length(ml$rhs)
  if (!is.null(ml$obj)) n_var <- length(ml$obj)

  list(n_con = as.integer(n_con), n_var = as.integer(n_var), nnz = as.integer(nnz))
}
.pa_model_blocks <- function(self) {
  # Returns list(n_w, n_x, n_z) if index mapping exists; otherwise NULL.
  # This expects you to store something like idx from rcpp_add_base_variables()
  # in self$data$model_index. Your idx currently returns w_index/x_index and
  # could also return z_index (recommended).
  idx <- self$data$model_index
  if (is.null(idx) || !is.list(idx)) return(NULL)

  n_w <- if (!is.null(idx$w_index)) length(idx$w_index) else NA_integer_
  n_x <- if (!is.null(idx$x_index)) length(idx$x_index) else NA_integer_
  n_z <- if (!is.null(idx$z_index)) length(idx$z_index) else NA_integer_

  # if none exist, don't print a misleading line
  if (is.na(n_w) && is.na(n_x) && is.na(n_z)) return(NULL)

  list(
    n_w = ifelse(is.na(n_w), 0L, as.integer(n_w)),
    n_x = ifelse(is.na(n_x), 0L, as.integer(n_x)),
    n_z = ifelse(is.na(n_z), 0L, as.integer(n_z))
  )
}

.pa_model_args <- function(self) {
  # Return a standardized args list if present; otherwise NULL
  a <- self$data$model_args
  if (is.null(a) || !is.list(a)) return(NULL)

  # Standardize expected fields (do not error if missing)
  out <- list(
    model_type    = if (!is.null(a$model_type)) a$model_type else NA_character_,
    modelsense    = if (!is.null(a$modelsense)) a$modelsense else NA_character_,
    objective_id  = if (!is.null(a$objective_id)) a$objective_id else NA_character_,
    budget        = if (!is.null(a$budget)) a$budget else NA_real_,
    blm           = if (!is.null(a$blm)) a$blm else NA_real_,
    benefit_exponent         = if (!is.null(a$benefit_exponent)) a$benefit_exponent else NA_integer_,
    curve_segments      = if (!is.null(a$curve_segments)) a$curve_segments else NA_integer_
  )

  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# internal helper: detect tabular vs spatial vs invalid mixes
.pa_detect_input_mode <- function(pu, features, dist_features) {

  # ---- local helpers (NO imports; only type checks)
  .is_df <- function(x) inherits(x, "data.frame")

  .is_spatraster <- function(x) inherits(x, "SpatRaster")
  .is_spatvector <- function(x) inherits(x, "SpatVector")
  .is_sf <- function(x) inherits(x, "sf")

  .is_raster_path <- function(x) {
    is.character(x) && length(x) == 1L &&
      grepl("\\.(tif|tiff|grd|asc|nc)$", tolower(x))
  }

  .is_vector_path <- function(x) {
    is.character(x) && length(x) == 1L &&
      grepl("\\.(shp|gpkg|geojson|json|fgb)$", tolower(x))
  }

  .is_spatial_pu <- function(x) .is_spatraster(x) || .is_spatvector(x) || .is_sf(x) || .is_raster_path(x) || .is_vector_path(x)
  .is_spatial_features <- function(x) .is_spatraster(x) || .is_raster_path(x)

  dist_missing <- missing(dist_features) || is.null(dist_features)

  # ------------------------------------------------------------
  # Rule 1: If dist_features is provided as data.frame -> TABULAR
  # ------------------------------------------------------------
  if (!dist_missing) {
    if (!.is_df(dist_features)) {
      stop("`dist_features` must be a data.frame in tabular mode, or NULL/missing in spatial mode.", call. = FALSE)
    }

    # dist_features df => require pu + features df (avoid ambiguity)
    if (!.is_df(pu)) {
      stop("Tabular mode detected because `dist_features` is a data.frame, but `pu` is not a data.frame.", call. = FALSE)
    }
    if (!.is_df(features)) {
      stop("Tabular mode detected because `dist_features` is a data.frame, but `features` is not a data.frame.", call. = FALSE)
    }

    return(list(mode = "tabular"))
  }

  # ------------------------------------------------------------
  # Rule 2: dist_features missing/NULL -> could be SPATIAL or invalid
  # ------------------------------------------------------------
  # spatial mode requires at least one spatial-ish input
  if (.is_spatial_features(features) || .is_spatial_pu(pu)) {
    return(list(mode = "spatial"))
  }

  # dist_features missing + no spatial inputs -> invalid (user forgot dist_features)
  stop(
    "Could not determine input style. Provide tabular inputs (data.frames: `pu`, `features`, `dist_features`) ",
    "or spatial inputs (e.g., `pu` and/or `features` as rasters/vectors/paths) with `dist_features = NULL`.",
    call. = FALSE
  )
}

# helpers
.read_rast <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "SpatRaster")) return(x)
  if (is.character(x)) return(terra::rast(x))
  stop("Unsupported raster input.", call. = FALSE)
}
.read_vect <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "SpatVector")) return(x)
  if (inherits(x, "sf")) return(terra::vect(x))
  if (is.character(x)) return(terra::vect(x))
  stop("Unsupported vector input.", call. = FALSE)
}
.is_raster_path <- function(x) {
  is.character(x) && grepl("\\.(tif|tiff|grd|asc|nc)$", tolower(x))
}

.fun_from_name <- function(name) {
  switch(
    name,
    mean = function(v, ...) mean(v, na.rm = TRUE),
    sum  = function(v, ...) sum(v, na.rm = TRUE),
    stop("Unknown aggregation function: ", name, call. = FALSE)
  )
}


.pa_has_coords <- function(x) {
  is.list(x$data) && !is.null(x$data$pu_coords) &&
    inherits(x$data$pu_coords, "data.frame") &&
    all(c("id","x","y") %in% names(x$data$pu_coords)) &&
    nrow(x$data$pu_coords) > 0
}

.pa_spatial_relations_summary <- function(x) {
  rels <- x$data$spatial_relations
  if (is.null(rels) || !is.list(rels) || length(rels) == 0) return(NULL)

  nm <- names(rels)
  if (is.null(nm)) nm <- rep("", length(rels))
  nm[nm == ""] <- paste0("rel_", seq_along(nm))

  out <- lapply(seq_along(rels), function(i) {
    r <- rels[[i]]
    if (!inherits(r, "data.frame") || nrow(r) == 0) {
      return(data.frame(name = nm[i], edges = 0L, w_min = NA_real_, w_max = NA_real_))
    }
    w <- suppressWarnings(as.numeric(r$weight))
    rng <- .pa_safe_range(w)
    data.frame(
      name  = nm[i],
      edges = as.integer(nrow(r)),
      w_min = if (is.null(rng)) NA_real_ else rng[[1]],
      w_max = if (is.null(rng)) NA_real_ else rng[[2]]
    )
  })

  do.call(rbind, out)
}




# -------------------------------------------------------------------------
# Internal helpers SOLVER
# -------------------------------------------------------------------------
.pa_get_solve_args <- function(x,
                               solver = NULL,
                               gap_limit = NULL,
                               time_limit = NULL,
                               solution_limit = NULL,
                               cores = NULL,
                               verbose = NULL,
                               name_output_file = NULL,
                               output_file = NULL,
                               solver_params = NULL) {

  stopifnot(inherits(x, "Data"))

  defaults <- list(
    solver = "auto",
    gap_limit = 0.0,
    time_limit = .Machine$integer.max,
    solution_limit = FALSE,
    cores = 2L,
    verbose = TRUE,
    name_output_file = "output",
    output_file = TRUE,
    solver_params = list()
  )

  stored <- x$data$solve_args %||% list()
  if (!is.list(stored)) stored <- list()

  out <- modifyList(defaults, stored)

  # ---- explicit overrides (optional)
  if (!is.null(gap_limit)) out$gap_limit <- gap_limit
  if (!is.null(time_limit)) out$time_limit <- time_limit
  if (!is.null(solution_limit)) out$solution_limit <- solution_limit
  if (!is.null(cores)) out$cores <- cores
  if (!is.null(verbose)) out$verbose <- verbose
  if (!is.null(name_output_file)) out$name_output_file <- name_output_file
  if (!is.null(output_file)) out$output_file <- output_file

  # solver override (NULL = not specified)
  if (!is.null(solver)) {
    solver <- as.character(solver)[1]
    if (!identical(solver, "")) out$solver <- solver
  }

  # solver_params override/merge
  if (!is.null(solver_params)) {
    if (!is.list(solver_params)) stop("solver_params must be a list.", call. = FALSE)
    out$solver_params <- modifyList(out$solver_params %||% list(), solver_params)
  }

  # ---- normalize
  out$solver <- as.character(out$solver)[1]
  if (!out$solver %in% c("auto", "gurobi", "cplex", "cbc", "symphony")) {
    stop("Unknown solver in x$data$solve_args$solver: ", out$solver,
         ". Use one of: auto, gurobi, cplex, cbc, symphony.", call. = FALSE)
  }

  out$gap_limit <- round(as.numeric(out$gap_limit), 3)
  if (!is.finite(out$gap_limit) || out$gap_limit < 0 || out$gap_limit > 1) {
    stop("gap_limit must be a finite number in [0, 1].", call. = FALSE)
  }

  out$time_limit <- round(as.numeric(out$time_limit), 3)
  if (!is.finite(out$time_limit) || out$time_limit < 0) {
    stop("time_limit must be a finite number >= 0.", call. = FALSE)
  }

  out$cores <- as.integer(out$cores)
  if (!is.finite(out$cores) || out$cores < 1) {
    stop("cores must be an integer >= 1.", call. = FALSE)
  }

  # cap cores defensively (avoid hard error inside solve)
  max_cores <- parallel::detectCores(TRUE)
  if (is.finite(max_cores) && out$cores > max_cores) {
    out$cores <- as.integer(max_cores)
  }

  out$solution_limit <- isTRUE(out$solution_limit)
  out$verbose <- isTRUE(out$verbose)
  out$output_file <- isTRUE(out$output_file)

  out$name_output_file <- as.character(out$name_output_file)[1]
  if (!nzchar(out$name_output_file)) out$name_output_file <- "output"

  if (is.null(out$solver_params) || !is.list(out$solver_params)) out$solver_params <- list()

  out
}

.pa_model_plan <- function(self) {
  args <- self$data$model_args %||% list()
  sa   <- self$data$solve_args %||% list()

  # ---- objective
  objective_id <- args$objective_id %||% NA_character_
  model_type   <- args$model_type %||% NA_character_
  oargs        <- args$objective_args %||% list()

  # ---- solver
  solver <- sa$solver %||% "auto"

  # ---- constraints: detect what is present/configured
  constraints <- list()

  # targets
  t <- self$data$targets %||% NULL
  if (inherits(t, "data.frame") && nrow(t) > 0) {
    types <- if ("type" %in% names(t)) sort(unique(as.character(t$type))) else character(0)
    constraints$targets <- list(
      present = TRUE,
      n = nrow(t),
      types = types
    )
  } else {
    constraints$targets <- list(present = FALSE)
  }

  # budget (propuesto: store in model_args$budget or model_args$constraints$budget)
  # aquí lo detecto de ambas formas para compatibilidad
  bud <- NULL
  if (!is.null(args$constraints) && is.list(args$constraints) && !is.null(args$constraints$budget)) {
    bud <- args$constraints$budget
  } else if (!is.null(args$budget)) {
    bud <- args$budget
  }
  if (!is.null(bud) && is.finite(as.numeric(bud)) && as.numeric(bud) > 0) {
    constraints$budget <- list(present = TRUE, value = as.numeric(bud))
  } else {
    constraints$budget <- list(present = FALSE)
  }

  # boundary/BLM (si existe config)
  blm <- args$blm %||% NULL
  if (!is.null(blm) && is.finite(as.numeric(blm)) && as.numeric(blm) != 0) {
    constraints$blm <- list(present = TRUE, value = as.numeric(blm))
  } else {
    constraints$blm <- list(present = FALSE)
  }

  # locks (ya existen en tablas; esto es “presence info”, no constraint config)
  # PU locks
  pu <- self$data$pu
  has_pu_locks <- !is.null(pu) && any(names(pu) %in% c("locked_in", "locked_out")) &&
    (any(pu$locked_in %||% FALSE) || any(pu$locked_out %||% FALSE))
  constraints$pu_locks <- list(present = isTRUE(has_pu_locks))

  # action locks (status in dist_actions)
  da <- self$data$dist_actions
  has_action_locks <- !is.null(da) && "status" %in% names(da) && any(da$status %in% c(2L, 3L), na.rm = TRUE)
  constraints$action_locks <- list(present = isTRUE(has_action_locks))

  # ---- readiness checks (lightweight)
  checks <- character(0)

  if (!is.na(objective_id) && identical(objective_id, "max_benefit")) {
    de <- self$data$dist_effects %||% NULL
    if (is.null(de) || .pa_nrow0(de) == 0 || !("benefit" %in% names(de))) {
      checks <- c(checks, "dist_effects: {.red missing} 'benefit' (required by max_benefit)")
    }
  } else if (!is.na(objective_id) && identical(objective_id, "min_loss")) {
    de <- self$data$dist_effects %||% NULL
    if (is.null(de) || .pa_nrow0(de) == 0 || !("loss" %in% names(de))) {
      checks <- c(checks, "dist_effects: {.red missing} 'loss' (required by min_loss)")
    }
  } else if (!is.na(objective_id) && identical(objective_id, "max_profit")) {
    if (is.null(self$data$dist_profit) || .pa_nrow0(self$data$dist_profit) == 0) {
      checks <- c(checks, "dist_profit: {.red missing} (required by max_profit)")
    }
  } else if (!is.na(objective_id) && identical(objective_id, "max_net_profit")) {
    if (is.null(self$data$dist_profit) || .pa_nrow0(self$data$dist_profit) == 0) {
      checks <- c(checks, "dist_profit: {.red missing} (required by max_net_profit)")
    }
  }

  # targets checks (basic)
  if (inherits(t, "data.frame") && nrow(t) > 0 && "type" %in% names(t)) {
    bad <- setdiff(sort(unique(as.character(t$type))), c("conservation", "recovery", "mixed_total"))
    if (length(bad) > 0) {
      checks <- c(checks, paste0("targets$type: {.red invalid} -> ", paste(bad, collapse = ", ")))
    }
  }

  list(
    objective_id = objective_id,
    model_type = model_type,
    objective_args = oargs,
    solver = solver,
    solve_args = sa,
    constraints = constraints,
    checks = checks
  )
}


.pa_print_model_section <- function(self, ch) {
  mp <- .pa_model_plan(self)

  cli::cli_text("{ch$l}{ch$b}{.h model}", .envir = environment())

  built <- !is.null(self$data$model_ptr)
  if (built) {
    d <- .pa_model_dims(self)
    cli::cli_text(" {ch$v}{ch$j}{ch$b}status:         {.green built}",
                  .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}dimensions:     {d$n_con} constraints, {d$n_var} vars, {d$nnz} nnz",
                  .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}status:         {.muted not built yet} (will build in solve())",
                  .envir = environment())
  }

  # objective
  if (is.na(mp$objective_id)) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}objective:      {.yellow not set} (defaults to {.code min_cost})",
                  .envir = environment())
  } else {
    mt <- ifelse(is.na(mp$model_type), "{.muted unknown}", paste0("{.code ", mp$model_type, "}"))
    cli::cli_text(" {ch$v}{ch$j}{ch$b}objective:      {.code {mp$objective_id}} ({mt})",
                  .envir = environment())
  }

  # constraints summary
  con <- mp$constraints %||% list()
  cli::cli_text(" {ch$v}{ch$j}{ch$b}constraints:",
                .envir = environment())

  # targets
  if (isTRUE(con$targets$present %||% FALSE)) {
    n <- con$targets$n %||% 0L
    tt <- con$targets$types %||% character(0)
    tt_txt <- if (length(tt) == 0) "{.muted unknown types}" else paste(tt, collapse = ", ")
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - targets:    {n} rows ({tt_txt})",
                  .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - targets:    {.muted none}",
                  .envir = environment())
  }

  # budget
  if (isTRUE(con$budget$present %||% FALSE)) {
    bv <- con$budget$value
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - budget:     {bv}",
                  .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - budget:     {.muted none}",
                  .envir = environment())
  }

  # BLM
  if (isTRUE(con$blm$present %||% FALSE)) {
    blm <- con$blm$value
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - blm:        {blm}",
                  .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}  - blm:        {.muted none}",
                  .envir = environment())
  }

  # locks presence
  cli::cli_text(" {ch$v}{ch$j}{ch$b}  - pu_locks:   {if (isTRUE(con$pu_locks$present %||% FALSE)) '{.code present}' else '{.muted none}'}",
                .envir = environment())
  cli::cli_text(" {ch$v}{ch$j}{ch$b}  - action_locks:{if (isTRUE(con$action_locks$present %||% FALSE)) '{.code present}' else '{.muted none}'}",
                .envir = environment())

  # solver
  sa <- mp$solve_args %||% list()
  if (length(sa) == 0) {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}solver:         {.muted not set} (auto)",
                  .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}solver:         {.code {mp$solver}}",
                  .envir = environment())
    gl <- sa$gap_limit %||% 0
    tl <- sa$time_limit %||% .Machine$integer.max
    cc <- sa$cores %||% 2L
    cli::cli_text(" {ch$v}{ch$j}{ch$b}gap/time/cores: {gl} / {tl} / {cc}",
                  .envir = environment())
  }

  # checks
  if (length(mp$checks) > 0) {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}checks:         ",
                  .envir = environment())
    for (i in seq_along(mp$checks)) {
      prefix <- if (i < length(mp$checks)) "{ch$v}{ch$j}{ch$b}" else "{ch$v}{ch$l}{ch$b}"
      cli::cli_text(paste0(" ", prefix, mp$checks[i]),
                    .envir = environment())
    }
  } else {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}checks:         {.green ok}",
                  .envir = environment())
  }

  invisible(TRUE)
}


# ------------------------------------------------------------
# Legacy fallback: build 1 action per threat + effects from threats/sensitivity
# ------------------------------------------------------------
.pa_add_actions_default_from_threats <- function(x,
                                                 action_prefix = "threat_",
                                                 overwrite = FALSE,
                                                 benefit_exponent = 1,     # NUEVO
                                                 curve_segments = 3L) {
  stopifnot(inherits(x, "Data"))

  # already has actions?
  if (!overwrite && !is.null(x$data$actions) && inherits(x$data$actions, "data.frame") && nrow(x$data$actions) > 0) {
    return(invisible(x))
  }

  # check legacy inputs exist
  if (is.null(x$data$threats) || is.null(x$data$dist_threats)) {
    stop("Legacy default actions require x$data$threats and x$data$dist_threats.", call. = FALSE)
  }
  if (is.null(x$data$pu) || is.null(x$data$features) || is.null(x$data$dist_features)) {
    stop("Missing required tables: pu/features/dist_features must exist before building actions.", call. = FALSE)
  }

  pu <- x$data$pu
  threats <- x$data$threats
  dist_threats <- x$data$dist_threats
  dist_features <- x$data$dist_features

  # small util
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ------------------------------------------------------------
  # 0) Normalize / validate minimal columns
  # ------------------------------------------------------------
  if (!all(c("id","internal_id") %in% names(pu))) {
    stop("x$data$pu must contain columns: id, internal_id.", call. = FALSE)
  }
  if (!all(c("id","internal_id") %in% names(x$data$features))) {
    stop("x$data$features must contain columns: id, internal_id.", call. = FALSE)
  }
  if (!all(c("pu","feature","amount","internal_pu","internal_feature") %in% names(dist_features))) {
    stop("x$data$dist_features must contain columns: pu, feature, amount, internal_pu, internal_feature.", call. = FALSE)
  }

  if (!all(c("id") %in% names(threats))) {
    stop("x$data$threats must contain column: id.", call. = FALSE)
  }
  if (!all(c("pu","threat","amount","action_cost") %in% names(dist_threats))) {
    stop("x$data$dist_threats must contain columns: pu, threat, amount, action_cost.", call. = FALSE)
  }

  dist_threats$pu <- as.integer(dist_threats$pu)
  dist_threats$threat <- as.integer(dist_threats$threat)
  dist_threats$amount <- as.numeric(dist_threats$amount)
  dist_threats$action_cost <- as.numeric(dist_threats$action_cost)

  if (!("status" %in% names(dist_threats))) dist_threats$status <- 0L
  dist_threats$status <- as.integer(dist_threats$status)

  # enforce PU locked_out -> all actions there locked_out
  if ("locked_out" %in% names(pu) && any(pu$locked_out, na.rm = TRUE)) {
    locked_out_pus <- pu$id[isTRUE(pu$locked_out) | (!is.na(pu$locked_out) & pu$locked_out)]
    idx2 <- dist_threats$pu %in% locked_out_pus
    dist_threats$status[idx2] <- 3L
  }

  # drop zero threat intensity rows for modeling (consistent with your inputData behavior)
  dt_present <- dist_threats[is.finite(dist_threats$amount) & !is.na(dist_threats$amount) & dist_threats$amount > 0, ,
                             drop = FALSE]

  # ------------------------------------------------------------
  # 1) Build actions: one per threat
  # ------------------------------------------------------------
  threat_ids <- sort(unique(threats$id))
  action_ids <- paste0(action_prefix, threat_ids)

  actions <- data.frame(
    id = as.character(action_ids),
    threat = as.integer(threat_ids),
    stringsAsFactors = FALSE
  )
  actions$internal_id <- seq_len(nrow(actions))

  action_index <- stats::setNames(actions$internal_id, actions$id)

  # ------------------------------------------------------------
  # 2) Build dist_actions from dist_threats
  # ------------------------------------------------------------
  dist_actions <- dist_threats
  dist_actions$action <- paste0(action_prefix, dist_actions$threat)
  dist_actions$cost <- dist_actions$action_cost

  # map internal ids
  pu_index <- stats::setNames(pu$internal_id, as.character(pu$id))
  dist_actions$internal_pu <- unname(pu_index[as.character(dist_actions$pu)])
  dist_actions$internal_action <- unname(action_index[as.character(dist_actions$action)])

  # keep model-ready cols
  dist_actions <- dist_actions[, c("pu","action","cost","status","internal_pu","internal_action"), drop = FALSE]

  # ------------------------------------------------------------
  # 3) Build dist_effects (benefit) from legacy rule
  #    - binary (0/1): amount_is / (# sensitive threats present)
  #    - continuous: amount_is * (resp_var*alpha/sum_alpha)  (your C++)
  # ------------------------------------------------------------

  # detect binary
  # (binary if all amounts in dist_threats are 0/1)
  is_binary <- all(dist_threats$amount %in% c(0, 1), na.rm = TRUE)

  # sensitivity table
  sens <- x$data$sensitivity
  if (is.null(sens) || !inherits(sens, "data.frame") || nrow(sens) == 0) {
    # default: all features sensitive to all threats
    sens <- base::expand.grid(
      feature = x$data$features$id,
      threat  = threats$id,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    sens <- unique(sens)
  }

  if (!all(c("feature","threat") %in% names(sens))) {
    stop("x$data$sensitivity must contain columns: feature, threat.", call. = FALSE)
  }

  # ensure deltas exist with defaults
  if (!("delta1" %in% names(sens))) sens$delta1 <- 0
  if (!("delta2" %in% names(sens))) sens$delta2 <- NA_real_
  if (!("delta3" %in% names(sens))) sens$delta3 <- 0
  if (!("delta4" %in% names(sens))) sens$delta4 <- 1

  sens$feature <- as.integer(sens$feature)
  sens$threat  <- as.integer(sens$threat)
  sens$delta1  <- as.numeric(sens$delta1); sens$delta1[is.na(sens$delta1)] <- 0
  sens$delta3  <- as.numeric(sens$delta3); sens$delta3[is.na(sens$delta3)] <- 0
  sens$delta4  <- as.numeric(sens$delta4); sens$delta4[is.na(sens$delta4)] <- 1
  sens$delta2  <- as.numeric(sens$delta2)

  # fill delta2 with max intensity per threat (like your old code path)
  if (anyNA(sens$delta2)) {
    if (nrow(dt_present) > 0) {
      max_int <- stats::aggregate(amount ~ threat, data = dt_present[, c("threat","amount")], FUN = max)
      names(max_int)[2] <- "max_amount"
      sens <- merge(sens, max_int, by = "threat", all.x = TRUE)
      sens$delta2[is.na(sens$delta2)] <- sens$max_amount[is.na(sens$delta2)]
      sens$max_amount <- NULL
    }
  }

  # helper: response & alpha exactly as legacy C++
  .pa_resp_alpha <- function(intensity, a, b, c, d) {
    if (intensity <= a) {
      resp_const <- d
      resp_var   <- 0.0
      alpha      <- 1.0 - resp_const
    } else if (intensity >= b) {
      resp_const <- c
      resp_var   <- d - c
      alpha      <- 1.0 - resp_const
    } else {
      resp_const <- (c * (intensity - a) - d * (intensity - b)) / (b - a)
      resp_var   <- ((a - intensity) * (c - d)) / (b - a)
      alpha      <- 1.0 - resp_const
    }
    list(resp_var = resp_var, alpha = alpha)
  }

  # Prepare df_base (feature presence/amount in PU)
  df_base <- dist_features[, c("pu","feature","amount","internal_pu","internal_feature"), drop = FALSE]
  df_base$pu <- as.integer(df_base$pu)
  df_base$feature <- as.integer(df_base$feature)
  df_base$amount <- as.numeric(df_base$amount)
  df_base <- df_base[is.finite(df_base$amount) & !is.na(df_base$amount) & df_base$amount > 0, , drop = FALSE]

  # Prepare threats present with intensity
  dtp <- dt_present[, c("pu","threat","amount"), drop = FALSE]
  names(dtp)[names(dtp) == "amount"] <- "intensity"

  if (nrow(df_base) == 0 || nrow(dtp) == 0) {
    de <- data.frame(
      pu = integer(0),
      action = character(0),
      feature = integer(0),
      benefit = numeric(0),
      internal_pu = integer(0),
      internal_action = integer(0),
      internal_feature = integer(0),
      stringsAsFactors = FALSE
    )
  } else {

    # Join (pu,feature) x (pu,threat) then filter by sensitivity(feature,threat)
    pf <- merge(df_base, dtp, by = "pu")                    # adds threat + intensity
    pf <- merge(pf, sens, by = c("feature","threat", "internal_feature"))       # keep only sensitive pairs

    if (nrow(pf) == 0) {
      de <- data.frame(
        pu = integer(0),
        action = character(0),
        feature = integer(0),
        benefit = numeric(0),
        internal_pu = integer(0),
        internal_action = integer(0),
        internal_feature = integer(0),
        stringsAsFactors = FALSE
      )
    } else {

      if (is_binary) {
        # benefit = amount_is / count_sensitive_threats_present
        denom <- stats::aggregate(threat ~ pu + feature, data = pf, FUN = function(z) length(unique(z)))
        names(denom)[3] <- "d_is"
        pf <- merge(pf, denom, by = c("pu","feature"), all.x = TRUE)

        pf$action  <- paste0(action_prefix, pf$threat)
        pf$benefit <- pf$amount / pf$d_is

      } else {
        # continuous: amount_is * (resp_var*alpha/sum_alpha)
        ra <- mapply(
          FUN = function(intensity, a, b, c, d) .pa_resp_alpha(intensity, a, b, c, d),
          intensity = pf$intensity,
          a = pf$delta1, b = pf$delta2, c = pf$delta3, d = pf$delta4,
          SIMPLIFY = FALSE
        )
        pf$resp_var <- vapply(ra, `[[`, numeric(1), "resp_var")
        pf$alpha    <- vapply(ra, `[[`, numeric(1), "alpha")

        sum_a <- stats::aggregate(alpha ~ pu + feature, data = pf, FUN = sum)
        names(sum_a)[3] <- "sum_alpha"
        pf <- merge(pf, sum_a, by = c("pu","feature"), all.x = TRUE)

        pf$action <- paste0(action_prefix, pf$threat)
        pf$benefit <- 0.0
        ok <- is.finite(pf$sum_alpha) & !is.na(pf$sum_alpha) & (pf$sum_alpha > 0)
        pf$benefit[ok] <- pf$amount[ok] * (pf$resp_var[ok] * pf$alpha[ok]) / pf$sum_alpha[ok]
      }

      # map internal_action
      pf$internal_action <- unname(action_index[as.character(pf$action)])

      # build dist_effects
      de <- pf[, c("pu","action","internal_feature","benefit","internal_pu","internal_action"), drop = FALSE]
      names(de)[names(de) == "internal_feature"] <- "feature"
      de$feature <- as.integer(de$feature)
      de$internal_feature <- de$feature
      de$benefit <- as.numeric(de$benefit)

      de <- de[, c("pu","action","feature","benefit","internal_pu","internal_action","internal_feature"), drop = FALSE]
      de <- de[is.finite(de$benefit) & !is.na(de$benefit) & de$benefit != 0, , drop = FALSE]
    }
  }

  # ------------------------------------------------------------
  # 4) Store into Data
  # ------------------------------------------------------------
  x$data$actions <- actions
  x$data$dist_actions <- dist_actions
  x$data$dist_effects <- de

  # optional compat: keep dist_benefit alias
  x$data$dist_benefit <- if (inherits(de, "data.frame") && nrow(de) > 0) de else NULL

  # meta for debugging/printing
  if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
  x$data$meta$actions_generated_from_legacy <- TRUE
  x$data$meta$legacy_actions_rule <- "one action per threat"
  x$data$meta$effects_rule <- if (is_binary) {
    "binary: benefit = amount_is / (# sensitive threats present in PU for feature)"
  } else {
    "continuous: benefit = amount_is * (resp_var*alpha/sum_alpha) using delta1..delta4"
  }

  if (is.null(x$data$model_args)) x$data$model_args <- list()

  if (is.null(x$data$model_args$benefit_exponent)) {
    x$data$model_args$benefit_exponent <- as.numeric(benefit_exponent)[1]
  }
  if (is.null(x$data$model_args$curve_segments)) {
    x$data$model_args$curve_segments <- as.integer(curve_segments)[1]
  }

  if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
  x$data$meta$benefit_transform <- list(
    type = "power",
    exponent = x$data$model_args$benefit_exponent,
    segments = x$data$model_args$curve_segments
  )

  invisible(x)
}


# ------------------------------------------------------------------------------
# Legacy adapter: convert features$target_* to x$data$targets (model-ready format)
# - required cols: feature (INTERNAL feature id), type, target_value
# - type ∈ {conservation, recovery, mixed_total}
# ------------------------------------------------------------------------------

.pa_targets_from_features_legacy <- function(x, overwrite = FALSE, warn = TRUE) {

  stopifnot(inherits(x, "Data"))

  fmt <- x$data$meta$input_format %||% NA_character_
  if (!identical(fmt, "legacy")) {
    return(invisible(x))
  }

  # if already has targets, do nothing unless overwrite=TRUE
  if (!is.null(x$data$targets) &&
      inherits(x$data$targets, "data.frame") &&
      nrow(x$data$targets) > 0 &&
      !isTRUE(overwrite)) {
    return(invisible(x))
  }

  feats <- x$data$features
  if (is.null(feats) || !inherits(feats, "data.frame") || nrow(feats) == 0) {
    stop("Legacy targets: missing x$data$features.", call. = FALSE)
  }

  # Need internal_id mapping
  if (!("internal_id" %in% names(feats))) {
    # fall back to index mapping if present
    if (is.null(x$data$index$feature)) stop("Legacy targets: missing feature index.", call. = FALSE)
    feats$internal_id <- unname(x$data$index$feature[as.character(feats$id)])
  }

  if (!("target_recovery" %in% names(feats))) {
    stop("Legacy targets: features$target_recovery is required in legacy mode.", call. = FALSE)
  }
  if (!("target_conservation" %in% names(feats))) {
    feats$target_conservation <- 0
  }

  tr <- as.numeric(feats$target_recovery)
  tc <- as.numeric(feats$target_conservation)

  if (anyNA(feats$internal_id)) {
    stop("Legacy targets: some features have missing internal_id.", call. = FALSE)
  }

  out <- rbind(
    data.frame(
      feature = as.integer(feats$internal_id),
      type = "recovery",
      target_value = tr,
      stringsAsFactors = FALSE
    ),
    data.frame(
      feature = as.integer(feats$internal_id),
      type = "conservation",
      target_value = tc,
      stringsAsFactors = FALSE
    )
  )

  # drop zeros / NAs
  out <- out[is.finite(out$target_value) & !is.na(out$target_value) & out$target_value > 0, , drop = FALSE]

  # if nothing to add, keep targets NULL (clean)
  if (nrow(out) == 0) {
    x$data$targets <- NULL
    if (isTRUE(warn)) {
      warning("Legacy targets: no positive target_* values found in features; x$data$targets left as NULL.", call. = FALSE, immediate. = TRUE)
    }
    return(invisible(x))
  }

  # aggregate (feature,type) just in case
  out <- stats::aggregate(target_value ~ feature + type, data = out, FUN = sum)

  x$data$targets <- out

  if (isTRUE(warn)) {
    warning(
      "Legacy input: created x$data$targets from features$target_recovery/target_conservation.",
      call. = FALSE, immediate. = TRUE
    )
  }

  invisible(x)
}



# -------------------------------------------------------------------------
# Internal helpers SOLUTIONS
# -------------------------------------------------------------------------
.pa_extract_solution_tables <- function(x, sol, threshold = 0.5) {
  stopifnot(inherits(x, "Data"))
  stopifnot(is.numeric(sol))

  # --- get model list (snapshot) or from ptr
  ml <- x$data$model_list %||% NULL
  if (is.null(ml)) {
    stopifnot(!is.null(x$data$model_ptr))
    ml <- rcpp_optimization_problem_as_list(x$data$model_ptr)
  }

  # --- fallbacks if model metadata missing
  pu_df <- x$data$pu
  da_df <- x$data$dist_actions_model %||% x$data$dist_actions
  df_df <- x$data$dist_features

  n_pu <- as.integer(ml$n_pu %||% nrow(pu_df) %||% 0L)
  n_x  <- as.integer(ml$n_x  %||% (if (inherits(da_df, "data.frame")) nrow(da_df) else 0L))
  n_z  <- as.integer(ml$n_z  %||% (if (inherits(df_df, "data.frame")) nrow(df_df) else 0L))

  w_off <- as.integer(ml$w_offset %||% 0L)
  x_off <- as.integer(ml$x_offset %||% 0L)
  z_off <- as.integer(ml$z_offset %||% 0L)

  # --- safe slice helper
  slice_safe <- function(v, start1, end1) {
    if (length(v) == 0) return(numeric(0))
    if (is.na(start1) || is.na(end1) || start1 > end1) return(numeric(0))
    end1 <- min(end1, length(v))
    start1 <- min(max(1L, start1), end1)
    v[start1:end1]
  }

  # --- slices (offsets 0-based; R 1-based)
  w  <- if (n_pu > 0L) slice_safe(sol, w_off + 1L, w_off + n_pu) else numeric(0)
  xv <- if (n_x  > 0L) slice_safe(sol, x_off + 1L, x_off + n_x ) else numeric(0)
  zv <- if (n_z  > 0L) slice_safe(sol, z_off + 1L, z_off + n_z ) else numeric(0)

  # -------------------------
  # 1) PU selection table
  # -------------------------
  pu_out <- pu_df

  if (!inherits(pu_out, "data.frame") || nrow(pu_out) == 0) {
    pu_out <- data.frame()
  } else {
    if (length(w) != nrow(pu_out)) {
      warning(
        "Mismatch: nrow(pu) = ", nrow(pu_out),
        " but length(w slice) = ", length(w),
        ". Setting pu$selected = NA.",
        call. = FALSE
      )
      pu_out$selected <- rep(NA_integer_, nrow(pu_out))
    } else {
      pu_out$selected <- as.integer(w > threshold)
    }
  }

  # -------------------------
  # 2) Actions selection table
  # -------------------------
  da_out <- da_df

  if (!inherits(da_out, "data.frame") || nrow(da_out) == 0) {
    da_out <- data.frame()
  } else {
    if (length(xv) != nrow(da_out)) {
      warning(
        "Mismatch: nrow(dist_actions_model) = ", nrow(da_out),
        " but length(x slice) = ", length(xv),
        ". Setting actions$selected = NA.",
        call. = FALSE
      )
      da_out$selected <- rep(NA_integer_, nrow(da_out))
    } else {
      da_out$selected <- as.integer(xv > threshold)
    }

    # uniqueness check for mapping keys used later
    if (all(c("internal_pu", "internal_action") %in% names(da_out))) {
      key_da <- paste0(da_out$internal_pu, ":", da_out$internal_action)
      if (anyDuplicated(key_da)) {
        stop(
          "dist_actions_model has duplicated (internal_pu, internal_action) pairs. ",
          "This breaks mapping of effects->x. Fix upstream (ensure unique feasible pairs).",
          call. = FALSE
        )
      }
    }
  }

  # -------------------------
  # 3) Features achieved (baseline + recovery + total)
  # -------------------------
  df <- df_df
  feats <- x$data$features

  # baseline from z
  base_by_feat <- data.frame(internal_feature = integer(0), baseline_contrib = numeric(0))
  if (!is.null(df) && inherits(df, "data.frame") && nrow(df) > 0 &&
      length(zv) == nrow(df) &&
      all(c("internal_feature", "amount") %in% names(df))) {

    df2 <- df
    df2$z <- zv
    df2$baseline_contrib <- as.numeric(df2$amount) * as.numeric(df2$z)

    base_by_feat <- stats::aggregate(
      baseline_contrib ~ internal_feature,
      data = df2,
      FUN = sum
    )
  } else if (!is.null(df) && inherits(df, "data.frame") && nrow(df) > 0 && length(zv) != nrow(df)) {
    warning(
      "Mismatch: nrow(dist_features) = ", nrow(df),
      " but length(z slice) = ", length(zv),
      ". baseline_contrib set to 0.",
      call. = FALSE
    )
  }

  # recovery from effects/benefit-loss (respect objective column if configured)
  args  <- x$data$model_args %||% list()
  oid   <- args$objective_id %||% NA_character_
  oargs <- args$objective_args %||% list()

  de <- x$data$dist_effects_model %||% x$data$dist_effects

  value_col <- if (identical(oid, "min_loss")) {
    as.character(oargs$loss_col %||% "loss")[1]
  } else {
    as.character(oargs$benefit_col %||% "benefit")[1]
  }

  rec_by_feat <- data.frame(internal_feature = integer(0), recovery_contrib = numeric(0))

  if (!is.null(de) && inherits(de, "data.frame") && nrow(de) > 0 &&
      length(xv) > 0 &&
      all(c("internal_pu","internal_action","internal_feature") %in% names(de)) &&
      value_col %in% names(de) &&
      inherits(da_out, "data.frame") && nrow(da_out) > 0 &&
      all(c("internal_pu","internal_action") %in% names(da_out))) {

    key_da <- paste0(da_out$internal_pu, ":", da_out$internal_action)
    map <- stats::setNames(seq_len(nrow(da_out)), key_da)

    key_de <- paste0(de$internal_pu, ":", de$internal_action)
    xrow <- unname(map[key_de])

    ok <- !is.na(xrow)
    if (!all(ok)) {
      warning(
        sum(!ok),
        " rows in dist_effects have no matching (pu,action) in dist_actions_model and were ignored.",
        call. = FALSE
      )
      de <- de[ok, , drop = FALSE]
      xrow <- xrow[ok]
    }

    # IMPORTANT: guard if xrow indexes exceed xv
    ok2 <- xrow >= 1L & xrow <= length(xv)
    if (!all(ok2)) {
      warning(
        sum(!ok2),
        " mapped x rows fall outside the x slice (length(x) = ", length(xv), "); ignored.",
        call. = FALSE
      )
      de <- de[ok2, , drop = FALSE]
      xrow <- xrow[ok2]
    }

    rec_contrib <- as.numeric(de[[value_col]]) * as.numeric(xv[xrow])

    tmp <- data.frame(
      internal_feature = as.integer(de$internal_feature),
      recovery_contrib = rec_contrib
    )
    rec_by_feat <- stats::aggregate(recovery_contrib ~ internal_feature, data = tmp, FUN = sum)
  }

  # merge feature summary
  if (is.null(feats) || !inherits(feats, "data.frame") || nrow(feats) == 0) {
    feat_tbl <- data.frame()
  } else {
    feat_tbl <- data.frame(
      internal_feature = as.integer(feats$internal_id),
      feature = as.character(feats$name),
      stringsAsFactors = FALSE
    )

    feat_tbl <- dplyr::left_join(feat_tbl, base_by_feat, by = "internal_feature")
    feat_tbl <- dplyr::left_join(feat_tbl, rec_by_feat,  by = "internal_feature")
    feat_tbl$baseline_contrib[is.na(feat_tbl$baseline_contrib)] <- 0
    feat_tbl$recovery_contrib[is.na(feat_tbl$recovery_contrib)] <- 0
    feat_tbl$total <- feat_tbl$baseline_contrib + feat_tbl$recovery_contrib
  }

  # -------------------------
  # 4) Targets summary (si existen)
  # -------------------------
  tgt <- x$data$targets
  tgt_out <- NULL

  if (inherits(tgt, "data.frame") && nrow(tgt) > 0 &&
      all(c("feature","type","target_value") %in% names(tgt)) &&
      inherits(feat_tbl, "data.frame") && nrow(feat_tbl) > 0) {

    tgt2 <- tgt
    tgt2$feature <- as.integer(tgt2$feature)
    tgt2$type <- as.character(tgt2$type)
    tgt2$target_value <- as.numeric(tgt2$target_value)

    achieved <- feat_tbl[, c("internal_feature","baseline_contrib","recovery_contrib","total")]
    names(achieved)[1] <- "feature"

    tgt_out <- dplyr::left_join(tgt2, achieved, by = "feature")

    tgt_out$achieved <- dplyr::case_when(
      tgt_out$type == "conservation" ~ tgt_out$baseline_contrib,
      tgt_out$type == "recovery"     ~ tgt_out$recovery_contrib,
      tgt_out$type == "mixed_total"  ~ tgt_out$total,
      TRUE ~ NA_real_
    )
    tgt_out$gap <- tgt_out$achieved - tgt_out$target_value
  }

  list(
    pu = pu_out,
    actions = da_out,
    features = feat_tbl,
    targets = tgt_out
  )
}


# -------------------------------------------------------------------------
# Internal helpers AREAS
# -------------------------------------------------------------------------
.pa_get_area_vec <- function(x, area_col = NULL, area_unit = c("m2", "ha", "km2")) {
  stopifnot(inherits(x, "Data"))
  area_unit <- match.arg(area_unit)

  pu <- x$data$pu
  stopifnot(!is.null(pu), inherits(pu, "data.frame"))

  # choose column
  if (is.null(area_col)) {
    cand <- c("area", "Area", "AREA", "area_m2", "area_ha", "area_km2")
    area_col <- cand[cand %in% names(pu)][1]
  }
  if (is.na(area_col) || is.null(area_col)) {
    # try sf geometry if present
    if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) {
      a <- as.numeric(sf::st_area(x$data$pu_sf))
      # st_area is in m2 usually (units)
      area <- a
    } else if ("geometry" %in% names(pu) && inherits(pu$geometry, "sfc")) {
      a <- as.numeric(sf::st_area(sf::st_as_sf(pu)$geometry))
      area <- a
    } else {
      stop("No area column found. Provide area_col or store PU geometry (x$data$pu_sf).", call. = FALSE)
    }
  } else {
    area <- as.numeric(pu[[area_col]])
  }

  if (any(!is.finite(area))) stop("Area contains non-finite values.", call. = FALSE)

  # convert units if needed (assume column is in m2 unless name hints)
  nm <- tolower(area_col %||% "")
  if (grepl("ha", nm)) {
    # already ha
    area_m2 <- area * 10000
  } else if (grepl("km2", nm)) {
    area_m2 <- area * 1e6
  } else {
    area_m2 <- area
  }

  out <- switch(
    area_unit,
    m2  = area_m2,
    ha  = area_m2 / 10000,
    km2 = area_m2 / 1e6
  )

  out
}

# helper: ensure model built and model_list snapshot available
.pa_ensure_model_snapshot <- function(x) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_ptr) || !isTRUE(x$data$has_model)) {
    x <- .pa_build_model(x)
  }
  if (is.null(x$data$model_list)) {
    x <- .pa_refresh_model_snapshot(x)
  }
  x
}

# helper: add 1 linear constraint sum(coeff_j * x_j) (sense) rhs
# NOTE: replace this with YOUR real C++ bridge that appends rows/triplets/rhs/sense.
.pa_add_linear_constraint <- function(x, var_index_0based, coeff, sense = c(">=", "<=", "=="), rhs, name = NULL) {
  sense <- match.arg(sense)
  stopifnot(inherits(x, "Data"))
  stopifnot(is.numeric(var_index_0based), is.numeric(coeff), length(var_index_0based) == length(coeff))
  stopifnot(is.numeric(rhs), length(rhs) == 1)

  if (is.null(x$data$model_ptr)) stop("Model pointer missing. Build model first.", call. = FALSE)

  # ---- HERE: call your Rcpp to append one row.
  # You likely have something like:
  # rcpp_add_constraint(model_ptr, i = row_ids, j = col_ids, x = values, sense, rhs, name)
  #
  # I'll write a generic placeholder:
  rcpp_add_linear_constraint(
    x$data$model_ptr,
    j0 = as.integer(var_index_0based),
    x  = as.numeric(coeff),
    sense = sense,
    rhs = as.numeric(rhs),
    name = as.character(name %||% "")
  )

  # refresh snapshot (optional but useful for printing/sol extraction)
  x <- .pa_refresh_model_snapshot(x)
  x
}




# -------------------------------------------------------------------------
# Internal helpers spatial relations
# -------------------------------------------------------------------------

.pa_validate_relation <- function(rel, n_pu, allow_self = FALSE,
                                  dup_agg = c("sum", "max", "min", "mean")) {

  stopifnot(is.data.frame(rel))
  dup_agg <- match.arg(dup_agg)

  req <- c("internal_pu1", "internal_pu2", "weight")
  miss <- setdiff(req, names(rel))
  if (length(miss) > 0) stop("Relation is missing columns: ", paste(miss, collapse = ", "), call. = FALSE)

  rel$internal_pu1 <- as.integer(rel$internal_pu1)
  rel$internal_pu2 <- as.integer(rel$internal_pu2)
  rel$weight <- as.numeric(rel$weight)

  if (anyNA(rel$internal_pu1) || anyNA(rel$internal_pu2) || anyNA(rel$weight)) {
    stop("Relation has NA in internal_pu1/internal_pu2/weight.", call. = FALSE)
  }
  if (any(rel$internal_pu1 < 1L | rel$internal_pu1 > n_pu)) stop("internal_pu1 out of range.", call. = FALSE)
  if (any(rel$internal_pu2 < 1L | rel$internal_pu2 > n_pu)) stop("internal_pu2 out of range.", call. = FALSE)
  if (!allow_self && any(rel$internal_pu1 == rel$internal_pu2)) stop("Self-edges are not allowed.", call. = FALSE)
  if (any(!is.finite(rel$weight)) || any(rel$weight < 0)) stop("weight must be finite and >= 0.", call. = FALSE)

  # undirected: canonical ordering
  a <- pmin(rel$internal_pu1, rel$internal_pu2)
  b <- pmax(rel$internal_pu1, rel$internal_pu2)
  rel$internal_pu1 <- a
  rel$internal_pu2 <- b

  # aggregate duplicates
  key <- paste(a, b)
  if (anyDuplicated(key) != 0) {
    FUN <- switch(
      dup_agg,
      sum  = sum,
      max  = max,
      min  = min,
      mean = mean
    )
    rel <- stats::aggregate(weight ~ internal_pu1 + internal_pu2, data = rel, FUN = FUN)
  } else {
    rel <- rel[, c("internal_pu1", "internal_pu2", "weight",
                   intersect(names(rel), c("pu1","pu2","distance","source"))),
               drop = FALSE]
  }

  rel
}


.pa_action_weights_vector <- function(actions_df,
                                      action_weights = NULL,
                                      subset_actions = NULL,
                                      default_weight = 1) {

  stopifnot(is.data.frame(actions_df), nrow(actions_df) > 0)

  n_actions <- nrow(actions_df)

  w <- rep(as.numeric(default_weight)[1], n_actions)

  if (!is.null(subset_actions)) {
    subset_actions <- as.integer(subset_actions)
    if (anyNA(subset_actions)) stop("subset_actions contains NA.", call. = FALSE)
    if (any(subset_actions < 1L | subset_actions > n_actions)) {
      stop("subset_actions out of range (1..n_actions).", call. = FALSE)
    }

    # si se especifica subset y NO se da vector completo, el default para acciones fuera del subset suele ser 0
    w[-subset_actions] <- 0
  }

  if (!is.null(action_weights)) {
    action_weights <- as.numeric(action_weights)
    if (any(!is.finite(action_weights)) || any(action_weights < 0)) {
      stop("action_weights must be finite and >= 0.", call. = FALSE)
    }

    if (length(action_weights) == n_actions) {
      w <- action_weights

    } else if (!is.null(subset_actions) && length(action_weights) == length(subset_actions)) {
      w[subset_actions] <- action_weights

    } else {
      stop(
        "action_weights must have length n_actions (= ", n_actions, ") ",
        "or length(subset_actions) (= ", length(subset_actions) %||% 0, ").",
        call. = FALSE
      )
    }
  }

  w
}


.pa_model_frag_vars_summary <- function(x) {
  if (!inherits(x, "Data")) return(NULL)
  ml <- x$data$model_list
  if (is.null(ml) || !is.list(ml)) return(NULL)

  # defensivo: algunos campos pueden no existir
  get_int0 <- function(nm) {
    v <- ml[[nm]]
    if (is.null(v)) return(0L)
    as.integer(v)[1]
  }

  out <- list(
    n_y_pu            = get_int0("n_y_pu"),
    n_y_actions       = get_int0("n_y_actions"),
    n_y_interventions = get_int0("n_y_interventions"),
    y_pu_offset            = get_int0("y_pu_offset"),
    y_actions_offset       = get_int0("y_actions_offset"),
    y_interventions_offset = get_int0("y_interventions_offset")
  )

  # si no hay ninguna y*, devuelve NULL para no ensuciar print
  if ((out$n_y_pu + out$n_y_actions + out$n_y_interventions) == 0L) return(NULL)
  out
}


# -------------------------------------------------------------------------
# Internal helpers multi objectives
# -------------------------------------------------------------------------

.pa_abort <- function(...) stop(paste0(...), call. = FALSE)

.pa_apply_runtime_updates_to_model <- function(model, x) {

  upd <- x$data$runtime_updates %||% list()
  reg <- x$data$model_registry %||% list()

  # nada que hacer
  if (length(upd) == 0) return(model)

  bigM <- as.numeric(upd$bigM %||% 1e15)[1]

  # Helper: relajar una fila para que sea redundante
  relax_row <- function(i) {
    s <- model$sense[i]
    if (identical(s, ">=")) {
      model$rhs[i] <<- -bigM
    } else if (identical(s, "<=")) {
      model$rhs[i] <<-  bigM
    } else if (identical(s, "==")) {
      # relaja igualdad convirtiéndola en <= bigM (redundante)
      model$sense[i] <<- "<="
      model$rhs[i]   <<-  bigM
    } else {
      # por seguridad: si aparece algo raro, lo relajo como <= bigM
      model$sense[i] <<- "<="
      model$rhs[i]   <<-  bigM
    }
    invisible(NULL)
  }

  # 1) activar/desactivar constraints por grupos (si el registry lo soporta)
  # upd$deactivate_constraints puede ser vector de ids lógicos (por nombre) o filas directas
  if (!is.null(upd$deactivate_rows)) {
    rows <- as.integer(upd$deactivate_rows)
    rows <- rows[rows >= 1L & rows <= length(model$rhs)]
    for (i in rows) relax_row(i)
  }

  if (!is.null(upd$deactivate_groups) && !is.null(reg$cons)) {
    for (g in upd$deactivate_groups) {
      rows <- reg$cons[[g]] %||% integer(0)
      rows <- as.integer(rows)
      rows <- rows[rows >= 1L & rows <= length(model$rhs)]
      for (i in rows) relax_row(i)
    }
  }

  # 2) epsilons: update RHS de constraints epsilon (sin cambiar estructura)
  # ejemplo: upd$epsilon <- list(f2 = 0.25, f3 = 10)
  if (!is.null(upd$epsilon) && !is.null(reg$cons$epsilon)) {
    eps_list <- upd$epsilon
    for (nm in names(eps_list)) {
      rows <- reg$cons$epsilon[[nm]] %||% integer(0)
      rows <- as.integer(rows)
      if (length(rows) == 0) next
      rhs <- as.numeric(eps_list[[nm]])[1]
      # aquí asumes que la fila es del tipo f(x) >= epsilon
      model$rhs[rows] <- rhs
    }
  }

  # 3A) objetivo: override directo (vector obj completo)
  # Esto es lo que necesita prioriactionsMO (weighted, etc.)
  if (!is.null(upd$obj)) {
    obj_new <- as.numeric(upd$obj)

    if (length(obj_new) != length(model$obj)) {
      stop(
        "runtime_updates$obj length mismatch: ",
        length(obj_new), " != ", length(model$obj),
        call. = FALSE
      )
    }

    model$obj <- obj_new

    if (!is.null(upd$modelsense)) {
      ms <- as.character(upd$modelsense)[1]
      if (!ms %in% c("min", "max")) stop("runtime_updates$modelsense must be 'min' or 'max'.", call. = FALSE)
      model$modelsense <- ms
    }
  }

  # 3) objetivo: swap por plantilla (vector obj completo)
  if (!is.null(upd$objective_template) && !is.null(reg$obj_templates)) {
    tpl <- reg$obj_templates[[upd$objective_template]]
    if (!is.null(tpl)) {
      if (length(tpl$obj) != length(model$obj)) {
        stop("Objective template length mismatch.", call. = FALSE)
      }
      model$obj <- tpl$obj
      if (!is.null(tpl$modelsense)) model$modelsense <- tpl$modelsense
    }
  }

  # 4) pesos AUGMECON: por ejemplo coef de slacks en obj (sin reconstruir)
  if (!is.null(upd$slack_weight) && !is.null(reg$vars$slack)) {
    w <- as.numeric(upd$slack_weight)[1]
    slack_ids <- as.integer(reg$vars$slack)
    slack_ids <- slack_ids[slack_ids >= 1L & slack_ids <= length(model$obj)]
    model$obj[slack_ids] <- w
  }

  model
}


# -------------------------------------------------------------------------
# Internal helpers spatial relations
# -------------------------------------------------------------------------
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

  pu_coords <- NULL


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

  # optional: store coordinates if present (tabular users)
  if (all(c("x", "y") %in% names(pu))) {
    pu_coords <- data.frame(
      id = pu$id,
      x  = as.numeric(pu$x),
      y  = as.numeric(pu$y),
      stringsAsFactors = FALSE
    )
    if (any(!is.finite(pu_coords$x) | !is.finite(pu_coords$y))) {
      stop("pu$x/pu$y contain non-finite values.", call. = FALSE)
    }
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
    # boundary$boundary <- base::round(as.numeric(boundary$boundary), 3)

    if (!all(boundary$id1 %in% pu$id) || !all(boundary$id2 %in% pu$id)) {
      warning("boundary contains PU ids not present in pu; they will be removed.", call. = FALSE, immediate. = TRUE)
      keep <- boundary$id1 %in% pu$id & boundary$id2 %in% pu$id
      boundary <- boundary[keep, , drop = FALSE]
    }

    if (nrow(boundary) == 0) boundary <- NULL
  }

  # =========================
  # rounding
  # =========================
  # pu$cost <- base::round(pu$cost, 3)
  # dist_features$amount <- base::round(dist_features$amount, 3)

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

    # threats$blm_actions <- base::round(threats$blm_actions, 3)
    # dist_threats$amount <- base::round(dist_threats$amount, 3)
    # dist_threats$action_cost <- base::round(dist_threats$action_cost, 3)
    # sensitivity$delta1 <- base::round(sensitivity$delta1, 3)
    # sensitivity$delta2 <- base::round(sensitivity$delta2, 3)
    # sensitivity$delta3 <- base::round(sensitivity$delta3, 3)
    # sensitivity$delta4 <- base::round(sensitivity$delta4, 3)

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

      # ---- NEW: spatial storage
      pu_coords = pu_coords,
      spatial_relations = list(),

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


  # =========================
  # boundary -> spatial relation ("boundary")
  # =========================
  if (!is.null(boundary) && inherits(boundary, "data.frame") && nrow(boundary) > 0) {

    rel <- data.frame(
      internal_pu1 = unname(pu_index[as.character(boundary$id1)]),
      internal_pu2 = unname(pu_index[as.character(boundary$id2)]),
      weight       = as.numeric(boundary$boundary),
      source       = "boundary_table",
      stringsAsFactors = FALSE
    )

    # por si acaso (aunque ya filtraste ids), chequeo defensivo:
    if (anyNA(rel$internal_pu1) || anyNA(rel$internal_pu2)) {
      stop("boundary contains PU ids not present in pu (after filtering).", call. = FALSE)
    }

    rel <- .pa_validate_relation(rel, n_pu = nrow(pu), allow_self = FALSE, dup_agg = "sum")
    rel$relation_name <- "boundary"

    if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
      x$data$spatial_relations <- list()
    }
    x$data$spatial_relations[["boundary"]] <- rel
  }

  x
}



# -------------------------------------------------------------------------
# Internal helpers objective relations
# -------------------------------------------------------------------------
.pa_register_objective <- function(x, alias, objective_id, model_type, objective_args, sense) {
  stopifnot(inherits(x, "Data"))
  if (is.null(alias)) return(x)

  alias <- as.character(alias)[1]
  if (!nzchar(alias)) stop("alias must be a non-empty string.", call. = FALSE)

  sense <- as.character(sense)[1]
  if (!sense %in% c("min", "max")) stop("sense must be 'min' or 'max'.", call. = FALSE)

  if (is.null(x$data$objectives) || !is.list(x$data$objectives)) {
    x$data$objectives <- list()
  }

  if (!is.null(x$data$objectives[[alias]])) {
    stop("Objective alias '", alias, "' already exists. Use a different alias.", call. = FALSE)
  }

  x$data$objectives[[alias]] <- list(
    alias = alias,
    objective_id = as.character(objective_id)[1],
    model_type = as.character(model_type)[1],
    objective_args = objective_args,
    sense = sense
  )

  x
}

# internal registry for MO objectives inside Data
.pa_init_objectives <- function(x) {
  if (is.null(x$data$objectives) || !is.list(x$data$objectives)) {
    x$data$objectives <- list()
  }
  x
}


.pa_get_objective_specs <- function(x) {
  if (is.null(x$data$objectives)) return(list())
  x$data$objectives
}



.pa_set_objective_linear <- function(x, obj, modelsense = c("min", "max")) {
  stopifnot(inherits(x, "Data"))
  modelsense <- match.arg(modelsense)

  # guarda runtime update para que .pa_apply_runtime_updates_to_model() lo aplique
  x$data$runtime_updates <- x$data$runtime_updates %||% list()
  x$data$runtime_updates$obj <- as.numeric(obj)
  x$data$runtime_updates$modelsense <- modelsense

  # marca "dirty" para que se regenere model_list desde ptr si aplica (o solo runtime update)
  x$data$meta$model_dirty <- TRUE
  x
}

.pa_mark_mo_needs <- function(x, needs) {
  stopifnot(inherits(x, "Data"))
  x$data$mo <- x$data$mo %||% list()
  x$data$mo$needs <- modifyList(x$data$mo$needs %||% list(), needs)
  x$data$meta$model_dirty <- TRUE
  x
}


.pa_fast_extract <- function(x, y, fun = c("sum", "mean", "max")) {
  fun <- match.arg(fun)

  stopifnot(inherits(x, "terra::SpatRaster") || inherits(x, "SpatRaster"))
  stopifnot(inherits(y, "terra::SpatVector") || inherits(y, "SpatVector") || inherits(y, "sf"))

  if (inherits(y, "SpatVector")) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Need 'sf' to convert SpatVector -> sf.", call. = FALSE)
    y <- sf::st_as_sf(y)
  }

  out <- matrix(NA_real_, nrow = nrow(y), ncol = terra::nlyr(x))
  colnames(out) <- make.unique(names(x))

  geomc <- as.character(sf::st_geometry_type(y, by_geometry = TRUE))

  # points
  point_idx <- grepl("POINT", geomc, fixed = TRUE)
  if (any(point_idx)) {
    ex <- terra::extract(x = x, y = sf::st_coordinates(y[point_idx, , drop = FALSE]))
    out[point_idx, ] <- as.matrix(ex[, -1, drop = FALSE]) # drop ID column
  }

  # lines
  line_idx <- grepl("LINE", geomc, fixed = TRUE)
  if (any(line_idx)) {
    fun2 <- switch(fun, mean = mean, sum = sum, max = max)
    out[line_idx, ] <- as.matrix(
      terra::extract(
        x = x,
        y = terra::vect(y[line_idx, , drop = FALSE]),
        ID = FALSE,
        touches = TRUE,
        fun = fun2,
        na.rm = TRUE
      )
    )
  }

  # polygons (prioritizr-like)
  poly_idx <- grepl("POLYGON", geomc, fixed = TRUE)
  if (any(poly_idx)) {
    if (!requireNamespace("exactextractr", quietly = TRUE)) {
      stop("Polygon feature extraction requires 'exactextractr'.", call. = FALSE)
    }

    # IMPORTANT: avoid mutating user's objects
    y2 <- y[poly_idx, , drop = FALSE]
    x2 <- x

    # (this is just to silence CRS/proj warnings like prioritizr; extraction uses coordinates)
    sf::st_crs(y2) <- sf::st_crs(NA_character_)
    terra::crs(x2) <- NA_character_

    out[poly_idx, ] <- as.matrix(
      exactextractr::exact_extract(
        x2,
        y2,
        fun = fun,
        progress = FALSE
      )
    )
  }

  out[!is.finite(out)] <- 0
  out[abs(out) < 1e-10] <- 0
  out
}



