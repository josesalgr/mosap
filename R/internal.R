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
#' problem_data <- input_data(
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
  # Preferred structure: Solution with diagnostics
  d <- NULL

  if (!is.null(x$diagnostics)) {
    d <- x$diagnostics
  } else {
    d <- x
  }

  code <- d$status_code %||% d$status %||% NA_integer_
  code <- as.integer(code)[1]

  if (is.na(code)) return("unknown")

  switch(
    as.character(code),
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
.pa_parse_targets <- function(x, targets, features = NULL) {
  feats <- x$data$features
  if (is.null(feats) || nrow(feats) == 0) {
    stop("x$data$features is missing/empty.", call. = FALSE)
  }
  if (!("id" %in% names(feats))) {
    stop("x$data$features must contain column 'id'.", call. = FALSE)
  }

  feat_ids <- as.numeric(feats$id)
  feat_names <- if ("name" %in% names(feats)) as.character(feats$name) else rep(NA_character_, length(feat_ids))

  .feature_name_from_id <- function(fid) {
    fid <- as.numeric(fid)
    feat_names[match(fid, feat_ids)]
  }

  # helper: map a feature identifier (id or name) to numeric id
  .map_feature <- function(v) {
    if (is.null(v)) return(NULL)
    if (is.factor(v)) v <- as.character(v)

    if (is.numeric(v) || is.integer(v)) {
      v <- as.numeric(v)
      bad <- v[!v %in% feat_ids]
      if (length(bad) > 0) {
        stop("Unknown feature id(s): ", paste(unique(bad), collapse = ", "), call. = FALSE)
      }
      return(v)
    }

    if (is.character(v)) {
      sup <- suppressWarnings(as.numeric(v))
      is_numlike <- !is.na(sup) & nzchar(v)
      out <- rep(NA_real_, length(v))

      if (any(is_numlike)) {
        vv <- sup[is_numlike]
        bad <- vv[!vv %in% feat_ids]
        if (length(bad) == 0) out[is_numlike] <- vv
      }

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
            ". Valid names include: ", paste0("'", utils::head(feat_names, 10), "'", collapse = ", "),
            if (length(feat_names) > 10) " ..." else "",
            call. = FALSE
          )
        }
        out[need_name] <- feat_ids[m]
      }

      return(as.numeric(out))
    }

    stop("Unsupported feature identifier type.", call. = FALSE)
  }

  .resolve_target_features <- function(features) {
    if (is.null(features)) return(feat_ids)
    f <- .map_feature(features)
    if (length(f) == 0) {
      stop("features resolved to an empty set.", call. = FALSE)
    }
    f <- unique(as.numeric(f))
    f
  }

  .finalize_targets <- function(f, t) {
    dt <- data.frame(
      feature = as.numeric(f),
      feature_name = .feature_name_from_id(f),
      target_raw = as.numeric(t),
      stringsAsFactors = FALSE
    )
    dt <- dt[order(dt$feature), , drop = FALSE]
    if (anyDuplicated(dt$feature)) {
      stop("Duplicate targets for the same feature. Please de-duplicate.", call. = FALSE)
    }
    dt
  }

  selected_features <- .resolve_target_features(features)

  # 1) data.frame(feature, target)
  if (inherits(targets, "data.frame")) {
    if (!all(c("feature", "target") %in% names(targets))) {
      stop("If targets is a data.frame it must contain columns 'feature' and 'target'.", call. = FALSE)
    }
    if (!is.null(features)) {
      warning(
        "'features' was supplied but targets is a data.frame with an explicit 'feature' column. ",
        "Ignoring 'features'.",
        call. = FALSE
      )
    }
    f <- .map_feature(targets$feature)
    t <- as.numeric(targets$target)
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)
    return(.finalize_targets(f, t))
  }

  # 2) matrix (one column) with optional rownames
  if (is.matrix(targets)) {
    if (ncol(targets) != 1) {
      stop("If targets is a matrix it must have exactly 1 column.", call. = FALSE)
    }
    t <- as.numeric(targets[, 1])
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)

    rn <- rownames(targets)
    if (!is.null(rn) && length(rn) == length(t)) {
      if (!is.null(features)) {
        warning(
          "'features' was supplied but targets is a matrix with rownames. Ignoring 'features'.",
          call. = FALSE
        )
      }
      f <- .map_feature(rn)
      return(.finalize_targets(f, t))
    }

    if (length(t) == 1) {
      return(.finalize_targets(selected_features, rep(t, length(selected_features))))
    }

    if (length(t) != length(selected_features)) {
      stop(
        "Matrix targets without rownames must have nrow = 1 or nrow = number of targeted features (",
        length(selected_features), ").",
        call. = FALSE
      )
    }

    return(.finalize_targets(selected_features, t))
  }

  # 3) scalar numeric -> recycle
  if (is.numeric(targets) && length(targets) == 1) {
    t <- as.numeric(targets)
    if (is.na(t)) stop("Target is NA.", call. = FALSE)
    return(.finalize_targets(selected_features, rep(t, length(selected_features))))
  }

  # 4) numeric vector
  if (is.numeric(targets) && length(targets) > 1) {
    t <- as.numeric(targets)
    if (any(is.na(t))) stop("Targets contain NA values.", call. = FALSE)

    nm <- names(targets)
    if (!is.null(nm) && any(nzchar(nm))) {
      if (!is.null(features)) {
        warning(
          "'features' was supplied but targets is a named vector. Ignoring 'features'.",
          call. = FALSE
        )
      }
      f <- .map_feature(nm)
      return(.finalize_targets(f, t))
    }

    if (length(t) != length(selected_features)) {
      stop(
        "Un-named numeric targets must have length 1 or length equal to the number of targeted features (",
        length(selected_features), ").",
        call. = FALSE
      )
    }

    return(.finalize_targets(selected_features, t))
  }

  # 5) character
  if (is.character(targets)) {
    if (length(targets) == 1 && grepl("^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*$", targets)) {
      t <- as.numeric(targets)
      return(.finalize_targets(selected_features, rep(t, length(selected_features))))
    }

    if (any(grepl("=", targets, fixed = TRUE))) {
      if (!is.null(features)) {
        warning(
          "'features' was supplied but targets uses 'feature=target' syntax. Ignoring 'features'.",
          call. = FALSE
        )
      }
      parts <- strsplit(targets, "=", fixed = TRUE)
      feat_part <- vapply(parts, `[[`, character(1), 1)
      val_part  <- vapply(parts, `[[`, character(1), 2)
      f <- .map_feature(trimws(feat_part))
      t <- as.numeric(trimws(val_part))
      if (any(is.na(t))) {
        stop("Could not parse numeric target values from character input.", call. = FALSE)
      }
      return(.finalize_targets(f, t))
    }

    stop(
      "Unsupported character targets format. Use 'feature=target' pairs or a single numeric string.",
      call. = FALSE
    )
  }

  stop("Unsupported targets format.", call. = FALSE)
}

.pa_store_targets <- function(x, targets_df) {
  stopifnot(inherits(x, "Problem"))
  stopifnot(inherits(targets_df, "data.frame"))
  stopifnot(all(c("feature", "type", "target_value") %in% names(targets_df)))

  # normalize minimal fields
  targets_df$feature <- as.integer(targets_df$feature)
  targets_df$type <- as.character(targets_df$type)
  targets_df$target_value <- as.numeric(targets_df$target_value)

  # ensure actions column exists
  if (!("actions" %in% names(targets_df))) {
    targets_df$actions <- NA_character_
  }
  targets_df$actions <- as.character(targets_df$actions)

  valid_types <- c("actions")
  bad_type <- setdiff(unique(targets_df$type), valid_types)
  if (length(bad_type) > 0) {
    stop("Unknown target types: ", paste(bad_type, collapse = ", "), call. = FALSE)
  }

  # initialize if empty
  if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
    x$data$targets <- targets_df

    if (!is.null(x$data$model_ptr)) {
      x$data$meta <- x$data$meta %||% list()
      x$data$meta$model_dirty <- TRUE
    }

    return(x)
  }

  old <- x$data$targets
  old$feature <- as.integer(old$feature)
  old$type <- as.character(old$type)
  old$target_value <- as.numeric(old$target_value)

  # backward compatibility: migrate old subset column if present
  if (!("actions" %in% names(old))) {
    if ("subset" %in% names(old)) {
      old$actions <- as.character(old$subset)
    } else {
      old$actions <- NA_character_
    }
  }
  old$actions <- as.character(old$actions)

  # optional informative warning for repeated keys
  key_old <- paste0(old$feature, "||", old$type, "||", old$actions)
  key_new <- paste0(targets_df$feature, "||", targets_df$type, "||", targets_df$actions)

  overlap <- intersect(key_old, key_new)
  if (length(overlap) > 0) {
    warning(
      "Additional targets were added for existing (feature, type, actions) combinations. ",
      "These target rows remain stored simultaneously and will be handled downstream. ",
      "Example key: ", overlap[1],
      call. = FALSE
    )
  }

  x$data$targets <- rbind(old, targets_df)

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
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

  if (is.null(de) || !inherits(de, "data.frame") || nrow(de) == 0) {
    return(NULL)
  }

  benefit_col <- as.character(benefit_col)[1]
  if (is.na(benefit_col) || !nzchar(benefit_col)) {
    stop("`benefit_col` must be a non-empty string.", call. = FALSE)
  }

  if (!(benefit_col %in% names(de))) {
    stop("dist_effects is missing column '", benefit_col, "'.", call. = FALSE)
  }

  out <- de

  if ("feature" %in% names(out))          out$feature <- as.integer(out$feature)
  if ("internal_pu" %in% names(out))      out$internal_pu <- as.integer(out$internal_pu)
  if ("internal_action" %in% names(out))  out$internal_action <- as.integer(out$internal_action)
  if ("internal_feature" %in% names(out)) out$internal_feature <- as.integer(out$internal_feature)

  out$benefit <- as.numeric(out[[benefit_col]])

  if (anyNA(out$benefit) || any(!is.finite(out$benefit))) {
    stop(
      "Column '", benefit_col, "' in dist_effects contains NA or non-finite values.",
      call. = FALSE
    )
  }

  out
}

.get_dist_effects_model <- function(x, mode = c("benefit", "loss", "delta")) {
  mode <- match.arg(mode)

  de <- x$data$dist_effects_model %||% x$data$dist_effects
  if (is.null(de) || !inherits(de, "data.frame") || nrow(de) == 0) {
    return(NULL)
  }

  out <- de

  if ("feature" %in% names(out))          out$feature <- as.integer(out$feature)
  if ("internal_pu" %in% names(out))      out$internal_pu <- as.integer(out$internal_pu)
  if ("internal_action" %in% names(out))  out$internal_action <- as.integer(out$internal_action)
  if ("internal_feature" %in% names(out)) out$internal_feature <- as.integer(out$internal_feature)

  if (mode == "benefit") {
    if (!("benefit" %in% names(out))) {
      stop("dist_effects is missing column 'benefit'.", call. = FALSE)
    }

    out$benefit <- as.numeric(out$benefit)
    if (anyNA(out$benefit) || any(!is.finite(out$benefit))) {
      stop("dist_effects$benefit contains NA or non-finite values.", call. = FALSE)
    }

    out$effect <- out$benefit
    return(out)
  }

  if (mode == "loss") {
    if (!("loss" %in% names(out))) {
      stop("dist_effects is missing column 'loss'.", call. = FALSE)
    }

    out$loss <- as.numeric(out$loss)
    if (anyNA(out$loss) || any(!is.finite(out$loss))) {
      stop("dist_effects$loss contains NA or non-finite values.", call. = FALSE)
    }

    out$effect <- out$loss
    return(out)
  }

  # mode == "delta"
  if (!("benefit" %in% names(out)) && !("loss" %in% names(out))) {
    stop(
      "dist_effects must contain at least one of 'benefit' or 'loss' to compute delta.",
      call. = FALSE
    )
  }

  if (!("benefit" %in% names(out))) out$benefit <- 0
  if (!("loss" %in% names(out)))    out$loss <- 0

  out$benefit <- as.numeric(out$benefit)
  out$loss    <- as.numeric(out$loss)

  if (anyNA(out$benefit) || any(!is.finite(out$benefit))) {
    stop("dist_effects$benefit contains NA or non-finite values.", call. = FALSE)
  }
  if (anyNA(out$loss) || any(!is.finite(out$loss))) {
    stop("dist_effects$loss contains NA or non-finite values.", call. = FALSE)
  }

  out$effect <- out$benefit - out$loss
  out
}


.pa_apply_targets_if_present <- function(x,
                                         zero_tol = 1e-12) {

  stopifnot(inherits(x, "Problem"))

  if (is.null(x$data$targets) || !inherits(x$data$targets, "data.frame") || nrow(x$data$targets) == 0) {
    return(invisible(x))
  }
  if (is.null(x$data$model_ptr)) {
    stop("No active model pointer found in x$data$model_ptr. Run add_targets_*() before applying targets.", call. = FALSE)
  }

  t <- x$data$targets
  req <- c("feature", "type", "target_value")
  miss <- setdiff(req, names(t))
  if (length(miss) > 0) {
    stop("x$data$targets is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  # backward compatibility: migrate subset -> actions
  if (!("actions" %in% names(t))) {
    if ("subset" %in% names(t)) {
      t$actions <- as.character(t$subset)
    } else {
      t$actions <- NA_character_
    }
  }

  t$feature <- as.integer(t$feature)
  t$type <- as.character(t$type)
  t$actions <- as.character(t$actions)
  t$target_value <- as.numeric(t$target_value)

  bad_type <- setdiff(unique(t$type), c("actions"))
  if (length(bad_type) > 0) {
    stop("Unknown target types in x$data$targets: ", paste(bad_type, collapse = ", "), call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 1) exact duplicates by (feature, type, actions) are NOT allowed
  # ------------------------------------------------------------------
  key_exact <- paste0(
    t$feature, "||",
    t$type, "||",
    ifelse(is.na(t$actions), "", t$actions)
  )

  dup_exact <- duplicated(key_exact) | duplicated(key_exact, fromLast = TRUE)

  if (any(dup_exact)) {
    bad <- t[dup_exact, c("feature", "type", "actions", "target_value"), drop = FALSE]
    ex <- utils::head(
      paste0(
        "(feature=", bad$feature,
        ", type='", bad$type,
        "', actions='", ifelse(is.na(bad$actions) | !nzchar(bad$actions), "ALL", bad$actions),
        "', target_value=", bad$target_value, ")"
      ),
      8
    )

    stop(
      "Multiple targets were defined for the same (feature, type, actions) combination.\n",
      "This is ambiguous and is not allowed.\n",
      "Examples: ", paste(ex, collapse = ", "),
      if (nrow(bad) > 8) paste0(" ... (", nrow(bad), " rows)") else "",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 2) same (feature, actions) with different type: allowed, but warn
  # ------------------------------------------------------------------
  key_feature_actions <- paste0(
    t$feature, "||",
    ifelse(is.na(t$actions), "", t$actions)
  )

  split_fa <- split(t$type, key_feature_actions, drop = TRUE)
  multi_type_keys <- names(split_fa)[vapply(split_fa, function(z) length(unique(z)) > 1, logical(1))]

  if (length(multi_type_keys) > 0) {
    ex_keys <- utils::head(multi_type_keys, 8)
    warning(
      "Multiple targets were defined for the same (feature, actions) combination with different target types.\n",
      "They will be applied as separate constraints, and the strongest one may dominate in practice.\n",
      "Examples: ", paste(ex_keys, collapse = ", "),
      if (length(multi_type_keys) > 8) paste0(" ... (", length(multi_type_keys), " combinations)") else "",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 3) drop near-zero targets
  # ------------------------------------------------------------------
  idx_zero <- is.finite(t$target_value) & abs(t$target_value) <= zero_tol
  if (any(idx_zero)) {
    z <- t[idx_zero, , drop = FALSE]
    warning(
      "Skipping ", nrow(z), " target(s) with target_value ~ 0 (|rhs| <= ", format(zero_tol), "). ",
      "They are redundant and can obscure infeasibility diagnostics.",
      call. = FALSE
    )
    t <- t[!idx_zero, , drop = FALSE]
  }

  if (nrow(t) == 0) {
    x$data$model_args$targets_applied <- TRUE
    x$data$model_args$targets_counts <- list(actions = 0L)
    return(invisible(x))
  }

  op <- x$data$model_ptr

  .mk_targets_df <- function(features, values, colname) {
    df <- data.frame(internal_id = as.integer(features), stringsAsFactors = FALSE)
    df[[colname]] <- as.numeric(values)
    df
  }

  .check_feature_present_in_df <- function(features, df, feature_col, what) {
    features <- as.integer(features)
    if (length(features) == 0) return(invisible(TRUE))

    present <- unique(as.integer(df[[feature_col]]))
    miss <- features[!features %in% present]

    if (length(miss) > 0) {
      ex <- paste(utils::head(sort(unique(miss)), 8), collapse = ", ")
      stop(
        "Infeasible targets detected: some features in ", what, " targets have no contributions in the model tables.\n",
        "Missing feature ids: ", ex,
        if (length(unique(miss)) > 8) paste0(" ... (", length(unique(miss)), " total)") else "",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  .filter_effects_by_actions <- function(de, actions_string) {
    if (is.na(actions_string) || !nzchar(actions_string)) {
      return(de)
    }

    actions_vals <- strsplit(actions_string, "\\|")[[1]]
    actions_vals <- unique(actions_vals[nzchar(actions_vals)])

    matched <- .pa_resolve_action_subset(x, actions_vals)
    keep_internal <- as.integer(matched$internal_id)

    de[de$internal_action %in% keep_internal, , drop = FALSE]
  }

  dbm <- .get_dist_benefit_model_from_effects(x, benefit_col = "benefit")
  if (is.null(dbm) || nrow(dbm) == 0) {
    stop("Targets present but no benefit column available in dist_effects.", call. = FALSE)
  }

  if (!("feature" %in% names(dbm))) {
    stop("dist_benefit_data must contain a 'feature' column for validation.", call. = FALSE)
  }

  if (!exists("rcpp_add_target_recovery", mode = "function")) {
    stop("Missing rcpp_add_target_recovery() in the package.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 4) Apply each row as a separate constraint
  # ------------------------------------------------------------------
  n_applied <- 0L

  for (ii in seq_len(nrow(t))) {
    tt <- t[ii, , drop = FALSE]
    actions_string <- tt$actions[1]

    dbm_sub <- .filter_effects_by_actions(dbm, actions_string)

    if (nrow(dbm_sub) == 0) {
      actions_lab <- if (is.na(actions_string) || !nzchar(actions_string)) "ALL actions" else actions_string
      stop(
        "Target for feature ", tt$feature[1], " and actions subset '", actions_lab,
        "' cannot be applied because no matching effect rows remain after filtering.",
        call. = FALSE
      )
    }

    .check_feature_present_in_df(tt$feature, dbm_sub, "feature", what = "actions")

    rcpp_add_target_recovery(
      op,
      features_data     = .mk_targets_df(tt$feature, tt$target_value, "target_actions"),
      dist_actions_data = x$data$dist_actions_model,
      dist_benefit_data = dbm_sub,
      target_col        = "target_actions"
    )

    n_applied <- n_applied + 1L
  }

  x$data$model_args$targets_applied <- TRUE
  x$data$model_args$targets_counts <- list(actions = n_applied)

  invisible(x)
}


# -------------------------------------------------------------------------
# Internal helpers MATHEMATICAL MODEL
# -------------------------------------------------------------------------
.pa_refresh_model_snapshot <- function(x, drop_triplets = TRUE) {
  stopifnot(inherits(x, "Problem"))

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


.pa_apply_action_max_per_pu_if_present <- function(x) {
  stopifnot(inherits(x, "Problem"))

  spec <- x$data$constraints$action_max_per_pu %||% NULL
  if (is.null(spec)) return(invisible(x))

  if (is.null(x$data$model_ptr)) {
    stop("No active model pointer found in x$data$model_ptr.", call. = FALSE)
  }

  if (!exists("rcpp_add_action_max_per_pu", mode = "function")) {
    stop("Missing rcpp_add_action_max_per_pu() in the package.", call. = FALSE)
  }

  da <- x$data$dist_actions_model
  if (is.null(da) || !inherits(da, "data.frame") || nrow(da) == 0) {
    stop("action_max_per_pu requires non-empty x$data$dist_actions_model.", call. = FALSE)
  }

  # defensive: required columns
  need <- c("internal_pu", "internal_action", "internal_row")
  miss <- setdiff(need, names(da))
  if (length(miss) > 0) {
    stop("dist_actions_model must contain columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  maxv <- as.integer(spec$max %||% 1L)
  if (!is.finite(maxv) || is.na(maxv) || maxv < 0L) stop("Invalid max in action_max_per_pu.", call. = FALSE)

  # Convert external ids -> internal ids
  pu_ext <- spec$pu %||% x$data$pu$id
  pu_ext <- as.integer(pu_ext)
  pu_map <- x$data$pu[, c("id","internal_id")]
  pu_int <- pu_map$internal_id[match(pu_ext, pu_map$id)]
  pu_int <- pu_int[!is.na(pu_int)]
  if (length(pu_int) == 0) stop("action_max_per_pu: no valid PUs after mapping to internal ids.", call. = FALSE)

  act_ext <- spec$actions %||% x$data$actions$id
  act_ext <- as.character(act_ext)
  act_map <- x$data$actions[, c("id","internal_id")]
  act_int <- act_map$internal_id[match(act_ext, act_map$id)]
  act_int <- act_int[!is.na(act_int)]
  if (length(act_int) == 0) stop("action_max_per_pu: no valid actions after mapping to internal ids.", call. = FALSE)

  # call C++
  res <- rcpp_add_action_max_per_pu(
    x = x$data$model_ptr,
    dist_actions_data = da,
    max_per_pu = maxv,
    internal_pu_ids = as.integer(pu_int),
    internal_action_ids = as.integer(act_int)
  )

  # optional registry
  x$data$model_registry <- x$data$model_registry %||% list(cons = list(), vars = list(), obj_templates = list(), objective = list())
  x$data$model_registry$cons$action_max_per_pu <- res

  invisible(x)
}

.pa_apply_area_constraints_if_present <- function(x) {
  stopifnot(inherits(x, "Problem"))

  cons <- x$data$constraints %||% list()
  if (length(cons) == 0L) return(x)

  if (is.null(cons$area_min) && is.null(cons$area_max)) {
    return(x)
  }

  if (is.null(x$data$model_ptr)) {
    stop("Model pointer is missing while applying area constraints.", call. = FALSE)
  }

  x <- .pa_refresh_model_snapshot(x)

  ml <- x$data$model_list
  if (is.null(ml)) {
    stop("Model snapshot is missing while applying area constraints.", call. = FALSE)
  }

  n_pu <- as.integer(ml$n_pu %||% 0L)
  if (n_pu <= 0L) {
    stop("Model has n_pu=0; cannot apply area constraints.", call. = FALSE)
  }

  w0 <- as.integer(ml$w_offset %||% 0L)
  j0 <- w0 + (0:(n_pu - 1L))

  # ---- area_min
  if (!is.null(cons$area_min)) {
    spec <- cons$area_min
    a <- .pa_get_area_vec(
      x,
      area_col = spec$area_col %||% NULL,
      area_unit = spec$unit %||% "m2"
    )

    if (length(a) != n_pu) {
      stop(
        "Area vector length (", length(a), ") != n_pu (", n_pu,
        ") while applying area_min constraint.",
        call. = FALSE
      )
    }

    x <- .pa_add_linear_constraint(
      x,
      var_index_0based = j0,
      coeff = a,
      sense = ">=",
      rhs = as.numeric(spec$value),
      name = spec$name %||% "area_min"
    )
  }

  # ---- area_max
  if (!is.null(cons$area_max)) {
    spec <- cons$area_max
    a <- .pa_get_area_vec(
      x,
      area_col = spec$area_col %||% NULL,
      area_unit = spec$unit %||% "m2"
    )

    if (length(a) != n_pu) {
      stop(
        "Area vector length (", length(a), ") != n_pu (", n_pu,
        ") while applying area_max constraint.",
        call. = FALSE
      )
    }

    x <- .pa_add_linear_constraint(
      x,
      var_index_0based = j0,
      coeff = a,
      sense = "<=",
      rhs = as.numeric(spec$value),
      name = spec$name %||% "area_max"
    )
  }

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
    .cls   = list("font-weight" = "bold", color = "#4C78A8"),
    .code  = list(color = "#3A7D44"),
    .muted = list(color = "grey60"),
    .ok    = list(color = "#569746", "font-weight" = "bold"),
    .warn  = list(color = "#C78A00", "font-weight" = "bold"),
    .bad   = list(color = "#C0392B", "font-weight" = "bold")
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


# -------------------------------------------------------------------------
# Internal helpers DATA
# -------------------------------------------------------------------------
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

  stopifnot(inherits(x, "Problem"))

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

  out <- utils::modifyList(defaults, stored)

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
    out$solver_params <- utils::modifyList(out$solver_params %||% list(), solver_params)
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


# -------------------------------------------------------------------------
# Internal helpers SOLUTIONS
# -------------------------------------------------------------------------
.pa_extract_solution_tables <- function(x, sol, threshold = 0.5) {
  stopifnot(inherits(x, "Problem"))
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

  slice_safe <- function(v, start1, end1) {
    if (length(v) == 0) return(numeric(0))
    if (is.na(start1) || is.na(end1) || start1 > end1) return(numeric(0))
    end1 <- min(end1, length(v))
    start1 <- min(max(1L, start1), end1)
    v[start1:end1]
  }

  # --- slices
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
  # 3) Features achieved
  # -------------------------
  # -------------------------
  # 3) Features summary
  # -------------------------
  feats <- x$data$features
  de <- x$data$dist_effects_model %||% x$data$dist_effects

  # total available in the landscape (independent of the solution)
  base_by_feat <- data.frame(
    internal_feature = integer(0),
    total_available = numeric(0)
  )

  if (!is.null(df_df) && inherits(df_df, "data.frame") && nrow(df_df) > 0 &&
      all(c("internal_feature", "amount") %in% names(df_df))) {

    df2 <- df_df
    df2$amount <- as.numeric(df2$amount)

    base_by_feat <- stats::aggregate(
      total_available ~ internal_feature,
      data = data.frame(
        internal_feature = as.integer(df2$internal_feature),
        total_available = as.numeric(df2$amount)
      ),
      FUN = sum
    )
  }

  # map x rows to effects table
  de_with_x <- NULL
  if (!is.null(de) && inherits(de, "data.frame") && nrow(de) > 0 &&
      length(xv) > 0 &&
      all(c("internal_pu", "internal_action", "internal_feature") %in% names(de)) &&
      inherits(da_out, "data.frame") && nrow(da_out) > 0 &&
      all(c("internal_pu", "internal_action") %in% names(da_out))) {

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

    de_with_x <- de
    de_with_x$x_value <- as.numeric(xv[xrow])

    if (!("benefit" %in% names(de_with_x))) de_with_x$benefit <- 0
    if (!("loss" %in% names(de_with_x))) de_with_x$loss <- 0

    de_with_x$benefit <- as.numeric(de_with_x$benefit)
    de_with_x$loss <- as.numeric(de_with_x$loss)

    de_with_x$selected_benefit <- de_with_x$benefit * de_with_x$x_value
    de_with_x$selected_loss <- de_with_x$loss * de_with_x$x_value
    de_with_x$selected_net <- de_with_x$selected_benefit - de_with_x$selected_loss
  }

  benefit_by_feat <- data.frame(
    internal_feature = integer(0),
    benefit = numeric(0)
  )

  loss_by_feat <- data.frame(
    internal_feature = integer(0),
    loss = numeric(0)
  )

  net_by_feat <- data.frame(
    internal_feature = integer(0),
    net = numeric(0)
  )

  if (!is.null(de_with_x) && nrow(de_with_x) > 0) {
    benefit_by_feat <- stats::aggregate(
      benefit ~ internal_feature,
      data = data.frame(
        internal_feature = as.integer(de_with_x$internal_feature),
        benefit = as.numeric(de_with_x$selected_benefit)
      ),
      FUN = sum
    )

    loss_by_feat <- stats::aggregate(
      loss ~ internal_feature,
      data = data.frame(
        internal_feature = as.integer(de_with_x$internal_feature),
        loss = as.numeric(de_with_x$selected_loss)
      ),
      FUN = sum
    )

    net_by_feat <- stats::aggregate(
      net ~ internal_feature,
      data = data.frame(
        internal_feature = as.integer(de_with_x$internal_feature),
        net = as.numeric(de_with_x$selected_net)
      ),
      FUN = sum
    )
  }

  # main feature table
  if (is.null(feats) || !inherits(feats, "data.frame") || nrow(feats) == 0) {
    feat_tbl <- data.frame()
  } else {
    feat_tbl <- data.frame(
      feature = as.integer(feats$internal_id),
      feature_name = as.character(feats$name),
      stringsAsFactors = FALSE
    )

    feat_tbl <- dplyr::left_join(feat_tbl, base_by_feat,    by = c("feature" = "internal_feature"))
    feat_tbl <- dplyr::left_join(feat_tbl, benefit_by_feat, by = c("feature" = "internal_feature"))
    feat_tbl <- dplyr::left_join(feat_tbl, loss_by_feat,    by = c("feature" = "internal_feature"))
    feat_tbl <- dplyr::left_join(feat_tbl, net_by_feat,     by = c("feature" = "internal_feature"))

    feat_tbl$total_available[is.na(feat_tbl$total_available)] <- 0
    feat_tbl$benefit[is.na(feat_tbl$benefit)] <- 0
    feat_tbl$loss[is.na(feat_tbl$loss)] <- 0
    feat_tbl$net[is.na(feat_tbl$net)] <- 0

    feat_tbl$total <- feat_tbl$total_available + feat_tbl$net
  }

  # -------------------------
  # 4) Targets summary
  # -------------------------
  tgt <- x$data$targets
  tgt_out <- NULL

  if (inherits(tgt, "data.frame") && nrow(tgt) > 0 &&
      all(c("feature", "type", "target_value") %in% names(tgt)) &&
      inherits(feat_tbl, "data.frame") && nrow(feat_tbl) > 0) {

    tgt2 <- tgt
    tgt2$feature <- as.integer(tgt2$feature)
    tgt2$type <- as.character(tgt2$type)
    tgt2$target_value <- as.numeric(tgt2$target_value)

    if (!("subset" %in% names(tgt2))) {
      tgt2$subset <- NA_character_
    }
    tgt2$subset <- as.character(tgt2$subset)

    # default achieved = NA
    tgt2$achieved <- NA_real_

    # helper to compute achieved by subset
    .achieved_for_subset <- function(feature_ids, subset_string) {
      if (is.null(de_with_x) || nrow(de_with_x) == 0) {
        return(stats::setNames(rep(0, length(feature_ids)), feature_ids))
      }

      dd <- de_with_x

      if (!is.na(subset_string) && nzchar(subset_string)) {
        matched <- .pa_resolve_action_subset(x, strsplit(subset_string, "\\|")[[1]])
        keep_actions <- as.integer(matched$internal_id)
        dd <- dd[dd$internal_action %in% keep_actions, , drop = FALSE]
      }

      if (nrow(dd) == 0) {
        return(stats::setNames(rep(0, length(feature_ids)), feature_ids))
      }

      tmp <- data.frame(
        feature = as.integer(dd$internal_feature),
        achieved = as.numeric(dd$selected_benefit)
      )
      agg <- stats::aggregate(achieved ~ feature, data = tmp, FUN = sum)

      out <- stats::setNames(rep(0, length(feature_ids)), feature_ids)
      hit <- match(feature_ids, agg$feature)
      ok <- !is.na(hit)
      out[ok] <- agg$achieved[hit[ok]]
      out
    }

    split_key <- ifelse(is.na(tgt2$subset) | !nzchar(tgt2$subset), "__ALL__", tgt2$subset)
    idx_split <- split(seq_len(nrow(tgt2)), split_key)

    for (nm in names(idx_split)) {
      ii <- idx_split[[nm]]
      subset_string <- tgt2$subset[ii][1]
      feats_i <- tgt2$feature[ii]
      achieved_i <- .achieved_for_subset(feats_i, subset_string)
      tgt2$achieved[ii] <- as.numeric(achieved_i[as.character(feats_i)])
    }

    tgt2$gap <- tgt2$achieved - tgt2$target_value
    tgt_out <- tgt2
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
  stopifnot(inherits(x, "Problem"))
  area_unit <- match.arg(area_unit)

  pu <- x$data$pu
  if (is.null(pu) || !inherits(pu, "data.frame")) {
    stop("x$data$pu is missing or is not a data.frame.", call. = FALSE)
  }

  n_pu <- nrow(pu)
  if (n_pu <= 0L) {
    stop("x$data$pu has zero rows; cannot retrieve planning unit areas.", call. = FALSE)
  }

  area <- NULL
  area_source <- NULL
  area_col_used <- NULL

  # ---------------------------------------------------------
  # 1) explicit area_col
  # ---------------------------------------------------------
  if (!is.null(area_col)) {
    area_col <- as.character(area_col)[1]
    if (is.na(area_col) || !nzchar(area_col)) {
      stop("area_col must be a non-empty string or NULL.", call. = FALSE)
    }

    # 1a) from x$data$pu
    if (area_col %in% names(pu)) {
      area <- as.numeric(pu[[area_col]])
      area_source <- "pu"
      area_col_used <- area_col

      # 1b) from x$data$pu_sf
    } else if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf") &&
               area_col %in% names(x$data$pu_sf)) {
      area <- as.numeric(x$data$pu_sf[[area_col]])
      area_source <- "pu_sf"
      area_col_used <- area_col

    } else {
      stop(
        "area_col '", area_col, "' was not found in x$data$pu",
        if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) " or x$data$pu_sf." else ".",
        " Provide a valid area column or spatial information.",
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------
  # 2) automatic lookup if area_col is NULL
  # ---------------------------------------------------------
  if (is.null(area)) {
    cand <- c("area", "Area", "AREA", "surf", "surface", "area_m2", "area_ha", "area_km2")
    hit_pu <- cand[cand %in% names(pu)][1]

    if (!is.na(hit_pu) && length(hit_pu) == 1L) {
      area <- as.numeric(pu[[hit_pu]])
      area_source <- "pu"
      area_col_used <- hit_pu
    }
  }

  if (is.null(area) && !is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) {
    cand <- c("area", "Area", "AREA", "surf", "surface", "area_m2", "area_ha", "area_km2")
    hit_sf <- cand[cand %in% names(x$data$pu_sf)][1]

    if (!is.na(hit_sf) && length(hit_sf) == 1L) {
      area <- as.numeric(x$data$pu_sf[[hit_sf]])
      area_source <- "pu_sf"
      area_col_used <- hit_sf
    }
  }

  # ---------------------------------------------------------
  # 3) derive from geometry if possible
  # ---------------------------------------------------------
  if (is.null(area)) {
    if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) {
      area <- as.numeric(sf::st_area(x$data$pu_sf))
      area_source <- "geometry_sf"
      area_col_used <- NA_character_

      # future raster branch could go here
      # else if (...) {
      #   area <- ...
      #   area_source <- "geometry_raster"
      # }

    } else {
      stop(
        "Could not retrieve planning unit area.\n",
        "No usable area column was found in x$data$pu, and no spatial geometry is available ",
        "to derive area internally.\n",
        "Provide `area_col` explicitly or build the Problem object with spatial information.",
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------
  # 4) validate length and values
  # ---------------------------------------------------------
  if (length(area) != n_pu) {
    stop(
      "Area vector length (", length(area), ") != n_pu (", n_pu, "). ",
      "Area source: ", area_source,
      if (!is.null(area_col_used) && !is.na(area_col_used)) paste0(" [column='", area_col_used, "']") else "",
      ".",
      call. = FALSE
    )
  }

  if (anyNA(area) || any(!is.finite(area))) {
    stop("Area vector contains NA or non-finite values.", call. = FALSE)
  }

  if (any(area < 0)) {
    stop("Area vector contains negative values.", call. = FALSE)
  }

  # ---------------------------------------------------------
  # 5) convert to requested output units
  # assume raw geometry/st_area is m2
  # if column name suggests ha/km2, convert accordingly
  # ---------------------------------------------------------
  area_m2 <- area

  if (!is.null(area_col_used) && !is.na(area_col_used)) {
    nm <- tolower(area_col_used)

    if (grepl("km2", nm, fixed = TRUE)) {
      area_m2 <- area * 1e6
    } else if (grepl("ha", nm, fixed = TRUE)) {
      area_m2 <- area * 1e4
    } else {
      # assume already in m2 for generic names like area/surf/surface
      area_m2 <- area
    }
  }

  out <- switch(
    area_unit,
    m2  = area_m2,
    ha  = area_m2 / 1e4,
    km2 = area_m2 / 1e6
  )

  out
}

# helper: ensure model built and model_list snapshot available
.pa_ensure_model_snapshot <- function(x) {
  stopifnot(inherits(x, "Problem"))

  if (is.null(x$data$model_ptr)) {
    x <- .pa_build_model(x)
  }
  if (is.null(x$data$model_list) || isTRUE(x$data$meta$model_dirty)) {
    x <- .pa_refresh_model_snapshot(x)
    x$data$meta$model_dirty <- FALSE
  }
  x
}


# helper: add 1 linear constraint sum(coeff_j * x_j) (sense) rhs
# NOTE: replace this with YOUR real C++ bridge that appends rows/triplets/rhs/sense.
.pa_add_linear_constraint <- function(
    x,
    var_index_0based,
    coeff,
    sense = c(">=", "<=", "=="),
    rhs,
    name = NULL,
    block_name = "linear_constraint",
    tag = "",
    drop_zeros = TRUE,
    merge_duplicates = TRUE,
    refresh_snapshot = FALSE
) {
  sense <- match.arg(sense)

  stopifnot(inherits(x, "Problem"))
  if (is.null(x$data$model_ptr)) {
    stop("Model pointer missing. Build model first.", call. = FALSE)
  }

  # ---- validate inputs
  if (!is.numeric(var_index_0based) || !is.numeric(coeff)) {
    stop("var_index_0based and coeff must be numeric.", call. = FALSE)
  }
  if (length(var_index_0based) != length(coeff)) {
    stop("Length mismatch: var_index_0based and coeff must have the same length.", call. = FALSE)
  }
  if (!is.numeric(rhs) || length(rhs) != 1L || !is.finite(rhs)) {
    stop("rhs must be a single finite numeric value.", call. = FALSE)
  }

  j0 <- as.integer(var_index_0based)
  a  <- as.numeric(coeff)

  # ---- drop NA / non-finite / zero
  keep <- !(is.na(j0) | is.na(a) | !is.finite(a))
  if (isTRUE(drop_zeros)) keep <- keep & (a != 0)

  j0 <- j0[keep]
  a  <- a[keep]

  if (length(j0) == 0L) {
    stop("Linear constraint has no non-zero coefficients after filtering.", call. = FALSE)
  }

  # ---- optionally merge duplicates (important if you build vectors by rbind/cbind)
  if (isTRUE(merge_duplicates) && length(j0) > 1L) {
    o <- order(j0)
    j0 <- j0[o]; a <- a[o]
    # aggregate by j0
    u <- unique(j0)
    a2 <- numeric(length(u))
    idx <- match(j0, u)
    for (k in seq_along(a)) a2[idx[k]] <- a2[idx[k]] + a[k]
    j0 <- u
    a  <- a2
    if (isTRUE(drop_zeros)) {
      keep2 <- a != 0
      j0 <- j0[keep2]
      a  <- a[keep2]
    }
    if (length(j0) == 0L) {
      stop("Linear constraint became empty after merging duplicates.", call. = FALSE)
    }
  }

  name <- as.character(name %||% "")[1]
  block_name <- as.character(block_name %||% "linear_constraint")[1]
  tag <- as.character(tag %||% "")[1]

  # ---- call C++
  out <- rcpp_add_linear_constraint(
    model_ptr  = x$data$model_ptr,
    j0         = j0,
    x          = a,
    sense      = sense,
    rhs        = as.numeric(rhs),
    name       = name,
    block_name = block_name,
    tag        = tag
  )

  if (isTRUE(refresh_snapshot)) {
    x <- .pa_refresh_model_snapshot(x)
  }

  # store last-added info (optional, but handy for debugging)
  x$data$model_registry <- x$data$model_registry %||% list()
  x$data$model_registry$last_linear_constraint <- out

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

  # undirected canonical ordering
  a <- pmin(rel$internal_pu1, rel$internal_pu2)
  b <- pmax(rel$internal_pu1, rel$internal_pu2)
  rel$internal_pu1 <- a
  rel$internal_pu2 <- b

  # columns to keep
  core <- c("internal_pu1", "internal_pu2", "weight")
  extra_cols <- intersect(names(rel), c("pu1","pu2","distance","source"))
  keep <- c(core, extra_cols)

  # aggregate duplicates
  key <- paste(rel$internal_pu1, rel$internal_pu2)
  if (anyDuplicated(key) != 0) {
    FUN <- switch(
      dup_agg,
      sum  = sum,
      max  = max,
      min  = min,
      mean = mean
    )

    # aggregate ONLY weight (core), then optionally restore extras deterministically
    agg <- stats::aggregate(
      weight ~ internal_pu1 + internal_pu2,
      data = rel[, core, drop = FALSE],
      FUN = FUN
    )

    # if extras exist, attach a deterministic representative per edge (first row)
    if (length(extra_cols) > 0) {
      # representative rows: first occurrence per key
      rep_idx <- match(paste(agg$internal_pu1, agg$internal_pu2),
                       paste(rel$internal_pu1, rel$internal_pu2))
      extras <- rel[rep_idx, extra_cols, drop = FALSE]
      agg <- cbind(agg, extras)
    }

    rel <- agg
  } else {
    rel <- rel[, keep, drop = FALSE]
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
  if (!inherits(x, "Problem")) return(NULL)
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
    } else if (identical(s, "==") || identical(s, "=")) {
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
    gs <- as.character(upd$deactivate_groups)
    for (g in gs) {
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
  # Esto es lo que necesita paretoscape (weighted, etc.)
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
  else if (!is.null(upd$objective_template) && !is.null(reg$obj_templates)) {
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
.pa_input_data_tabular_impl <- function(pu, features, dist_features, boundary = NULL, ...) {

  dots <- list(...)
  `%||%` <- function(a, b) if (!is.null(a)) a else b

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

  # keep raw PU table for downstream helpers such as add_locked_pu()
  pu_raw <- pu

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

  # IMPORTANT:
  # internal contract: pu always carries locked_in / locked_out,
  # defaulting to FALSE when they are not provided.
  if (!("locked_in" %in% names(pu)))  pu$locked_in  <- FALSE
  if (!("locked_out" %in% names(pu))) pu$locked_out <- FALSE

  pu$locked_in  <- as.logical(pu$locked_in)
  pu$locked_out <- as.logical(pu$locked_out)

  pu$locked_in[is.na(pu$locked_in)] <- FALSE
  pu$locked_out[is.na(pu$locked_out)] <- FALSE

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

  # internal PU table keeps the core fields needed by the model
  pu <- pu[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]
  pu <- pu[order(pu$id), , drop = FALSE]

  # raw PU table aligned to normalized id order
  pu_raw$id <- .as_int_id(pu_raw$id, "pu_raw$id")
  pu_raw <- pu_raw[match(pu$id, pu_raw$id), , drop = FALSE]

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
  assertthat::assert_that(is.null(boundary) || inherits(boundary, "data.frame"))
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

    if (!all(boundary$id1 %in% pu$id) || !all(boundary$id2 %in% pu$id)) {
      warning("boundary contains PU ids not present in pu; they will be removed.", call. = FALSE, immediate. = TRUE)
      keep <- boundary$id1 %in% pu$id & boundary$id2 %in% pu$id
      boundary <- boundary[keep, , drop = FALSE]
    }

    if (nrow(boundary) == 0) boundary <- NULL
  }

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
  # build Problem object
  # =========================
  x <- pproto(
    NULL, Problem,
    data = list(
      pu = pu,
      pu_data_raw = pu_raw,
      features = features,
      dist_features = dist_features,

      # ---- spatial / auxiliary storage
      pu_coords = pu_coords,
      spatial_relations = list(),

      index = list(
        pu = pu_index,
        feature = feature_index,
        feature_name_to_id = stats::setNames(features$id, features$name)
      ),

      meta = list(
        dist_features_meaning = "baseline_amount",
        dist_benefit_meaning  = "delta_by_default"
      ),

      # workflow placeholders
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
.pa_register_objective <- function(
    x,
    alias,
    objective_id,
    model_type,
    objective_args = list(),
    sense = c("min", "max")
) {
  stopifnot(inherits(x, "Problem"))

  if (is.null(alias)) return(x)

  alias <- as.character(alias)[1]
  if (is.na(alias) || !nzchar(alias)) {
    stop("alias must be a non-empty string.", call. = FALSE)
  }

  objective_id <- as.character(objective_id)[1]
  if (is.na(objective_id) || !nzchar(objective_id)) {
    stop("objective_id must be a non-empty string.", call. = FALSE)
  }

  model_type <- as.character(model_type)[1]
  if (is.na(model_type) || !nzchar(model_type)) {
    stop("model_type must be a non-empty string.", call. = FALSE)
  }

  sense <- match.arg(sense)

  if (is.null(objective_args)) {
    objective_args <- list()
  }
  if (!is.list(objective_args)) {
    stop("objective_args must be a list.", call. = FALSE)
  }

  if (is.null(x$data$objectives) || !is.list(x$data$objectives)) {
    x$data$objectives <- list()
  }

  # prevent duplicated aliases
  if (!is.null(x$data$objectives[[alias]])) {
    stop(
      "Objective alias '", alias, "' already exists. ",
      "Use a different alias.",
      call. = FALSE
    )
  }

  x$data$objectives[[alias]] <- list(
    alias = alias,
    objective_id = objective_id,
    model_type = model_type,
    objective_args = objective_args,
    sense = sense,
    created_at = as.character(Sys.time())
  )

  # keep deterministic ordering by alias
  x$data$objectives <- x$data$objectives[sort(names(x$data$objectives))]

  # if a model already exists, mark dirty
  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}

# internal registry for MO objectives inside Problem
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
  stopifnot(inherits(x, "Problem"))
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
  stopifnot(inherits(x, "Problem"))
  x$data$mo <- x$data$mo %||% list()
  x$data$mo$needs <- utils::modifyList(x$data$mo$needs %||% list(), needs)
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


# -------------------------------------------------------------------------
# Internal helpers SPATIAL relations
# -------------------------------------------------------------------------
.pa_has_sf <- function() requireNamespace("sf", quietly = TRUE)

.pa_ensure_pu_index <- function(x) {
  stopifnot(inherits(x, "Problem"))
  if (is.null(x$data$pu) || !inherits(x$data$pu, "data.frame")) {
    stop("x$data$pu is missing. Create the problem with input_data().", call. = FALSE)
  }
  if (is.null(x$data$pu$internal_id)) x$data$pu$internal_id <- seq_len(nrow(x$data$pu))
  if (is.null(x$data$pu$id)) {
    stop("x$data$pu must contain column 'id' (planning unit id).", call. = FALSE)
  }
  x$data$pu$id <- as.integer(x$data$pu$id)
  x$data$pu$internal_id <- as.integer(x$data$pu$internal_id)
  if (anyNA(x$data$pu$id) || anyNA(x$data$pu$internal_id)) {
    stop("x$data$pu$id/internal_id contain NA after coercion.", call. = FALSE)
  }
  if (anyDuplicated(x$data$pu$internal_id) != 0) stop("x$data$pu$internal_id must be unique.", call. = FALSE)
  if (anyDuplicated(x$data$pu$id) != 0) stop("x$data$pu$id must be unique.", call. = FALSE)
  if (is.null(x$data$index) || !is.list(x$data$index)) x$data$index <- list()
  x$data$index$pu <- stats::setNames(x$data$pu$internal_id, as.character(x$data$pu$id))
  x
}

.pa_store_relation <- function(x, rel, name) {
  stopifnot(inherits(x, "Problem"))
  if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
    x$data$spatial_relations <- list()
  }
  x$data$spatial_relations[[name]] <- rel
  x
}


.pa_coords_from_input <- function(x, coords = NULL) {
  stopifnot(inherits(x, "Problem"))

  if (!is.null(coords)) {
    if (inherits(coords, "data.frame")) {
      if (!all(c("id", "x", "y") %in% names(coords))) {
        stop("coords data.frame must contain columns id, x, y.", call. = FALSE)
      }
      out <- coords[, c("id", "x", "y")]
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
    if (is.matrix(coords)) {
      if (ncol(coords) < 2) stop("coords matrix must have at least 2 columns (x,y).", call. = FALSE)
      out <- data.frame(id = x$data$pu$id, x = coords[, 1], y = coords[, 2])
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
    stop("Unsupported coords type. Use data.frame(id,x,y) or a matrix with 2 columns.", call. = FALSE)
  }

  # NEW fallback: x$data$pu_coords
  if (!is.null(x$data$pu_coords) && inherits(x$data$pu_coords, "data.frame")) {
    pc <- x$data$pu_coords
    if (all(c("id", "x", "y") %in% names(pc))) {
      out <- pc[, c("id", "x", "y")]
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
  }

  # fallback: try x$data$pu columns
  if (all(c("x", "y") %in% names(x$data$pu))) {
    out <- data.frame(id = x$data$pu$id, x = x$data$pu$x, y = x$data$pu$y)
    out$id <- as.integer(out$id)
    out$x  <- as.numeric(out$x)
    out$y  <- as.numeric(out$y)
    return(out)
  }

  stop(
    "No coordinates available. Provide coords=data.frame(id,x,y) or store x$data$pu_coords (id,x,y), or add x$data$pu$x/y.",
    call. = FALSE
  )
}


.pa_get_pu_sf_aligned <- function(x, pu_sf = NULL, arg_name = "pu_sf") {
  stopifnot(inherits(x, "Problem"))
  x <- .pa_ensure_pu_index(x)

  if (is.null(pu_sf)) pu_sf <- x$data$pu_sf

  if (is.null(pu_sf)) {
    stop(
      arg_name, " is NULL and x$data$pu_sf is missing.\n",
      "Provide ", arg_name, " (sf polygons with an 'id' column) or make sure input_data() stored x$data$pu_sf.",
      call. = FALSE
    )
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("This function requires the 'sf' package.", call. = FALSE)
  }

  if (!inherits(pu_sf, "sf")) stop(arg_name, " must be an sf object.", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop(arg_name, " must contain an 'id' column.", call. = FALSE)

  pu_sf$id <- as.integer(pu_sf$id)

  # align to x$data$pu$id (critical)
  ord <- match(x$data$pu$id, pu_sf$id)
  if (anyNA(ord)) {
    missing_ids <- x$data$pu$id[is.na(ord)]
    stop(
      arg_name, "$id does not match x$data$pu$id (some ids missing). Missing: ",
      paste(utils::head(missing_ids, 20), collapse = ", "),
      if (length(missing_ids) > 20) " ..." else "",
      call. = FALSE
    )
  }

  pu_sf[ord, , drop = FALSE]
}

.pa_rbind_fill <- function(a, b) {
  stopifnot(inherits(a, "data.frame"), inherits(b, "data.frame"))
  cols <- union(names(a), names(b))
  for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
  for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
  a <- a[, cols, drop = FALSE]
  b <- b[, cols, drop = FALSE]
  rbind(a, b)
}

.pa_swap_edges <- function(rel) {
  out <- rel
  out$internal_pu1 <- rel$internal_pu2
  out$internal_pu2 <- rel$internal_pu1
  if ("pu1" %in% names(rel) && "pu2" %in% names(rel)) {
    out$pu1 <- rel$pu2
    out$pu2 <- rel$pu1
  }
  out
}


# -------------------------------------------------------------------------
# Needs flags (feature requirements) for build
# -------------------------------------------------------------------------

.pa_needs_default <- function() {
  list(
    z = FALSE,                # z vars (representation)
    y_pu = FALSE,             # PU fragmentation auxiliaries
    y_action = FALSE,         # action fragmentation auxiliaries
    y_intervention = FALSE,   # intervention fragmentation auxiliaries
    u_intervention = FALSE    # intervention extra vars (if applicable)
  )
}

.pa_build_model_init_needs <- function(x) {
  stopifnot(inherits(x, "Problem"))
  if (is.null(x$data$model_args) || !is.list(x$data$model_args)) x$data$model_args <- list()

  needs <- x$data$model_args$needs
  if (is.null(needs) || !is.list(needs)) needs <- list()

  # merge defaults without overwriting existing values
  def <- .pa_needs_default()
  for (nm in names(def)) {
    if (is.null(needs[[nm]])) needs[[nm]] <- def[[nm]]
  }

  x$data$model_args$needs <- needs
  x
}

.pa_build_model_set_needs_from_objective <- function(x) {
  stopifnot(inherits(x, "Problem"))

  args  <- x$data$model_args %||% list()
  mtype <- args$model_type %||% "minimizeCosts"
  oargs <- args$objective_args %||% list()
  raw_needs <- args$needs %||% list()

  needs <- .pa_needs_default()

  # decisión de diseño actual: z siempre disponible
  needs$z <- TRUE

  # primero respetamos overrides explícitos si vienen dados
  if (!is.null(raw_needs$z))              needs$z <- isTRUE(raw_needs$z)
  if (!is.null(raw_needs$y_pu))           needs$y_pu <- isTRUE(raw_needs$y_pu)
  if (!is.null(raw_needs$y_action))       needs$y_action <- isTRUE(raw_needs$y_action)
  if (!is.null(raw_needs$y_intervention)) needs$y_intervention <- isTRUE(raw_needs$y_intervention)
  if (!is.null(raw_needs$u_intervention)) needs$u_intervention <- isTRUE(raw_needs$u_intervention)

  # luego inferimos solo lo que no vino explícito
  if (is.null(raw_needs$y_pu)) {
    needs$y_pu <- identical(mtype, "minimizeFragmentation")
  }

  if (is.null(raw_needs$y_action)) {
    needs$y_action <- identical(mtype, "minimizeActionFragmentation")
  }

  if (is.null(raw_needs$y_intervention)) {
    needs$y_intervention <- identical(mtype, "minimizeInterventionFragmentation")
  }

  if (is.null(raw_needs$u_intervention)) {
    needs$u_intervention <- identical(mtype, "minimizeInterventionImpact")
  }

  # propagación explícita de metadatos estructurales necesarios
  if (isTRUE(needs$y_pu) || isTRUE(needs$y_action) || isTRUE(needs$y_intervention)) {
    if (is.null(raw_needs$relation_name)) {
      needs$relation_name <- as.character(oargs$relation_name %||% "boundary")[1]
    }
  }

  if (isTRUE(needs$y_action)) {
    if (is.null(raw_needs$actions_to_use)) {
      needs$actions_to_use <- oargs$actions_to_use %||% oargs$actions %||% NULL
    }
  }

  if (isTRUE(needs$u_intervention)) {
    if (is.null(raw_needs$u_intervention_actions)) {
      needs$u_intervention_actions <- oargs$actions %||% NULL
    }
  }

  # preserva metadatos extra útiles para MO
  extra_fields <- setdiff(names(raw_needs), names(needs))
  if (length(extra_fields) > 0L) {
    for (nm in extra_fields) {
      needs[[nm]] <- raw_needs[[nm]]
    }
  }

  needs$z              <- isTRUE(needs$z)
  needs$y_pu           <- isTRUE(needs$y_pu)
  needs$y_action       <- isTRUE(needs$y_action)
  needs$y_intervention <- isTRUE(needs$y_intervention)
  needs$u_intervention <- isTRUE(needs$u_intervention)

  x$data$model_args$needs <- needs
  x
}





.pa_build_model_prepare_needs_cpp <- function(x) {
  stopifnot(inherits(x, "Problem"))
  .pa_abort <- function(...) stop(paste0(...), call. = FALSE)

  op    <- x$data$model_ptr
  args  <- x$data$model_args %||% list()
  needs <- args$needs %||% list()
  oargs <- args$objective_args %||% list()

  mo_mode <- isTRUE(args$mo_mode)

  if (isTRUE(mo_mode) && isTRUE(needs$y_intervention)) {
    .pa_abort(
      "MO phase 1 does not support preparing y_intervention in the shared superset yet."
    )
  }

  if (!isTRUE(needs$y_pu) &&
      !isTRUE(needs$y_action) &&
      !isTRUE(needs$y_intervention) &&
      !isTRUE(needs$u_intervention)) {
    return(x)
  }

  x$data$model_registry <- x$data$model_registry %||% list()
  x$data$model_registry$vars <- x$data$model_registry$vars %||% list()

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

  rel_model <- NULL
  if (isTRUE(needs$y_pu) ||
      isTRUE(needs$y_action) ||
      isTRUE(needs$y_intervention)) {

    rel_name <- as.character(needs$relation_name %||% oargs$relation_name %||% "boundary")[1]

    rels <- x$data$spatial_relations
    if (is.null(rels) || is.null(rels[[rel_name]])) {
      .pa_abort("prepare_needs: missing spatial relation '", rel_name, "'.")
    }

    x$data$spatial_relations_model <- x$data$spatial_relations_model %||% list()
    if (is.null(x$data$spatial_relations_model[[rel_name]])) {
      x$data$spatial_relations_model[[rel_name]] <- .pa_prepare_relation_model(rels[[rel_name]])
    }
    rel_model <- x$data$spatial_relations_model[[rel_name]]
  }

  # ---- y_pu
  if (isTRUE(needs$y_pu)) {
    if (!exists("rcpp_prepare_objective_min_fragmentation", mode = "function")) {
      .pa_abort("Missing rcpp_prepare_objective_min_fragmentation() in the package.")
    }

    res <- rcpp_prepare_objective_min_fragmentation(
      x = op,
      relation_data = rel_model
    )
    x$data$model_registry$vars$y_pu <- res
  }

  # ---- y_action
  if (isTRUE(needs$y_action)) {
    if (!exists("rcpp_prepare_objective_min_fragmentation_actions", mode = "function")) {
      .pa_abort("Missing rcpp_prepare_objective_min_fragmentation_actions() in the package.")
    }

    if (is.null(x$data$dist_actions_model) || !inherits(x$data$dist_actions_model, "data.frame")) {
      .pa_abort("prepare_needs: y_action requires x$data$dist_actions_model (model-ready).")
    }
    if (nrow(x$data$dist_actions_model) == 0) {
      .pa_abort("prepare_needs: y_action requires action variables, but dist_actions_model has 0 rows.")
    }

    actions_to_use_raw <- needs$actions_to_use %||% oargs$actions_to_use %||% oargs$actions %||% NULL

    actions_to_use <- NULL
    if (!is.null(actions_to_use_raw)) {
      act_subset <- .pa_resolve_action_subset(x, subset = actions_to_use_raw)
      actions_to_use <- as.integer(act_subset$internal_id)
    }

    res <- rcpp_prepare_objective_min_fragmentation_actions(
      x = op,
      dist_actions_data = x$data$dist_actions_model,
      relation_data     = rel_model,
      actions_to_use    = actions_to_use,
      block_name        = "fragmentation_actions",
      tag               = ""
    )

    x$data$model_registry$vars$y_action <- res
  }

  # ---- u_intervention for min_intervention_impact
  if (isTRUE(needs$u_intervention)) {
    if (!exists("rcpp_prepare_objective_min_intervention_impact", mode = "function")) {
      .pa_abort("Missing rcpp_prepare_objective_min_intervention_impact() in the package.")
    }

    if (is.null(x$data$dist_actions_model) || !inherits(x$data$dist_actions_model, "data.frame")) {
      .pa_abort("prepare_needs: u_intervention requires x$data$dist_actions_model (model-ready).")
    }
    if (nrow(x$data$dist_actions_model) == 0) {
      .pa_abort("prepare_needs: u_intervention requires action variables, but dist_actions_model has 0 rows.")
    }

    if (is.null(x$data$dist_features) || !inherits(x$data$dist_features, "data.frame")) {
      .pa_abort("prepare_needs: u_intervention requires x$data$dist_features.")
    }
    if (nrow(x$data$dist_features) == 0) {
      .pa_abort("prepare_needs: u_intervention requires non-empty x$data$dist_features.")
    }

    actions_chr <- needs$u_intervention_actions %||% oargs$actions %||% NULL
    if (is.null(actions_chr) || length(actions_chr) == 0) {
      .pa_abort(
        "prepare_needs: u_intervention requires a non-empty shared action subset.\n",
        "For MO phase 1, all intervention_impact objectives must use the same actions."
      )
    }

    act_subset <- .pa_resolve_action_subset(x, subset = actions_chr)
    acts_internal <- as.integer(act_subset$internal_id)

    if (length(acts_internal) == 0) {
      .pa_abort("prepare_needs: u_intervention action subset resolved to zero actions.")
    }

    subset_key <- .pa_subset_to_string(act_subset$id)

    impact_col <- as.character(oargs$impact_col %||% "amount")[1]
    if (!(impact_col %in% names(x$data$dist_features))) {
      impact_col <- "amount"
      if (!("amount" %in% names(x$data$dist_features))) {
        .pa_abort(
          "prepare_needs: could not find a valid impact column in x$data$dist_features."
        )
      }
    }

    res <- rcpp_prepare_objective_min_intervention_impact(
      x = op,
      pu_data = x$data$pu,
      dist_actions_data = x$data$dist_actions_model,
      dist_features_data = x$data$dist_features,
      subset_key = subset_key,
      impact_col = impact_col,
      features_to_use = integer(0),
      actions_to_use = acts_internal,
      internal_feature_col = "internal_feature",
      block_name = "objective_min_intervention_impact",
      tag = if (isTRUE(mo_mode)) "from=prepare_needs_cpp;mode=MO" else ""
    )

    x$data$model_registry$vars$u_intervention_impact <- res
  }

  x
}


.pa_deepcopy_data <- function(d) {
  unserialize(serialize(d, NULL))
}

.pa_clone_data <- function(x, drop_model = TRUE) {
  stopifnot(inherits(x, "Problem"))

  # crear un NUEVO proto heredando de x
  y <- pproto(NULL, x)

  # ahora sí, sustituir data por una copia profunda
  y$data <- .pa_deepcopy_data(x$data)

  if (isTRUE(drop_model)) {
    y$data$model_ptr   <- NULL
    y$data$model_index <- NULL
    y$data$model_list  <- NULL
    y$data$has_model   <- FALSE

    if (is.null(y$data$meta) || !is.list(y$data$meta)) {
      y$data$meta <- list()
    }
    y$data$meta$model_dirty <- TRUE
  }

  y
}



.pa_log_add <- function(x, key, value) {
  stopifnot(inherits(x, "Problem"))

  key <- as.character(key)[1]
  if (is.na(key) || !nzchar(key)) {
    stop("`key` must be a non-empty string.", call. = FALSE)
  }

  value <- as.numeric(value)[1]
  if (!is.finite(value)) {
    stop("`value` must be a finite numeric scalar.", call. = FALSE)
  }

  if (is.null(x$data$build_log) || !is.list(x$data$build_log)) {
    x$data$build_log <- list()
  }

  old <- x$data$build_log[[key]] %||% 0
  x$data$build_log[[key]] <- old + value

  x
}



.pa_resolve_action_subset <- function(x, subset = NULL) {
  stopifnot(inherits(x, "Problem"))

  acts <- x$data$actions
  if (is.null(acts) || !inherits(acts, "data.frame") || nrow(acts) == 0) {
    stop("No actions available in x$data$actions.", call. = FALSE)
  }
  if (!all(c("id", "internal_id") %in% names(acts))) {
    stop("x$data$actions must contain columns 'id' and 'internal_id'.", call. = FALSE)
  }

  # NULL = all actions
  if (is.null(subset)) {
    out <- acts[, c("id", "internal_id"), drop = FALSE]
    if ("action_set" %in% names(acts)) out$action_set <- acts$action_set
    return(out)
  }

  subset <- as.character(subset)
  subset <- unique(subset[!is.na(subset) & nzchar(subset)])
  if (length(subset) == 0) {
    stop("subset must contain at least one non-empty value, or be NULL.", call. = FALSE)
  }

  hit_id <- acts$id %in% subset
  hit_set <- rep(FALSE, nrow(acts))
  if ("action_set" %in% names(acts)) {
    hit_set <- acts$action_set %in% subset
  }

  keep <- hit_id | hit_set

  if (!any(keep)) {
    stop(
      "subset did not match any action ids",
      if ("action_set" %in% names(acts)) " or action_set values" else "",
      ".",
      call. = FALSE
    )
  }

  out <- acts[keep, c("id", "internal_id"), drop = FALSE]
  if ("action_set" %in% names(acts)) out$action_set <- acts$action_set[keep]
  rownames(out) <- NULL
  out
}

#' @include internal.R
NULL

# -------------------------------------------------------------------------
# Internal helpers for normalized atomic objectives
# -------------------------------------------------------------------------

.pa_set_active_and_register_objective <- function(
    x,
    model_type,
    objective_id,
    objective_args = list(),
    sense = c("min", "max"),
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))
  sense <- match.arg(sense)

  x <- .pa_clone_data(x)

  if (is.null(x$data$model_args) || !is.list(x$data$model_args)) {
    x$data$model_args <- list()
  }

  x$data$model_args$model_type <- as.character(model_type)[1]
  x$data$model_args$objective_id <- as.character(objective_id)[1]
  x$data$model_args$objective_args <- objective_args

  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = objective_id,
    model_type = model_type,
    objective_args = objective_args,
    sense = sense
  )

  x
}

.pa_resolve_feature_subset <- function(x, features = NULL) {
  stopifnot(inherits(x, "Problem"))

  feats <- x$data$features
  if (is.null(feats) || !inherits(feats, "data.frame") || nrow(feats) == 0) {
    stop("No features available in x$data$features.", call. = FALSE)
  }
  if (!all(c("id", "internal_id") %in% names(feats))) {
    stop("x$data$features must contain columns 'id' and 'internal_id'.", call. = FALSE)
  }

  if (is.null(features)) {
    out <- feats[, c("id", "internal_id"), drop = FALSE]
    if ("name" %in% names(feats)) out$name <- feats$name
    return(out)
  }

  vals <- unique(features)
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0) {
    stop("features must contain at least one non-missing value, or be NULL.", call. = FALSE)
  }

  hit_id <- feats$id %in% vals
  hit_name <- rep(FALSE, nrow(feats))
  if ("name" %in% names(feats)) {
    hit_name <- as.character(feats$name) %in% as.character(vals)
  }

  keep <- hit_id | hit_name

  if (!any(keep)) {
    stop(
      "features did not match any feature ids",
      if ("name" %in% names(feats)) " or feature names" else "",
      ".",
      call. = FALSE
    )
  }

  out <- feats[keep, c("id", "internal_id"), drop = FALSE]
  if ("name" %in% names(feats)) out$name <- feats$name[keep]
  rownames(out) <- NULL
  out
}

.pa_subset_to_string <- function(subset) {
  if (is.null(subset)) return(NA_character_)

  subset <- as.character(subset)
  subset <- unique(subset[!is.na(subset) & nzchar(subset)])

  if (length(subset) == 0) return(NA_character_)

  paste(sort(subset), collapse = "|")
}

.pa_add_feature_labels <- function(df,
                                   features_df,
                                   feature_col = "feature",
                                   internal_feature_col = "internal_feature",
                                   out_col = "feature_name") {
  stopifnot(is.data.frame(df), is.data.frame(features_df))

  if (nrow(df) == 0) {
    df[[out_col]] <- character(0)
    return(df)
  }

  if (!("name" %in% names(features_df))) {
    df[[out_col]] <- rep(NA_character_, nrow(df))
    return(df)
  }

  out <- rep(NA_character_, nrow(df))

  # Prefer external feature id
  if (feature_col %in% names(df) && "id" %in% names(features_df)) {
    m <- match(df[[feature_col]], features_df$id)
    ok <- !is.na(m)
    out[ok] <- as.character(features_df$name[m[ok]])
  }

  # Fill remaining using internal id
  if (anyNA(out) &&
      internal_feature_col %in% names(df) &&
      "internal_id" %in% names(features_df)) {
    miss <- which(is.na(out))
    m2 <- match(df[[internal_feature_col]][miss], features_df$internal_id)
    ok2 <- !is.na(m2)
    out[miss[ok2]] <- as.character(features_df$name[m2[ok2]])
  }

  df[[out_col]] <- out
  df
}

.pa_add_action_labels <- function(df,
                                  actions_df,
                                  action_col = "action",
                                  internal_action_col = "internal_action",
                                  out_col = "action_name") {
  stopifnot(is.data.frame(df), is.data.frame(actions_df))

  if (nrow(df) == 0) {
    df[[out_col]] <- character(0)
    return(df)
  }

  if (!("name" %in% names(actions_df))) {
    df[[out_col]] <- rep(NA_character_, nrow(df))
    return(df)
  }

  out <- rep(NA_character_, nrow(df))

  # Prefer external action id
  if (action_col %in% names(df) && "id" %in% names(actions_df)) {
    m <- match(df[[action_col]], actions_df$id)
    ok <- !is.na(m)
    out[ok] <- as.character(actions_df$name[m[ok]])
  }

  # Fill remaining using internal id
  if (anyNA(out) &&
      internal_action_col %in% names(df) &&
      "internal_id" %in% names(actions_df)) {
    miss <- which(is.na(out))
    m2 <- match(df[[internal_action_col]][miss], actions_df$internal_id)
    ok2 <- !is.na(m2)
    out[miss[ok2]] <- as.character(actions_df$name[m2[ok2]])
  }

  df[[out_col]] <- out
  df
}



.pa_solve_single_problem <- function(x, ...) {
  assertthat::assert_that(inherits(x, "Problem"))

  # ---- gather stored solve args (defaults + stored)
  dots <- list(...)

  # consideramos “ya configurado” si solve_args existe y tiene al menos un campo no nulo
  has_stored_args <- !is.null(x$data$solve_args) &&
    length(x$data$solve_args) > 0 &&
    any(!vapply(x$data$solve_args, is.null, logical(1)))

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


  # ---- ensure model is built
  if (is.null(x$data$model_ptr) || isTRUE(x$data$meta$model_dirty)) {
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

    params <- utils::modifyList(params, solver_params_user)

    sol <- gurobi::gurobi(model, params)

    # status_code <- dplyr::case_when(
    #   sol$status == "OPTIMAL" ~ 0L,
    #   sol$status %in% c("INF_OR_UNBD", "INFEASIBLE", "UNBOUNDED") ~ 1L,
    #   (sol$status == "TIME_LIMIT" && !is.null(sol$objval)) ~ 2L,
    #   (sol$status == "TIME_LIMIT" && is.null(sol$objval)) ~ 3L,
    #   sol$status == "SOLUTION_LIMIT" ~ 4L,
    #   TRUE ~ 999L
    # )

    status_chr <- as.character(sol$status %||% NA_character_)[1]

    status_code <- if (identical(status_chr, "OPTIMAL")) {
      0L
    } else if (status_chr %in% c("INF_OR_UNBD", "INFEASIBLE", "UNBOUNDED")) {
      1L
    } else if (identical(status_chr, "TIME_LIMIT") && !is.null(sol$objval)) {
      2L
    } else if (identical(status_chr, "TIME_LIMIT") && is.null(sol$objval)) {
      3L
    } else if (identical(status_chr, "SOLUTION_LIMIT")) {
      4L
    } else {
      999L
    }

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
      threads = as.character(cores),
      log = as.integer(verbose),
      verbose = 15,
      ratio = as.character(gap_limit),
      sec = as.character(time_limit),
      timem = "elapsed",
      heuristicsOnOff = "on"
    )
    cbc_args <- utils::modifyList(cbc_args, solver_params_user)

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
    params <- utils::modifyList(params, solver_params_user)

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
  # n_pu <- as.integer(model$n_pu %||% 0L)
  # n_x  <- as.integer(model$n_x  %||% 0L)
  # n_z  <- as.integer(model$n_z  %||% 0L)
  #
  # w0 <- as.integer(model$w_offset %||% 0L)
  # x0 <- as.integer(model$x_offset %||% 0L)
  # z0 <- as.integer(model$z_offset %||% 0L)
  #
  # sol_monitoring <- if (n_pu > 0L && length(solvec) >= (w0 + n_pu)) {
  #   base::round(solvec[(w0 + 1L):(w0 + n_pu)])
  # } else numeric(0)
  #
  # sol_actions <- if (n_x > 0L && length(solvec) >= (x0 + n_x)) {
  #   base::round(solvec[(x0 + 1L):(x0 + n_x)])
  # } else numeric(0)
  #
  # sol_conservation <- if (n_z > 0L && length(solvec) >= (z0 + n_z)) {
  #   solvec[(z0 + 1L):(z0 + n_z)]
  # } else numeric(0)

  # ------------------------------------------------------------
  # 3) Build human-readable tables (you implement this helper)
  # ------------------------------------------------------------
  summary <- .pa_extract_solution_tables(x, solvec)

  x$data$runtime_updates <- NULL

  objs <- x$data$objectives %||% list()

  alias_values <- NULL
  if (is.list(objs) && length(objs) == 1L) {
    alias_single <- names(objs)[1]
    alias_values <- stats::setNames(objval, alias_single)
  }


  s <- pproto(
    NULL, Solution,
    problem = x,
    solution = list(
      objective = objval,
      vector = solvec,
      alias_values = alias_values
    ),
    summary = summary,
    diagnostics = list(
      status_code = as.integer(status_code),
      gap = gap,
      runtime = runtime,
      solver = solve_args$solver,
      cores = solve_args$cores,
      timelimit = solve_args$timelimit,
      verbose = solve_args$verbose,
      solution_limit = solve_args$solution_limit,
      name_output_file = solve_args$name_output_file,
      output_file = solve_args$output_file,
      solver_args = solve_args
    ),
    method = list(
      type = "single"
    ),
    meta = list()
  )

  s
}


.validate_split_effects <- function(tbl, context = "effects") {
  if (!("benefit" %in% names(tbl)) || !("loss" %in% names(tbl))) {
    stop("Internal error: .validate_split_effects() requires 'benefit' and 'loss' columns.", call. = FALSE)
  }

  tbl$benefit <- as.numeric(tbl$benefit)
  tbl$loss    <- as.numeric(tbl$loss)

  #if (na_to_zero) {
  tbl$benefit[is.na(tbl$benefit)] <- 0
  tbl$loss[is.na(tbl$loss)] <- 0
  #}

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
      ": a single (pu, action, feature) effect cannot have both positive 'benefit' and positive 'loss'. ",
      "This is checked after aggregation by (pu, action, feature), so the same triple cannot contain both gains and damages."
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



.pa_model_is_current <- function(x) {
  stopifnot(inherits(x, "Problem"))

  has_ptr <- !is.null(x$data$model_ptr)
  has_list <- !is.null(x$data$model_list) && is.list(x$data$model_list)
  dirty <- isTRUE(x$data$meta$model_dirty)

  isTRUE(has_ptr && has_list && !dirty)
}

.pa_compile_problem <- function(x, ...) {
  .pa_build_model(x)
}
