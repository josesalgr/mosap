# internal.R (prioriactionsMO)
# -------------------------------------------------------------------------
# Utilities
# -------------------------------------------------------------------------

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


`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------------------------------------------------------------------------
# Promote Data -> MOProblem (internal; never exposed to user)
# -------------------------------------------------------------------------

.pamo_as_mo <- function(x) {
  if (inherits(x, "MOProblem")) return(x)

  if (inherits(x, "Data")) {
    obj <- pproto(NULL, MOProblem, base = x)

    # fuerza clase S3 para que UseMethod("solve") encuentre solve.MOProblem
    cls <- class(obj)
    if (is.null(cls)) cls <- character()
    class(obj) <- unique(c("MOProblem", cls))

    return(obj)
  }

  stop("Expected a Data or a MOProblem.", call. = FALSE)
}


# -------------------------------------------------------------------------
# Atomic objective registry accessors (stored in prioriactions::Data)
# Registry: x$base$data$objectives[[alias]] = list(...)
# -------------------------------------------------------------------------

.pamo_get_specs <- function(x) {
  stopifnot(inherits(x, "MOProblem"))
  specs <- x$base$data$objectives %||% list()
  if (!is.list(specs)) specs <- list()
  specs
}

# internal: get ONE atomic objective spec by alias
.pamo_get_objective_spec <- function(x, alias) {
  stopifnot(inherits(x, "MOProblem"))
  alias <- as.character(alias)[1]

  if (is.na(alias) || !nzchar(alias)) {
    stop("alias must be a non-empty string.", call. = FALSE)
  }

  specs <- .pamo_get_specs(x)
  sp <- specs[[alias]]

  if (is.null(sp)) {
    stop(
      "Objective alias not found: '", alias, "'.\n",
      "Tip: register objectives via add_objective_* (alias='...') before calling set_method_*().",
      call. = FALSE
    )
  }

  # defensive minimal validation (clearer errors later)
  if (is.null(sp$objective_id)) stop("Objective '", alias, "' has no objective_id.", call. = FALSE)
  if (is.null(sp$sense)) stop("Objective '", alias, "' has no sense.", call. = FALSE)

  sp
}

# internal: get MANY objective specs in order (and validate duplicates)
.pamo_get_objective_specs <- function(x, aliases) {
  stopifnot(inherits(x, "MOProblem"))

  aliases <- as.character(aliases)
  if (length(aliases) == 0L) stop("aliases must have length > 0.", call. = FALSE)

  if (anyNA(aliases) || any(!nzchar(aliases))) {
    stop("aliases must be non-empty strings.", call. = FALSE)
  }

  if (anyDuplicated(aliases) != 0L) {
    dups <- unique(aliases[duplicated(aliases)])
    stop("Duplicated objective aliases: ", paste(dups, collapse = ", "), call. = FALSE)
  }

  specs <- lapply(aliases, function(a) .pamo_get_objective_spec(x, a))
  names(specs) <- aliases
  specs
}

# internal: validate registry sanity
.pamo_validate_objectives <- function(x) {
  stopifnot(inherits(x, "MOProblem"))

  specs <- .pamo_get_specs(x)
  if (length(specs) == 0) {
    stop("No objectives registered. Use add_objective_* (with alias=...) first.", call. = FALSE)
  }

  al <- names(specs)
  if (anyNA(al) || any(!nzchar(al))) stop("Objective registry has invalid aliases.", call. = FALSE)
  if (anyDuplicated(al)) stop("Duplicated objective aliases are not allowed.", call. = FALSE)

  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Objective IR (intermediate representation)
# Each IR carries:
# - sense: "min"|"max"
# - terms: list of atomic terms (still indivisible objectives)
# - objective_id, objective_args: to be able to re-activate the objective in a single-objective model
# -------------------------------------------------------------------------

.pamo_objective_to_ir <- function(x, spec) {
  stopifnot(inherits(x, "Data"))
  stopifnot(is.list(spec), !is.null(spec$objective_id))

  id <- as.character(spec$objective_id)[1]
  a  <- spec$objective_args %||% list()

  # sense defensivo (ojo: %||% no trata NA, solo NULL)
  sense <- as.character(spec$sense %||% NA_character_)[1]
  if (is.na(sense) || !nzchar(sense)) sense <- NA_character_

  # helpers para defaults estables
  .c1 <- function(z, default = NULL) {
    if (is.null(z)) return(default)
    as.character(z)[1]
  }
  .n1 <- function(z, default = NULL) {
    if (is.null(z)) return(default)
    as.numeric(z)[1]
  }
  .l1 <- function(z, default = FALSE) {
    if (is.null(z)) return(default)
    isTRUE(z)
  }

  # NOTE: devolvemos objective_id/objective_args para soportar "rebuild + pad".

  # --- min cost
  if (identical(id, "min_cost")) {

    # defaults coherentes con tu API: si no viene, asumimos TRUE
    inc_pu  <- .l1(a$include_pu_cost, TRUE)
    inc_act <- .l1(a$include_action_cost, TRUE)

    terms <- list()
    if (inc_pu)  terms <- c(terms, list(list(type = "pu_cost")))
    if (inc_act) terms <- c(terms, list(list(type = "action_cost")))

    a$include_pu_cost <- inc_pu
    a$include_action_cost <- inc_act

    return(list(
      sense = "min",
      terms = terms,
      objective_id = id,
      objective_args = a
    ))
  }

  # --- max benefit
  if (identical(id, "max_benefit")) {
    bcol <- .c1(a$benefit_col, "benefit")
    a$benefit_col <- bcol

    return(list(
      sense = "max",
      terms = list(list(type = "benefit", benefit_col = bcol)),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- max profit
  if (identical(id, "max_profit")) {
    pcol <- .c1(a$profit_col, "profit")
    a$profit_col <- pcol

    return(list(
      sense = "max",
      terms = list(list(type = "profit", profit_col = pcol)),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- max net profit
  if (identical(id, "max_net_profit")) {
    pcol <- .c1(a$profit_col, "profit")
    inc_pu  <- .l1(a$include_pu_cost, TRUE)
    inc_act <- .l1(a$include_action_cost, TRUE)

    a$profit_col <- pcol
    a$include_pu_cost <- inc_pu
    a$include_action_cost <- inc_act

    return(list(
      sense = "max",
      terms = list(list(
        type = "net_profit",
        profit_col = pcol,
        include_pu_cost = inc_pu,
        include_action_cost = inc_act
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- PU fragmentation
  if (identical(id, "min_fragmentation")) {
    rel <- .c1(a$relation_name, "boundary")
    mul <- .n1(a$weight_multiplier, 1)

    a$relation_name <- rel
    a$weight_multiplier <- mul

    return(list(
      sense = "min",
      terms = list(list(
        type = "boundary_cut",
        relation_name = rel,
        weight_multiplier = mul
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- action fragmentation
  if (identical(id, "min_action_fragmentation")) {
    rel <- .c1(a$relation_name, "boundary")
    mul <- .n1(a$weight_multiplier, 1)

    a$relation_name <- rel
    a$weight_multiplier <- mul
    # a$action_weights y a$actions los dejas tal cual (pueden ser NULL, DF, named numeric, etc.)

    return(list(
      sense = "min",
      terms = list(list(
        type = "action_boundary_cut",
        relation_name = rel,
        weight_multiplier = mul,
        action_weights = a$action_weights %||% NULL,
        actions = a$actions %||% NULL
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- intervention fragmentation
  if (identical(id, "min_intervention_fragmentation")) {
    rel <- .c1(a$relation_name, "boundary")
    mul <- .n1(a$weight_multiplier, 1)

    a$relation_name <- rel
    a$weight_multiplier <- mul

    return(list(
      sense = "min",
      terms = list(list(
        type = "intervention_boundary_cut",
        relation_name = rel,
        weight_multiplier = mul
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- max representation
  if (identical(id, "max_representation")) {
    acol <- .c1(a$amount_col, "amount")
    a$amount_col <- acol

    return(list(
      sense = "max",
      terms = list(list(type = "representation", amount_col = acol)),
      objective_id = id,
      objective_args = a
    ))
  }

  # --- custom objective (advanced path)
  if (identical(id, "custom")) {
    # aquí el sentido REAL lo controlas por spec$sense; si viene NA, fija "min"
    if (is.na(sense)) sense <- "min"

    return(list(
      sense = sense,
      terms = list(list(type = "custom")),
      objective_id = id,
      objective_args = a
    ))
  }

  stop("Unknown objective_id in .pamo_objective_to_ir(): ", id, call. = FALSE)
}


# -------------------------------------------------------------------------
# Cloning base Data safely for MO runs
# (Avoid copying externalptr / built model pointer)
# -------------------------------------------------------------------------

.pamo_deepcopy_data <- function(d) {
  # Deep copy list/data.frames. WARNING: do NOT deep-copy externalptr; we drop model_ptr later.
  unserialize(serialize(d, NULL))
}

.pamo_clone_base <- function(base) {
  stopifnot(inherits(base, "Data"))
  b <- base
  b$data <- .pamo_deepcopy_data(base$data)

  # drop any previously built model pointer/state
  b$data$model_ptr <- NULL
  b$data$has_model <- FALSE

  if (is.null(b$data$meta) || !is.list(b$data$meta)) b$data$meta <- list()
  b$data$meta$model_dirty <- TRUE

  b
}

# -------------------------------------------------------------------------
# Activate an IR as a single-objective config in prioriactions::Data
# (Used by the "rebuild + pad" objective vector strategy)
# -------------------------------------------------------------------------

.pamo_activate_ir_as_single_objective <- function(x, ir) {
  stopifnot(inherits(x, "Data"))
  stopifnot(is.list(ir), !is.null(ir$objective_id))

  id <- as.character(ir$objective_id)[1]
  a  <- ir$objective_args %||% list()

  map <- list(
    min_cost = "minimizeCosts",
    max_benefit = "maximizeBenefits",
    max_profit = "maximizeProfit",
    max_net_profit = "maximizeNetProfit",
    max_representation = "maximizeRepresentation",
    min_fragmentation = "minimizeFragmentation",
    min_action_fragmentation = "minimizeActionFragmentation",
    min_intervention_fragmentation = "minimizeInterventionFragmentation",
    custom = "custom"
  )

  mt <- map[[id]]
  if (is.null(mt)) stop("No model_type mapping for objective_id: ", id, call. = FALSE)

  if (is.null(x$data$model_args) || !is.list(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- mt
  x$data$model_args$objective_id <- id
  x$data$model_args$objective_args <- a

  if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
  x$data$meta$model_dirty <- TRUE

  x
}

# -------------------------------------------------------------------------
# Superset model selection + objective-vector extraction
# Strategy:
# - Build ONE "superset" model using the objective that likely introduces most aux vars
# - For each objective, build its own model and extract model$obj
# - Pad with zeros to match superset length
# This avoids dealing with offsets/variable mapping for now.
# -------------------------------------------------------------------------
.pamo_prepare_relation_model <- function(rel) {
  stopifnot(is.data.frame(rel), nrow(rel) > 0)

  need <- c("internal_pu1", "internal_pu2", "weight")
  if (!all(need %in% names(rel))) {
    stop(
      "Spatial relation must contain columns: ",
      paste(need, collapse = ", "),
      call. = FALSE
    )
  }

  rel <- rel[, c("internal_pu1","internal_pu2","weight",
                 intersect(names(rel), c("distance","source","relation_name"))),
             drop = FALSE]

  rel$internal_pu1 <- as.integer(rel$internal_pu1)
  rel$internal_pu2 <- as.integer(rel$internal_pu2)
  rel$weight <- as.numeric(rel$weight)

  if (anyNA(rel$internal_pu1) || anyNA(rel$internal_pu2)) {
    stop("Relation has NA in internal_pu1/internal_pu2.", call. = FALSE)
  }
  if (any(!is.finite(rel$weight)) || any(rel$weight < 0)) {
    stop("Relation has non-finite or negative weights.", call. = FALSE)
  }

  rel <- rel[order(rel$internal_pu1, rel$internal_pu2), , drop = FALSE]
  rel$internal_edge <- seq_len(nrow(rel))
  rel
}

.pamo_prepare_superset_model <- function(base, ir_list) {
  stopifnot(inherits(base, "Data"))
  stopifnot(is.list(ir_list), length(ir_list) > 0)

  # ---- collect needs across ALL objectives
  all_terms <- unlist(lapply(ir_list, function(ir) ir$terms %||% list()), recursive = FALSE)
  all_types <- unique(vapply(all_terms, `[[`, "", "type"))

  need_z      <- "representation" %in% all_types
  need_y_pu   <- "boundary_cut" %in% all_types
  need_y_act  <- "action_boundary_cut" %in% all_types
  need_y_int  <- "intervention_boundary_cut" %in% all_types

  # ---- choose IR used to build the base model
  # Priority 1: if we need z, try to build using a representation objective
  ir_best <- NULL
  if (isTRUE(need_z)) {
    idx_rep <- which(vapply(ir_list, function(ir) {
      any(vapply(ir$terms %||% list(), function(t) identical(t$type, "representation"), logical(1)))
    }, logical(1)))
    if (length(idx_rep) > 0) ir_best <- ir_list[[idx_rep[1]]]
  }

  # Priority 2: otherwise use the one most likely to require prepares (fragmentation)
  if (is.null(ir_best)) {
    score_one <- function(ir) {
      types <- vapply(ir$terms %||% list(), `[[`, "", "type")
      s <- 0L
      if ("intervention_boundary_cut" %in% types) s <- s + 30L
      if ("action_boundary_cut"       %in% types) s <- s + 20L
      if ("boundary_cut"              %in% types) s <- s + 10L
      # representation doesn't add aux vars, but keep as a small preference
      if ("representation"            %in% types) s <- s + 1L
      s
    }
    scores  <- vapply(ir_list, score_one, integer(1))
    ir_best <- ir_list[[which.max(scores)]]
  }

  # ---- build once (engine expects exactly one objective active)
  b <- .pamo_clone_base(base)
  b <- .pamo_activate_ir_as_single_objective(b, ir_best)

  b$data$model_args$needs <- list(
    z = need_z,
    y_pu = need_y_pu,
    y_action = need_y_act,
    y_intervention = need_y_int
  )

  b$data$model_args$mo_mode <- TRUE
  b <- .pa_build_model(b)

  op <- b$data$model_ptr
  if (is.null(op)) stop("Superset build failed: model_ptr is NULL.", call. = FALSE)

  # ---- sanity check: if we need z, ensure the built model has z
  if (isTRUE(need_z)) {
    op_list <- .pa_model_from_ptr(op, args = b$data$model_args %||% list(), drop_triplets = TRUE)
    n_z <- as.integer(op_list$n_z %||% 0L)
    if (n_z <= 0L) {
      stop(
        "MO superset requires z variables (representation objectives present), ",
        "but the built model has n_z = 0.\n",
        "Fix: make .pa_build_model() create z when model_args$mo_mode=TRUE and any IR needs representation.",
        call. = FALSE
      )
    }
  }

  did_prepare <- FALSE

  # ---- prepare PU fragmentation auxiliaries if needed
  if (isTRUE(need_y_pu)) {

    op_list <- .pa_model_from_ptr(op, args = b$data$model_args %||% list(), drop_triplets = TRUE)
    n_y_pu <- as.integer(op_list$n_y_pu %||% 0L)

    if (n_y_pu <= 0L) {

      rel_names <- unique(vapply(
        Filter(function(t) identical(t$type, "boundary_cut"), all_terms),
        function(t) as.character(t$relation_name %||% "boundary")[1],
        character(1)
      ))

      for (rel_name in rel_names) {

        rel_raw <- b$data$spatial_relations[[rel_name]]
        if (is.null(rel_raw)) {
          stop("Missing spatial relation in Data: '", rel_name, "'.", call. = FALSE)
        }

        rel_model <- b$data$spatial_relations_model[[rel_name]]
        if (is.null(rel_model)) {
          rel_model <- .pamo_prepare_relation_model(rel_raw)
          b$data$spatial_relations_model <- b$data$spatial_relations_model %||% list()
          b$data$spatial_relations_model[[rel_name]] <- rel_model
        }

        rcpp_prepare_fragmentation_pu(op, rel_model)
        did_prepare <- TRUE
      }
    }
  }

  # ---- placeholders for future: action/intervention fragmentation prepares
  # (Leave hooks now so adding epsilon/augmecon later doesn't require refactors)
  if (isTRUE(need_y_act)) {
    # TODO: when you implement it:
    # - check op_list$n_y_actions (or your chosen indicator)
    # - prepare using relation_names present in action_boundary_cut terms
    # did_prepare <- TRUE
  }

  if (isTRUE(need_y_int)) {
    # TODO: when you implement it:
    # - check op_list$n_y_interventions (or your chosen indicator)
    # - prepare using relation_names present in intervention_boundary_cut terms
    # did_prepare <- TRUE
  }

  # ---- refresh snapshot once if needed
  if (isTRUE(did_prepare)) {
    b$data$meta$model_dirty <- TRUE
    b <- .pa_refresh_model_snapshot(b)
  }

  b
}



.pamo_model_obj_length <- function(x) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_ptr)) stop("Data has no model_ptr; build model first.", call. = FALSE)

  m <- .pa_model_from_ptr(
    x$data$model_ptr,
    args = x$data$model_args %||% list(),
    drop_triplets = TRUE
  )
  length(m$obj)
}


# -------------------------------------------------------------------------
# Optional normalization helpers (for obj vectors)
# NOTE: this is normalization of objective *vectors* (not weights).
# If you don't want it now, keep but don't use.
# -------------------------------------------------------------------------

.pamo_normalize_vec <- function(v, method = c("max", "l1", "l2")) {
  method <- match.arg(method)
  v <- as.numeric(v)
  if (!any(is.finite(v))) return(v)

  if (method == "max") {
    s <- max(abs(v), na.rm = TRUE)
    if (is.finite(s) && s > 0) return(v / s)
    return(v)
  }

  if (method == "l1") {
    s <- sum(abs(v), na.rm = TRUE)
    if (is.finite(s) && s > 0) return(v / s)
    return(v)
  }

  if (method == "l2") {
    s <- sqrt(sum(v^2, na.rm = TRUE))
    if (is.finite(s) && s > 0) return(v / s)
    return(v)
  }

  v
}

# ---------------------------------------------------------
# Internal: solve weighted
# ---------------------------------------------------------
.pamo_solve_weighted <- function(x, ...) {
  aliases <- x$method$params$aliases
  w <- x$method$params$weights
  normalize <- isTRUE(x$method$params$normalize)

  if (is.null(w)) {
    stop("Weighted method: missing weights. Provide them in set_method_weighted().", call. = FALSE)
  }

  # build a single run specification (later: grid of weights)
  run_df <- data.frame(run_id = 1L, stringsAsFactors = FALSE)
  for (i in seq_along(aliases)) {
    run_df[[paste0("w_", aliases[i])]] <- w[i]
  }

  solutions <- vector("list", nrow(run_df))
  obj_vals  <- matrix(NA_real_, nrow(run_df), length(aliases))
  colnames(obj_vals) <- paste0("obj_", aliases)

  status <- character(nrow(run_df))
  runtime <- numeric(nrow(run_df))
  gap <- numeric(nrow(run_df))

  for (r in seq_len(nrow(run_df))) {
    w_r <- as.numeric(run_df[r, paste0("w_", aliases), drop = TRUE])

    one <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "weighted",
        aliases = aliases,
        weights = w_r,
        normalize = normalize
      )
    )

    solutions[[r]] <- one$solution
    status[r] <- one$status
    runtime[r] <- one$runtime
    gap[r] <- one$gap

    # TODO: objective evaluation (optional, later)
    # For now, keep NA_real_ (doesn't break, keeps schema stable)
    obj_vals[r, ] <- rep(NA_real_, length(aliases))
  }

  runs <- cbind(
    run_df,
    data.frame(status = status, runtime = runtime, gap = gap, stringsAsFactors = FALSE),
    as.data.frame(obj_vals, stringsAsFactors = FALSE)
  )

  pproto(
    NULL, MOProblem,
    runs = runs,
    solutions = solutions,
    meta = list(
      method = x$method,
      call = match.call()
    )
  )
}

.pamo_solve_epsilon_constraint <- function(x, ...) {

  primary     <- x$method$primary
  aliases     <- x$method$aliases
  constrained <- x$method$constrained
  run_df      <- x$method$runs

  if (is.null(run_df) || !inherits(run_df, "data.frame") || nrow(run_df) == 0) {
    stop("epsilon_constraint: x$method$runs is missing/empty.", call. = FALSE)
  }
  if (!("run_id" %in% names(run_df))) stop("epsilon_constraint: runs must include run_id.", call. = FALSE)

  solutions <- vector("list", nrow(run_df))
  obj_vals  <- matrix(NA_real_, nrow(run_df), length(aliases))
  colnames(obj_vals) <- paste0("obj_", aliases)

  status  <- character(nrow(run_df))
  runtime <- numeric(nrow(run_df))
  gap     <- numeric(nrow(run_df))

  for (r in seq_len(nrow(run_df))) {

    eps_r <- as.list(run_df[r, constrained, drop = FALSE])  # alias -> scalar

    one <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "epsilon_constraint",
        primary = primary,
        eps = eps_r
      )
    )

    solutions[[r]] <- one$solution
    status[r]  <- one$status
    runtime[r] <- one$runtime
    gap[r]     <- one$gap

    obj_vals[r, ] <- rep(NA_real_, length(aliases))
  }

  runs <- cbind(
    run_df,
    data.frame(status = status, runtime = runtime, gap = gap, stringsAsFactors = FALSE),
    as.data.frame(obj_vals, stringsAsFactors = FALSE)
  )

  pproto(
    NULL, MOProblem,
    runs = runs,
    solutions = solutions,
    meta = list(method = x$method, call = match.call())
  )
}

.pamo_apply_single_objective <- function(base, ir, sense = c("min","max")) {
  stopifnot(inherits(base, "Data"))
  sense <- match.arg(sense)

  if (is.null(base$data$model_ptr)) stop("Model not built (model_ptr is NULL).", call. = FALSE)

  v <- .pamo_objvec_from_ir(base, ir)

  # motor siempre MIN
  if (identical(sense, "max")) v <- -v

  base$data$runtime_updates <- list(
    obj = as.numeric(v),
    modelsense = "min"
  )

  base$data$meta$model_dirty <- FALSE
  base$data$has_model <- TRUE

  base
}



# ---------------------------------------------------------
# Internal: extract (minimal) results from prioriactions Solution
# ---------------------------------------------------------
.pamo_extract_solution <- function(out) {
  # out is expected to be a prioriactions::Solution (pproto)
  # We'll be defensive: if slots are missing, keep NA.
  objval <- tryCatch(out$data$objval, error = function(e) NA_real_)
  gap    <- tryCatch(out$data$gap,    error = function(e) NA_real_)
  runtime <- tryCatch(out$data$runtime, error = function(e) NA_real_)
  status <- tryCatch(out$data$status, error = function(e) "ok")

  list(
    solution = out,
    status = status,
    runtime = runtime,
    gap = gap,
    objval = objval
  )
}


# -------------------------------------------------------------------------
# "Custom objectives" (DEV/advanced) constructor + registry add
# (This keeps the path you started, but stores in base$data$objectives)
# -------------------------------------------------------------------------

.pamo_objective <- function(alias, sense = c("min", "max"), build, eval, meta = list()) {
  sense <- match.arg(sense)
  if (!is.character(alias) || length(alias) != 1 || !nzchar(alias)) {
    stop("objective `alias` must be a non-empty string.", call. = FALSE)
  }
  if (!is.function(build)) stop("objective `build` must be a function.", call. = FALSE)
  if (!is.function(eval))  stop("objective `eval` must be a function.", call. = FALSE)

  structure(
    list(alias = alias, sense = sense, build = build, eval = eval, meta = meta),
    class = "pa_objective"
  )
}

add_objective <- function(x, objective) {
  x <- .pamo_as_mo(x)

  if (!inherits(objective, "pa_objective")) {
    stop("add_objective() expects an objective of class 'pa_objective'.", call. = FALSE)
  }

  # store it as an atomic spec in the prioriactions registry
  if (is.null(x$base$data$objectives) || !is.list(x$base$data$objectives)) {
    x$base$data$objectives <- list()
  }
  if (!is.null(x$base$data$objectives[[objective$alias]])) {
    stop("Objective alias already exists: '", objective$alias, "'.", call. = FALSE)
  }

  x$base$data$objectives[[objective$alias]] <- list(
    alias = objective$alias,
    objective_id = "custom",
    model_type = "custom",
    objective_args = list(
      build = objective$build,
      eval  = objective$eval,
      meta  = objective$meta %||% list()
    ),
    sense = objective$sense
  )

  x
}



.pamo_objvec_from_ir <- function(base_superset, ir) {
  stopifnot(inherits(base_superset, "Data"))
  op <- base_superset$data$model_ptr
  if (is.null(op)) stop("Superset model is not built (missing model_ptr).", call. = FALSE)

  # Convención MO: motor en MIN; luego flip en R para sense=max
  rcpp_reset_objective(op, "min")

  terms <- ir$terms %||% list()
  for (t in terms) {
    type <- t$type

    if (identical(type, "pu_cost")) {
      rcpp_add_objective_min_cost(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = base_superset$data$dist_actions_model,
        include_pu_cost = TRUE,
        include_action_cost = FALSE,
        weight = 1.0
      )

    } else if (identical(type, "action_cost")) {
      rcpp_add_objective_min_cost(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = base_superset$data$dist_actions_model,
        include_pu_cost = FALSE,
        include_action_cost = TRUE,
        weight = 1.0
      )

    } else if (identical(type, "boundary_cut")) {
      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]
      if (is.null(rel_model)) stop("Missing relation '", rel_name, "'.", call. = FALSE)

      # ENSURE prepare (idempotente; en C++ ya chequea)
      rcpp_prepare_fragmentation_pu(op, rel_model)

      rcpp_add_objective_min_fragmentation(
        op,
        relation_data = rel_model,
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1]
      )
    } else if (identical(type, "representation")) {
      # Necesitas la versión aditiva: rcpp_add_objective_max_representation (C++)
      acol <- as.character(t$amount_col %||% "amount")[1]

      feats <- t$features_to_use %||% integer()
      ifcol <- as.character(t$internal_feature_col %||% "internal_feature")[1]

      rcpp_add_objective_max_representation(
        op,
        dist_features_data = base_superset$data$dist_features,
        amount_col = acol,
        features_to_use = as.integer(feats),
        internal_feature_col = ifcol,
        weight = 1.0
      )

    } else if (identical(type, "benefit")) {
      # Necesitas versión aditiva: rcpp_add_objective_max_benefit
      bcol <- as.character(t$benefit_col %||% "benefit")[1]
      rcpp_add_objective_max_benefit(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        dist_benefit_data = base_superset$data$dist_benefit_model,
        benefit_col = bcol,
        weight = 1.0
      )

    } else if (identical(type, "profit")) {
      # Necesitas versión aditiva: rcpp_add_objective_max_profit
      pcol <- as.character(t$profit_col %||% "profit")[1]
      rcpp_add_objective_max_profit(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        dist_profit_data  = base_superset$data$dist_profit_model,
        profit_col = pcol,
        weight = 1.0
      )

    } else if (identical(type, "net_profit")) {
      # Opción A: función aditiva directa rcpp_add_objective_max_net_profit
      # Opción B: componer: +profit  -pu_cost -action_cost (pero ojo con signos en convención MIN)
      pcol <- as.character(t$profit_col %||% "profit")[1]
      inc_pu  <- isTRUE(t$include_pu_cost %||% TRUE)
      inc_act <- isTRUE(t$include_action_cost %||% TRUE)

      rcpp_add_objective_max_net_profit(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = base_superset$data$dist_actions_model,
        dist_profit_data  = base_superset$data$dist_profit_model,
        profit_col = pcol,
        include_pu_cost = inc_pu,
        include_action_cost = inc_act,
        weight = 1.0
      )

    } else if (identical(type, "action_boundary_cut")) {
      # Requiere: superset preparado con auxiliares de action frag
      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]
      if (is.null(rel_model)) stop("Missing relation '", rel_name, "'.", call. = FALSE)

      aw <- t$action_weights %||% NULL
      acts <- t$actions %||% NULL

      # ideal: C++ aditivo que usa action_weights vector ya expandido
      rcpp_add_objective_min_fragmentation_actions_by_action(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        relation_data = rel_model,
        actions_to_use = acts %||% NULL,  # o NULL si pasas weights full-length
        action_weights = aw %||% NULL,    # según tu firma
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1],
        weight = 1.0
      )

    } else if (identical(type, "intervention_boundary_cut")) {
      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]
      if (is.null(rel_model)) stop("Missing relation '", rel_name, "'.", call. = FALSE)

      rcpp_add_objective_min_fragmentation_interventions(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        relation_data = rel_model,
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1],
        weight = 1.0
      )

    } else if (identical(type, "custom")) {
      stop("custom objectives are not supported in weighted yet.", call. = FALSE)

    } else {
      stop("Unknown term type in .pamo_objvec_from_ir(): ", type, call. = FALSE)
    }
  }

  m <- .pa_model_from_ptr(op, args = base_superset$data$model_args %||% list(), drop_triplets = TRUE)
  as.numeric(m$obj)
}



.pamo_apply_weighted_objective <- function(base, ir_list, weights, normalize = FALSE) {
  stopifnot(inherits(base, "Data"))
  stopifnot(is.list(ir_list), length(ir_list) > 0)
  weights <- as.numeric(weights)
  if (length(weights) != length(ir_list)) {
    stop("weights length must match ir_list length.", call. = FALSE)
  }

  # 1) construir superset UNA vez (incluye prepares necesarios)
  base2 <- .pamo_prepare_superset_model(base, ir_list)

  # 2) construir objvec para cada IR sobre el MISMO superset (sin rebuild)
  objvecs <- vector("list", length(ir_list))
  for (i in seq_along(ir_list)) {
    v <- .pamo_objvec_from_ir(base2, ir_list[[i]])

    # convención: todo se resuelve como MIN en el motor;
    # si IR es "max", flipa signo acá.
    if (identical(ir_list[[i]]$sense, "max")) v <- -v

    objvecs[[i]] <- v
  }

  # 3) normalización (provisional pero estable)
  if (isTRUE(normalize)) {
    objvecs <- lapply(objvecs, function(v) {
      s <- max(abs(v), na.rm = TRUE)
      if (!is.finite(s) || s <= 0) return(v)
      v / s
    })
  }

  # 4) combinar pesos -> objetivo final
  obj_w <- Reduce(`+`, Map(`*`, objvecs, as.list(weights)))

  # 5) IMPORTANTÍSIMO: inyectar runtime update para que prioriactions lo use en solve()
  base2$data$runtime_updates <- list(
    obj = obj_w,
    modelsense = "min"
  )

  # 6) asegurar que prioriactions NO reconstruya el modelo y no te borre runtime_updates
  base2$data$meta$model_dirty <- FALSE
  base2$data$has_model <- TRUE

  # 7) guarda cache para evaluación posterior (opcional pero muy útil)
  base2$data$mo_cache <- list(
    ir_list = ir_list,
    weights = weights,
    normalize = normalize,
    objvecs = objvecs,
    obj_weighted = obj_w
  )

  base2
}

# ---------------------------------------------------------
# Internal: solve weighted
# ---------------------------------------------------------
.pamo_solve_weighted <- function(x, ...) {
  aliases <- x$method$params$aliases
  w <- x$method$params$weights
  normalize <- isTRUE(x$method$params$normalize)

  if (is.null(w)) {
    stop("Weighted method: missing weights. Provide them in set_method_weighted().", call. = FALSE)
  }

  # build a single run specification (later: grid of weights)
  run_df <- data.frame(run_id = 1L, stringsAsFactors = FALSE)
  for (i in seq_along(aliases)) {
    run_df[[paste0("w_", aliases[i])]] <- w[i]
  }

  solutions <- vector("list", nrow(run_df))
  obj_vals  <- matrix(NA_real_, nrow(run_df), length(aliases))
  colnames(obj_vals) <- paste0("obj_", aliases)

  status <- character(nrow(run_df))
  runtime <- numeric(nrow(run_df))
  gap <- numeric(nrow(run_df))

  for (r in seq_len(nrow(run_df))) {
    w_r <- as.numeric(run_df[r, paste0("w_", aliases), drop = TRUE])

    one <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "weighted",
        aliases = aliases,
        weights = w_r,
        normalize = normalize
      )
    )

    solutions[[r]] <- one$solution
    status[r] <- one$status
    runtime[r] <- one$runtime
    gap[r] <- one$gap

    # TODO: objective evaluation (optional, later)
    # For now, keep NA_real_ (doesn't break, keeps schema stable)
    obj_vals[r, ] <- rep(NA_real_, length(aliases))
  }

  runs <- cbind(
    run_df,
    data.frame(status = status, runtime = runtime, gap = gap, stringsAsFactors = FALSE),
    as.data.frame(obj_vals, stringsAsFactors = FALSE)
  )

  pproto(
    NULL, MOProblem,
    runs = runs,
    solutions = solutions,
    meta = list(
      method = x$method,
      call = match.call()
    )
  )
}

# ---------------------------------------------------------
# Internal: solve a single run using prioriactions as engine
# ---------------------------------------------------------
.pamo_solve_one <- function(x, spec) {

  if (!inherits(x, "MOProblem")) stop(".pamo_solve_one expects MOProblem.", call. = FALSE)
  if (!inherits(x$base, "Data")) stop("MOProblem$base must be a Data.", call. = FALSE)

  base <- .pamo_clone_base(x$base)

  if (identical(spec$type, "weighted")) {

    aliases <- as.character(spec$aliases)
    weights <- as.numeric(spec$weights)
    normalize <- isTRUE(spec$normalize)

    specs <- .pamo_get_objective_specs(x, aliases)
    ir_list <- lapply(specs, function(sp) .pamo_objective_to_ir(base, sp))

    base <- .pamo_apply_weighted_objective(
      base = base,
      ir_list = ir_list,
      weights = weights,
      normalize = normalize
    )

  } else if (identical(spec$type, "epsilon_constraint")) {

    primary    <- as.character(spec$primary)[1]
    eps_list   <- spec$eps %||% list()
    sec_aliases <- names(eps_list)

    if (is.na(primary) || !nzchar(primary)) stop("epsilon_constraint: primary is invalid.", call. = FALSE)
    if (length(sec_aliases) == 0) stop("epsilon_constraint: eps list is empty.", call. = FALSE)

    # ---- build IRs for (primary + constrained) so we can build ONE superset
    all_aliases <- unique(c(primary, sec_aliases))
    specs_all <- .pamo_get_objective_specs(x, all_aliases)
    ir_all <- lapply(specs_all, function(sp) .pamo_objective_to_ir(base, sp))

    # ---- 0) build superset once (includes prepares)
    base <- .pamo_prepare_superset_model(base, ir_all)

    # ---- 1) add epsilon constraints for secondaries
    for (a in sec_aliases) {
      sp_sec <- specs_all[[a]]
      ir_sec <- ir_all[[a]]

      eps_val <- as.numeric(eps_list[[a]])[1]
      if (!is.finite(eps_val)) stop("epsilon_constraint: eps for '", a, "' must be finite.", call. = FALSE)

      base <- .pamo_apply_epsilon_constraint(
        base = base,
        ir = ir_sec,
        eps = eps_val,
        sense = as.character(sp_sec$sense %||% "min")[1],
        name = paste0("eps_", a),
        block_name = "epsilon_constraint"
      )
    }

    # ---- 2) set primary objective (as runtime update, like weighted)
    sp_primary <- specs_all[[primary]]
    ir_primary <- ir_all[[primary]]

    base <- .pamo_apply_single_objective(
      base = base,
      ir = ir_primary,
      sense = as.character(sp_primary$sense %||% "min")[1]
    )

  } else {
    stop("Unsupported spec$type in .pamo_solve_one: ", spec$type, call. = FALSE)
  }

  out <- solve(base)
  .pamo_extract_solution(out)
}

.pamo_apply_epsilon_constraint <- function(base, ir, eps, sense = c("min","max"),
                                           name = "", block_name = "epsilon_constraint", tag = "") {
  stopifnot(inherits(base, "Data"))
  sense <- match.arg(sense)

  if (is.null(base$data$model_ptr)) stop("Model not built (model_ptr is NULL).", call. = FALSE)

  eps <- as.numeric(eps)[1]
  if (!is.finite(eps)) stop("eps must be finite.", call. = FALSE)

  v <- .pamo_objvec_from_ir(base, ir)
  if (length(v) == 0) stop("epsilon objvec is empty.", call. = FALSE)

  # canonical: <=
  if (identical(sense, "max")) {
    v <- -v
    eps <- -eps
  }

  idx <- which(v != 0)
  if (length(idx) == 0) stop("epsilon objvec has no non-zero coefficients.", call. = FALSE)

  # add row: sum(v[j]*x[j]) <= eps
  rcpp_add_linear_constraint(
    base$data$model_ptr,
    j0 = as.integer(idx - 1L),
    x  = as.numeric(v[idx]),
    sense = "<=",
    rhs = as.numeric(eps),
    name = as.character(name %||% ""),
    block_name = block_name,
    tag = tag
  )

  base$data$meta$model_dirty <- TRUE
  base <- .pa_refresh_model_snapshot(base)
  base
}

# ---------------------------------------------------------
# Internal: extract (minimal) results from prioriactions Solution
# ---------------------------------------------------------
.pamo_extract_solution <- function(out) {
  # out is expected to be a prioriactions::Solution (pproto)
  # We'll be defensive: if slots are missing, keep NA.
  objval <- tryCatch(out$data$objval, error = function(e) NA_real_)
  gap    <- tryCatch(out$data$gap,    error = function(e) NA_real_)
  runtime <- tryCatch(out$data$runtime, error = function(e) NA_real_)
  status <- tryCatch(out$data$status, error = function(e) "ok")

  list(
    solution = out,
    status = status,
    runtime = runtime,
    gap = gap,
    objval = objval
  )
}




#' @keywords internal
.mo_abort <- function(...) stop(paste0(...), call. = FALSE)

#' @keywords internal
.mo_get_solution_from <- function(x, run = 1L) {

  # Case 1: already a Solution
  if (inherits(x, "Solution")) return(x)

  # Case 2: MOProblem -> x$results$solutions
  if (inherits(x, "MOProblem")) {
    r <- x$results %||% NULL
    if (is.null(r)) .mo_abort("MOProblem has no results (x$results is NULL).")
    sols <- r$solutions %||% NULL
    if (is.null(sols)) .mo_abort("MOProblem results has no solutions (x$results$solutions is NULL).")

    run <- as.integer(run)[1]
    if (!is.finite(run) || run < 1L) .mo_abort("run must be a positive integer (1-based).")
    if (run > length(sols)) .mo_abort("run=", run, " out of range. There are only ", length(sols), " solutions.")
    sol <- sols[[run]]
    if (!inherits(sol, "Solution")) .mo_abort("x$results$solutions[[run]] is not a Solution.")
    return(sol)
  }

  # Case 3: generic MOResults list-like (optional)
  # Accept either list of Solution or object with $solutions
  if (is.list(x) && !is.null(x$solutions)) {
    sols <- x$solutions
    run <- as.integer(run)[1]
    if (!is.finite(run) || run < 1L) .mo_abort("run must be a positive integer (1-based).")
    if (run > length(sols)) .mo_abort("run=", run, " out of range. There are only ", length(sols), " solutions.")
    sol <- sols[[run]]
    if (!inherits(sol, "Solution")) .mo_abort("x$solutions[[run]] is not a Solution.")
    return(sol)
  }

  if (is.list(x) && length(x) > 0 && inherits(x[[1]], "Solution")) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || run < 1L) .mo_abort("run must be a positive integer (1-based).")
    if (run > length(x)) .mo_abort("run=", run, " out of range. There are only ", length(x), " solutions.")
    return(x[[run]])
  }

  .mo_abort("Unsupported object. Expected Solution, MOProblem, or MOResults-like with $solutions.")
}
