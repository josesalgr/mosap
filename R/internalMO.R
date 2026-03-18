# internal.R (mosap)
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
# Promote Problem -> MOProblem (internal; never exposed to user)
# -------------------------------------------------------------------------

.pamo_as_mo <- function(x) {
  if (inherits(x, "MOProblem")) return(x)

  if (inherits(x, "Problem")) {
    obj <- pproto(NULL, MOProblem, base = x)

    # fuerza clase S3 para que UseMethod("solve") encuentre solve.MOProblem
    cls <- class(obj)
    if (is.null(cls)) cls <- character()
    class(obj) <- unique(c("MOProblem", cls))

    return(obj)
  }

  stop("Expected a Problem or a MOProblem.", call. = FALSE)
}


# -------------------------------------------------------------------------
# Atomic objective registry accessors (stored in mosap::Problem)
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
.pamo_objvec_action_boundary_cut <- function(base_superset, term) {
  stopifnot(inherits(base_superset, "Problem"))

  op <- base_superset$data$model_ptr
  if (is.null(op)) {
    stop("Superset model is not built (missing model_ptr).", call. = FALSE)
  }

  ml <- base_superset$data$model_list %||% NULL
  if (is.null(ml) || !is.list(ml)) {
    base_superset <- .pa_refresh_model_snapshot(base_superset)
    ml <- base_superset$data$model_list %||% NULL
  }
  if (is.null(ml) || !is.list(ml)) {
    stop("Model snapshot is missing in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }

  m0 <- .pa_model_from_ptr(
    op,
    args = base_superset$data$model_args %||% list(),
    drop_triplets = TRUE
  )

  n_col <- as.integer(
    ml$ncol %||%
      ml$n_col %||%
      length(m0$obj)
  )

  n_x <- as.integer(ml$n_x %||% NA_integer_)
  x_offset <- as.integer(ml$x_offset %||% NA_integer_)
  y0 <- as.integer(ml$y_action_offset %||% NA_integer_)
  n_y <- as.integer(ml$n_y_action %||% NA_integer_)

  if (!is.finite(n_col) || is.na(n_col) || n_col <= 0L) {
    stop("Invalid model dimension (ncol) in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  if (!is.finite(n_x) || is.na(n_x) || n_x <= 0L) {
    stop("Invalid n_x in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  if (!is.finite(x_offset) || is.na(x_offset) || x_offset < 0L) {
    stop("Invalid x_offset in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  if (!is.finite(y0) || is.na(y0) || y0 < 0L) {
    stop("Invalid y_action_offset in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  if (!is.finite(n_y) || is.na(n_y) || n_y <= 0L) {
    stop("Invalid n_y_action in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }

  v <- numeric(n_col)

  # ---- relation
  rel_name <- as.character(term$relation_name %||% "boundary")[1]
  rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
    base_superset$data$spatial_relations[[rel_name]] %||%
    NULL

  if (is.null(rel_model) || !inherits(rel_model, "data.frame")) {
    stop("Missing relation '", rel_name, "' in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  for (nm in c("internal_pu1", "internal_pu2", "weight")) {
    if (!nm %in% names(rel_model)) {
      stop("Relation '", rel_name, "' must contain column '", nm, "'.", call. = FALSE)
    }
  }

  # ---- dist_actions model-ready
  da <- base_superset$data$dist_actions_model %||% NULL
  if (is.null(da) || !inherits(da, "data.frame")) {
    stop("dist_actions_model is missing in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  for (nm in c("internal_row", "internal_pu", "internal_action")) {
    if (!nm %in% names(da)) {
      stop("dist_actions_model must contain column '", nm, "'.", call. = FALSE)
    }
  }

  # ---- infer global number of actions
  all_actions <- sort(unique(as.integer(da$internal_action)))
  all_actions <- all_actions[is.finite(all_actions) & !is.na(all_actions)]
  if (length(all_actions) == 0L) {
    stop("Could not infer internal actions in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }
  n_actions_global <- max(all_actions)

  # ---- selected actions
  act_raw <- term$actions %||% NULL
  if (is.null(act_raw) || length(act_raw) == 0L) {
    act_int <- all_actions
  } else {
    act_int <- as.integer(act_raw)
    act_int <- sort(unique(act_int[is.finite(act_int) & !is.na(act_int)]))
  }

  if (length(act_int) == 0L) {
    return(v)
  }

  # ---- action weights
  aw <- term$action_weights %||% NULL
  if (is.null(aw)) {
    aw_map <- stats::setNames(rep(1, length(act_int)), act_int)
  } else {
    aw <- as.numeric(aw)
    if (length(aw) == length(all_actions)) {
      tmp_map <- stats::setNames(aw, all_actions)
      aw_map <- tmp_map[as.character(act_int)]
    } else if (length(aw) == length(act_int)) {
      aw_map <- stats::setNames(aw, act_int)
    } else {
      stop(
        "action_weights length must match either all actions or the selected action subset.",
        call. = FALSE
      )
    }
  }

  weight_multiplier <- as.numeric(term$weight_multiplier %||% 1)[1]
  if (!is.finite(weight_multiplier)) weight_multiplier <- 1

  n_pu <- nrow(base_superset$data$pu)
  if (!is.finite(n_pu) || is.na(n_pu) || n_pu <= 0L) {
    stop("Invalid n_pu in .pamo_objvec_action_boundary_cut().", call. = FALSE)
  }

  # ---- canonicalize relation exactly like C++
  ip1 <- as.integer(rel_model$internal_pu1)
  ip2 <- as.integer(rel_model$internal_pu2)
  ww  <- as.numeric(rel_model$weight)

  ok <- is.finite(ip1) & is.finite(ip2) & is.finite(ww)
  ip1 <- ip1[ok]
  ip2 <- ip2[ok]
  ww  <- ww[ok]

  self_w <- numeric(n_pu)
  edge_map <- new.env(parent = emptyenv(), hash = TRUE)

  make_key <- function(a, b) paste0(a, "::", b)

  for (r in seq_along(ip1)) {
    i <- ip1[r]
    j <- ip2[r]
    wij <- ww[r]

    if (i == j) {
      self_w[i] <- self_w[i] + wij
    } else {
      a <- min(i, j)
      b <- max(i, j)
      k <- make_key(a, b)

      old <- if (exists(k, envir = edge_map, inherits = FALSE)) {
        get(k, envir = edge_map, inherits = FALSE)
      } else {
        NA_real_
      }

      if (is.na(old) || wij > old) {
        assign(k, wij, envir = edge_map)
      }
    }
  }

  edge_keys <- ls(edge_map, all.names = TRUE)
  if (length(edge_keys) == 0L) {
    return(v)
  }

  edge_df <- data.frame(
    a = integer(length(edge_keys)),
    b = integer(length(edge_keys)),
    weight = numeric(length(edge_keys))
  )

  for (k in seq_along(edge_keys)) {
    parts <- strsplit(edge_keys[k], "::", fixed = TRUE)[[1]]
    edge_df$a[k] <- as.integer(parts[1])
    edge_df$b[k] <- as.integer(parts[2])
    edge_df$weight[k] <- get(edge_keys[k], envir = edge_map, inherits = FALSE)
  }

  edge_df <- edge_df[order(edge_df$a, edge_df$b), , drop = FALSE]
  k_edges <- nrow(edge_df)

  expected_n_y <- k_edges * n_actions_global
  if (expected_n_y != n_y) {
    stop(
      "Mismatch in y_action layout in .pamo_objvec_action_boundary_cut().\n",
      "expected n_y_action = k_edges * n_actions_global = ", expected_n_y,
      ", but model snapshot reports n_y_action = ", n_y, ".",
      call. = FALSE
    )
  }

  incident_w <- numeric(n_pu)
  for (r in seq_len(k_edges)) {
    incident_w[edge_df$a[r]] <- incident_w[edge_df$a[r]] + edge_df$weight[r]
    incident_w[edge_df$b[r]] <- incident_w[edge_df$b[r]] + edge_df$weight[r]
  }

  # ---- sparse map (pu, action) -> x column
  key2 <- function(i, a) paste0(i, "::", a)

  da_key <- key2(da$internal_pu, da$internal_action)
  x_cols <- x_offset + as.integer(da$internal_row) - 1L
  x_map <- stats::setNames(as.integer(x_cols), da_key)

  # ---- linear part on x
  for (act in act_int) {
    awi <- as.numeric(aw_map[[as.character(act)]])
    if (!is.finite(awi) || awi == 0) next

    for (i in seq_len(n_pu)) {
      k <- key2(i, act)

      # use [ ] instead of [[ ]] so missing keys return NA instead of error
      col_x <- unname(x_map[k])[1]

      if (!is.finite(col_x) || is.na(col_x)) next

      coef <- weight_multiplier * awi * (incident_w[i] + self_w[i])
      if (coef != 0) {
        v[col_x + 1L] <- v[col_x + 1L] + coef
      }
    }
  }

  # ---- edge part on global y_action block
  for (e in seq_len(k_edges)) {
    we <- edge_df$weight[e]

    for (act in act_int) {
      awi <- as.numeric(aw_map[[as.character(act)]])
      if (!is.finite(awi) || awi == 0) next

      # global layout: e is 0-based in the formula
      bcol0 <- y0 + (e - 1L) * n_actions_global + (act - 1L)
      coef <- weight_multiplier * (-2) * we * awi

      if (coef != 0) {
        v[bcol0 + 1L] <- v[bcol0 + 1L] + coef
      }
    }
  }

  v
}

.pamo_objective_to_ir <- function(x, spec) {
  stopifnot(inherits(x, "Problem"))
  stopifnot(is.list(spec), !is.null(spec$objective_id))

  id <- as.character(spec$objective_id)[1]
  a  <- spec$objective_args %||% list()

  sense <- as.character(spec$sense %||% NA_character_)[1]
  if (is.na(sense) || !nzchar(sense)) sense <- NA_character_

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

  .chr <- function(z) {
    if (is.null(z)) return(NULL)
    z <- as.character(z)
    z <- unique(z[!is.na(z) & nzchar(z)])
    if (length(z) == 0) return(NULL)
    z
  }

  # ------------------------------------------------------------------
  # min_cost
  # ------------------------------------------------------------------
  if (identical(id, "min_cost")) {

    inc_pu  <- .l1(a$include_pu_cost, TRUE)
    inc_act <- .l1(a$include_action_cost, TRUE)
    actions <- .chr(a$actions)
    feats   <- .chr(a$features)

    terms <- list()
    if (inc_pu) {
      terms <- c(terms, list(list(
        type = "pu_cost",
        features = feats
      )))
    }
    if (inc_act) {
      terms <- c(terms, list(list(
        type = "action_cost",
        actions = actions
      )))
    }

    a$include_pu_cost     <- inc_pu
    a$include_action_cost <- inc_act
    a$actions             <- actions
    a$features            <- feats

    return(list(
      sense = "min",
      terms = terms,
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # max_benefit
  # ------------------------------------------------------------------
  if (identical(id, "max_benefit")) {
    bcol    <- .c1(a$benefit_col, "benefit")
    actions <- .chr(a$actions)
    feats   <- .chr(a$features)

    a$benefit_col <- bcol
    a$actions     <- actions
    a$features    <- feats

    return(list(
      sense = "max",
      terms = list(list(
        type = "benefit",
        benefit_col = bcol,
        actions = actions,
        features = feats
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # max_profit
  # ------------------------------------------------------------------
  if (identical(id, "max_profit")) {
    pcol    <- .c1(a$profit_col, "profit")
    actions <- .chr(a$actions)

    a$profit_col <- pcol
    a$actions    <- actions

    return(list(
      sense = "max",
      terms = list(list(
        type = "profit",
        profit_col = pcol,
        actions = actions
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # max_net_profit
  # ------------------------------------------------------------------
  if (identical(id, "max_net_profit")) {
    pcol    <- .c1(a$profit_col, "profit")
    inc_pu  <- .l1(a$include_pu_cost, TRUE)
    inc_act <- .l1(a$include_action_cost, TRUE)
    actions <- .chr(a$actions)
    feats   <- .chr(a$features)

    a$profit_col          <- pcol
    a$include_pu_cost     <- inc_pu
    a$include_action_cost <- inc_act
    a$actions             <- actions
    a$features            <- feats

    return(list(
      sense = "max",
      terms = list(list(
        type = "net_profit",
        profit_col = pcol,
        include_pu_cost = inc_pu,
        include_action_cost = inc_act,
        actions = actions,
        features = feats
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # min_fragmentation
  # ------------------------------------------------------------------
  if (identical(id, "min_fragmentation")) {
    rel   <- .c1(a$relation_name, "boundary")
    mul   <- .n1(a$weight_multiplier, 1)
    feats <- .chr(a$features)

    a$relation_name     <- rel
    a$weight_multiplier <- mul
    a$features          <- feats

    return(list(
      sense = "min",
      terms = list(list(
        type = "boundary_cut",
        relation_name = rel,
        weight_multiplier = mul,
        features = feats
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # min_action_fragmentation
  # ------------------------------------------------------------------
  if (identical(id, "min_action_fragmentation")) {
    rel     <- .c1(a$relation_name, "boundary")
    mul     <- .n1(a$weight_multiplier, 1)
    actions <- .chr(a$actions)

    aw <- a$action_weights %||% NULL
    if (!is.null(aw)) aw <- as.numeric(aw)

    a$relation_name     <- rel
    a$weight_multiplier <- mul
    a$actions           <- actions
    a$action_weights    <- aw

    return(list(
      sense = "min",
      terms = list(list(
        type = "action_boundary_cut",
        relation_name = rel,
        weight_multiplier = mul,
        actions = actions,
        action_weights = aw
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # min_intervention_fragmentation
  # ------------------------------------------------------------------
  if (identical(id, "min_intervention_fragmentation")) {
    rel     <- .c1(a$relation_name, "boundary")
    mul     <- .n1(a$weight_multiplier, 1)
    actions <- .chr(a$actions)

    a$relation_name     <- rel
    a$weight_multiplier <- mul
    a$actions           <- actions

    return(list(
      sense = "min",
      terms = list(list(
        type = "intervention_boundary_cut",
        relation_name = rel,
        weight_multiplier = mul,
        actions = actions
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # min_intervention_impact
  # ------------------------------------------------------------------
  if (identical(id, "min_intervention_impact")) {
    icol    <- .c1(a$impact_col, "amount")
    feats   <- .chr(a$features)
    actions <- .chr(a$actions)

    a$impact_col <- icol
    a$features   <- feats
    a$actions    <- actions

    return(list(
      sense = "min",
      terms = list(list(
        type = "intervention_impact",
        impact_col = icol,
        features = feats,
        actions = actions
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # max_representation
  # ------------------------------------------------------------------
  if (identical(id, "max_representation")) {
    acol  <- .c1(a$amount_col, "amount")
    feats <- .chr(a$features)

    a$amount_col <- acol
    a$features   <- feats

    return(list(
      sense = "max",
      terms = list(list(
        type = "representation",
        amount_col = acol,
        features = feats
      )),
      objective_id = id,
      objective_args = a
    ))
  }

  # ------------------------------------------------------------------
  # custom
  # ------------------------------------------------------------------
  if (identical(id, "custom")) {
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
# Cloning base Problem safely for MO runs
# (Avoid copying externalptr / built model pointer)
# -------------------------------------------------------------------------

.pamo_deepcopy_data <- function(d) {
  # Deep copy list/data.frames. WARNING: do NOT deep-copy externalptr; we drop model_ptr later.
  unserialize(serialize(d, NULL))
}

.pamo_clone_base <- function(base) {
  stopifnot(inherits(base, "Problem"))

  b <- pproto(NULL, base)
  b$data <- .pamo_deepcopy_data(base$data)

  b$data$model_ptr <- NULL
  b$data$has_model <- FALSE
  b$data$model_list <- NULL
  b$data$model_index <- NULL

  if (is.null(b$data$meta) || !is.list(b$data$meta)) b$data$meta <- list()
  b$data$meta$model_dirty <- TRUE

  b
}

# -------------------------------------------------------------------------
# Activate an IR as a single-objective config in mosap::Problem
# (Used by the "rebuild + pad" objective vector strategy)
# -------------------------------------------------------------------------

.pamo_activate_ir_as_single_objective <- function(x, ir) {
  stopifnot(inherits(x, "Problem"))
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
    min_intervention_impact = "minimizeInterventionImpact",
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
  stopifnot(inherits(base, "Problem"))
  stopifnot(is.list(ir_list), length(ir_list) > 0)

  spec <- .pamo_compile_superset_spec(base, ir_list)

  # elegir un objetivo "semilla" válido para que el core del modelo se materialice.
  # preferimos min_cost si existe; si no, el primero.
  pick_seed_ir <- function(ir_list) {
    idx_cost <- which(vapply(
      ir_list,
      function(ir) identical(ir$objective_id %||% "", "min_cost"),
      logical(1)
    ))
    if (length(idx_cost) > 0L) return(ir_list[[idx_cost[1]]])
    ir_list[[1]]
  }

  ir_seed <- pick_seed_ir(ir_list)

  b <- .pamo_clone_base(base)
  b <- .pamo_activate_ir_as_single_objective(b, ir_seed)

  b$data$model_args <- b$data$model_args %||% list()
  b$data$model_args$mo_mode <- TRUE

  # la clave del superset: needs estructurales, independientes del método
  b$data$model_args$needs <- modifyList(
    b$data$model_args$needs %||% list(),
    spec$needs
  )

  if (!is.null(spec$relation_name)) {
    b$data$model_args$needs$relation_name <- spec$relation_name
  }

  b <- .pa_build_model(b)

  op <- b$data$model_ptr
  if (is.null(op)) {
    stop("Superset build failed: model_ptr is NULL.", call. = FALSE)
  }

  # sanity check mínimo: si hay objetivos de representación, el modelo debe tener z
  if (isTRUE(spec$needs$z)) {
    op_list <- .pa_model_from_ptr(
      op,
      args = b$data$model_args %||% list(),
      drop_triplets = TRUE
    )

    n_z <- as.integer(op_list$n_z %||% 0L)
    if (n_z <= 0L) {
      stop(
        "MO superset requires z variables, but the built model has n_z = 0.",
        call. = FALSE
      )
    }
  }

  # sanity check PU fragmentation
  if (isTRUE(spec$needs$y_pu)) {
    op_list <- .pa_model_from_ptr(
      op,
      args = b$data$model_args %||% list(),
      drop_triplets = TRUE
    )

    n_y_pu <- as.integer(op_list$n_y_pu %||% 0L)
    if (n_y_pu <= 0L) {
      stop(
        "MO superset requires PU fragmentation auxiliaries, but the built model has n_y_pu = 0.",
        call. = FALSE
      )
    }
  }

  b
}


.pamo_model_obj_length <- function(x) {
  stopifnot(inherits(x, "Problem"))
  if (is.null(x$data$model_ptr)) stop("Problem has no model_ptr; build model first.", call. = FALSE)

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
  stopifnot(inherits(x, "MOProblem"))

  aliases   <- x$method$params$aliases
  w         <- x$method$params$weights
  normalize <- isTRUE(x$method$params$normalize)

  if (is.null(aliases) || length(aliases) == 0L) {
    stop("Weighted method: missing aliases.", call. = FALSE)
  }

  if (is.null(w)) {
    stop("Weighted method: missing weights. Provide them in set_method_weighted().", call. = FALSE)
  }

  if (length(w) != length(aliases)) {
    stop(
      "Weighted method: length(weights) must match length(aliases).\n",
      "length(weights) = ", length(w), ", length(aliases) = ", length(aliases), ".",
      call. = FALSE
    )
  }

  # ---- design: one row per run, only design parameters
  design_df <- data.frame(run_id = 1L, stringsAsFactors = FALSE)
  for (i in seq_along(aliases)) {
    design_df[[paste0("weight_", aliases[i])]] <- as.numeric(w[i])
  }

  n_runs <- nrow(design_df)

  solutions <- vector("list", n_runs)

  value_mat <- matrix(NA_real_, n_runs, length(aliases))
  colnames(value_mat) <- paste0("value_", aliases)

  status  <- character(n_runs)
  runtime <- numeric(n_runs)
  gap     <- numeric(n_runs)

  # pass-through solver controls
  dots <- list(...)
  gap_limit  <- dots$gap_limit %||% NULL
  time_limit <- dots$time_limit %||% NULL

  for (r in seq_len(n_runs)) {
    w_r <- as.numeric(design_df[r, paste0("weight_", aliases), drop = TRUE])

    one <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "weighted",
        aliases = aliases,
        weights = w_r,
        normalize = normalize,
        gap_limit = gap_limit,
        time_limit = time_limit
      )
    )

    solutions[[r]] <- one$solution
    status[r]  <- as.character(one$status %||% NA_character_)
    runtime[r] <- as.numeric(one$runtime %||% NA_real_)
    gap[r]     <- as.numeric(one$gap %||% NA_real_)

    # evaluate all registered aliases on the obtained solution
    alias_values <- setNames(
      vapply(
        aliases,
        function(a) .pamo_eval_alias_on_solution(x, one$solution, a),
        numeric(1)
      ),
      aliases
    )

    value_mat[r, ] <- unname(as.numeric(alias_values))

    # store metadata inside each individual Solution
    if (!is.null(solutions[[r]]) && !is.null(solutions[[r]]$data)) {
      solutions[[r]]$data$alias_values <- alias_values
      solutions[[r]]$data$run_id <- design_df$run_id[r]
      solutions[[r]]$data$method_name <- "weighted"
      solutions[[r]]$data$weights <- stats::setNames(as.numeric(w_r), aliases)
    }
  }

  # ---- runs: one row per run, only outputs / summaries
  runs <- data.frame(
    run_id = design_df$run_id,
    status = status,
    runtime = runtime,
    gap = gap,
    stringsAsFactors = FALSE
  )

  runs <- cbind(
    runs,
    as.data.frame(value_mat, stringsAsFactors = FALSE)
  )

  pproto(
    NULL, SolutionMO,
    method = x$method,
    design = design_df,
    runs = runs,
    solutions = solutions,
    extras = list(),
    meta = list(
      call = match.call()
    )
  )
}

.pamo_solve_epsilon_constraint <- function(x, ...) {

  stopifnot(inherits(x, "MOProblem"))

  method <- x$method %||% NULL
  if (is.null(method) || !is.list(method)) {
    stop("epsilon_constraint: x$method is missing or invalid.", call. = FALSE)
  }

  primary     <- as.character(method$primary %||% NA_character_)[1]
  aliases     <- as.character(method$aliases %||% character(0))
  constrained <- as.character(method$constrained %||% character(0))
  mode        <- as.character(method$mode %||% "manual")[1]

  if (is.na(primary) || !nzchar(primary)) {
    stop("epsilon_constraint: missing primary objective.", call. = FALSE)
  }
  if (length(aliases) == 0L) {
    stop("epsilon_constraint: aliases are missing.", call. = FALSE)
  }
  if (length(constrained) == 0L) {
    stop("epsilon_constraint: no constrained objectives were defined.", call. = FALSE)
  }
  if (!mode %in% c("manual", "auto")) {
    stop("epsilon_constraint: unknown mode '", mode, "'.", call. = FALSE)
  }

  # pass-through solver controls
  dots <- list(...)
  gap_limit  <- dots$gap_limit %||% NULL
  time_limit <- dots$time_limit %||% NULL

  # progress control
  progress <- dots$progress %||% dots$verbose %||% interactive()
  progress <- isTRUE(progress)

  # ---------------------------------------------------------
  # Build or recover epsilon design
  # ---------------------------------------------------------
  if (identical(mode, "manual")) {
    design_df <- method$runs

    if (is.null(design_df) || !inherits(design_df, "data.frame") || nrow(design_df) == 0L) {
      stop("epsilon_constraint (manual): x$method$runs is missing/empty.", call. = FALSE)
    }

  } else {
    if (!exists(".pamo_build_auto_epsilon_runs", mode = "function")) {
      stop(
        "epsilon_constraint (auto): missing internal helper .pamo_build_auto_epsilon_runs().",
        call. = FALSE
      )
    }

    design_df <- method$runs %||% NULL
    if (is.null(design_df)) {
      design_df <- .pamo_build_auto_epsilon_runs(x)
    }

    if (is.null(design_df) || !inherits(design_df, "data.frame") || nrow(design_df) == 0L) {
      stop("epsilon_constraint (auto): generated epsilon runs are empty.", call. = FALSE)
    }

    x$method$runs <- design_df
  }

  if (!("run_id" %in% names(design_df))) {
    design_df$run_id <- seq_len(nrow(design_df))
  }

  eps_cols <- paste0("eps_", constrained)
  miss_cols <- setdiff(eps_cols, names(design_df))
  if (length(miss_cols) > 0L) {
    stop(
      "epsilon_constraint: design is missing epsilon columns: ",
      paste(miss_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------
  # Solve runs
  # ---------------------------------------------------------
  n_runs <- nrow(design_df)

  solutions <- vector("list", n_runs)

  value_mat <- matrix(NA_real_, n_runs, length(aliases))
  colnames(value_mat) <- paste0("value_", aliases)

  status  <- character(n_runs)
  runtime <- numeric(n_runs)
  gap     <- numeric(n_runs)

  pb <- NULL
  if (progress && n_runs > 1L) {
    pb <- utils::txtProgressBar(min = 0, max = n_runs, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (r in seq_len(n_runs)) {

    eps_r <- as.list(design_df[r, eps_cols, drop = FALSE])
    names(eps_r) <- constrained

    one <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "epsilon_constraint",
        primary = primary,
        eps = eps_r,
        gap_limit = gap_limit,
        time_limit = time_limit
      )
    )

    solutions[[r]] <- one$solution
    status[r]  <- as.character(one$status %||% NA_character_)
    runtime[r] <- as.numeric(one$runtime %||% NA_real_)
    gap[r]     <- as.numeric(one$gap %||% NA_real_)

    alias_values <- setNames(
      vapply(
        aliases,
        function(a) .pamo_eval_alias_on_solution(x, one$solution, a),
        numeric(1)
      ),
      aliases
    )

    value_mat[r, ] <- unname(as.numeric(alias_values))

    if (!is.null(solutions[[r]]) && !is.null(solutions[[r]]$data)) {
      solutions[[r]]$data$alias_values <- alias_values
      solutions[[r]]$data$run_id <- design_df$run_id[r]
      solutions[[r]]$data$method_name <- "epsilon_constraint"
      solutions[[r]]$data$primary_alias <- primary
      solutions[[r]]$data$eps <- stats::setNames(
        as.numeric(design_df[r, eps_cols, drop = TRUE]),
        constrained
      )
    }

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, r)
    }
  }

  # ---------------------------------------------------------
  # Runs summary: outputs only
  # ---------------------------------------------------------
  runs <- data.frame(
    run_id = design_df$run_id,
    status = status,
    runtime = runtime,
    gap = gap,
    stringsAsFactors = FALSE
  )

  runs <- cbind(
    runs,
    as.data.frame(value_mat, stringsAsFactors = FALSE)
  )

  # ---------------------------------------------------------
  # Extras: method-specific metadata
  # ---------------------------------------------------------
  extras <- list()
  extras$epsilon_grid <- design_df

  ext <- attr(design_df, "extremes", exact = TRUE)
  if (!is.null(ext)) {
    extras$extremes <- ext
  }

  if (!is.null(method$meta)) {
    extras$method_meta <- method$meta
  }

  pproto(
    NULL, SolutionMO,
    method = x$method,
    design = design_df,
    runs = runs,
    solutions = solutions,
    extras = extras,
    meta = list(
      call = match.call()
    )
  )
}

.pamo_build_auto_epsilon_runs <- function(x) {
  stopifnot(inherits(x, "MOProblem"))

  method <- x$method %||% list()

  primary          <- as.character(method$primary %||% NA_character_)[1]
  aliases          <- as.character(method$aliases %||% character(0))
  constrained      <- as.character(method$constrained %||% character(0))
  n_points         <- as.integer(method$n_points %||% NA_integer_)[1]
  include_extremes <- isTRUE(method$include_extremes)

  if (is.na(primary) || !nzchar(primary)) {
    stop("Auto epsilon mode: missing primary objective.", call. = FALSE)
  }

  if (length(aliases) == 0L) {
    stop("Auto epsilon mode: aliases are missing.", call. = FALSE)
  }

  if (!primary %in% aliases) {
    stop("Auto epsilon mode: primary must be included in aliases.", call. = FALSE)
  }

  if (length(constrained) == 0L) {
    stop("Auto epsilon mode requires at least one constrained objective.", call. = FALSE)
  }

  if (!is.finite(n_points) || is.na(n_points) || n_points < 2L) {
    stop("Auto epsilon mode requires n_points >= 2.", call. = FALSE)
  }

  if (length(aliases) != 2L) {
    stop(
      "Auto epsilon mode currently supports exactly 2 objectives.\n",
      "You provided ", length(aliases), " objective(s): ",
      paste(aliases, collapse = ", "), ".\n",
      "Use mode='manual' for 3+ objectives.",
      call. = FALSE
    )
  }

  if (length(constrained) != 1L) {
    stop(
      "Auto epsilon mode currently expects exactly 1 constrained objective.",
      call. = FALSE
    )
  }

  secondary <- constrained[1]

  ext <- .pamo_compute_epsilon_extremes_2obj(
    x = x,
    primary = primary,
    secondary = secondary
  )

  sec_min <- as.numeric(ext$secondary_min)[1]
  sec_max <- as.numeric(ext$secondary_max)[1]

  if (!is.finite(sec_min) || !is.finite(sec_max)) {
    stop("Auto epsilon mode: computed epsilon bounds are not finite.", call. = FALSE)
  }

  if (sec_min > sec_max) {
    tmp <- sec_min
    sec_min <- sec_max
    sec_max <- tmp
  }

  eps_vals <- seq(from = sec_min, to = sec_max, length.out = n_points)

  if (!isTRUE(include_extremes)) {
    if (length(eps_vals) <= 2L) {
      stop(
        "include_extremes=FALSE requires n_points >= 3.",
        call. = FALSE
      )
    }
    eps_vals <- eps_vals[2:(length(eps_vals) - 1L)]
  }

  if (length(eps_vals) == 0L) {
    stop("Auto epsilon mode produced an empty epsilon grid.", call. = FALSE)
  }

  eps_col <- paste0("eps_", secondary)

  design_df <- data.frame(
    run_id = seq_along(eps_vals),
    stringsAsFactors = FALSE
  )
  design_df[[eps_col]] <- as.numeric(eps_vals)

  attr(design_df, "extremes") <- ext
  attr(design_df, "primary_alias") <- primary
  attr(design_df, "secondary_alias") <- secondary
  attr(design_df, "include_extremes") <- include_extremes
  attr(design_df, "n_points_requested") <- n_points

  design_df
}




# ---------------------------------------------------------
# Internal: extract (minimal) results from mosap Solution
# ---------------------------------------------------------
# .pamo_extract_solution <- function(out) {
#   # out is expected to be a mosap::Solution (pproto)
#   # We'll be defensive: if slots are missing, keep NA.
#   objval <- tryCatch(out$data$objval, error = function(e) NA_real_)
#   gap    <- tryCatch(out$data$gap,    error = function(e) NA_real_)
#   runtime <- tryCatch(out$data$runtime, error = function(e) NA_real_)
#   status <- tryCatch(out$data$status, error = function(e) "ok")
#
#   list(
#     solution = out,
#     status = status,
#     runtime = runtime,
#     gap = gap,
#     objval = objval
#   )
# }


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

  # store it as an atomic spec in the mosap registry
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
  stopifnot(inherits(base_superset, "Problem"))

  op <- base_superset$data$model_ptr
  if (is.null(op)) {
    stop("Superset model is not built (missing model_ptr).", call. = FALSE)
  }

  # ------------------------------------------------------------
  # local helpers
  # ------------------------------------------------------------
  .chr <- function(z) {
    if (is.null(z)) return(NULL)
    z <- as.character(z)
    z <- unique(z[!is.na(z) & nzchar(z)])
    if (length(z) == 0) return(NULL)
    z
  }

  .resolve_feature_internal <- function(features) {
    features <- .chr(features)
    if (is.null(features)) return(integer(0))

    feat_df <- base_superset$data$features
    if (is.null(feat_df) || !inherits(feat_df, "data.frame") || nrow(feat_df) == 0) {
      stop("Cannot resolve feature subset: x$data$features is missing/empty.", call. = FALSE)
    }
    if (!all(c("id", "internal_id") %in% names(feat_df))) {
      stop("x$data$features must contain 'id' and 'internal_id'.", call. = FALSE)
    }

    idx <- match(features, as.character(feat_df$id))
    if (anyNA(idx)) {
      bad <- features[is.na(idx)]
      stop("Unknown feature ids in objective subset: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    as.integer(feat_df$internal_id[idx])
  }

  .resolve_action_internal <- function(actions) {
    actions <- .chr(actions)
    if (is.null(actions)) return(integer(0))

    out <- .pa_resolve_action_subset(base_superset, subset = actions)
    as.integer(out$internal_id)
  }

  .subset_dist_actions <- function(actions = NULL) {
    da <- base_superset$data$dist_actions_model
    if (is.null(da) || !inherits(da, "data.frame")) {
      stop("dist_actions_model is missing.", call. = FALSE)
    }

    act_int <- .resolve_action_internal(actions)
    if (length(act_int) == 0) return(da)

    da[da$internal_action %in% act_int, , drop = FALSE]
  }

  .subset_dist_effects <- function(df, actions = NULL, features = NULL) {
    if (is.null(df) || !inherits(df, "data.frame")) return(df)

    act_int  <- .resolve_action_internal(actions)
    feat_int <- .resolve_feature_internal(features)

    out <- df

    if (length(act_int) > 0 && "internal_action" %in% names(out)) {
      out <- out[out$internal_action %in% act_int, , drop = FALSE]
    }

    if (length(feat_int) > 0 && "internal_feature" %in% names(out)) {
      out <- out[out$internal_feature %in% feat_int, , drop = FALSE]
    }

    out
  }

  .subset_dist_profit <- function(df, actions = NULL) {
    if (is.null(df) || !inherits(df, "data.frame")) return(df)

    act_int <- .resolve_action_internal(actions)
    if (length(act_int) == 0) return(df)

    df[df$internal_action %in% act_int, , drop = FALSE]
  }

  .subset_dist_features <- function(df, features = NULL) {
    if (is.null(df) || !inherits(df, "data.frame")) return(df)

    feat_int <- .resolve_feature_internal(features)
    if (length(feat_int) == 0) return(df)

    df[df$internal_feature %in% feat_int, , drop = FALSE]
  }

  # Convención MO: motor en MIN; luego en R se invierte signo para objetivos max
  rcpp_reset_objective(op, "min")

  terms <- ir$terms %||% list()

  for (t in terms) {
    type <- t$type

    if (identical(type, "pu_cost")) {

      feats <- .chr(t$features)
      if (!is.null(feats)) {
        stop(
          "Subset by features is not currently supported for 'pu_cost'. ",
          "PU costs are attached to w variables, not directly to feature-specific contributions.",
          call. = FALSE
        )
      }

      rcpp_add_objective_min_cost(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = base_superset$data$dist_actions_model,
        include_pu_cost = TRUE,
        include_action_cost = FALSE,
        weight = 1.0
      )

    } else if (identical(type, "action_cost")) {

      da_sub <- .subset_dist_actions(actions = t$actions)

      rcpp_add_objective_min_cost(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = da_sub,
        include_pu_cost = FALSE,
        include_action_cost = TRUE,
        weight = 1.0
      )

    } else if (identical(type, "boundary_cut")) {

      feats <- .chr(t$features)
      if (!is.null(feats)) {
        stop(
          "Subset by features is not currently supported for 'boundary_cut'. ",
          "PU fragmentation is defined over selected units, not over feature-specific selections.",
          call. = FALSE
        )
      }

      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]

      if (is.null(rel_model)) {
        stop("Missing relation '", rel_name, "'.", call. = FALSE)
      }

      rcpp_prepare_objective_min_fragmentation(op, rel_model)

      rcpp_add_objective_min_fragmentation(
        op,
        relation_data = rel_model,
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1]
      )

    } else if (identical(type, "representation")) {

      acol <- as.character(t$amount_col %||% "amount")[1]
      feat_int <- .resolve_feature_internal(t$features)

      rcpp_add_objective_max_representation(
        op,
        dist_features_data = base_superset$data$dist_features,
        amount_col = acol,
        features_to_use = as.integer(feat_int),
        internal_feature_col = "internal_feature",
        weight = 1.0
      )

    } else if (identical(type, "benefit")) {

      bcol <- as.character(t$benefit_col %||% "benefit")[1]

      db_sub <- .subset_dist_effects(
        df = base_superset$data$dist_benefit_model,
        actions = t$actions,
        features = t$features
      )

      da_sub <- .subset_dist_actions(actions = t$actions)

      rcpp_add_objective_max_benefit(
        op,
        dist_actions_data = da_sub,
        dist_benefit_data = db_sub,
        benefit_col = bcol,
        weight = 1.0
      )

    } else if (identical(type, "profit")) {

      pcol <- as.character(t$profit_col %||% "profit")[1]

      dp_sub <- .subset_dist_profit(
        df = base_superset$data$dist_profit_model,
        actions = t$actions
      )

      da_sub <- .subset_dist_actions(actions = t$actions)

      rcpp_add_objective_max_profit(
        op,
        dist_actions_data = da_sub,
        dist_profit_data  = dp_sub,
        profit_col = pcol,
        weight = 1.0
      )

    } else if (identical(type, "net_profit")) {

      pcol    <- as.character(t$profit_col %||% "profit")[1]
      inc_pu  <- isTRUE(t$include_pu_cost %||% TRUE)
      inc_act <- isTRUE(t$include_action_cost %||% TRUE)

      feats <- .chr(t$features)
      if (!is.null(feats) && isTRUE(inc_pu)) {
        stop(
          "Subset by features is not currently supported for 'net_profit' when include_pu_cost=TRUE. ",
          "PU costs are attached to w variables and cannot yet be attributed to feature subsets.",
          call. = FALSE
        )
      }

      da_sub <- .subset_dist_actions(actions = t$actions)
      dp_sub <- .subset_dist_profit(
        df = base_superset$data$dist_profit_model,
        actions = t$actions
      )

      rcpp_add_objective_max_net_profit(
        op,
        pu_data = base_superset$data$pu,
        dist_actions_data = da_sub,
        dist_profit_data  = dp_sub,
        profit_col = pcol,
        include_pu_cost = inc_pu,
        include_action_cost = inc_act,
        weight = 1.0
      )

    } else if (identical(type, "action_boundary_cut")) {

      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]

      if (is.null(rel_model)) {
        stop("Missing relation '", rel_name, "'.", call. = FALSE)
      }

      act_int <- .resolve_action_internal(t$actions)

      # ensure the global y_action block exists and matches the current layout
      rcpp_prepare_objective_min_fragmentation_actions(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        relation_data = rel_model,
        actions_to_use = if (length(act_int) > 0) act_int else NULL,
        block_name = "fragmentation_actions",
        tag = "from=.pamo_objvec_from_ir"
      )

      # build the objective vector explicitly instead of relying on side effects
      term2 <- t
      term2$actions <- if (length(act_int) > 0) as.integer(act_int) else NULL

      v_term <- .pamo_objvec_action_boundary_cut(base_superset, term2)

      # write directly into the model objective buffer
      if (length(v_term) == 0L) {
        stop("Empty vector returned by .pamo_objvec_action_boundary_cut().", call. = FALSE)
      }

      if (!exists("rcpp_model_set_objective_vector", mode = "function")) {
        # fallback: accumulate in current objective by recreating the whole vector
        m0 <- .pa_model_from_ptr(
          op,
          args = base_superset$data$model_args %||% list(),
          drop_triplets = TRUE
        )
        obj0 <- as.numeric(m0$obj)
        if (length(obj0) != length(v_term)) {
          stop("Current model objective length does not match action_boundary_cut term vector.", call. = FALSE)
        }
        rcpp_model_set_objective_vector(
          x = op,
          obj = obj0 + v_term,
          model_sense = "min"
        )
      } else {
        m0 <- .pa_model_from_ptr(
          op,
          args = base_superset$data$model_args %||% list(),
          drop_triplets = TRUE
        )
        obj0 <- as.numeric(m0$obj)
        if (length(obj0) != length(v_term)) {
          stop("Current model objective length does not match action_boundary_cut term vector.", call. = FALSE)
        }
        rcpp_model_set_objective_vector(
          x = op,
          obj = obj0 + v_term,
          model_sense = "min"
        )
      }

    } else if (identical(type, "intervention_boundary_cut")) {

      acts <- .chr(t$actions)
      if (!is.null(acts)) {
        stop(
          "Subset by actions is not currently supported for 'intervention_boundary_cut' ",
          "with the current C++ signature.",
          call. = FALSE
        )
      }

      rel_name <- as.character(t$relation_name %||% "boundary")[1]
      rel_model <- base_superset$data$spatial_relations_model[[rel_name]] %||%
        base_superset$data$spatial_relations[[rel_name]]

      if (is.null(rel_model)) {
        stop("Missing relation '", rel_name, "'.", call. = FALSE)
      }

      rcpp_add_objective_min_fragmentation_interventions(
        op,
        dist_actions_data = base_superset$data$dist_actions_model,
        relation_data = rel_model,
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1],
        weight = 1.0
      )

    } else if (identical(type, "intervention_impact")) {

      icol <- as.character(t$impact_col %||% "amount")[1]

      df_sub <- .subset_dist_features(
        df = base_superset$data$dist_features,
        features = t$features
      )

      feat_int <- .resolve_feature_internal(t$features)
      act_int  <- .resolve_action_internal(t$actions)

      if (length(act_int) == 0) {
        stop(
          "intervention_impact requires a non-empty action subset.",
          call. = FALSE
        )
      }

      subset_key <- .pa_subset_to_string(as.character(t$actions))

      rcpp_add_objective_min_intervention_impact(
        op,
        pu_data = base_superset$data$pu,
        dist_features_data = df_sub,
        dist_actions_data = base_superset$data$dist_actions_model,
        subset_key = subset_key,
        impact_col = icol,
        features_to_use = as.integer(feat_int),
        actions_to_use = as.integer(act_int),
        internal_feature_col = "internal_feature",
        weight = 1.0,
        weight_multiplier = as.numeric(t$weight_multiplier %||% 1)[1],
        block_name = "objective_add_min_intervention_impact",
        tag = "from=.pamo_objvec_from_ir"
      )

    } else if (identical(type, "custom")) {
      stop("custom objectives are not supported in weighted yet.", call. = FALSE)

    } else {
      stop("Unknown term type in .pamo_objvec_from_ir(): ", type, call. = FALSE)
    }
  }

  m <- .pa_model_from_ptr(
    op,
    args = base_superset$data$model_args %||% list(),
    drop_triplets = TRUE
  )

  as.numeric(m$obj)
}



.pamo_apply_weighted_objective <- function(base, ir_list, weights, normalize = FALSE) {
  stopifnot(inherits(base, "Problem"))
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

  # 5) IMPORTANTÍSIMO: inyectar runtime update para que mosap lo use en solve()
  base2$data$runtime_updates <- list(
    obj = obj_w,
    modelsense = "min"
  )

  # 6) asegurar que mosap NO reconstruya el modelo y no te borre runtime_updates
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
# .pamo_solve_weighted <- function(x, ...) {
#   aliases <- x$method$params$aliases
#   w <- x$method$params$weights
#   normalize <- isTRUE(x$method$params$normalize)
#
#   if (is.null(w)) {
#     stop("Weighted method: missing weights. Provide them in set_method_weighted().", call. = FALSE)
#   }
#
#   # build a single run specification (later: grid of weights)
#   run_df <- data.frame(run_id = 1L, stringsAsFactors = FALSE)
#   for (i in seq_along(aliases)) {
#     run_df[[paste0("w_", aliases[i])]] <- w[i]
#   }
#
#   solutions <- vector("list", nrow(run_df))
#   obj_vals  <- matrix(NA_real_, nrow(run_df), length(aliases))
#   colnames(obj_vals) <- paste0("obj_", aliases)
#
#   status <- character(nrow(run_df))
#   runtime <- numeric(nrow(run_df))
#   gap <- numeric(nrow(run_df))
#
#   for (r in seq_len(nrow(run_df))) {
#     w_r <- as.numeric(run_df[r, paste0("w_", aliases), drop = TRUE])
#
#     one <- .pamo_solve_one(
#       x = x,
#       spec = list(
#         type = "weighted",
#         aliases = aliases,
#         weights = w_r,
#         normalize = normalize
#       )
#     )
#
#     solutions[[r]] <- one$solution
#     status[r] <- one$status
#     runtime[r] <- one$runtime
#     gap[r] <- one$gap
#
#     # TODO: objective evaluation (optional, later)
#     # For now, keep NA_real_ (doesn't break, keeps schema stable)
#     obj_vals[r, ] <- rep(NA_real_, length(aliases))
#   }
#
#   runs <- cbind(
#     run_df,
#     data.frame(status = status, runtime = runtime, gap = gap, stringsAsFactors = FALSE),
#     as.data.frame(obj_vals, stringsAsFactors = FALSE)
#   )
#
#   pproto(
#     NULL, MOProblem,
#     runs = runs,
#     solutions = solutions,
#     meta = list(
#       method = x$method,
#       call = match.call()
#     )
#   )
# }

# ---------------------------------------------------------
# Internal: solve a single run using mosap as engine
# ---------------------------------------------------------
.pamo_solve_one <- function(x, spec) {

  if (!inherits(x, "MOProblem")) stop(".pamo_solve_one expects MOProblem.", call. = FALSE)
  if (!inherits(x$base, "Problem")) stop("MOProblem$base must be a Problem.", call. = FALSE)

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

    primary     <- as.character(spec$primary)[1]
    eps_list    <- spec$eps %||% list()
    eps_tol     <- spec$eps_tol %||% list()
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
      if (!is.finite(eps_val)) {
        stop("epsilon_constraint: eps for '", a, "' must be finite.", call. = FALSE)
      }

      tol_a <- as.numeric(eps_tol[[a]] %||% 0)[1]
      if (!is.finite(tol_a) || tol_a < 0) {
        stop("epsilon_constraint: eps_tol for '", a, "' must be finite and >= 0.", call. = FALSE)
      }

      base <- .pamo_apply_epsilon_constraint(
        base = base,
        ir = ir_sec,
        eps = eps_val + tol_a,
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

  gap_limit  <- spec$gap_limit %||% NULL
  time_limit <- spec$time_limit %||% NULL

  out <- solve(
    base,
    gap_limit = gap_limit,
    time_limit = time_limit
  )
  .pamo_extract_solution(out)
}

.pamo_apply_epsilon_constraint <- function(base, ir, eps, sense = c("min","max"),
                                           name = "", block_name = "epsilon_constraint", tag = "") {
  stopifnot(inherits(base, "Problem"))
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
# Internal: extract (minimal) results from mosap Solution
# ---------------------------------------------------------
.pamo_extract_solution <- function(out) {
  # out is expected to be a mosap::Solution (pproto)
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



# -------------------------------------------------------------------------
# Internal: extract raw decision vector from a mosap Solution
# -------------------------------------------------------------------------
.pamo_get_solution_vector <- function(sol) {

  # accept Solution or list-like object
  d <- if (!is.null(sol$data)) sol$data else sol

  candidates <- c(
    "solution",
    "sol",
    "x",
    "best_solution",
    "solution_vector"
  )

  for (nm in candidates) {
    v <- d[[nm]] %||% NULL
    if (is.numeric(v) && length(v) > 0) {
      return(as.numeric(v))
    }
  }

  stop(
    "Could not extract the raw decision vector from the solution object.\n",
    "Expected one of: data$solution, data$sol, data$x, data$best_solution, data$solution_vector.",
    call. = FALSE
  )
}




.pamo_eval_boundary_cut_on_solution <- function(x, solution, term) {
  stopifnot(inherits(x, "MOProblem"))

  rel_name <- as.character(term$relation_name %||% "boundary")[1]
  rel <- x$base$data$spatial_relations[[rel_name]] %||% NULL

  if (is.null(rel) || !inherits(rel, "data.frame")) {
    stop("Missing spatial relation '", rel_name, "' while evaluating boundary_cut.", call. = FALSE)
  }

  for (nm in c("internal_pu1", "internal_pu2", "weight")) {
    if (!nm %in% names(rel)) {
      stop("Spatial relation '", rel_name, "' must contain column '", nm, "'.", call. = FALSE)
    }
  }

  sol_vec <- .pamo_get_solution_vector(solution)

  n_pu <- nrow(x$base$data$pu)
  if (!is.finite(n_pu) || is.na(n_pu) || n_pu <= 0L) {
    stop("Could not determine n_pu while evaluating boundary_cut.", call. = FALSE)
  }

  if (length(sol_vec) < n_pu) {
    stop(
      "Solution vector is shorter than n_pu while evaluating boundary_cut.\n",
      "length(sol_vec) = ", length(sol_vec), ", n_pu = ", n_pu, ".",
      call. = FALSE
    )
  }

  # Phase 1/2A assumption: w occupies the first n_pu decision variables
  w <- as.numeric(sol_vec[seq_len(n_pu)])
  w_bin <- as.numeric(w > 0.5)

  ip1 <- as.integer(rel$internal_pu1)
  ip2 <- as.integer(rel$internal_pu2)
  ww  <- as.numeric(rel$weight)

  ok <- is.finite(ip1) & is.finite(ip2) & is.finite(ww)
  ip1 <- ip1[ok]
  ip2 <- ip2[ok]
  ww  <- ww[ok]

  if (length(ip1) == 0L) return(0)

  # self weights (diagonal rows) and canonical off-diagonal edges
  self_w <- numeric(n_pu)
  edge_map <- new.env(parent = emptyenv(), hash = TRUE)

  make_key <- function(a, b) paste0(a, "::", b)

  for (r in seq_along(ip1)) {
    i <- ip1[r]
    j <- ip2[r]
    wij <- ww[r]

    if (i == j) {
      self_w[i] <- self_w[i] + wij
    } else {
      a <- min(i, j)
      b <- max(i, j)
      k <- make_key(a, b)
      old <- if (exists(k, envir = edge_map, inherits = FALSE)) {
        get(k, envir = edge_map, inherits = FALSE)
      } else {
        NA_real_
      }
      if (is.na(old) || wij > old) {
        assign(k, wij, envir = edge_map)
      }
    }
  }

  edge_keys <- ls(edge_map, all.names = TRUE)

  incident <- numeric(n_pu)
  edge_sum <- 0

  for (k in edge_keys) {
    parts <- strsplit(k, "::", fixed = TRUE)[[1]]
    a <- as.integer(parts[1])
    b <- as.integer(parts[2])
    wij <- get(k, envir = edge_map, inherits = FALSE)

    incident[a] <- incident[a] + wij
    incident[b] <- incident[b] + wij

    edge_sum <- edge_sum + wij * w_bin[a] * w_bin[b]
  }

  linear_sum <- sum((incident + self_w) * w_bin)
  val <- linear_sum - 2 * edge_sum

  mult <- as.numeric(term$weight_multiplier %||% 1)[1]
  if (!is.finite(mult)) mult <- 1

  as.numeric(mult * val)
}



# -------------------------------------------------------------------------
# Internal: evaluate one registered objective alias on a solved solution
# Uses the same IR/objective-vector machinery, so evaluation is consistent
# with the MO engine.
# -------------------------------------------------------------------------

.pamo_get_alias_value_from_solution <- function(x, solution, alias) {
  stopifnot(inherits(x, "MOProblem"))
  .pamo_eval_alias_on_solution(x, solution, alias)
}

.pamo_eval_alias_on_solution <- function(x, solution, alias) {
  stopifnot(inherits(x, "MOProblem"))

  alias <- as.character(alias)[1]
  if (is.na(alias) || !nzchar(alias)) {
    stop("alias must be a non-empty string.", call. = FALSE)
  }

  spec <- .pamo_get_objective_spec(x, alias)
  ir   <- .pamo_objective_to_ir(x$base, spec)

  terms <- ir$terms %||% list()

  # Fase 1:
  # si el objetivo es exactamente min_fragmentation (PU boundary cut),
  # lo evaluamos directamente desde w sin reconstruir auxiliares y_pu.
  if (length(terms) == 1L && identical(terms[[1]]$type %||% "", "boundary_cut")) {
    val <- .pamo_eval_boundary_cut_on_solution(x, solution, terms[[1]])
    return(as.numeric(val))
  }

  if (length(terms) == 1L && identical(terms[[1]]$type %||% "", "intervention_impact")) {
    val <- .pamo_eval_intervention_impact_on_solution(x, solution, terms[[1]])
    return(as.numeric(val))
  }

  # if (length(terms) == 1L && identical(terms[[1]]$type %||% "", "action_boundary_cut")) {
  #   val <- .pamo_eval_action_boundary_cut_on_solution(x, solution, terms[[1]])
  #   return(as.numeric(val))
  # }

  # resto de objetivos: evaluación por objvec
  base_eval <- .pamo_prepare_superset_model(x$base, list(ir))

  obj_vec <- .pamo_objvec_from_ir(base_eval, ir)
  sol_vec <- .pamo_get_solution_vector(solution)

  n_obj <- length(obj_vec)
  n_sol <- length(sol_vec)

  if (n_obj > n_sol) {
    stop(
      "Objective vector length does not match solution vector length when evaluating alias '", alias, "'.\n",
      "length(obj_vec) = ", n_obj, ", length(sol_vec) = ", n_sol, ".",
      call. = FALSE
    )
  }

  # permitir evaluar objetivos 'base' sobre soluciones de un superset
  sol_use <- as.numeric(sol_vec[seq_len(n_obj)])

  val_engine <- sum(as.numeric(obj_vec) * sol_use, na.rm = TRUE)

  as.numeric(val_engine)
}

.pamo_add_alias_upper_bound_constraint <- function(base_eval, x, alias, rhs, tol = 0, name = NULL) {
  stopifnot(inherits(base_eval, "Problem"))
  stopifnot(inherits(x, "MOProblem"))

  alias <- as.character(alias)[1]
  rhs   <- as.numeric(rhs)[1]
  tol   <- as.numeric(tol)[1]

  if (!is.finite(rhs)) {
    stop("rhs must be finite in .pamo_add_alias_upper_bound_constraint().", call. = FALSE)
  }
  if (!is.finite(tol) || tol < 0) {
    stop("tol must be a finite non-negative number.", call. = FALSE)
  }

  spec <- .pamo_get_objective_spec(x, alias)
  ir   <- .pamo_objective_to_ir(x$base, spec)

  obj_vec <- .pamo_objvec_from_ir(base_eval, ir)

  op_list <- .pa_model_from_ptr(
    base_eval$data$model_ptr,
    args = base_eval$data$model_args %||% list(),
    drop_triplets = TRUE
  )

  n_model <- as.integer(op_list$ncol %||% length(obj_vec))
  if (length(obj_vec) != n_model) {
    stop(
      "Cannot add alias bound for '", alias, "': objective vector length does not match model dimension.\n",
      "length(obj_vec) = ", length(obj_vec), ", model dimension = ", n_model, ".",
      call. = FALSE
    )
  }

  nz <- which(abs(obj_vec) > 0)
  if (length(nz) == 0L) {
    stop("Alias '", alias, "' produced an empty objective vector.", call. = FALSE)
  }

  if (is.null(name)) {
    name <- paste0("eps_bound_", alias)
  }

  base_eval <- .pa_add_linear_constraint(
    base_eval,
    var_index_0based = as.integer(nz - 1L),
    coeff = as.numeric(obj_vec[nz]),
    sense = "<=",
    rhs = as.numeric(rhs + tol),
    name = name
  )

  base_eval
}


.pamo_solve_lexicographic_extreme_2obj <- function(x, first, second, gap_limit = NULL, time_limit = NULL) {
  stopifnot(inherits(x, "MOProblem"))

  method <- x$method %||% list()
  tol <- as.numeric(method$lexicographic_tol %||% 0)[1]
  if (!is.finite(tol) || tol < 0) tol <- 0

  ir_first  <- .pamo_objective_to_ir(x$base, .pamo_get_objective_spec(x, first))
  ir_second <- .pamo_objective_to_ir(x$base, .pamo_get_objective_spec(x, second))

  # 1) resolver el primer objetivo
  sol_first <- .pamo_solve_one(
    x = x,
    primary_alias = first,
    constrained_aliases = character(0),
    eps_named = NULL,
    gap_limit = gap_limit,
    time_limit = time_limit
  )

  first_star <- .pamo_get_alias_value_from_solution(x, sol_first$solution, first)

  # 2) construir un superset para ambos objetivos
  base_eval <- .pamo_prepare_superset_model(x$base, list(ir_first, ir_second))

  # 3) restringir el primer objetivo en su óptimo
  base_eval <- .pamo_add_alias_upper_bound_constraint(
    base_eval = base_eval,
    x = x,
    alias = first,
    rhs = first_star,
    tol = tol,
    name = paste0("lexi_fix_", first)
  )

  # 4) reconfigurar el objetivo activo del modelo para optimizar el segundo
  #    reutilizamos la lógica existente: creamos un problema temporal con segundo como primary
  x_tmp <- x
  x_tmp$method <- list(
    name = "epsilon_constraint",
    mode = "manual",
    primary = second,
    aliases = c(first, second),
    constrained = character(0),
    eps = NULL,
    runs = NULL
  )

  # Resolver sobre el modelo modificado
  sol_second <- .pamo_solve_on_prebuilt_model(
    x = x_tmp,
    base_eval = base_eval,
    primary_alias = second,
    gap_limit = gap_limit,
    time_limit = time_limit
  )

  list(
    first_alias = first,
    second_alias = second,
    first_star = as.numeric(first_star),
    solution = sol_second$solution,
    first_value = .pamo_get_alias_value_from_solution(x, sol_second$solution, first),
    second_value = .pamo_get_alias_value_from_solution(x, sol_second$solution, second),
    stage1 = sol_first,
    stage2 = sol_second
  )
}

.pamo_solve_on_prebuilt_model <- function(x, base_eval, primary_alias, gap_limit = NULL, time_limit = NULL) {
  stopifnot(inherits(x, "MOProblem"))
  stopifnot(inherits(base_eval, "Problem"))

  spec <- .pamo_get_objective_spec(x, primary_alias)
  ir   <- .pamo_objective_to_ir(x$base, spec)

  # reset objetivo del modelo preconstruido
  base_eval <- .pamo_set_ir_as_active_objective(base_eval, ir)

  ans <- solve(
    base_eval,
    gap_limit = gap_limit,
    time_limit = time_limit
  )

  list(
    solution = ans,
    primary = primary_alias
  )
}

.pamo_set_ir_as_active_objective <- function(base_eval, ir) {
  stopifnot(inherits(base_eval, "Problem"))

  obj_vec <- .pamo_objvec_from_ir(base_eval, ir)

  op <- base_eval$data$model_ptr
  if (is.null(op)) {
    stop("model_ptr is NULL in .pamo_set_ir_as_active_objective().", call. = FALSE)
  }

  # limpia objetivo actual y escribe el nuevo
  if (!exists("rcpp_model_set_objective_vector", mode = "function")) {
    stop(
      "Missing C++ helper rcpp_model_set_objective_vector(). ",
      "You need a low-level setter to overwrite the active objective.",
      call. = FALSE
    )
  }

  rcpp_model_set_objective_vector(
    op = op,
    obj = as.numeric(obj_vec),
    model_sense = "min"
  )

  base_eval
}
# -------------------------------------------------------------------------
# Internal: compute 2-objective extreme points for auto epsilon mode
#
# Returns:
# - primary_at_primary
# - secondary_at_primary
# - primary_at_secondary
# - secondary_at_secondary
# - secondary_min
# - secondary_max
# -------------------------------------------------------------------------
.pamo_compute_epsilon_extremes_2obj <- function(x, primary, secondary, gap_limit = NULL, time_limit = NULL) {
  stopifnot(inherits(x, "MOProblem"))

  method <- x$method %||% list()
  do_lexi <- isTRUE(method$lexicographic)
  lexi_tol <- as.numeric(method$lexicographic_tol %||% 0)[1]
  if (!is.finite(lexi_tol) || lexi_tol < 0) lexi_tol <- 0

  specs_all <- .pamo_get_specs(x)
  obj_alias <- names(specs_all)

  if (!primary %in% obj_alias) {
    stop("primary alias not found: '", primary, "'.", call. = FALSE)
  }
  if (!secondary %in% obj_alias) {
    stop("secondary alias not found: '", secondary, "'.", call. = FALSE)
  }

  # ------------------------------------------------------------
  # Caso simple (no lexicográfico)
  # ------------------------------------------------------------
  if (!isTRUE(do_lexi)) {

    # Extreme 1: optimize primary
    sol_primary <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "weighted",
        aliases = primary,
        weights = 1,
        normalize = FALSE,
        gap_limit = gap_limit,
        time_limit = time_limit
      )
    )

    primary_at_primary <- .pamo_eval_alias_on_solution(x, sol_primary$solution, primary)
    secondary_at_primary <- .pamo_eval_alias_on_solution(x, sol_primary$solution, secondary)

    # Extreme 2: optimize secondary
    sol_secondary <- .pamo_solve_one(
      x = x,
      spec = list(
        type = "weighted",
        aliases = secondary,
        weights = 1,
        normalize = FALSE,
        gap_limit = gap_limit,
        time_limit = time_limit
      )
    )

    primary_at_secondary <- .pamo_eval_alias_on_solution(x, sol_secondary$solution, primary)
    secondary_at_secondary <- .pamo_eval_alias_on_solution(x, sol_secondary$solution, secondary)

    vals_secondary <- c(secondary_at_primary, secondary_at_secondary)

    if (any(!is.finite(vals_secondary))) {
      stop(
        "Could not compute finite extreme values for the secondary objective.\n",
        "secondary_at_primary = ", secondary_at_primary, "\n",
        "secondary_at_secondary = ", secondary_at_secondary,
        call. = FALSE
      )
    }

    return(list(
      primary = primary,
      secondary = secondary,

      primary_at_primary = as.numeric(primary_at_primary),
      secondary_at_primary = as.numeric(secondary_at_primary),

      primary_at_secondary = as.numeric(primary_at_secondary),
      secondary_at_secondary = as.numeric(secondary_at_secondary),

      secondary_min = min(vals_secondary),
      secondary_max = max(vals_secondary),

      solution_primary = sol_primary$solution,
      solution_secondary = sol_secondary$solution
    ))
  }

  # ------------------------------------------------------------
  # Caso lexicográfico
  # ------------------------------------------------------------

  # 1) resolver primary puro
  sol_primary_stage1 <- .pamo_solve_one(
    x = x,
    spec = list(
      type = "weighted",
      aliases = primary,
      weights = 1,
      normalize = FALSE,
      gap_limit = gap_limit,
      time_limit = time_limit
    )
  )

  primary_star <- .pamo_eval_alias_on_solution(x, sol_primary_stage1$solution, primary)

  # 2) resolver secondary sujeto a primary <= primary_star + tol
  sol_primary_lexi <- .pamo_solve_one(
    x = x,
    spec = list(
      type = "epsilon_constraint",
      primary = secondary,
      eps = stats::setNames(list(primary_star), primary),
      gap_limit = gap_limit,
      time_limit = time_limit,
      eps_tol = stats::setNames(list(lexi_tol), primary)
    )
  )

  primary_at_primary <- .pamo_eval_alias_on_solution(x, sol_primary_lexi$solution, primary)
  secondary_at_primary <- .pamo_eval_alias_on_solution(x, sol_primary_lexi$solution, secondary)

  # 3) resolver secondary puro
  sol_secondary_stage1 <- .pamo_solve_one(
    x = x,
    spec = list(
      type = "weighted",
      aliases = secondary,
      weights = 1,
      normalize = FALSE,
      gap_limit = gap_limit,
      time_limit = time_limit
    )
  )

  secondary_star <- .pamo_eval_alias_on_solution(x, sol_secondary_stage1$solution, secondary)

  # 4) resolver primary sujeto a secondary <= secondary_star + tol
  sol_secondary_lexi <- .pamo_solve_one(
    x = x,
    spec = list(
      type = "epsilon_constraint",
      primary = primary,
      eps = stats::setNames(list(secondary_star), secondary),
      gap_limit = gap_limit,
      time_limit = time_limit,
      eps_tol = stats::setNames(list(lexi_tol), secondary)
    )
  )

  primary_at_secondary <- .pamo_eval_alias_on_solution(x, sol_secondary_lexi$solution, primary)
  secondary_at_secondary <- .pamo_eval_alias_on_solution(x, sol_secondary_lexi$solution, secondary)

  vals_secondary <- c(secondary_at_primary, secondary_at_secondary)

  if (any(!is.finite(vals_secondary))) {
    stop(
      "Could not compute finite extreme values for the secondary objective.\n",
      "secondary_at_primary = ", secondary_at_primary, "\n",
      "secondary_at_secondary = ", secondary_at_secondary,
      call. = FALSE
    )
  }

  list(
    primary = primary,
    secondary = secondary,

    primary_at_primary = as.numeric(primary_at_primary),
    secondary_at_primary = as.numeric(secondary_at_primary),

    primary_at_secondary = as.numeric(primary_at_secondary),
    secondary_at_secondary = as.numeric(secondary_at_secondary),

    secondary_min = min(vals_secondary),
    secondary_max = max(vals_secondary),
    vals_secondary = vals_secondary,

    solution_primary = sol_primary_lexi$solution,
    solution_secondary = sol_secondary_lexi$solution,

    meta = list(
      lexicographic = TRUE,
      lexicographic_tol = lexi_tol,
      stage1_primary_solution = sol_primary_stage1$solution,
      stage1_secondary_solution = sol_secondary_stage1$solution
    )
  )
}


.pamo_compile_superset_spec <- function(base, ir_list) {
  stopifnot(inherits(base, "Problem"))
  stopifnot(is.list(ir_list), length(ir_list) > 0)

  all_terms <- unlist(
    lapply(ir_list, function(ir) ir$terms %||% list()),
    recursive = FALSE
  )

  all_types <- unique(vapply(all_terms, `[[`, character(1), "type"))

  need_z     <- "representation" %in% all_types
  need_y_pu  <- "boundary_cut" %in% all_types
  need_y_act <- "action_boundary_cut" %in% all_types
  need_y_int <- "intervention_boundary_cut" %in% all_types
  need_u_int <- "intervention_impact" %in% all_types

  if (isTRUE(need_y_int)) {
    stop(
      "Phase 1 superset does not yet support 'intervention_boundary_cut' objectives.\n",
      "Please remove intervention fragmentation from the MO method for now.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Spatial fragmentation terms: at most one relation_name in phase 1
  # ------------------------------------------------------------
  spatial_terms <- Filter(
    function(t) t$type %in% c("boundary_cut", "action_boundary_cut", "intervention_boundary_cut"),
    all_terms
  )

  rel_names <- unique(vapply(
    spatial_terms,
    function(t) as.character(t$relation_name %||% "boundary")[1],
    character(1)
  ))

  rel_names <- rel_names[!is.na(rel_names) & nzchar(rel_names)]

  if (length(rel_names) > 1L) {
    stop(
      "Phase 1 superset supports only one relation_name across fragmentation objectives.\n",
      "Found: ", paste(rel_names, collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # intervention_impact: phase 1 supported ONLY if all terms
  # share the same action subset
  # ------------------------------------------------------------
  u_int_actions <- NULL

  if (isTRUE(need_u_int)) {
    int_terms <- Filter(function(t) identical(t$type, "intervention_impact"), all_terms)

    norm_actions <- function(x) {
      x <- as.character(x %||% character(0))
      x <- unique(x[!is.na(x) & nzchar(x)])
      sort(x)
    }

    action_keys <- vapply(
      int_terms,
      function(t) paste(norm_actions(t$actions), collapse = "||"),
      character(1)
    )

    uniq_keys <- unique(action_keys)

    if (length(uniq_keys) != 1L) {
      pretty <- vapply(
        int_terms,
        function(t) {
          aa <- norm_actions(t$actions)
          if (length(aa) == 0L) "<empty>" else paste(aa, collapse = ", ")
        },
        character(1)
      )

      stop(
        "Phase 1 superset supports 'intervention_impact' only when all such objectives ",
        "share the same action subset.\n",
        "Found subsets: ", paste(unique(pretty), collapse = " | "),
        call. = FALSE
      )
    }

    u_int_actions <- norm_actions(int_terms[[1]]$actions)

    if (length(u_int_actions) == 0L) {
      stop(
        "Phase 1 superset requires a non-empty action subset for 'intervention_impact'.",
        call. = FALSE
      )
    }
  }

  list(
    needs = list(
      z = isTRUE(need_z),
      y_pu = isTRUE(need_y_pu),
      y_action = isTRUE(need_y_act),
      y_intervention = FALSE,
      u_intervention = isTRUE(need_u_int),
      u_intervention_actions = u_int_actions
    ),
    relation_name = if (length(rel_names) == 1L) rel_names[1] else NULL
  )
}


.pamo_apply_single_objective <- function(base, ir, sense = c("min", "max")) {
  stopifnot(inherits(base, "Problem"))
  sense <- match.arg(sense)

  if (is.null(base$data$model_ptr)) {
    stop("Model not built (model_ptr is NULL).", call. = FALSE)
  }

  v <- .pamo_objvec_from_ir(base, ir)

  # El motor trabaja siempre en minimización
  if (identical(sense, "max")) {
    v <- -as.numeric(v)
  } else {
    v <- as.numeric(v)
  }

  # Si existe un setter C++ del objetivo, úsalo
  if (exists("rcpp_model_set_objective_vector", mode = "function")) {
    rcpp_model_set_objective_vector(
      x = base$data$model_ptr,
      obj = v,
      model_sense = "min"
    )
  } else {
    # fallback temporal: deja la actualización pendiente
    base$data$runtime_updates <- list(
      obj = v,
      modelsense = "min"
    )
  }

  base$data$meta <- base$data$meta %||% list()
  base$data$meta$model_dirty <- FALSE
  base$data$has_model <- TRUE

  base
}


.pamo_eval_action_boundary_cut_on_solution <- function(x, solution, term) {
  stopifnot(inherits(x, "MOProblem"))

  rel_name <- as.character(term$relation_name %||% "boundary")[1]
  sol_data <- solution$data %||% list()

  rel <- sol_data$spatial_relations_model[[rel_name]] %||%
    sol_data$spatial_relations[[rel_name]] %||%
    x$base$data$spatial_relations_model[[rel_name]] %||%
    x$base$data$spatial_relations[[rel_name]] %||%
    NULL

  if (is.null(rel) || !inherits(rel, "data.frame")) {
    stop("Missing spatial relation '", rel_name, "' while evaluating action_boundary_cut.", call. = FALSE)
  }

  for (nm in c("internal_pu1", "internal_pu2", "weight")) {
    if (!nm %in% names(rel)) {
      stop("Spatial relation '", rel_name, "' must contain column '", nm, "'.", call. = FALSE)
    }
  }

  da <- sol_data$dist_actions_model %||%
    sol_data$dist_actions %||%
    x$base$data$dist_actions_model %||%
    x$base$data$dist_actions %||%
    NULL

  if (is.null(da) || !inherits(da, "data.frame")) {
    stop("Missing dist_actions data while evaluating action_boundary_cut.", call. = FALSE)
  }

  for (nm in c("internal_pu", "internal_action")) {
    if (!nm %in% names(da)) {
      stop("dist_actions data must contain column '", nm, "'.", call. = FALSE)
    }
  }

  sol_vec <- .pamo_get_solution_vector(solution)

  ml <- sol_data$model_list %||% x$base$data$model_list %||% NULL

  n_pu <- as.integer(ml$n_pu %||% nrow(x$base$data$pu))
  if (!is.finite(n_pu) || is.na(n_pu) || n_pu <= 0L) {
    stop("Could not determine n_pu while evaluating action_boundary_cut.", call. = FALSE)
  }

  n_x <- as.integer(ml$n_x %||% nrow(da))
  if (!is.finite(n_x) || is.na(n_x) || n_x <= 0L) {
    stop("Could not determine n_x while evaluating action_boundary_cut.", call. = FALSE)
  }

  # IMPORTANT:
  # fallback must follow the structural layout w | x | ...
  x0 <- as.integer(ml$x_offset %||% n_pu)
  if (!is.finite(x0) || is.na(x0) || x0 < 0L) {
    stop("Could not determine x_offset while evaluating action_boundary_cut.", call. = FALSE)
  }

  if (!"internal_row" %in% names(da)) {
    if (nrow(da) != n_x) {
      stop(
        "dist_actions data does not contain 'internal_row', and nrow(dist_actions) != n_x.\n",
        "nrow(dist_actions) = ", nrow(da), ", n_x = ", n_x, ".",
        call. = FALSE
      )
    }
    da$internal_row <- seq_len(nrow(da))
  }

  if (any(!is.finite(da$internal_row)) || any(is.na(da$internal_row))) {
    stop("dist_actions$internal_row contains NA/non-finite values.", call. = FALSE)
  }
  if (any(da$internal_row < 1L) || any(da$internal_row > n_x)) {
    stop("dist_actions$internal_row is out of valid range 1..n_x.", call. = FALSE)
  }

  if (length(sol_vec) < (x0 + n_x)) {
    stop(
      "Solution vector is too short for action boundary evaluation.\n",
      "length(sol_vec) = ", length(sol_vec),
      ", required >= ", x0 + n_x, ".",
      call. = FALSE
    )
  }

  all_actions <- sort(unique(as.integer(da$internal_action)))
  all_actions <- all_actions[is.finite(all_actions) & !is.na(all_actions)]

  act_raw <- term$actions %||% NULL
  if (is.null(act_raw) || length(act_raw) == 0L) {
    act_int <- all_actions
  } else {
    act_int <- as.integer(act_raw)
    act_int <- sort(unique(act_int))
  }

  if (length(act_int) == 0L) return(0)

  aw <- term$action_weights %||% NULL
  if (is.null(aw)) {
    aw_map <- stats::setNames(rep(1, length(act_int)), act_int)
  } else {
    aw <- as.numeric(aw)
    if (length(aw) == length(all_actions)) {
      tmp_map <- stats::setNames(aw, all_actions)
      aw_map <- tmp_map[as.character(act_int)]
    } else if (length(aw) == length(act_int)) {
      aw_map <- stats::setNames(aw, act_int)
    } else {
      stop(
        "action_weights length must match either all actions or the selected action subset.",
        call. = FALSE
      )
    }
  }

  da_sub <- da[, c("internal_row", "internal_pu", "internal_action"), drop = FALSE]
  da_sub <- da_sub[da_sub$internal_action %in% act_int, , drop = FALSE]
  if (nrow(da_sub) == 0L) return(0)

  x_vals <- sol_vec[x0 + da_sub$internal_row]
  x_bin  <- as.numeric(x_vals > 0.5)

  ip1 <- as.integer(rel$internal_pu1)
  ip2 <- as.integer(rel$internal_pu2)
  ww  <- as.numeric(rel$weight)

  ok <- is.finite(ip1) & is.finite(ip2) & is.finite(ww)
  ip1 <- ip1[ok]
  ip2 <- ip2[ok]
  ww  <- ww[ok]

  self_w <- numeric(n_pu)
  edge_map <- new.env(parent = emptyenv(), hash = TRUE)

  make_key <- function(a, b) paste0(a, "::", b)

  for (r in seq_along(ip1)) {
    i <- ip1[r]
    j <- ip2[r]
    wij <- ww[r]

    if (i == j) {
      self_w[i] <- self_w[i] + wij
    } else {
      a <- min(i, j)
      b <- max(i, j)
      k <- make_key(a, b)
      old <- if (exists(k, envir = edge_map, inherits = FALSE)) {
        get(k, envir = edge_map, inherits = FALSE)
      } else {
        NA_real_
      }
      if (is.na(old) || wij > old) {
        assign(k, wij, envir = edge_map)
      }
    }
  }

  edge_keys <- ls(edge_map, all.names = TRUE)
  if (length(edge_keys) == 0L) return(0)

  edge_df <- data.frame(
    a = integer(length(edge_keys)),
    b = integer(length(edge_keys)),
    weight = numeric(length(edge_keys))
  )

  for (k in seq_along(edge_keys)) {
    parts <- strsplit(edge_keys[k], "::", fixed = TRUE)[[1]]
    edge_df$a[k] <- as.integer(parts[1])
    edge_df$b[k] <- as.integer(parts[2])
    edge_df$weight[k] <- get(edge_keys[k], envir = edge_map, inherits = FALSE)
  }

  edge_df <- edge_df[order(edge_df$a, edge_df$b), , drop = FALSE]

  incident_w <- numeric(n_pu)
  for (r in seq_len(nrow(edge_df))) {
    incident_w[edge_df$a[r]] <- incident_w[edge_df$a[r]] + edge_df$weight[r]
    incident_w[edge_df$b[r]] <- incident_w[edge_df$b[r]] + edge_df$weight[r]
  }

  val <- 0

  for (act in act_int) {
    awi <- as.numeric(aw_map[[as.character(act)]])
    if (!is.finite(awi) || awi == 0) next

    x_i <- numeric(n_pu)
    idx_act <- da_sub$internal_action == act
    if (any(idx_act)) {
      pu_act <- da_sub$internal_pu[idx_act]
      xv_act <- x_bin[idx_act]
      x_i[pu_act] <- xv_act
    }

    linear_sum <- sum((incident_w + self_w) * x_i)

    edge_sum <- 0
    for (r in seq_len(nrow(edge_df))) {
      edge_sum <- edge_sum + edge_df$weight[r] * x_i[edge_df$a[r]] * x_i[edge_df$b[r]]
    }

    val <- val + awi * (linear_sum - 2 * edge_sum)
  }

  mult <- as.numeric(term$weight_multiplier %||% 1)[1]
  if (!is.finite(mult)) mult <- 1

  as.numeric(mult * val)
}


.pamo_eval_intervention_impact_on_solution <- function(x, solution, term) {
  stopifnot(inherits(x, "MOProblem"))

  icol <- as.character(term$impact_col %||% "amount")[1]
  feats <- as.character(term$features %||% character(0))
  acts  <- as.character(term$actions %||% character(0))

  distf <- x$base$data$dist_features %||% NULL
  if (is.null(distf) || !inherits(distf, "data.frame") || nrow(distf) == 0) {
    stop("dist_features is missing or empty while evaluating intervention_impact.", call. = FALSE)
  }

  if (!(icol %in% names(distf))) {
    stop("impact column '", icol, "' not found in dist_features.", call. = FALSE)
  }

  # filtrar features si corresponde
  if (length(feats) > 0) {
    feat_subset <- .pa_resolve_feature_subset(x$base, features = feats)
    feat_ids <- unique(as.character(feat_subset$id))
    distf <- distf[as.character(distf$feature) %in% feat_ids, , drop = FALSE]
  }

  if (nrow(distf) == 0) return(0)

  # resolver subset de acciones
  act_subset <- .pa_resolve_action_subset(x$base, subset = acts)
  if (!inherits(act_subset, "data.frame") || nrow(act_subset) == 0) {
    stop("intervention_impact resolved to zero actions.", call. = FALSE)
  }

  act_ids <- unique(as.character(act_subset$id))

  # tabla de acciones de la solución
  act_tbl <- solution$data$tables$actions %||% NULL
  if (is.null(act_tbl) || !inherits(act_tbl, "data.frame")) {
    stop("No actions table found in solution while evaluating intervention_impact.", call. = FALSE)
  }

  if (!("selected" %in% names(act_tbl))) {
    stop("Solution actions table has no 'selected' column.", call. = FALSE)
  }

  # columnas esperadas: pu, action
  pu_col <- if ("pu" %in% names(act_tbl)) "pu" else if ("internal_pu" %in% names(act_tbl)) "internal_pu" else NULL
  ac_col <- if ("action" %in% names(act_tbl)) "action" else if ("id" %in% names(act_tbl)) "id" else NULL

  if (is.null(pu_col) || is.null(ac_col)) {
    stop(
      "Could not identify PU/action columns in solution$data$tables$actions while evaluating intervention_impact.",
      call. = FALSE
    )
  }

  act_sel <- act_tbl[
    act_tbl$selected > 0.5 & as.character(act_tbl[[ac_col]]) %in% act_ids,
    ,
    drop = FALSE
  ]

  if (nrow(act_sel) == 0) return(0)

  pu_hit <- unique(act_sel[[pu_col]])

  # sumar impacto por PU intervenida
  val <- sum(
    distf[distf$pu %in% pu_hit, icol, drop = TRUE],
    na.rm = TRUE
  )

  as.numeric(val)
}
