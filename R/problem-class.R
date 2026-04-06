#' @include internal.R
#'
#' @export
if (!methods::isClass("Problem")) methods::setOldClass("Problem")
NULL

#' @name problem-class
#' @aliases Problem
#' @title Problem class
#'
#' @description
#' The \code{Problem} class is the central container used by \pkg{multiscape} to
#' represent a planning problem before, during, and after model construction.
#'
#' A \code{Problem} object stores the full problem specification in a modular
#' way. This includes the baseline planning data, optional spatial metadata,
#' action definitions, effects, profit, targets, constraints, objective
#' registrations, solver settings, and, when available, a built optimization
#' model or model snapshot.
#'
#' In other words, \code{Problem} is the persistent object that connects the full
#' \code{multiscape} workflow:
#' \preformatted{
#' create_problem()
#' -> add_*() / set_*()
#' -> solve()
#' }
#'
#' @details
#' \strong{Conceptual role}
#'
#' The \code{Problem} class is designed for a data-first and modular workflow.
#' User-facing functions do not usually modify a solver object directly. Instead,
#' they enrich the \code{Problem} object by storing new specifications in its
#' internal \code{data} field.
#'
#' Thus, a \code{Problem} object should be understood as a structured container
#' for the mathematical planning problem, not necessarily as a built
#' optimization model.
#'
#' Before \code{\link{solve}} is called, the object may contain only input data
#' and user specifications. During or after solving, it may additionally contain
#' a built model pointer, model metadata, and solver-related information.
#'
#' \strong{Core mathematical interpretation}
#'
#' At a high level, the \code{Problem} object stores the ingredients required to
#' define an optimization problem over:
#' \itemize{
#'   \item planning units \eqn{i \in \mathcal{P}},
#'   \item features \eqn{f \in \mathcal{F}},
#'   \item actions \eqn{a \in \mathcal{A}},
#'   \item optional spatial relations over planning units,
#'   \item and user-defined objectives and constraints.
#' }
#'
#' The baseline ecological state is typically stored through a planning
#' unit--feature table of amounts:
#' \deqn{
#' a_{if} \ge 0,
#' }
#' where \eqn{a_{if}} is the baseline amount of feature \eqn{f} in planning unit
#' \eqn{i}.
#'
#' Subsequent functions then add action feasibility, effects, profit, targets,
#' spatial relations, and optimization settings to this baseline representation.
#'
#' \strong{How objects are created}
#'
#' \code{Problem} objects are usually created by \code{\link{create_problem}}.
#'
#' After creation, downstream functions such as \code{\link{add_actions}},
#' \code{\link{add_effects}}, \code{\link{add_profit}},
#' \code{\link{add_constraint_targets_absolute}}, \code{\link{add_constraint_targets_relative}},
#' spatial relation constructors, objective setters, and solver setters extend
#' the internal \code{data} list.
#'
#' \strong{Internal storage}
#'
#' The class contains a single field:
#' \describe{
#'   \item{\code{data}}{A named \code{list} storing the full problem
#'   specification, metadata, and, when available, built-model information.}
#' }
#'
#' Common entries of \code{data} include:
#' \describe{
#'   \item{\code{pu}}{Planning-unit table.}
#'   \item{\code{features}}{Feature table.}
#'   \item{\code{actions}}{Action catalog.}
#'   \item{\code{dist_features}}{Planning unit--feature baseline amounts.}
#'   \item{\code{dist_actions}}{Feasible planning unit--action pairs.}
#'   \item{\code{dist_effects}}{Action effects by planning unit, action, and
#'   feature.}
#'   \item{\code{dist_profit}}{Profit by planning unit--action pair.}
#'   \item{\code{pu_sf}}{Planning-unit geometry when available.}
#'   \item{\code{pu_coords}}{Planning-unit coordinates when available.}
#'   \item{\code{spatial_relations}}{Registered spatial relations.}
#'   \item{\code{targets}}{Stored target specifications.}
#'   \item{\code{constraints}}{Stored user-defined constraints.}
#'   \item{\code{objectives}}{Registered atomic objectives for single- or
#'   multi-objective workflows.}
#'   \item{\code{method}}{Stored multi-objective method configuration, when
#'   applicable.}
#'   \item{\code{solve_args}}{Stored solver settings.}
#'   \item{\code{model_ptr}}{Pointer to a built optimization model, when
#'   available.}
#'   \item{\code{model_args}}{Metadata describing model construction.}
#'   \item{\code{model_list}}{Optional exported model snapshot or representation.}
#'   \item{\code{meta}}{Auxiliary metadata, including model-dirty flags and other
#'   bookkeeping fields.}
#' }
#'
#' Not every \code{Problem} object contains all of these entries. The content of
#' \code{data} depends on how far the workflow has progressed.
#'
#' \strong{Lifecycle}
#'
#' A \code{Problem} object typically moves through the following stages:
#' \enumerate{
#'   \item input stage: baseline planning units, features, and feature
#'   distributions are stored,
#'   \item specification stage: actions, effects, targets, objectives,
#'   constraints, and spatial relations are added,
#'   \item model stage: an optimization model is built from the stored
#'   specification,
#'   \item solve stage: the model is solved and results are returned in a
#'   separate \code{Solution} or \code{SolutionSet} object.
#' }
#'
#' The \code{Problem} object itself is not the solution. It is the structured
#' problem definition from which a solution can be obtained.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{print()}}{Print a structured summary of the stored problem
#'   specification, including data, actions and effects, spatial inputs, targets
#'   and constraints, and model status. If a model has already been built,
#'   additional dimensions and auxiliary-variable information are displayed.}
#'
#'   \item{\code{show()}}{Alias of \code{print()}.}
#'
#'   \item{\code{repr()}}{Return a short one-line representation of the object.}
#'
#'   \item{\code{getData(name)}}{Return a named entry from \code{self$data}.}
#'
#'   \item{\code{getPlanningUnitsAmount()}}{Return the number of planning units
#'   stored in \code{x$data$pu}.}
#'
#'   \item{\code{getMonitoringCosts()}}{Return the planning-unit cost vector,
#'   typically taken from \code{x$data$pu$cost}.}
#'
#'   \item{\code{getFeatureAmount()}}{Return the number of stored features.}
#'
#'   \item{\code{getFeatureNames()}}{Return feature names from
#'   \code{x$data$features$name}, or feature ids if names are unavailable.}
#'
#'   \item{\code{getActionCosts()}}{Return action-level costs from
#'   \code{x$data$dist_actions$cost} when available.}
#'
#'   \item{\code{getActionsAmount()}}{Return the number of stored actions.}
#' }
#'
#' @section Printing and diagnostics:
#' The \code{print()} method is intended as a quick diagnostic summary. It helps
#' users understand:
#' \itemize{
#'   \item what data have already been loaded,
#'   \item whether actions, effects, spatial relations, targets, and constraints
#'   have been registered,
#'   \item whether objectives and methods have been configured,
#'   \item whether a model has already been built,
#'   \item and whether the object appears ready to be solved.
#' }
#'
#' In particular, the model section of the printed output summarizes whether the
#' current problem specification is incomplete, ready, or already materialized as
#' a built optimization model.
#'
#' @return No return value. This page documents the \code{Problem} class.
#'
#' @seealso
#' \code{\link{create_problem}},
#' \code{\link{add_actions}},
#' \code{\link{add_effects}},
#' \code{\link{add_constraint_targets_absolute}},
#' \code{\link{solve}}
NULL

.pa_data_status_text <- function(self) {
  if (.pa_has_model(self)) {
    "built"
  } else {
    "not built yet (will build in solve())"
  }
}

.pa_data_solver_text <- function(self) {
  sa <- self$data$solve_args %||% list()
  solver <- sa$solver %||% NULL
  if (is.null(solver) || !nzchar(as.character(solver)[1])) {
    return("not set (auto)")
  }
  as.character(solver)[1]
}

.pa_method_summary <- function(self) {
  obj_sum <- .pa_objectives_summary(self)
  method <- self$data$method %||% NULL

  if (is.list(method) && length(method) > 0L) {
    mtype <- as.character(method$type %||% method$name %||% "configured")[1]

    if (identical(mtype, "epsilon_constraint")) {
      primary <- as.character(method$primary %||% NA_character_)[1]
      if (!is.na(primary) && nzchar(primary)) {
        return(list(
          text = paste0("epsilon_constraint (primary: ", primary, ")"),
          is_set = TRUE
        ))
      }
    }

    if (identical(mtype, "weighted")) {
      aliases <- as.character(method$aliases %||% character(0))
      if (length(aliases) > 0L) {
        return(list(
          text = paste0("weighted (", paste(aliases, collapse = ", "), ")"),
          is_set = TRUE
        ))
      }
    }

    return(list(
      text = mtype,
      is_set = TRUE
    ))
  }

  if (obj_sum$n <= 1L) {
    return(list(
      text = "single-objective",
      is_set = TRUE
    ))
  }

  list(
    text = "not set",
    is_set = FALSE
  )
}

.pa_model_checks_text <- function(self) {
  obj_sum <- .pa_objectives_summary(self)
  met_sum <- .pa_method_summary(self)

  if (obj_sum$n == 0L) {
    return("incomplete (no objective registered)")
  }

  if (obj_sum$n > 1L && !isTRUE(met_sum$is_set)) {
    return("incomplete (multiple objectives registered but no MO method selected)")
  }

  "ok"
}

.pa_preview_text <- function(x, max_show = 3L, quote = TRUE) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return("none")
  }

  prev <- utils::head(x, max_show)
  if (isTRUE(quote)) {
    prev <- paste0('"', prev, '"')
  }

  txt <- paste(prev, collapse = ", ")
  if (length(x) > max_show) {
    txt <- paste0(txt, ", ...")
  }
  txt
}

.pa_features_summary <- function(self, max_show = 3L) {
  feats <- self$data$features
  if (is.null(feats) || !inherits(feats, "data.frame") || nrow(feats) == 0) {
    return(list(
      n = 0L,
      preview = "none"
    ))
  }

  nm <- if ("name" %in% names(feats)) as.character(feats$name) else as.character(feats$id)

  list(
    n = nrow(feats),
    preview = .pa_preview_text(nm, max_show = max_show, quote = TRUE)
  )
}

.pa_actions_summary <- function(self, max_show = 3L) {
  acts <- self$data$actions

  if (is.null(acts) || !inherits(acts, "data.frame") || nrow(acts) == 0) {
    return(list(
      n = 0L,
      preview = "none"
    ))
  }

  nm <- if ("name" %in% names(acts)) {
    as.character(acts$name)
  } else {
    as.character(acts$id)
  }

  list(
    n = nrow(acts),
    preview = .pa_preview_text(nm, max_show = max_show, quote = TRUE)
  )
}

.pa_effects_summary <- function(self) {
  de <- self$data$dist_effects
  dp <- self$data$dist_profit

  out <- list(
    n_effects = .pa_nrow0(de),
    n_profit = .pa_nrow0(dp),
    effect_mode = "none"
  )

  if (!is.null(de) && inherits(de, "data.frame") && nrow(de) > 0) {
    has_b <- "benefit" %in% names(de) && any(de$benefit > 0, na.rm = TRUE)
    has_l <- "loss" %in% names(de) && any(de$loss > 0, na.rm = TRUE)

    out$effect_mode <- if (has_b && has_l) {
      "benefit + loss"
    } else if (has_b) {
      "benefit only"
    } else if (has_l) {
      "loss only"
    } else {
      "all zero"
    }
  }

  out
}

.pa_targets_summary <- function(self, max_show = 3L) {
  t <- self$data$targets

  if (is.null(t) || !inherits(t, "data.frame") || nrow(t) == 0) {
    return(list(
      n_targets = 0L,
      preview = NULL
    ))
  }

  lab <- if ("feature_name" %in% names(t)) {
    as.character(t$feature_name)
  } else if ("feature" %in% names(t)) {
    as.character(t$feature)
  } else {
    rep("?", nrow(t))
  }

  val <- if ("target_value" %in% names(t)) {
    as.numeric(t$target_value)
  } else {
    rep(NA_real_, nrow(t))
  }

  sense <- if ("sense" %in% names(t)) as.character(t$sense) else rep("ge", nrow(t))
  sense_txt <- ifelse(sense %in% c("ge", ">="), ">=", ifelse(sense %in% c("le", "<="), "<=", sense))

  prev_n <- min(nrow(t), max_show)
  prev <- paste0('"', lab[seq_len(prev_n)], '" ', sense_txt[seq_len(prev_n)], " ", signif(val[seq_len(prev_n)], 4))

  list(
    n_targets = nrow(t),
    preview = prev
  )
}

.pa_constraints_summary <- function(self) {
  out <- list(
    area_constraints = 0L,
    pu_locked_in = 0L,
    pu_locked_out = 0L,
    action_locked_in = 0L,
    action_locked_out = 0L
  )

  cons <- self$data$constraints %||% list()

  # area constraints
  area_n <- 0L
  if (is.list(cons)) {
    if (!is.null(cons$area_min)) area_n <- area_n + 1L
    if (!is.null(cons$area_max)) area_n <- area_n + 1L
  }
  out$area_constraints <- area_n

  # planning-unit locks
  pu <- self$data$pu
  if (!is.null(pu) && inherits(pu, "data.frame")) {
    if ("locked_in" %in% names(pu)) {
      out$pu_locked_in <- sum(isTRUE(pu$locked_in) | (!is.na(pu$locked_in) & pu$locked_in), na.rm = TRUE)
    }
    if ("locked_out" %in% names(pu)) {
      out$pu_locked_out <- sum(isTRUE(pu$locked_out) | (!is.na(pu$locked_out) & pu$locked_out), na.rm = TRUE)
    }
  }

  # action locks
  da <- self$data$dist_actions
  if (!is.null(da) && inherits(da, "data.frame") && "status" %in% names(da)) {
    # ajusta estos códigos si tu convención final cambia
    out$action_locked_in  <- sum(da$status %in% c(1, 2), na.rm = TRUE)
    out$action_locked_out <- sum(da$status %in% c(3), na.rm = TRUE)
  }

  out
}

.pa_model_aux_summary <- function(self) {
  if (!.pa_has_model(self)) return(NULL)

  d <- .pa_model_dims(self)
  frag <- .pa_model_frag_vars_summary(self)

  out <- list(
    n_con = d$n_con %||% 0L,
    n_var = d$n_var %||% 0L,
    nnz   = d$nnz %||% 0L,
    n_z = 0L,
    n_y_pu = 0L,
    n_y_actions = 0L,
    n_y_interventions = 0L
  )

  op_list <- tryCatch(
    .pa_model_from_ptr(
      self$data$model_ptr,
      args = self$data$model_args %||% list(),
      drop_triplets = TRUE
    ),
    error = function(e) NULL
  )

  if (!is.null(op_list)) {
    out$n_z <- as.integer(op_list$n_z %||% 0L)
  }

  if (!is.null(frag)) {
    out$n_y_pu <- as.integer(frag$n_y_pu %||% 0L)
    out$n_y_actions <- as.integer(frag$n_y_actions %||% 0L)
    out$n_y_interventions <- as.integer(frag$n_y_interventions %||% 0L)
  }

  out
}

.pa_objectives_summary <- function(self, max_show = 4L) {
  objs <- self$data$objectives %||% NULL

  if (is.null(objs) || !is.list(objs) || length(objs) == 0L) {
    return(list(
      n = 0L,
      aliases = character(0),
      preview = "none"
    ))
  }

  aliases <- names(objs)

  if (is.null(aliases) || !any(nzchar(aliases))) {
    aliases <- vapply(
      objs,
      function(o) as.character(o$objective_id %||% "objective")[1],
      character(1)
    )
  }

  aliases <- aliases[!is.na(aliases) & nzchar(aliases)]

  list(
    n = length(aliases),
    aliases = aliases,
    preview = .pa_preview_text(aliases, max_show = max_show, quote = FALSE)
  )
}

.pa_print_model_section <- function(self, ch) {
  status_txt <- .pa_data_status_text(self)
  obj_sum <- .pa_objectives_summary(self)
  met_sum <- .pa_method_summary(self)
  solver_txt <- .pa_data_solver_text(self)
  checks_txt <- .pa_model_checks_text(self)

  cli::cli_text("{ch$l}{ch$b}{.h model}", .envir = environment())

  if (.pa_has_model(self)) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}status:         {.ok {status_txt}}", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}status:         {.warn {status_txt}}", .envir = environment())
  }

  if (obj_sum$n == 0L) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}objectives:     {.muted none}", .envir = environment())
  } else if (obj_sum$n == 1L) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}objectives:     1 registered ({obj_sum$preview})", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}objectives:     {obj_sum$n} registered ({obj_sum$preview})", .envir = environment())
  }

  if (identical(met_sum$text, "not set")) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}method:         {.warn {met_sum$text}}", .envir = environment())
  } else if (identical(met_sum$text, "single-objective")) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}method:         {.ok {met_sum$text}}", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}method:         {met_sum$text}", .envir = environment())
  }

  if (identical(solver_txt, "not set (auto)")) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}solver:         {.muted {solver_txt}}", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}solver:         {solver_txt}", .envir = environment())
  }

  if (identical(checks_txt, "ok")) {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}checks:         {.ok {checks_txt}}", .envir = environment())
  } else if (startsWith(checks_txt, "incomplete")) {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}checks:         {.warn {checks_txt}}", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$l}{ch$b}checks:         {.bad {checks_txt}}", .envir = environment())
  }
}

.pa_print_built_model_section <- function(self, ch) {
  if (!.pa_has_model(self)) return(invisible(NULL))

  sm <- .pa_model_aux_summary(self)
  if (is.null(sm)) return(invisible(NULL))

  cli::cli_text("{ch$l}{ch$b}{.h built model details}", .envir = environment())
  cli::cli_text(" {ch$v}{ch$j}{ch$b}dimensions:     {sm$n_con} constraints, {sm$n_var} vars, {sm$nnz} nnz",
                .envir = environment())

  if (sm$n_z > 0) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}z:              {sm$n_z} vars", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}z:              {.muted none}", .envir = environment())
  }

  if (sm$n_y_pu > 0) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}y_pu:           {sm$n_y_pu} vars", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}y_pu:           {.muted none}", .envir = environment())
  }

  if (sm$n_y_actions > 0) {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}y_actions:      {sm$n_y_actions} vars", .envir = environment())
  } else {
    cli::cli_text(" {ch$v}{ch$j}{ch$b}y_actions:      {.muted none}", .envir = environment())
  }

  if (sm$n_y_interventions > 0) {
    cli::cli_text(
      " {ch$v}{ch$l}{ch$b}y_interventions: {sm$n_y_interventions} vars",
      .envir = environment()
    )
  } else {
    cli::cli_text(
      " {ch$v}{ch$l}{ch$b}y_interventions: {.muted none}",
      .envir = environment()
    )
  }

  invisible(NULL)
}

#' @export
Problem <- pproto(
  "Problem",
  data = list(),

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    cli::cli_text("A multiscape object ({.cls Problem})")

    pu <- self$data$pu
    n_pu <- .pa_nrow0(pu)
    unit_rng <- .pa_safe_range(.pa_get_cost_vec(pu))

    feat_sum <- .pa_features_summary(self, max_show = 3L)
    act_sum <- .pa_actions_summary(self, max_show = 3L)

    pu_coords <- self$data$pu_coords
    has_coords <- .pa_has_coords(self)

    pu_sf <- self$data$pu_sf
    has_sf <- !is.null(pu_sf) && inherits(pu_sf, "sf")

    rels_sum <- .pa_spatial_relations_summary(self)
    n_rels <- if (is.null(rels_sum)) 0L else nrow(rels_sum)

    actions <- self$data$actions
    n_act <- .pa_nrow0(actions)

    dist_actions <- self$data$dist_actions
    n_dist_act <- .pa_nrow0(dist_actions)
    act_cost_rng <- .pa_safe_range(.pa_get_action_cost_vec(self))

    eff_sum <- .pa_effects_summary(self)
    tgt_sum <- .pa_targets_summary(self)
    cons_sum <- .pa_constraints_summary(self)

    # ---- PROBLEM SECTION
    cli::cli_text("{ch$j}{ch$b}{.h data}", .envir = environment())

    pu_cls <- if (is.null(pu)) "NULL" else class(pu)[1]
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}planning units:  {.cls {pu_cls}} ({n_pu} total)",
      .envir = environment()
    )

    if (is.null(unit_rng)) {
      cli::cli_text("{ch$v}{ch$j}{ch$b}costs:           {.muted (not available)}",
                    .envir = environment())
    } else {
      mn <- unit_rng[[1]]
      mx <- unit_rng[[2]]
      cli::cli_text("{ch$v}{ch$j}{ch$b}costs:           min: {mn}, max: {mx}",
                    .envir = environment())
    }

    cli::cli_text(
      "{ch$v}{ch$l}{ch$b}features:        {feat_sum$n} total ({feat_sum$preview})",
      .envir = environment()
    )

    # ---- ACTIONS AND EFFECTS SECTION
    cli::cli_text("{ch$l}{ch$b}{.h actions and effects}", .envir = environment())

    if (n_act == 0) {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}actions:         {.muted none}",
                    .envir = environment())
      cli::cli_text(" {ch$v}{ch$j}{ch$b}feasible action pairs:    {.muted none}",
                    .envir = environment())
    } else {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}actions:         {act_sum$n} total ({act_sum$preview})",
        .envir = environment()
      )
      cli::cli_text(" {ch$v}{ch$j}{ch$b}feasible action pairs:    {n_dist_act} feasible rows",
                    .envir = environment())

      if (is.null(act_cost_rng)) {
        cli::cli_text(" {ch$v}{ch$j}{ch$b}action costs:    {.muted (not available)}",
                      .envir = environment())
      } else {
        mn <- act_cost_rng[[1]]
        mx <- act_cost_rng[[2]]
        cli::cli_text(" {ch$v}{ch$j}{ch$b}action costs:    min: {mn}, max: {mx}",
                      .envir = environment())
      }
    }

    if (eff_sum$n_effects == 0) {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}effect data:    {.muted none}",
                    .envir = environment())
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}effect data:    {eff_sum$n_effects} rows",
                    .envir = environment())
      cli::cli_text(" {ch$v}{ch$j}{ch$b}effect mode:     {eff_sum$effect_mode}",
                    .envir = environment())
    }

    if (eff_sum$n_profit == 0) {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}profit data:     {.muted none}",
                    .envir = environment())
    } else {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}profit data:     {eff_sum$n_profit} rows",
                    .envir = environment())
    }

    # ---- SPATIAL SECTION
    cli::cli_text("{ch$l}{ch$b}{.h spatial}", .envir = environment())

    if (has_sf) {
      n_sf <- nrow(pu_sf)
      cli::cli_text(" {ch$v}{ch$j}{ch$b}geometry:        sf ({n_sf} rows)",
                    .envir = environment())
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}geometry:        {.muted none}",
                    .envir = environment())
    }

    if (has_coords) {
      n_c <- nrow(pu_coords)
      xr <- .pa_safe_range(pu_coords$x)
      yr <- .pa_safe_range(pu_coords$y)
      if (is.null(xr) || is.null(yr)) {
        cli::cli_text(" {ch$v}{ch$j}{ch$b}coordinates:      {n_c} rows",
                      .envir = environment())
      } else {
        cli::cli_text(" {ch$v}{ch$j}{ch$b}coordinates:      {n_c} rows (x: {xr[[1]]}..{xr[[2]]}, y: {yr[[1]]}..{yr[[2]]})",
                      .envir = environment())
      }
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}coordinates:      {.muted none}",
                    .envir = environment())
    }

    if (n_rels == 0) {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}relations:       {.muted none}",
                    .envir = environment())
    } else if (n_rels == 1L) {
      nm <- rels_sum$name[1]
      ed <- rels_sum$edges[1]
      w1 <- rels_sum$w_min[1]
      w2 <- rels_sum$w_max[1]

      if (is.na(w1) || is.na(w2)) {
        cli::cli_text(" {ch$v}{ch$l}{ch$b}relations:       {nm} ({ed} edges)",
                      .envir = environment())
      } else {
        cli::cli_text(" {ch$v}{ch$l}{ch$b}relations:       {nm} ({ed} edges, w: {w1}..{w2})",
                      .envir = environment())
      }
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}relations:       {n_rels} registered",
                    .envir = environment())

      max_show <- 6L
      show_sum <- rels_sum[seq_len(min(n_rels, max_show)), , drop = FALSE]

      for (i in seq_len(nrow(show_sum))) {
        nm <- show_sum$name[i]
        ed <- show_sum$edges[i]
        w1 <- show_sum$w_min[i]
        w2 <- show_sum$w_max[i]
        if (is.na(w1) || is.na(w2)) {
          cli::cli_text(" {ch$v}{ch$j}{ch$b}- {nm}: {ed} edges",
                        .envir = environment())
        } else {
          cli::cli_text(" {ch$v}{ch$j}{ch$b}- {nm}: {ed} edges (w: {w1}..{w2})",
                        .envir = environment())
        }
      }

      if (n_rels > max_show) {
        extra <- n_rels - max_show
        cli::cli_text(" {ch$v}{ch$l}{ch$b}{.muted ... +{extra} more relation(s)}",
                      .envir = environment())
      }
    }

    # ---- TARGETS AND CONSTRAINTS SECTION
    cli::cli_text("{ch$l}{ch$b}{.h targets and constraints}", .envir = environment())

    if (tgt_sum$n_targets == 0L) {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}targets:          {.muted none}",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}targets:          {tgt_sum$n_targets} rows",
        .envir = environment()
      )

      if (!is.null(tgt_sum$preview) && length(tgt_sum$preview) > 0) {
        prev_txt <- paste(tgt_sum$preview, collapse = ", ")
        cli::cli_text(
          " {ch$v}{ch$j}{ch$b}target preview:   {prev_txt}",
          .envir = environment()
        )
      }
    }

    if (cons_sum$area_constraints > 0) {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}area constraints: {cons_sum$area_constraints}",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}area constraints: {.muted none}",
        .envir = environment()
      )
    }

    pu_lock_total <- cons_sum$pu_locked_in + cons_sum$pu_locked_out
    if (pu_lock_total > 0L) {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}planning-unit locks: {pu_lock_total} units ({cons_sum$pu_locked_in} locked-in, {cons_sum$pu_locked_out} locked-out)",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$j}{ch$b}planning-unit locks: {.muted none}",
        .envir = environment()
      )
    }

    act_lock_total <- cons_sum$action_locked_in + cons_sum$action_locked_out
    if (act_lock_total > 0L) {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}action locks:       {act_lock_total} rows ({cons_sum$action_locked_in} locked-in, {cons_sum$action_locked_out} locked-out)",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}action locks:       {.muted none}",
        .envir = environment()
      )
    }

    # ---- MODEL SECTION
    .pa_print_model_section(self, ch)

    # ---- BUILT MODEL DETAILS
    .pa_print_built_model_section(self, ch)

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(cli::col_grey(
      paste0("# ", info_sym, " Use {.code x$data} to inspect stored tables and model snapshots.")
    ))

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    pu_n <- .pa_nrow0(self$data$pu)
    ft_n <- .pa_nrow0(self$data$features)
    paste0("<Problem> ", pu_n, " planning units, ", ft_n, " features")
  },

  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) return("x object do not found")
    self$data[[x]]
  },

  getPlanningUnitsAmount = function(self) {
    pu <- self$data$pu
    if (is.null(pu)) return(0L)
    nrow(pu)
  },

  getMonitoringCosts = function(self) {
    pu <- self$data$pu
    if (is.null(pu) || nrow(pu) == 0) return(numeric(0))
    if ("cost" %in% names(pu)) return(as.numeric(pu$cost))
    numeric(0)
  },

  getFeatureAmount = function(self) {
    feats <- self$data$features
    if (is.null(feats)) return(0L)
    nrow(feats)
  },

  getFeatureNames = function(self) {
    feats <- self$data$features
    if (is.null(feats) || nrow(feats) == 0) return(character(0))
    if ("name" %in% names(feats)) return(as.character(feats$name))
    as.character(feats$id)
  },

  getActionCosts = function(self) {
    if (!is.null(self$data$dist_actions) &&
        inherits(self$data$dist_actions, "data.frame") &&
        nrow(self$data$dist_actions) > 0 &&
        "cost" %in% names(self$data$dist_actions)) {
      return(as.numeric(self$data$dist_actions$cost))
    }
    numeric(0)
  },

  getActionsAmount = function(self) {
    if (!is.null(self$data$actions) && inherits(self$data$actions, "data.frame")) {
      return(nrow(self$data$actions))
    }
    0L
  }
)
