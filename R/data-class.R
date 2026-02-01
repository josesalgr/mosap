#' @include internal.R
#'
#' @export
if (!methods::isClass("Data")) methods::setOldClass("Data")
NULL

#' Data class
#'
#' This class is used to represent data of the instances of the corresponding
#' multi-action planning problem. It includes several methods for retrieving the information
#' of the instance (such as the spatial allocation of threats and species, the cost
#' of management actions or the structure of the spatial connectivity across
#' the area where the planning is carried out. This class is created using the
#' [inputData()] function.
#'
#' @section Fields: \describe{
#'   \item{data}{`list` object containing data.}
#' }
#'
#' @section Methods: \describe{
#'   \item{getActionsAmount():}{`integer`. Number of possible actions.}
#'   \item{getData(`character` name):}{[data.frame()]. Object stored in the `data` field with name `name`.}
#'   \item{getFeatureAmount():}{`integer`. Number of features.}
#'   \item{getFeatureNames():}{`character`. Names of features.}
#'   \item{getMonitoringCosts():}{`numeric` [vector()]. Cost of monitoring each planning unit.}
#'   \item{getPlanningUnitsAmount():}{`integer`. Number of planning units.}
#'   \item{getActionCosts():}{`numeric` [vector()]. Cost of actions each planning unit and threat.}
#'   \item{getThreatNames():}{`character`. Names of threats.}
#'   \item{getThreatsAmount():}{`integer`. Number of threats.}
#'   \item{print():}{Print basic information of the data instance and (if present) the model state.}
#'   \item{show():}{Call print method.}
#' }
#'
#' @return No return value.
#'
#' @name data-class
#' @aliases Data
NULL

#' @export
Data <- pproto(
  "Data",
  data = list(),

  print = function(self) {

    # ---- fallback if cli not installed
    if (!requireNamespace("cli", quietly = TRUE)) {
      pu <- self$data$pu
      n_pu <- if (is.null(pu)) 0L else nrow(pu)
      unit_rng <- .pa_safe_range(.pa_get_cost_vec(pu))
      feat_names <- self$getFeatureNames()
      thr_names <- self$getThreatNames()

      msg <- paste0(
        "Data",
        "\n  planning units: ", if (is.null(pu)) "NULL" else class(pu)[1],
        " (", n_pu, " units)"
      )

      if (is.null(unit_rng)) {
        msg <- paste0(msg, "\n  costs:           (not available)")
      } else {
        msg <- paste0(msg, "\n  costs:           min: ", unit_rng[1], ", max: ", unit_rng[2])
      }

      msg <- paste0(msg, "\n  features:        ", paste(feat_names, collapse = ", "))
      msg <- paste0(msg, "\n  threats:         ", paste(thr_names, collapse = ", "))

      # model summary (if present)
      if (.pa_has_model(self)) {
        d <- .pa_model_dims(self)
        a <- .pa_model_args(self)
        msg <- paste0(msg,
                      "\n  model:           ",
                      if (!is.null(a)) paste0(a$model_type, " (", a$modelsense, ")") else "(built)",
                      "\n    dimensions:    ", d$n_con, " constraints, ", d$n_var, " vars, ", d$nnz, " nnz")

        frag <- .pa_model_frag_vars_summary(self)
        if (!is.null(frag)) {
          msg <- paste0(
            msg,
            "\n    frag vars:     y_pu=", frag$n_y_pu,
            " (off ", frag$y_pu_offset, ")",
            ", y_actions=", frag$n_y_actions,
            " (off ", frag$y_actions_offset, ")",
            ", y_interv=", frag$n_y_interventions,
            " (off ", frag$y_interventions_offset, ")"
          )
        }

      }

      # targets summary (if present)
      if (!is.null(self$data$targets)) {
        t <- self$data$targets
        n_t <- if (inherits(t, "data.frame")) nrow(t) else length(t)
        msg <- paste0(msg, "\n  targets:         ", n_t)
      }

      message(msg)
      return(invisible(TRUE))
    }

    # ---- pretty print with cli
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    cli::cli_text("A prioriactions object ({.cls Data})")

    # --- precompute counts / ranges safely
    pu <- self$data$pu
    n_pu <- .pa_nrow0(pu)
    unit_rng <- .pa_safe_range(.pa_get_cost_vec(pu))

    feats <- self$data$features
    n_feat <- .pa_nrow0(feats)
    feat_names <- self$getFeatureNames()

    thr <- self$data$threats
    n_thr <- .pa_nrow0(thr)
    thr_names <- self$getThreatNames()

    pu_coords <- self$data$pu_coords
    has_coords <- .pa_has_coords(self)

    rels_sum <- .pa_spatial_relations_summary(self)
    n_rels <- if (is.null(rels_sum)) 0L else nrow(rels_sum)

    actions <- self$data$actions
    n_act <- .pa_nrow0(actions)

    dist_actions <- self$data$dist_actions
    n_dist_act <- .pa_nrow0(dist_actions)
    act_cost_rng <- .pa_safe_range(.pa_get_action_cost_vec(self))

    dist_benefit <- self$data$dist_benefit
    n_dist_ben <- .pa_nrow0(dist_benefit)

    # ---- DATA SECTION
    cli::cli_text("{ch$j}{ch$b}{.h data}", .envir = environment())

    pu_cls <- if (is.null(pu)) "NULL" else class(pu)[1]
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}planning units:  {.cls {pu_cls}} ({n_pu} total)",
      .envir = environment()
    )

    if (is.null(unit_rng)) {
      cli::cli_text("{ch$v}{ch$j}{ch$b}costs:          {.muted (not available)}",
                    .envir = environment())
    } else {
      mn <- unit_rng[[1]]
      mx <- unit_rng[[2]]
      cli::cli_text("{ch$v}{ch$j}{ch$b}costs:          min: {mn}, max: {mx}",
                    .envir = environment())
    }

    txt <- .pa_repr_atomic(feat_names, "features")
    cli::cli_text("{ch$v}{ch$j}{ch$b}features:       {txt}",
                  .envir = environment())

    txt <- if (n_thr == 0) "{.muted (0 threats)}" else .pa_repr_atomic(thr_names, "threats")
    cli::cli_text("{ch$v}{ch$j}{ch$b}threats:        {txt}",
                  .envir = environment())

    # ---- ACTIONS SECTION
    cli::cli_text("{ch$l}{ch$b}{.h actions}", .envir = environment())

    if (n_act == 0) {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}actions:        {.muted none specified}",
                    .envir = environment())
    } else {
      act_n <- n_act
      da_n  <- n_dist_act
      cli::cli_text(" {ch$v}{ch$j}{ch$b}actions:        {act_n} total",
                    .envir = environment())
      cli::cli_text(" {ch$v}{ch$j}{ch$b}dist_actions:   {da_n} feasible rows",
                    .envir = environment())

      if (is.null(act_cost_rng)) {
        cli::cli_text(" {ch$v}{ch$l}{ch$b}action costs:   {.muted (not available)}",
                      .envir = environment())
      } else {
        mn <- act_cost_rng[[1]]
        mx <- act_cost_rng[[2]]
        cli::cli_text(" {ch$v}{ch$l}{ch$b}action costs:   min: {mn}, max: {mx}",
                      .envir = environment())
      }
    }

    # ---- BENEFITS SECTION
    cli::cli_text("{ch$l}{ch$b}{.h benefits}", .envir = environment())
    if (n_dist_ben == 0) {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}dist_benefit:   {.muted none specified}",
                    .envir = environment())
    } else {
      n_rows <- n_dist_ben
      cli::cli_text(" {ch$v}{ch$l}{ch$b}dist_benefit:   {n_rows} rows",
                    .envir = environment())
    }

    # ---- SPATIAL SECTION
    cli::cli_text("{ch$l}{ch$b}{.h spatial}", .envir = environment())

    if (has_coords) {
      n_c <- nrow(pu_coords)
      xr <- .pa_safe_range(pu_coords$x)
      yr <- .pa_safe_range(pu_coords$y)
      if (is.null(xr) || is.null(yr)) {
        cli::cli_text(" {ch$v}{ch$j}{ch$b}pu_coords:      {n_c} rows",
                      .envir = environment())
      } else {
        cli::cli_text(" {ch$v}{ch$j}{ch$b}pu_coords:      {n_c} rows (x: {xr[[1]]}..{xr[[2]]}, y: {yr[[1]]}..{yr[[2]]})",
                      .envir = environment())
      }
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}pu_coords:      {.muted none}",
                    .envir = environment())
    }

    if (n_rels == 0) {
      cli::cli_text(" {ch$v}{ch$l}{ch$b}relations:      {.muted none}",
                    .envir = environment())
    } else {
      cli::cli_text(" {ch$v}{ch$j}{ch$b}relations:      {n_rels} registered",
                    .envir = environment())

      # print each relation on its own line (limit to avoid super long prints)
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

    # ---- MODEL EXTRAS: fragmentation auxiliary vars (if present)
    if (.pa_has_model(self)) {
      frag <- .pa_model_frag_vars_summary(self)

      if (!is.null(frag)) {

        cli::cli_text("{ch$l}{ch$b}{.h fragmentation vars}", .envir = environment())

        if (frag$n_y_pu > 0) {
          n1 <- frag$n_y_pu
          o1 <- frag$y_pu_offset
          cli::cli_text(" {ch$v}{ch$j}{ch$b}y_pu:            {n1} vars (offset: {o1})",
                        .envir = environment())
        } else {
          cli::cli_text(" {ch$v}{ch$j}{ch$b}y_pu:            {.muted none}",
                        .envir = environment())
        }

        if (frag$n_y_actions > 0) {
          n2 <- frag$n_y_actions
          o2 <- frag$y_actions_offset
          cli::cli_text(" {ch$v}{ch$j}{ch$b}y_actions:       {n2} vars (offset: {o2})",
                        .envir = environment())
        } else {
          cli::cli_text(" {ch$v}{ch$j}{ch$b}y_actions:       {.muted none}",
                        .envir = environment())
        }

        if (frag$n_y_interventions > 0) {
          n3 <- frag$n_y_interventions
          o3 <- frag$y_interventions_offset
          cli::cli_text(" {ch$v}{ch$l}{ch$b}y_interventions: {n3} vars (offset: {o3})",
                        .envir = environment())
        } else {
          cli::cli_text(" {ch$v}{ch$l}{ch$b}y_interventions: {.muted none}",
                        .envir = environment())
        }
      }
    }


    # ---- MODEL SECTION (always)
    .pa_print_model_section(self, ch)

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(cli::col_grey(
      paste0("# ", info_sym, " Use {.code x$data} to inspect stored tables and model snapshots.")
    ))

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),
  repr = function(self) "Data object",

  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) return("x object do not found")
    self$data[[x]]
  },

  # ---- getters compatibles con el cambio monitoring_cost -> cost
  getPlanningUnitsAmount = function(self) {
    pu <- self$data$pu
    if (is.null(pu)) return(0L)
    nrow(pu)
  },

  getMonitoringCosts = function(self) {
    pu <- self$data$pu
    if (is.null(pu) || nrow(pu) == 0) return(numeric(0))
    if ("cost" %in% names(pu)) return(as.numeric(pu$cost))
    if ("monitoring_cost" %in% names(pu)) return(as.numeric(pu$monitoring_cost))
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
    as.character(feats$name)
  },

  getThreatNames = function(self) {
    thr <- self$data$threats
    if (is.null(thr) || nrow(thr) == 0) return(character(0))
    as.character(thr$name)
  },

  getThreatsAmount = function(self) {
    thr <- self$data$threats
    if (is.null(thr)) return(0L)
    nrow(thr)
  },

  getActionCosts = function(self) {
    # prefer new dist_actions
    if (!is.null(self$data$dist_actions) &&
        nrow(self$data$dist_actions) > 0 &&
        "cost" %in% names(self$data$dist_actions)) {
      return(as.numeric(self$data$dist_actions$cost))
    }
    # legacy dist_threats
    dt <- self$data$dist_threats
    if (is.null(dt) || nrow(dt) == 0 || !("action_cost" %in% names(dt))) return(numeric(0))
    as.numeric(dt$action_cost)
  },

  getActionsAmount = function(self) {
    # prefer new actions table if present
    if (!is.null(self$data$actions) && inherits(self$data$actions, "data.frame")) {
      return(nrow(self$data$actions))
    }
    # legacy
    dt <- self$data$dist_threats
    if (is.null(dt)) return(0L)
    nrow(dt)
  }
)
