#' @include internalMO.R
NULL

# -------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------

.pa_plot_spatial_resolve_solutions <- function(x, runs = NULL) {
  if (inherits(x, "Solution")) {
    if (!is.null(runs)) {
      runs <- as.integer(runs)
      if (length(runs) != 1L || is.na(runs[1]) || runs[1] != 1L) {
        stop("For a Solution object, `runs` must be NULL or 1.", call. = FALSE)
      }
    }
    runs <- 1L
    solutions <- list(x)
    names(solutions) <- "1"
    return(solutions)
  }

  if (inherits(x, "SolutionSet")) {
    all_solutions <- x$solution$solutions %||% NULL
    if (is.null(all_solutions) || !is.list(all_solutions) || length(all_solutions) == 0L) {
      stop("SolutionSet has no solutions in x$solution$solutions.", call. = FALSE)
    }

    if (is.null(runs)) {
      runs <- 1L
    } else {
      runs <- as.integer(runs)
      if (any(!is.finite(runs)) || any(is.na(runs)) || any(runs < 1L)) {
        stop("`runs` must contain positive integers.", call. = FALSE)
      }
      if (any(runs > length(all_solutions))) {
        stop(
          "`runs` contains values larger than the number of available solutions (",
          length(all_solutions), ").",
          call. = FALSE
        )
      }
      runs <- unique(runs)
    }

    solutions <- all_solutions[runs]
    names(solutions) <- as.character(runs)
    return(solutions)
  }

  stop("x must be a Solution or SolutionSet.", call. = FALSE)
}

.pa_plot_spatial_get_geometry <- function(solutions) {
  sol0 <- solutions[[1]]
  pr <- sol0$problem %||% NULL
  if (is.null(pr) || !inherits(pr, "Problem")) {
    stop("Solution does not contain a valid problem object.", call. = FALSE)
  }

  pu_sf <- pr$data$pu_sf %||% NULL
  if (is.null(pu_sf) || !inherits(pu_sf, "sf")) {
    stop("No PU geometry found in solution$problem$data$pu_sf.", call. = FALSE)
  }
  if (!("id" %in% names(pu_sf))) {
    stop("PU geometry must contain an 'id' column.", call. = FALSE)
  }

  pu_sf$id <- as.integer(pu_sf$id)
  pu_sf[, "id", drop = FALSE]
}

.pa_plot_spatial_make_base_plot <- function(
    pu_sf_min,
    show_base = TRUE,
    base_fill = "grey92",
    base_color = NA,
    base_alpha = 0.10
) {
  p <- ggplot2::ggplot()
  if (isTRUE(show_base)) {
    p <- p +
      ggplot2::geom_sf(
        data = pu_sf_min,
        fill = base_fill,
        color = base_color,
        alpha = base_alpha
      )
  }
  p + ggplot2::theme_minimal()
}

#' @title Plot spatial outputs from a solution or solution set
#'
#' @description
#' Convenience wrapper to plot spatial outputs from a \code{Solution} or
#' \code{SolutionSet}.
#'
#' Depending on \code{what}, this function dispatches to one of:
#' \itemize{
#'   \item \code{\link{plot_spatial_pu}},
#'   \item \code{\link{plot_spatial_actions}},
#'   \item \code{\link{plot_spatial_features}}.
#' }
#'
#' This wrapper is useful as a compact entry point, while the specialised
#' plotting functions provide a cleaner and more explicit user interface for
#' each spatial output type.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object.
#' @param what Character string indicating what to plot. Must be one of
#'   \code{"pu"}, \code{"actions"}, or \code{"features"}.
#' @param runs Optional integer vector of run ids. If \code{NULL}, a
#'   \code{Solution} is used directly and a \code{SolutionSet} defaults to the
#'   first run.
#' @param actions Optional action subset used when \code{what = "actions"}.
#' @param features Optional feature subset used when \code{what = "features"}.
#' @param value Character string used only when \code{what = "features"}.
#'   Must be one of \code{"final"}, \code{"baseline"}, or \code{"benefit"}.
#' @param layout Character string controlling the layout. Must be one of
#'   \code{"single"} or \code{"facet"}. If \code{NULL}, the default is
#'   \code{"single"} for planning units and actions, and \code{"facet"} for
#'   features.
#' @param max_facets Maximum number of facets shown when faceting without an
#'   explicit action or feature subset.
#' @param ... Additional arguments passed to the specialised plotting function.
#' @param base_alpha Numeric value in \eqn{[0,1]} giving the alpha of the base
#'   planning-unit layer.
#' @param selected_alpha Numeric value in \eqn{[0,1]} giving the alpha of the
#'   highlighted layer.
#' @param base_fill Fill colour for the base planning-unit layer.
#' @param base_color Border colour for the base planning-unit layer.
#' @param selected_color Border colour for highlighted layers.
#' @param draw_borders Logical. If \code{FALSE}, borders are not drawn.
#' @param show_base Logical. If \code{TRUE}, draw the base planning-unit layer
#'   underneath the highlighted output.
#' @param fill_values Optional named vector of colours for discrete maps.
#' @param fill_na Fill colour for missing values.
#' @param use_viridis Logical. If \code{TRUE} and the \pkg{viridis} package is
#'   available, use viridis scales.
#'
#' @return Invisibly returns a \code{ggplot} object.
#'
#' @seealso
#' \code{\link{plot_spatial_pu}},
#' \code{\link{plot_spatial_actions}},
#' \code{\link{plot_spatial_features}}
#'
#' @export
plot_spatial <- function(
    x,
    what = c("pu", "actions", "features"),
    runs = NULL,
    actions = NULL,
    features = NULL,
    value = c("final", "baseline", "benefit"),
    layout = NULL,
    max_facets = 4L,
    ...,
    base_alpha = 0.10,
    selected_alpha = 0.90,
    base_fill = "grey92",
    base_color = NA,
    selected_color = NA,
    draw_borders = FALSE,
    show_base = TRUE,
    fill_values = NULL,
    fill_na = "grey80",
    use_viridis = TRUE
) {
  what <- match.arg(what)
  value <- match.arg(value)

  if (identical(what, "pu")) {
    return(plot_spatial_pu(
      x = x,
      runs = runs,
      ...,
      base_alpha = base_alpha,
      selected_alpha = selected_alpha,
      base_fill = base_fill,
      base_color = base_color,
      selected_color = selected_color,
      draw_borders = draw_borders,
      show_base = show_base
    ))
  }

  if (identical(what, "actions")) {
    return(plot_spatial_actions(
      x = x,
      runs = runs,
      actions = actions,
      layout = layout,
      max_facets = max_facets,
      ...,
      base_alpha = base_alpha,
      selected_alpha = selected_alpha,
      base_fill = base_fill,
      base_color = base_color,
      selected_color = selected_color,
      draw_borders = draw_borders,
      show_base = show_base,
      fill_values = fill_values,
      fill_na = fill_na,
      use_viridis = use_viridis
    ))
  }

  plot_spatial_features(
    x = x,
    runs = runs,
    features = features,
    value = value,
    layout = layout,
    max_facets = max_facets,
    ...,
    base_alpha = base_alpha,
    selected_alpha = selected_alpha,
    base_fill = base_fill,
    base_color = base_color,
    selected_color = selected_color,
    draw_borders = draw_borders,
    show_base = show_base,
    fill_na = fill_na,
    use_viridis = use_viridis
  )
}

#' @title Plot selected planning units in space
#'
#' @description
#' Plot the spatial distribution of selected planning units from a
#' \code{Solution} or \code{SolutionSet}.
#'
#' This function maps the planning-unit selection summary returned by
#' \code{\link{get_pu}} onto the planning-unit geometry stored in the associated
#' \code{Problem} object.
#'
#' @details
#' Let \eqn{w_i \in \{0,1\}} denote the planning-unit selection variable for
#' planning unit \eqn{i}. This function plots the user-facing
#' \code{selected == 1} representation of \eqn{w_i}.
#'
#' If several runs are requested, the output is faceted by \code{run_id}.
#'
#' Planning-unit geometry must be available in \code{x$problem$data$pu_sf}.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object.
#' @param runs Optional integer vector of run ids. If \code{NULL}, a
#'   \code{Solution} is used directly and a \code{SolutionSet} defaults to the
#'   first run.
#' @param ... Reserved for future extensions.
#' @param base_alpha Numeric value in \eqn{[0,1]} giving the alpha of the base
#'   planning-unit layer.
#' @param selected_alpha Numeric value in \eqn{[0,1]} giving the alpha of the
#'   selected planning-unit layer.
#' @param base_fill Fill colour for the base planning-unit layer.
#' @param base_color Border colour for the base planning-unit layer.
#' @param selected_color Border colour for selected planning units.
#' @param draw_borders Logical. If \code{FALSE}, borders are not drawn.
#' @param show_base Logical. If \code{TRUE}, draw the base planning-unit layer
#'   underneath the selected units.
#'
#' @return Invisibly returns a \code{ggplot} object.
#'
#' @seealso
#' \code{\link{get_pu}},
#' \code{\link{plot_spatial}},
#' \code{\link{plot_spatial_actions}},
#' \code{\link{plot_spatial_features}}
#'
#' @export
plot_spatial_pu <- function(
    x,
    runs = NULL,
    ...,
    base_alpha = 0.10,
    selected_alpha = 0.90,
    base_fill = "grey92",
    base_color = NA,
    selected_color = NA,
    draw_borders = FALSE,
    show_base = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("plot_spatial_pu() requires the 'sf' package.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_spatial_pu() requires the 'ggplot2' package.", call. = FALSE)
  }

  if (!isTRUE(draw_borders)) {
    base_color <- NA
    selected_color <- NA
  }

  solutions <- .pa_plot_spatial_resolve_solutions(x, runs = runs)
  multi_runs <- length(solutions) > 1L
  pu_sf_min <- .pa_plot_spatial_get_geometry(solutions)

  pu_list <- vector("list", length(solutions))
  for (i in seq_along(solutions)) {
    sol_i <- solutions[[i]]
    run_i <- as.integer(names(solutions)[i])

    pu_tbl <- get_pu(sol_i, only_selected = FALSE)
    if (!all(c("id", "selected") %in% names(pu_tbl))) {
      stop("PU summary must contain 'id' and 'selected'.", call. = FALSE)
    }

    pu_tbl$id <- as.integer(pu_tbl$id)
    g_i <- merge(pu_sf_min, pu_tbl[, c("id", "selected")], by = "id", all.x = TRUE)
    g_i$selected[is.na(g_i$selected)] <- 0L
    g_i$run_id <- run_i
    pu_list[[i]] <- g_i
  }

  g <- do.call(rbind, pu_list)
  g <- sf::st_as_sf(g)
  g_sel <- g[g$selected %in% 1L, , drop = FALSE]

  if (nrow(g_sel) == 0L) {
    stop("No selected planning units to plot.", call. = FALSE)
  }

  p <- .pa_plot_spatial_make_base_plot(
    pu_sf_min = pu_sf_min,
    show_base = show_base,
    base_fill = base_fill,
    base_color = base_color,
    base_alpha = base_alpha
  ) +
    ggplot2::geom_sf(
      data = g_sel,
      fill = "#2C7FB8",
      color = selected_color,
      alpha = selected_alpha
    ) +
    ggplot2::labs(title = "Selected planning units")

  if (isTRUE(multi_runs)) {
    p <- p + ggplot2::facet_wrap(~run_id)
  }

  print(p)
  invisible(p)
}

#' @title Plot selected actions in space
#'
#' @description
#' Plot the spatial distribution of selected actions from a
#' \code{Solution} or \code{SolutionSet}.
#'
#' This function maps the selected planning unit--action pairs returned by
#' \code{\link{get_actions}(only_selected = TRUE)} onto the planning-unit
#' geometry stored in the associated \code{Problem} object.
#'
#' @details
#' Let \eqn{x_{ia} \in \{0,1\}} denote whether action \eqn{a} is selected in
#' planning unit \eqn{i}. This function plots the selected
#' \code{(pu, action)} pairs in geographic space.
#'
#' If \code{layout = "facet"} and only one run is plotted, one panel is drawn
#' per action.
#'
#' If \code{layout = "single"}, all selected actions are drawn in a single map
#' using discrete fills. If more than one action is selected in the same
#' planning unit, the action labels are collapsed using \code{"+"}.
#'
#' When plotting multiple runs, only \code{layout = "single"} is supported.
#'
#' Planning-unit geometry must be available in \code{x$problem$data$pu_sf}.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object.
#' @param runs Optional integer vector of run ids. If \code{NULL}, a
#'   \code{Solution} is used directly and a \code{SolutionSet} defaults to the
#'   first run.
#' @param actions Optional action subset to display. Entries may match action
#'   ids or action-set labels.
#' @param layout Character string controlling the layout. Must be one of
#'   \code{"single"} or \code{"facet"}. If \code{NULL}, the default is
#'   \code{"single"}.
#' @param max_facets Maximum number of action facets shown when \code{actions}
#'   is \code{NULL} and faceting would otherwise create many panels.
#' @param ... Reserved for future extensions.
#' @param base_alpha Numeric value in \eqn{[0,1]} giving the alpha of the base
#'   planning-unit layer.
#' @param selected_alpha Numeric value in \eqn{[0,1]} giving the alpha of the
#'   highlighted action layer.
#' @param base_fill Fill colour for the base planning-unit layer.
#' @param base_color Border colour for the base planning-unit layer.
#' @param selected_color Border colour for highlighted layers.
#' @param draw_borders Logical. If \code{FALSE}, borders are not drawn.
#' @param show_base Logical. If \code{TRUE}, draw the base planning-unit layer
#'   underneath the highlighted output.
#' @param fill_values Optional named vector of colours for discrete action maps.
#' @param fill_na Fill colour for missing values.
#' @param use_viridis Logical. If \code{TRUE} and the \pkg{viridis} package is
#'   available, use viridis discrete scales.
#'
#' @return Invisibly returns a \code{ggplot} object.
#'
#' @seealso
#' \code{\link{get_actions}},
#' \code{\link{plot_spatial}},
#' \code{\link{plot_spatial_pu}},
#' \code{\link{plot_spatial_features}}
#'
#' @export
plot_spatial_actions <- function(
    x,
    runs = NULL,
    actions = NULL,
    layout = NULL,
    max_facets = 4L,
    ...,
    base_alpha = 0.08,
    selected_alpha = 0.95,
    base_fill = "grey95",
    base_color = NA,
    selected_color = NA,
    draw_borders = FALSE,
    show_base = TRUE,
    fill_values = NULL,
    fill_na = "grey80",
    use_viridis = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("plot_spatial_actions() requires the 'sf' package.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_spatial_actions() requires the 'ggplot2' package.", call. = FALSE)
  }
  has_viridis <- requireNamespace("viridis", quietly = TRUE)

  if (is.null(layout)) layout <- "single"
  layout <- match.arg(layout, c("single", "facet"))

  if (!isTRUE(draw_borders)) {
    base_color <- NA
    selected_color <- NA
  }

  solutions <- .pa_plot_spatial_resolve_solutions(x, runs = runs)
  multi_runs <- length(solutions) > 1L

  if (isTRUE(multi_runs) && identical(layout, "facet")) {
    stop(
      "When plotting multiple runs for actions, use layout = 'single'. ",
      "Faceting by both run and action is not supported.",
      call. = FALSE
    )
  }

  pu_sf_min <- .pa_plot_spatial_get_geometry(solutions)

  act_list <- vector("list", length(solutions))
  for (i in seq_along(solutions)) {
    sol_i <- solutions[[i]]
    run_i <- as.integer(names(solutions)[i])

    act_tbl <- get_actions(sol_i, only_selected = TRUE)
    if (!all(c("pu", "action", "selected") %in% names(act_tbl))) {
      stop("Action summary must contain 'pu', 'action', and 'selected'.", call. = FALSE)
    }

    act_tbl$pu <- as.integer(act_tbl$pu)
    act_tbl$action <- as.character(act_tbl$action)

    if (!is.null(actions)) {
      keep <- .pa_resolve_action_subset(sol_i$problem, subset = actions)
      keep_ids <- as.character(keep$id)
      act_tbl <- act_tbl[act_tbl$action %in% keep_ids, , drop = FALSE]
    }

    if (nrow(act_tbl) == 0L) next
    act_tbl$run_id <- run_i
    act_list[[i]] <- act_tbl
  }

  act_list <- Filter(Negate(is.null), act_list)
  if (length(act_list) == 0L) {
    stop("No selected actions to plot.", call. = FALSE)
  }

  act_tbl <- do.call(rbind, act_list)
  acts <- unique(act_tbl$action)

  base_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.2),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey96", colour = "grey85"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  # -------------------------------------------------------------------
  # one run + action facets
  # -------------------------------------------------------------------
  if (!isTRUE(multi_runs) && identical(layout, "facet")) {
    if (is.null(actions) && length(acts) > max_facets) {
      warning(
        "Showing only the first ", max_facets,
        " actions. Use actions=... or increase max_facets.",
        call. = FALSE
      )
      keep_acts <- acts[seq_len(max_facets)]
      act_tbl <- act_tbl[act_tbl$action %in% keep_acts, , drop = FALSE]
    }

    names(act_tbl)[names(act_tbl) == "pu"] <- "id"
    g <- merge(pu_sf_min, act_tbl[, c("id", "action")], by = "id", all.x = FALSE)
    g <- sf::st_as_sf(g)

    p <- .pa_plot_spatial_make_base_plot(
      pu_sf_min = pu_sf_min,
      show_base = show_base,
      base_fill = base_fill,
      base_color = base_color,
      base_alpha = base_alpha
    ) +
      ggplot2::geom_sf(
        data = g,
        ggplot2::aes(fill = action),
        color = selected_color,
        alpha = selected_alpha
      ) +
      ggplot2::facet_wrap(~action) +
      ggplot2::labs(title = "Selected actions", fill = "") +
      ggplot2::coord_sf(datum = NA) +
      base_theme

    if (!is.null(fill_values)) {
      p <- p + ggplot2::scale_fill_manual(values = fill_values, na.value = fill_na)
    } else if (isTRUE(use_viridis) && has_viridis) {
      p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
    }

    print(p)
    return(invisible(p))
  }

  # -------------------------------------------------------------------
  # single layout (possibly multiple runs)
  # -------------------------------------------------------------------
  lab_list <- split(act_tbl, act_tbl$run_id)
  warned_multi_action <- FALSE

  lab_out <- lapply(names(lab_list), function(rr) {
    dd <- lab_list[[rr]]

    tmp <- stats::aggregate(
      action ~ pu,
      data = dd,
      FUN = function(z) {
        z <- unique(as.character(z))
        if (length(z) > 1L) warned_multi_action <<- TRUE
        paste(sort(z), collapse = "+")
      }
    )

    names(tmp)[names(tmp) == "pu"] <- "id"
    tmp$id <- as.integer(tmp$id)
    tmp$action <- as.character(tmp$action)
    tmp$run_id <- as.integer(rr)
    tmp
  })

  if (isTRUE(warned_multi_action)) {
    warning(
      "More than one action detected in at least one PU. Labels were collapsed using '+'.",
      call. = FALSE
    )
  }

  lab_act <- do.call(rbind, lab_out)
  g <- merge(pu_sf_min, lab_act, by = "id", all.x = FALSE)
  g <- sf::st_as_sf(g)

  if (nrow(g) == 0L) {
    stop("No geometry matched the plotted action labels.", call. = FALSE)
  }

  p <- .pa_plot_spatial_make_base_plot(
    pu_sf_min = pu_sf_min,
    show_base = show_base,
    base_fill = base_fill,
    base_color = base_color,
    base_alpha = base_alpha
  ) +
    ggplot2::geom_sf(
      data = g,
      ggplot2::aes(fill = action),
      color = selected_color,
      alpha = selected_alpha
    ) +
    ggplot2::labs(title = "Selected actions", fill = "") +
    ggplot2::coord_sf(datum = NA) +
    base_theme

  if (isTRUE(multi_runs)) {
    p <- p +
      ggplot2::facet_wrap(
        ~run_id,
        labeller = ggplot2::labeller(run_id = function(x) paste("Run", x))
      ) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }

  if (!is.null(fill_values)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_values, na.value = fill_na)
  } else if (isTRUE(use_viridis) && has_viridis) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
  }

  print(p)
  invisible(p)
}

#' @title Plot spatial feature values from a solution
#'
#' @description
#' Plot feature values in space from a \code{Solution} or \code{SolutionSet}.
#'
#' This function combines baseline feature amounts from
#' \code{x$problem$data$dist_features} with positive effects induced by selected
#' actions to produce planning-unit-level feature maps.
#'
#' @details
#' For each planning unit \eqn{i} and feature \eqn{f}, the plotted quantities
#' are:
#' \deqn{
#' \mathrm{baseline}_{if},
#' }
#' \deqn{
#' \mathrm{benefit}_{if},
#' }
#' \deqn{
#' \mathrm{final}_{if} = \mathrm{baseline}_{if} + \mathrm{benefit}_{if}.
#' }
#'
#' In the current implementation:
#' \itemize{
#'   \item \code{baseline} is the summed baseline amount from
#'   \code{x$problem$data$dist_features},
#'   \item \code{benefit} is the summed positive effect from selected actions,
#'   \item \code{final} is \code{baseline + benefit}.
#' }
#'
#' Negative effects are not subtracted in this plotting method. Therefore,
#' \code{value = "final"} should be interpreted as baseline plus selected
#' positive effects under the current plotting logic.
#'
#' If \code{layout = "facet"} and only one run is plotted, one panel is drawn
#' per feature.
#'
#' If multiple runs are plotted, exactly one feature must be requested, and
#' faceting is done by run.
#'
#' Planning-unit geometry must be available in \code{x$problem$data$pu_sf}.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object.
#' @param runs Optional integer vector of run ids. If \code{NULL}, a
#'   \code{Solution} is used directly and a \code{SolutionSet} defaults to the
#'   first run.
#' @param features Optional feature subset to display. Matching is attempted
#'   against both feature ids and feature names.
#' @param value Character string indicating which feature quantity to plot. Must
#'   be one of \code{"final"}, \code{"baseline"}, or \code{"benefit"}.
#' @param layout Character string controlling the layout. Must be one of
#'   \code{"single"} or \code{"facet"}. If \code{NULL}, the default is
#'   \code{"facet"}.
#' @param max_facets Maximum number of feature facets shown when
#'   \code{features = NULL} and faceting would otherwise create many panels.
#' @param ... Reserved for future extensions.
#' @param base_alpha Unused in the current feature view, kept for interface
#'   consistency.
#' @param selected_alpha Unused in the current feature view, kept for interface
#'   consistency.
#' @param base_fill Unused in the current feature view, kept for interface
#'   consistency.
#' @param base_color Unused in the current feature view, kept for interface
#'   consistency.
#' @param selected_color Border colour for filled feature polygons.
#' @param draw_borders Logical. If \code{FALSE}, borders are not drawn.
#' @param show_base Unused in the current feature view, kept for interface
#'   consistency.
#' @param fill_na Fill colour for missing values.
#' @param use_viridis Logical. If \code{TRUE} and the \pkg{viridis} package is
#'   available, use a continuous viridis scale.
#'
#' @return Invisibly returns a \code{ggplot} object.
#'
#' @seealso
#' \code{\link{get_features}},
#' \code{\link{plot_spatial}},
#' \code{\link{plot_spatial_pu}},
#' \code{\link{plot_spatial_actions}}
#'
#' @export
plot_spatial_features <- function(
    x,
    runs = NULL,
    features = NULL,
    value = c("final", "baseline", "benefit"),
    layout = NULL,
    max_facets = 4L,
    ...,
    base_alpha = 0.10,
    selected_alpha = 0.90,
    base_fill = "grey92",
    base_color = NA,
    selected_color = NA,
    draw_borders = FALSE,
    show_base = TRUE,
    fill_na = "grey80",
    use_viridis = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("plot_spatial_features() requires the 'sf' package.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_spatial_features() requires the 'ggplot2' package.", call. = FALSE)
  }
  has_viridis <- requireNamespace("viridis", quietly = TRUE)

  value <- match.arg(value)
  if (is.null(layout)) layout <- "facet"
  layout <- match.arg(layout, c("single", "facet"))

  if (!isTRUE(draw_borders)) {
    selected_color <- NA
  }

  solutions <- .pa_plot_spatial_resolve_solutions(x, runs = runs)
  multi_runs <- length(solutions) > 1L

  if (isTRUE(multi_runs)) {
    if (is.null(features) || length(features) != 1L) {
      stop(
        "When plotting multiple runs for features, `features` must specify exactly one feature.",
        call. = FALSE
      )
    }
  }

  pu_sf_min <- .pa_plot_spatial_get_geometry(solutions)
  pr <- solutions[[1]]$problem

  distf <- pr$data$dist_features %||% NULL
  feats <- pr$data$features %||% NULL

  if (is.null(distf) || !inherits(distf, "data.frame")) {
    stop("Missing problem$data$dist_features.", call. = FALSE)
  }
  if (is.null(feats) || !inherits(feats, "data.frame")) {
    stop("Missing problem$data$features.", call. = FALSE)
  }
  if (!all(c("pu", "feature", "amount") %in% names(distf))) {
    stop("dist_features must contain 'pu', 'feature', and 'amount'.", call. = FALSE)
  }

  feat_map <- feats[, intersect(c("id", "name"), names(feats)), drop = FALSE]
  if (!("name" %in% names(feat_map))) feat_map$name <- as.character(feat_map$id)

  feature_frames <- vector("list", length(solutions))
  for (i in seq_along(solutions)) {
    sol_i <- solutions[[i]]
    run_i <- as.integer(names(solutions)[i])

    base_tbl <- stats::aggregate(
      amount ~ pu + feature,
      data = distf,
      FUN = sum
    )
    names(base_tbl)[names(base_tbl) == "amount"] <- "baseline"

    eff <- sol_i$problem$data$dist_effects_model %||% sol_i$problem$data$dist_effects %||% NULL
    ben_tbl <- NULL

    if (!is.null(eff) &&
        inherits(eff, "data.frame") &&
        all(c("pu", "feature", "action", "benefit") %in% names(eff))) {

      act_sel <- get_actions(sol_i, only_selected = TRUE)
      if (all(c("pu", "action") %in% names(act_sel))) {
        key_sel <- paste(act_sel$pu, act_sel$action, sep = "||")
        key_eff <- paste(eff$pu, eff$action, sep = "||")
        eff2 <- eff[key_eff %in% key_sel, , drop = FALSE]

        if (nrow(eff2) > 0L) {
          ben_tbl <- stats::aggregate(
            benefit ~ pu + feature,
            data = eff2,
            FUN = sum
          )
          names(ben_tbl)[names(ben_tbl) == "benefit"] <- "benefit"
        }
      }
    }

    if (is.null(ben_tbl)) {
      ben_tbl <- data.frame(
        pu = integer(0),
        feature = numeric(0),
        benefit = numeric(0)
      )
    }

    ff <- merge(base_tbl, ben_tbl, by = c("pu", "feature"), all = TRUE)
    ff$baseline[is.na(ff$baseline)] <- 0
    ff$benefit[is.na(ff$benefit)] <- 0
    ff$final <- ff$baseline + ff$benefit

    ff <- merge(ff, feat_map, by.x = "feature", by.y = "id", all.x = TRUE)
    ff$feature_label <- as.character(ff$name)
    ff$run_id <- run_i

    if (!is.null(features)) {
      features_chr <- as.character(features)
      ff <- ff[
        ff$feature_label %in% features_chr |
          as.character(ff$feature) %in% features_chr,
        ,
        drop = FALSE
      ]
    }

    feature_frames[[i]] <- ff
  }

  ff <- do.call(rbind, feature_frames)
  if (nrow(ff) == 0L) {
    stop("No features available to plot for the requested subset.", call. = FALSE)
  }

  if (!isTRUE(multi_runs)) {
    feat_levels <- unique(ff$feature_label)
    if (identical(layout, "facet") && is.null(features) && length(feat_levels) > max_facets) {
      warning(
        "Showing only the first ", max_facets,
        " features. Use features=... or increase max_facets.",
        call. = FALSE
      )
      keep_feats <- feat_levels[seq_len(max_facets)]
      ff <- ff[ff$feature_label %in% keep_feats, , drop = FALSE]
    }
  }

  names(ff)[names(ff) == "pu"] <- "id"
  g <- merge(pu_sf_min, ff[, c("id", "feature_label", "run_id", value)], by = "id", all.y = TRUE)
  g <- sf::st_as_sf(g)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = g,
      ggplot2::aes(fill = .data[[value]]),
      color = selected_color,
      alpha = 1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Spatial", value, "by feature"), fill = value)

  if (isTRUE(multi_runs)) {
    p <- p + ggplot2::facet_wrap(~run_id)
  } else if (identical(layout, "facet")) {
    p <- p + ggplot2::facet_wrap(~feature_label)
  }

  if (isTRUE(use_viridis) && has_viridis) {
    p <- p + viridis::scale_fill_viridis(option = "C", na.value = fill_na)
  }

  print(p)
  invisible(p)
}


#' @title Plot trade-offs from a multi-objective solution set
#'
#' @description
#' Plot pairwise trade-offs among objective values stored in a
#' \code{SolutionSet}.
#'
#' This function is intended for multi-objective workflows in which the solution
#' set contains one row per run and one or more objective value columns of the
#' form \code{value_*}.
#'
#' If exactly two objectives are selected, the function returns a single
#' scatterplot. If three or more objectives are selected, all pairwise
#' combinations are plotted using facets.
#'
#' @details
#' This function reads the run-level table stored in \code{x$solution$runs}. It
#' expects objective values to be stored in columns whose names begin with
#' \code{"value_"}.
#'
#' If the available objective columns are, for example,
#' \code{value_cost}, \code{value_benefit}, and \code{value_frag}, then the
#' corresponding objective aliases are \code{"cost"}, \code{"benefit"}, and
#' \code{"frag"}.
#'
#' Let \eqn{f_k(r)} denote the value of objective \eqn{k} in run \eqn{r}. This
#' function visualizes pairwise projections of the run table of the form:
#' \deqn{
#' \left(f_k(r), f_\ell(r)\right)
#' }
#' for selected pairs of objectives \eqn{k,\ell}.
#'
#' If exactly two objectives are selected, a single panel is produced.
#'
#' If three or more objectives are selected, all pairwise combinations are
#' generated:
#' \deqn{
#' \{(k,\ell): k < \ell,\; k,\ell \in \mathcal{O}\},
#' }
#' where \eqn{\mathcal{O}} is the selected set of objective aliases.
#'
#' By default, plotting more than four objectives is not allowed unless
#' \code{all_pairs = TRUE}, because the number of panels grows quadratically in
#' the number of objectives.
#'
#' \strong{Colouring}
#'
#' If \code{color_by} is supplied, points are coloured by either:
#' \itemize{
#'   \item one of the selected objective aliases, in which case the
#'   corresponding \code{value_*} column is used,
#'   \item or one of the run-level columns \code{run_id}, \code{status},
#'   \code{runtime}, or \code{gap}.
#' }
#'
#' \strong{Connecting runs}
#'
#' If \code{connect = TRUE}, runs are connected in their current table order
#' within each panel. This can be useful when runs correspond to an ordered scan
#' of weights, \eqn{\epsilon}-levels, or frontier points, but it should be used
#' with care when run order has no substantive meaning.
#'
#' \strong{Run labels}
#'
#' If \code{label_runs = TRUE}, each point is labelled by its \code{run_id}. If
#' the \pkg{ggrepel} package is available, repelled labels are used.
#'
#' @param x A \code{SolutionSet} object.
#' @param objectives Optional character vector of objective aliases to display.
#'   These must match the suffixes of the \code{value_*} columns in
#'   \code{x$solution$runs}. If \code{NULL}, all available objective columns are
#'   used.
#' @param color_by Optional character scalar used to colour points. This may be
#'   either one of the selected objective aliases or one of the run-level
#'   columns \code{"run_id"}, \code{"status"}, \code{"runtime"}, or
#'   \code{"gap"}.
#' @param all_pairs Logical. If \code{TRUE}, allow plotting all pairwise
#'   combinations even when more than four objectives are selected. If
#'   \code{NULL}, it is treated as \code{FALSE}.
#' @param connect Logical. If \code{TRUE}, connect points by run order within
#'   each panel.
#' @param label_runs Logical. If \code{TRUE}, add run labels to points.
#' @param point_size Numeric point size.
#' @param line_alpha Numeric alpha value for connecting lines.
#' @param text_size Numeric size for run labels.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns a \code{ggplot} object.
#'
#' @seealso
#' \code{\link{solve}},
#' \code{\link{get_solution_vector}}
#'
#' @export
plot_tradeoff <- function(
    x,
    objectives = NULL,
    color_by = NULL,
    all_pairs = NULL,
    connect = FALSE,
    label_runs = FALSE,
    point_size = 3,
    line_alpha = 0.5,
    text_size = 3,
    ...
) {
  if (!inherits(x, "SolutionSet")) {
    stop("plot_tradeoff() requires a SolutionSet object.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_tradeoff() requires the 'ggplot2' package.", call. = FALSE)
  }

  runs <- x$solution$runs %||% NULL
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    stop("No run table found in x$solution$runs.", call. = FALSE)
  }

  value_cols <- grep("^value_", names(runs), value = TRUE)
  if (length(value_cols) < 2L) {
    stop("At least two objective value columns ('value_*') are required.", call. = FALSE)
  }

  available_obj <- sub("^value_", "", value_cols)

  if (is.null(objectives)) {
    objectives <- available_obj
  } else {
    objectives <- unique(as.character(objectives))
    objectives <- objectives[!is.na(objectives) & nzchar(objectives)]
    if (length(objectives) < 2L) {
      stop("`objectives` must contain at least two objective names.", call. = FALSE)
    }
    bad <- setdiff(objectives, available_obj)
    if (length(bad) > 0L) {
      stop(
        "Unknown objective(s): ", paste(bad, collapse = ", "),
        ". Available objectives are: ", paste(available_obj, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  all_pairs <- isTRUE(all_pairs)

  if (length(objectives) > 4L && !all_pairs) {
    stop(
      "More than four objectives were selected (", length(objectives), "). ",
      "Please specify a smaller set in `objectives = ...` or use `all_pairs = TRUE`.",
      call. = FALSE
    )
  }

  if (!is.null(color_by)) {
    color_by <- as.character(color_by)[1]
    valid_color_vars <- c(objectives, "run_id", "status", "runtime", "gap")
    if (is.na(color_by) || !nzchar(color_by) || !color_by %in% valid_color_vars) {
      stop(
        "`color_by` must be one of: ",
        paste(valid_color_vars, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  pair_mat <- utils::combn(objectives, 2)
  pair_df <- vector("list", ncol(pair_mat))

  for (i in seq_len(ncol(pair_mat))) {
    ox <- pair_mat[1, i]
    oy <- pair_mat[2, i]

    dd <- data.frame(
      run_id = runs$run_id %||% seq_len(nrow(runs)),
      run_label = paste("run", runs$run_id %||% seq_len(nrow(runs))),
      status = if ("status" %in% names(runs)) runs$status else NA_character_,
      runtime = if ("runtime" %in% names(runs)) runs$runtime else NA_real_,
      gap = if ("gap" %in% names(runs)) runs$gap else NA_real_,
      obj_x = ox,
      obj_y = oy,
      x = as.numeric(runs[[paste0("value_", ox)]]),
      y = as.numeric(runs[[paste0("value_", oy)]]),
      pair = paste0(ox, " vs ", oy),
      stringsAsFactors = FALSE
    )

    if (!is.null(color_by)) {
      dd$color_value <- if (color_by %in% objectives) {
        runs[[paste0("value_", color_by)]]
      } else {
        runs[[color_by]]
      }
    }

    dd <- dd[order(dd$run_id), , drop = FALSE]
    pair_df[[i]] <- dd
  }

  plot_df <- do.call(rbind, pair_df)

  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey80"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y))

  if (isTRUE(connect)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(group = 1),
      alpha = line_alpha,
      linewidth = 0.5,
      colour = "grey50"
    )
  }

  if (is.null(color_by)) {
    p <- p + ggplot2::geom_point(
      size = point_size,
      shape = 21,
      fill = "#2C7FB8",
      colour = "white",
      stroke = 0.3
    )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(fill = color_value),
      size = point_size,
      shape = 21,
      colour = "white",
      stroke = 0.3
    )
  }

  if (isTRUE(label_runs)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$run_label),
        size = text_size,
        max.overlaps = Inf,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$run_label),
        size = text_size,
        vjust = -0.6,
        show.legend = FALSE
      )
    }
  }

  if (ncol(pair_mat) == 1L) {
    p <- p +
      ggplot2::labs(
        x = unique(plot_df$obj_x),
        y = unique(plot_df$obj_y),
        fill = if (!is.null(color_by)) color_by else NULL
      ) +
      base_theme
  } else {
    p <- p +
      ggplot2::facet_wrap(~pair, scales = "free") +
      ggplot2::labs(
        x = "Objective value",
        y = "Objective value",
        fill = if (!is.null(color_by)) color_by else NULL
      ) +
      base_theme
  }

  print(p)
  invisible(p)
}
