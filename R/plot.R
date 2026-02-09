#' @title Plot a Solution (if spatial geometry is available)
#'
#' @description
#' Plot a solution using planning unit (PU) geometries when available.
#'
#' The method supports two views controlled by \code{what}:
#' \itemize{
#'   \item \code{"pu"}: highlights selected planning units.
#'   \item \code{"actions"}: highlights selected actions within planning units. Optionally,
#'   selected PUs with no selected action can be labelled as \dQuote{conservation}.
#' }
#'
#' For \code{what = "actions"}, the function aggregates selected actions per PU into a
#' single label by concatenating unique action names using \code{"+"}
#' (e.g., \code{"harvest+sustainable"}). If many actions can co-occur in the same PU,
#' the number of distinct labels can grow quickly; in such cases, consider filtering a
#' single action via \code{action = "..."}, or ensure at most one action per PU.
#'
#' @details
#' This plotting method requires PU geometry stored as an \code{sf} object in either
#' \code{x$Data$data$pu_sf} or \code{x$data$pu_sf}. The geometry must contain an \code{id}
#' column matching planning unit identifiers.
#'
#' Colors for \code{what = "actions"} are controlled by either:
#' \itemize{
#'   \item \code{fill_values}: a named vector mapping action labels to colors; or
#'   \itemize{
#'     \item a discrete \pkg{viridis} palette if \code{fill_values} is \code{NULL},
#'     \code{use_viridis = TRUE}, and the \pkg{viridis} package is installed.
#'   }
#' }
#'
#' @param x A \code{Solution} object. It must contain PU geometry as an \code{sf} object
#'   in \code{x$Data$data$pu_sf} (preferred) or \code{x$data$pu_sf}.
#' @param what Character string indicating what to plot. One of \code{"pu"} or \code{"actions"}.
#' @param only_selected Logical. Only used when \code{what = "pu"}. If \code{TRUE}, throw an
#'   error when no selected PUs exist. If \code{FALSE}, the map is still drawn with the base layer.
#' @param action Optional character scalar. Only used when \code{what = "actions"}.
#'   If provided, filter the plot to a single action label.
#' @param ... Additional arguments (reserved for future extensions).
#' @param include_conservation Logical. Only used when \code{what = "actions"}.
#'   If \code{TRUE}, selected PUs that have no selected action are added to the map with
#'   label \code{conservation_label}.
#' @param conservation_label Character scalar. Label used for selected PUs with no selected action
#'   when \code{include_conservation = TRUE}.
#' @param base_alpha Numeric in \eqn{[0,1]}. Alpha used for the base PU layer.
#' @param selected_alpha Numeric in \eqn{[0,1]}. Alpha used for highlighted (selected) geometries.
#' @param base_fill Fill color for the base PU layer.
#' @param base_color Border color for the base PU layer. Use \code{NA} for no border.
#' @param selected_color Border color for highlighted geometries. Use \code{NA} for no border.
#' @param fill_values Optional named character vector of colors. Names must match the labels shown
#'   in the legend (i.e., values of the \code{action} field used in the plot, including
#'   \code{conservation_label} if enabled). If provided, overrides \pkg{viridis}.
#' @param fill_na Color used for missing values in the manual scale (only relevant when
#'   \code{fill_values} is provided).
#' @param use_viridis Logical. If \code{TRUE} and \code{fill_values} is \code{NULL}, use a
#'   discrete \pkg{viridis} palette when \pkg{viridis} is installed.
#'
#' @return Invisibly returns a \code{ggplot} object. The plot is printed as a side effect.
#'
#' @examples
#' \dontrun{
#' # Plot selected planning units
#' plot(sol, what = "pu")
#'
#' # Plot selected actions
#' plot(sol, what = "actions")
#'
#' # Plot a single action only
#' plot(sol, what = "actions", action = "harvest")
#'
#' # Manual colors for actions (and conservation)
#' cols <- c(harvest = "#E41A1C", sustainable = "#4DAF4A", conservation = "#377EB8")
#' plot(sol, what = "actions", fill_values = cols, conservation_label = "conservation")
#' }
#'
#' @seealso \code{\link{get_pu}}, \code{\link{get_actions}}
#'
#' @export
#' @method plot Solution

plot.Solution <- function(
    x,
    what = c("pu", "actions"),
    only_selected = FALSE,
    action = NULL,
    ...,
    include_conservation = TRUE,
    conservation_label = "conservation",
    base_alpha = 0.10,
    selected_alpha = 0.90,
    base_fill = "grey60",
    base_color = NA,
    selected_color = NA,
    fill_values = NULL,   # named vector: names must match labels in g_sel$action
    fill_na = "grey60",
    use_viridis = TRUE
) {
  what <- match.arg(what)

  if (!requireNamespace("sf", quietly = TRUE)) stop("Plotting requires the 'sf' package.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Plotting requires the 'ggplot2' package.", call. = FALSE)
  has_viridis <- requireNamespace("viridis", quietly = TRUE)

  # ---- PU geometry
  pu_sf <- NULL
  if (!is.null(x$Data) && !is.null(x$Data$data$pu_sf) && inherits(x$Data$data$pu_sf, "sf")) {
    pu_sf <- x$Data$data$pu_sf
  } else if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) {
    pu_sf <- x$data$pu_sf
  }
  if (is.null(pu_sf)) stop("No PU geometry found to plot (need sf in x$Data$data$pu_sf).", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop("PU geometry (sf) must have an 'id' column.", call. = FALSE)
  pu_sf$id <- as.integer(pu_sf$id)

  # base layer
  p_base <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = pu_sf, fill = base_fill, color = base_color, alpha = base_alpha) +
    ggplot2::theme_minimal()

  if (identical(what, "pu")) {

    pu_tbl <- get_pu(x, only_selected = FALSE)
    if (!all(c("id", "selected") %in% names(pu_tbl))) stop("PU table must contain 'id' and 'selected'.", call. = FALSE)
    pu_tbl$id <- as.integer(pu_tbl$id)

    g <- sf::st_as_sf(merge(pu_sf, pu_tbl[, c("id", "selected")], by = "id", all.x = TRUE))
    g$selected[is.na(g$selected)] <- 0L

    g_sel <- g[g$selected %in% 1L, , drop = FALSE]
    if (isTRUE(only_selected) && nrow(g_sel) == 0) stop("No selected PUs to plot.", call. = FALSE)

    p <- p_base +
      ggplot2::geom_sf(data = g_sel, fill = "dodgerblue3", color = selected_color, alpha = selected_alpha) +
      ggplot2::labs(title = "Selected planning units")

    print(p)
    return(invisible(p))
  }

  # -----------------------------
  # what == "actions"
  # -----------------------------

  pu_tbl <- get_pu(x, only_selected = FALSE)
  if (!all(c("id", "selected") %in% names(pu_tbl))) stop("PU table must contain 'id' and 'selected'.", call. = FALSE)
  pu_tbl$id <- as.integer(pu_tbl$id)
  pu_tbl$selected[is.na(pu_tbl$selected)] <- 0L

  act_tbl <- get_actions(x, only_selected = TRUE)
  if (!all(c("pu", "action", "selected") %in% names(act_tbl))) {
    stop("Actions table must contain 'pu', 'action', 'selected'.", call. = FALSE)
  }
  act_tbl$pu <- as.integer(act_tbl$pu)
  act_tbl$action <- as.character(act_tbl$action)

  if (!is.null(action)) {
    action <- as.character(action)[1]
    act_tbl <- act_tbl[act_tbl$action == action, , drop = FALSE]
  }

  # One label per PU for actions
  if (nrow(act_tbl) > 0) {
    lab_act <- stats::aggregate(
      action ~ pu,
      data = act_tbl,
      FUN = function(z) paste(unique(z), collapse = "+")
    )
    names(lab_act)[names(lab_act) == "pu"] <- "id"
    lab_act$id <- as.integer(lab_act$id)
  } else {
    lab_act <- data.frame(id = integer(0), action = character(0))
  }

  # Add "conservation" for selected PUs with no selected action
  if (isTRUE(include_conservation)) {
    sel_pus <- pu_tbl$id[pu_tbl$selected %in% 1L]
    has_action_pus <- unique(lab_act$id)
    cons_ids <- setdiff(sel_pus, has_action_pus)

    if (length(cons_ids) > 0) {
      lab_cons <- data.frame(
        id = as.integer(cons_ids),
        action = as.character(conservation_label),
        stringsAsFactors = FALSE
      )
      lab_act <- rbind(lab_act, lab_cons)
    }
  }

  if (nrow(lab_act) == 0) stop("No selected actions (and no conservation PUs) to plot.", call. = FALSE)

  g_sel <- sf::st_as_sf(merge(pu_sf, lab_act, by = "id", all.x = FALSE))
  if (nrow(g_sel) == 0) stop("No geometry matched the action/conservation labels.", call. = FALSE)

  p <- p_base +
    ggplot2::geom_sf(
      data = g_sel,
      ggplot2::aes(fill = action),
      color = selected_color,
      alpha = selected_alpha
    ) +
    ggplot2::labs(title = "", fill = "")

  # ---- choose fill scale (manual overrides viridis)
  if (!is.null(fill_values)) {
    if (is.null(names(fill_values)) || any(names(fill_values) == "")) {
      stop("fill_values must be a *named* vector with names matching action labels.", call. = FALSE)
    }
    p <- p + ggplot2::scale_fill_manual(values = fill_values, na.value = fill_na)
  } else if (isTRUE(use_viridis) && has_viridis) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
  }

  print(p)
  invisible(p)
}
