#' @title Plot a Solution (if spatial geometry is available)
#' @export
#' @method plot Solution
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
    selected_color = NA
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
  # what == "actions" (extended)
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
  lab_act <- NULL
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
    ggplot2::labs(title = "Selected actions (+ conservation when no action)", fill = "allocation")

  if (has_viridis) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
  }

  print(p)
  invisible(p)
}
