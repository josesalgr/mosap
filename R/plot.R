#' @title Plot a Solution (if spatial geometry is available)
#'
#' @description
#' Plots the solution using planning-unit geometry when available (sf).
#' If no geometry is present, an error is thrown with guidance.
#'
#' @param x A [solution-class] object.
#' @param what What to plot: "pu" (selected PUs) or "actions" (selected actions by PU).
#' @param only_selected For what="pu", show only selected PUs?
#' @param action If what="actions", optionally select a single action id to highlight.
#' @param ... Passed to graphics.
#'
#' @return Invisibly returns the ggplot object.
#' @export
#' @method plot Solution
plot.Solution <- function(
    x,
    what = c("pu", "actions"),
    only_selected = FALSE,
    action = NULL,
    ...
) {
  what <- match.arg(what)

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Plotting requires the 'sf' package.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires the 'ggplot2' package.", call. = FALSE)
  }

  # ---- retrieve PU geometry
  pu_sf <- NULL

  if (!is.null(x$Data) && !is.null(x$Data$data$pu_sf) && inherits(x$Data$data$pu_sf, "sf")) {
    pu_sf <- x$Data$data$pu_sf
  } else if (!is.null(x$data$pu_sf) && inherits(x$data$pu_sf, "sf")) {
    pu_sf <- x$data$pu_sf
  }

  if (is.null(pu_sf)) {
    stop(
      "No PU geometry found to plot.\n",
      "Expected x$Data$data$pu_sf (sf) to exist. Make sure inputData() stored PU geometry, ",
      "or use a vector-PU workflow (sf/SpatVector).",
      call. = FALSE
    )
  }
  if (!("id" %in% names(pu_sf))) {
    stop("PU geometry (sf) must have an 'id' column.", call. = FALSE)
  }

  # ensure id type matches solution tables
  pu_sf$id <- as.integer(pu_sf$id)

  if (identical(what, "pu")) {

    pu_tbl <- get_pu(x, only_selected = FALSE)
    if (!all(c("id", "selected") %in% names(pu_tbl))) {
      stop("PU table must contain columns 'id' and 'selected'.", call. = FALSE)
    }
    pu_tbl$id <- as.integer(pu_tbl$id)

    g <- sf::st_as_sf(merge(pu_sf, pu_tbl[, c("id", "selected")], by = "id", all.x = TRUE))

    if (isTRUE(only_selected)) {
      g <- g[g$selected %in% 1L, , drop = FALSE]
    }

    p <- ggplot2::ggplot(g) +
      ggplot2::geom_sf(ggplot2::aes(fill = factor(selected)), color = NA) +
      ggplot2::labs(fill = "selected")

    print(p)
    return(invisible(p))

  } else {

    act_tbl <- get_actions(x, only_selected = TRUE)

    if (!all(c("pu", "action", "selected") %in% names(act_tbl))) {
      stop("Actions table must contain columns 'pu', 'action', 'selected'.", call. = FALSE)
    }

    act_tbl$pu <- as.integer(act_tbl$pu)
    act_tbl$action <- as.character(act_tbl$action)

    if (!is.null(action)) {
      action <- as.character(action)[1]
      act_tbl <- act_tbl[act_tbl$action == action, , drop = FALSE]
    }

    if (nrow(act_tbl) == 0) {
      stop("No selected actions to plot (after filtering).", call. = FALSE)
    }

    # One label per PU: if multiple actions selected per PU, collapse
    lab <- stats::aggregate(
      action ~ pu,
      data = act_tbl,
      FUN = function(z) paste(unique(z), collapse = "+")
    )
    names(lab)[names(lab) == "pu"] <- "id"
    lab$id <- as.integer(lab$id)

    g <- sf::st_as_sf(merge(pu_sf, lab, by = "id", all.x = FALSE))

    p <- ggplot2::ggplot(g) +
      ggplot2::geom_sf(ggplot2::aes(fill = action), color = NA) +
      ggplot2::labs(fill = "action")

    print(p)
    return(invisible(p))
  }
}
