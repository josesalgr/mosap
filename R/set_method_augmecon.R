#' Set the AUGMECON multi-objective method
#'
#' Configure a problem to be solved with the augmented epsilon-constraint
#' method (AUGMECON). One objective is treated as the primary objective and
#' the remaining objectives are converted into epsilon constraints.
#'
#' In the AUGMECON formulation, each secondary objective is enforced through
#' an epsilon equality with a non-negative slack variable, and the primary
#' objective is augmented with a small reward term based on the normalized
#' slacks. This augmentation helps avoid weakly efficient solutions, following
#' the formulation proposed by Mavrotas (2009).
#'
#' The set of epsilon levels for secondary objectives can be supplied
#' manually through `grid`, or generated automatically when `grid = NULL`.
#'
#' @param x A problem object.
#'
#' @param primary `character(1)`. Alias of the primary objective, i.e. the
#'   objective optimized directly in the AUGMECON formulation.
#'
#' @param aliases `NULL` or `character`. Objective aliases to include in the
#'   method. If `NULL`, all registered objective aliases are used. `primary`
#'   must be included in `aliases`.
#'
#' @param grid `NULL` or a named `list`. Epsilon levels for the secondary
#'   objectives. When `NULL`, epsilon levels are generated automatically.
#'   When supplied, it must be a named list with one numeric vector per
#'   secondary objective.
#'
#' @param n_points `numeric(1)`. Number of automatically generated epsilon
#'   levels per secondary objective when `grid = NULL`. Must be at least 2.
#'   Ignored when `grid` is supplied.
#'
#' @param include_extremes `logical(1)`. Should automatically generated grids
#'   include the extreme values of each secondary objective?
#'
#' @param lexicographic `logical(1)`. Should lexicographic anchoring be used
#'   when computing extreme points for automatic grid construction?
#'
#' @param lexicographic_tol `numeric(1)`. Non-negative tolerance used in
#'   lexicographic anchoring.
#'
#' @param augmentation `numeric(1)`. Positive augmentation coefficient used
#'   to weight the normalized slack variables in the AUGMECON objective.
#'   Smaller values make the primary objective dominate more strongly, while
#'   larger values give more emphasis to slack maximization within the same
#'   epsilon-constrained region. The effective coefficient of each slack is
#'   computed internally as `augmentation / range`, where `range` is the
#'   payoff-table range of the corresponding secondary objective.
#'
#' @param slack_upper_bound `numeric(1)`. Upper bound used for the explicit
#'   non-negative slack variables associated with secondary objectives.
#'
#' @details
#' AUGMECON is an augmented version of the epsilon-constraint method. Let
#' `f_primary(x)` denote the primary objective and let the remaining objectives
#' be treated as secondary objectives. For each secondary objective, the method
#' introduces an epsilon equality with a slack variable. The objective then
#' optimizes the primary objective while rewarding normalized slack values.
#'
#' This follows the spirit of the AUGMECON formulation of Mavrotas (2009),
#' where augmentation is used to avoid weakly efficient solutions and improve
#' the generation of the Pareto frontier.
#'
#' If `grid = NULL`, the method will later build epsilon levels
#' automatically from the ranges of the secondary objectives. If `grid` is
#' supplied, those values are used directly.
#'
#' For `m` objectives, one is the `primary` objective and the remaining
#' `m - 1` objectives are treated as secondary objectives.
#'
#' @return The updated problem object with the AUGMECON method stored in
#'   `x$data$method`.
#'
#' @references
#' Mavrotas, G. (2009). Effective implementation of the epsilon-constraint
#' method in Multi-Objective Mathematical Programming problems.
#' \emph{Applied Mathematics and Computation}, 213(2), 455--465.
#'
#' @examples
#' \dontrun{
#' p <- p |>
#'   set_method_augmecon(
#'     primary = "benefit",
#'     aliases = c("benefit", "cost"),
#'     n_points = 10,
#'     include_extremes = TRUE,
#'     lexicographic = TRUE,
#'     augmentation = 1e-3
#'   )
#'
#' p <- p |>
#'   set_method_augmecon(
#'     primary = "benefit",
#'     aliases = c("benefit", "cost", "frag"),
#'     grid = list(
#'       cost = c(100, 150, 200, 250),
#'       frag = c(2, 4, 6, 8)
#'     ),
#'     augmentation = 1e-3
#'   )
#' }
#'
#' @export
set_method_augmecon <- function(
    x,
    primary,
    aliases = NULL,
    grid = NULL,
    n_points = 10,
    include_extremes = TRUE,
    lexicographic = TRUE,
    lexicographic_tol = 1e-9,
    augmentation = 1e-3,
    slack_upper_bound = 1e6
) {

  # ---- primary
  if (!is.character(primary) || length(primary) != 1L || is.na(primary) || !nzchar(primary)) {
    stop("`primary` must be a non-empty character string.", call. = FALSE)
  }
  primary <- as.character(primary)

  # ---- aliases
  if (is.null(aliases)) {
    aliases <- .pamo_list_objective_aliases(x)
  } else {
    if (!is.character(aliases) || length(aliases) == 0L || anyNA(aliases)) {
      stop("`aliases` must be NULL or a non-empty character vector without NA.", call. = FALSE)
    }
    aliases <- as.character(aliases)

    if (any(!nzchar(aliases))) {
      stop("`aliases` must not contain empty strings.", call. = FALSE)
    }

    if (anyDuplicated(aliases) != 0L) {
      dups <- unique(aliases[duplicated(aliases)])
      stop("`aliases` must not contain duplicates: ", paste(dups, collapse = ", "), call. = FALSE)
    }
  }

  # validate aliases exist
  .pamo_get_objective_specs(x, aliases)

  if (!(primary %in% aliases)) {
    stop("`primary` must be included in `aliases`.", call. = FALSE)
  }

  if (length(aliases) < 2L) {
    stop("AUGMECON requires at least 2 objectives.", call. = FALSE)
  }

  secondary <- setdiff(aliases, primary)
  if (length(secondary) == 0L) {
    stop("AUGMECON requires at least one secondary objective.", call. = FALSE)
  }

  # ---- common arguments
  if (!is.logical(include_extremes) || length(include_extremes) != 1L || is.na(include_extremes)) {
    stop("`include_extremes` must be TRUE or FALSE.", call. = FALSE)
  }
  include_extremes <- isTRUE(include_extremes)

  if (!is.logical(lexicographic) || length(lexicographic) != 1L || is.na(lexicographic)) {
    stop("`lexicographic` must be TRUE or FALSE.", call. = FALSE)
  }
  lexicographic <- isTRUE(lexicographic)

  if (!is.numeric(lexicographic_tol) || length(lexicographic_tol) != 1L ||
      is.na(lexicographic_tol) || !is.finite(lexicographic_tol) || lexicographic_tol < 0) {
    stop("`lexicographic_tol` must be a single finite non-negative number.", call. = FALSE)
  }
  lexicographic_tol <- as.numeric(lexicographic_tol)

  if (!is.numeric(augmentation) || length(augmentation) != 1L ||
      is.na(augmentation) || !is.finite(augmentation) || augmentation <= 0) {
    stop("`augmentation` must be a single finite positive number.", call. = FALSE)
  }
  augmentation <- as.numeric(augmentation)

  if (!is.numeric(slack_upper_bound) || length(slack_upper_bound) != 1L ||
      is.na(slack_upper_bound) || !is.finite(slack_upper_bound) || slack_upper_bound <= 0) {
    stop("`slack_upper_bound` must be a single finite positive number.", call. = FALSE)
  }
  slack_upper_bound <- as.numeric(slack_upper_bound)

  # ---- automatic grid
  if (is.null(grid)) {

    if (!is.numeric(n_points) || length(n_points) != 1L ||
        is.na(n_points) || !is.finite(n_points) || n_points < 2) {
      stop("`n_points` must be a single number >= 2 when `grid = NULL`.", call. = FALSE)
    }
    n_points <- as.integer(n_points)

  } else {

    # ---- manual grid
    if (is.atomic(grid) && !is.list(grid)) {
      stop("`grid` must be NULL or a named list.", call. = FALSE)
    }

    if (!is.list(grid) || length(grid) == 0L || is.null(names(grid)) || any(!nzchar(names(grid)))) {
      stop("`grid` must be a named non-empty list when supplied.", call. = FALSE)
    }

    gnames <- names(grid)

    extra <- setdiff(gnames, secondary)
    miss  <- setdiff(secondary, gnames)

    if (length(extra) > 0L) {
      stop(
        "`grid` contains names not corresponding to secondary objectives: ",
        paste(extra, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(miss) > 0L) {
      stop(
        "`grid` is missing secondary objectives: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }

    grid <- grid[secondary]

    grid <- lapply(seq_along(grid), function(i) {
      nm <- secondary[i]
      v  <- grid[[i]]

      if (!is.numeric(v) || length(v) == 0L || anyNA(v) || any(!is.finite(v))) {
        stop("`grid[['", nm, "']]` must be a non-empty numeric vector of finite values.", call. = FALSE)
      }

      sort(unique(as.numeric(v)))
    })
    names(grid) <- secondary

    n_points <- NULL
  }

  x$data$method <- list(
    name = "augmecon",
    primary = primary,
    aliases = aliases,
    secondary = secondary,
    grid = grid,
    n_points = n_points,
    include_extremes = include_extremes,
    lexicographic = lexicographic,
    lexicographic_tol = lexicographic_tol,
    augmentation = augmentation,
    slack_upper_bound = slack_upper_bound
  )

  x
}
