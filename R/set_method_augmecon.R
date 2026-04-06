#' @title Set the AUGMECON multi-objective method
#'
#' @description
#' Configure a \code{Problem} object to be solved with the augmented
#' \eqn{\epsilon}-constraint method (AUGMECON).
#'
#' AUGMECON is an exact multi-objective optimization method in which one
#' objective is treated as the primary objective and the remaining objectives are
#' converted into \eqn{\epsilon}-constraints. In the augmented formulation, each
#' secondary objective is associated with a non-negative slack variable, and the
#' primary objective is augmented with a small reward term based on the
#' normalized slacks. This augmentation is used to avoid weakly efficient
#' solutions, following Mavrotas (2009). AUGMECON is generally preferable to the
#' standard epsilon-constraint method when the goal is to generate a well-distributed
#' set of efficient solutions while reducing weakly efficient points.
#'
#' This function does not solve the problem directly. It stores the AUGMECON
#' configuration in \code{x$data$method}, to be used later by
#' \code{\link{solve}}.
#'
#' @details
#' \strong{General idea}
#'
#' Suppose that \eqn{m \ge 2} objective functions have been registered in the
#' problem:
#' \deqn{
#' f_1(x), f_2(x), \dots, f_m(x).
#' }
#'
#' AUGMECON selects one of them as the primary objective, say
#' \eqn{f_p(x)}, and treats the remaining \eqn{m - 1} objectives as secondary
#' objectives. For a fixed combination of \eqn{\epsilon}-levels, the method
#' solves a sequence of single-objective subproblems of the form:
#'
#' \deqn{
#' \max \; f_p(x) + \rho \sum_{k \in \mathcal{S}} \frac{s_k}{R_k}
#' }
#'
#' subject to
#'
#' \deqn{
#' f_k(x) - s_k = \epsilon_k, \qquad k \in \mathcal{S},
#' }
#'
#' \deqn{
#' s_k \ge 0, \qquad k \in \mathcal{S},
#' }
#'
#' together with all original feasibility constraints of the planning problem.
#'
#' Here:
#' \itemize{
#'   \item \eqn{f_p(x)} is the primary objective,
#'   \item \eqn{\mathcal{S}} is the set of secondary objectives,
#'   \item \eqn{\epsilon_k} is the imposed level for secondary objective
#'   \eqn{k},
#'   \item \eqn{s_k} is a non-negative slack variable,
#'   \item \eqn{R_k} is the payoff-table range used to normalize objective
#'   \eqn{k},
#'   \item \eqn{\rho > 0} is a small augmentation coefficient.
#' }
#'
#' In the original AUGMECON formulation of Mavrotas (2009), the augmentation
#' term ensures that, among solutions with the same primary objective value, the
#' solver prefers those with larger slack, thereby avoiding weakly efficient
#' points and improving Pareto-front generation.
#'
#' \strong{Secondary-objective equalities and slacks}
#'
#' The key difference between standard \eqn{\epsilon}-constraint and AUGMECON is
#' that the secondary objectives are written as equalities with slacks rather
#' than as simple inequalities. For a maximization-type secondary objective,
#' this takes the form:
#'
#' \deqn{
#' f_k(x) - s_k = \epsilon_k, \qquad s_k \ge 0.
#' }
#'
#' This implies:
#' \deqn{
#' f_k(x) \ge \epsilon_k,
#' }
#'
#' while explicitly measuring the excess above the imposed
#' \eqn{\epsilon}-level through \eqn{s_k}. The augmentation term then rewards
#' such excess in normalized form.
#'
#' In implementation terms, the exact sign convention for each objective depends
#' on whether it is internally treated as a minimization or maximization
#' objective, but the method always preserves the same AUGMECON principle:
#' \itemize{
#'   \item one objective is optimized directly,
#'   \item all others are turned into constrained objectives,
#'   \item non-negative slacks measure controlled deviation from the imposed
#'   \eqn{\epsilon}-levels,
#'   \item the primary objective is augmented with a small slack-based reward.
#' }
#'
#' \strong{Automatic and manual grids}
#'
#' AUGMECON requires a grid of \eqn{\epsilon}-levels for each secondary
#' objective.
#'
#' If \code{grid} is supplied, it must be a named list with one numeric vector
#' per secondary objective. Each vector defines the exact
#' \eqn{\epsilon}-levels to be explored for that objective.
#'
#' If \code{grid = NULL}, the grid is generated automatically later during
#' \code{\link{solve}}. In that case, the method first computes extreme points
#' and payoff-table ranges for the secondary objectives, and then generates
#' \code{n_points} levels for each one.
#'
#' If \code{include_extremes = TRUE}, the automatic grid includes the extreme
#' values of each secondary objective.
#'
#' If \code{lexicographic = TRUE}, extreme points are computed using
#' lexicographic anchoring, which can improve numerical stability and payoff
#' table quality when objectives are tightly competing. The tolerance used for
#' lexicographic anchoring is controlled by \code{lexicographic_tol}.
#'
#' \strong{Normalization and augmentation}
#'
#' The augmentation term is scaled using the payoff-table ranges of the
#' secondary objectives. If \eqn{R_k} denotes the range of secondary objective
#' \eqn{k}, then the effective coefficient applied to the slack is:
#'
#' \deqn{
#' \frac{\rho}{R_k},
#' }
#'
#' where \eqn{\rho = \code{augmentation}}.
#'
#' This normalization is important because different objectives may be measured
#' on very different numerical scales. Without normalization, a slack belonging
#' to a large-scale objective could dominate the augmentation term simply due to
#' units.
#'
#' In this implementation, the user supplies \code{augmentation} as the base
#' coefficient \eqn{\rho}, while the normalized slack coefficients are computed
#' internally at solve time using the corresponding payoff-table ranges.
#'
#' \strong{Stored configuration}
#'
#' This function stores the method definition in \code{x$data$method} with:
#' \itemize{
#'   \item \code{name = "augmecon"},
#'   \item the primary objective alias,
#'   \item the full set of participating aliases,
#'   \item the set of secondary aliases,
#'   \item either a manual grid or the information required to generate one
#'   automatically,
#'   \item augmentation and slack-bound parameters.
#' }
#'
#' The actual payoff table, grid construction, and subproblem solution loop are
#' performed later by \code{\link{solve}}.
#'
#' @param x A \code{Problem} object.
#' @param primary Character string giving the alias of the primary objective,
#'   that is, the objective optimized directly in the AUGMECON formulation.
#' @param aliases Optional character vector of objective aliases to include in
#'   the method. If \code{NULL}, all registered objective aliases are used. The
#'   value of \code{primary} must be included in \code{aliases}.
#' @param grid Optional named list defining manual \eqn{\epsilon}-levels for the
#'   secondary objectives. Each name must correspond to a secondary objective
#'   alias, and each element must be a non-empty numeric vector of finite values.
#'   If \code{NULL}, the grid is generated automatically.
#' @param n_points Number of automatically generated \eqn{\epsilon}-levels per
#'   secondary objective when \code{grid = NULL}. Must be at least 2. Ignored
#'   when \code{grid} is supplied.
#' @param include_extremes Logical. If \code{TRUE}, automatically generated
#'   grids include extreme values of each secondary objective.
#' @param lexicographic Logical. If \code{TRUE}, use lexicographic anchoring
#'   when computing extreme points for automatic grid construction.
#' @param lexicographic_tol Non-negative numeric tolerance used in
#'   lexicographic anchoring.
#' @param augmentation Positive numeric augmentation coefficient
#'   \eqn{\rho}. The effective coefficient of each secondary slack is computed
#'   internally as \eqn{\rho / R_k}, where \eqn{R_k} is the payoff-table range
#'   of the corresponding secondary objective.
#' @param slack_upper_bound Positive numeric upper bound imposed on the explicit
#'   non-negative slack variables associated with the secondary objectives.
#'
#' @return The updated \code{Problem} object with the AUGMECON method
#'   configuration stored in \code{x$data$method}.
#'
#' @references
#' Mavrotas, G. (2009). Effective implementation of the
#' \eqn{\epsilon}-constraint method in multi-objective mathematical programming
#' problems. \emph{Applied Mathematics and Computation}, 213(2), 455--465.
#'
#' @examples
#' \dontrun{
#' # Automatic epsilon grids from payoff ranges
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
#' # Manual epsilon grids for two secondary objectives
#' p <- p |>
#'   set_method_augmecon(
#'     primary = "benefit",
#'     aliases = c("benefit", "cost", "frag"),
#'     grid = list(
#'       cost = c(100, 150, 200, 250),
#'       frag = c(2, 4, 6, 8)
#'     ),
#'     augmentation = 1e-3,
#'     slack_upper_bound = 1e6
#'   )
#' }
#'
#' @seealso
#' \code{\link{set_method_epsilon_constraint}},
#' \code{\link{set_method_weighted_sum}},
#' \code{\link{solve}}
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
    aliases <- .pamo_get_specs(x)
    obj_alias <- names(aliases)
    aliases <- obj_alias
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
