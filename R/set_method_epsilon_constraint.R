#' @title Set the epsilon-constraint multi-objective method
#'
#' @description
#' Configure a \code{Problem} object to be solved with the
#' \eqn{\epsilon}-constraint multi-objective method.
#'
#' In this method, one objective is designated as the \emph{primary} objective
#' and is optimized directly, while the remaining objectives are transformed
#' into \eqn{\epsilon}-constraints.
#'
#' Two operating modes are supported:
#' \itemize{
#'   \item \strong{manual mode}: the user supplies the \eqn{\epsilon}-levels
#'   explicitly,
#'   \item \strong{automatic mode}: the \eqn{\epsilon}-levels are generated
#'   later during \code{\link{solve}} from extreme or payoff information.
#' }
#'
#' This function does not solve the problem. It stores the method configuration
#' in \code{x$data$method}, to be used later by \code{\link{solve}}.
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
#' The \eqn{\epsilon}-constraint method selects one of them as the primary
#' objective, say \eqn{f_p(x)}, and treats the remaining objectives as
#' constrained objectives.
#'
#' For a fixed vector of \eqn{\epsilon}-levels, the method solves subproblems of
#' the form:
#'
#' \deqn{
#' \max \; f_p(x)
#' }
#'
#' subject to
#'
#' \deqn{
#' f_k(x) \ge \epsilon_k, \qquad k \in \mathcal{C},
#' }
#'
#' together with all original feasibility constraints of the planning problem,
#' where \eqn{\mathcal{C}} is the set of constrained objectives.
#'
#' Depending on objective sense, the internal implementation may transform
#' minimization objectives into equivalent constrained forms, but the method
#' always follows the same principle:
#' \itemize{
#'   \item one objective is optimized directly,
#'   \item all remaining objectives are imposed through
#'   \eqn{\epsilon}-constraints.
#' }
#'
#' By solving the problem repeatedly for different \eqn{\epsilon}-levels, the
#' method generates a set of efficient trade-off solutions.
#'
#' \strong{Manual mode}
#'
#' In \code{mode = "manual"}, the user must provide \code{eps}.
#'
#' The \code{eps} argument can be supplied as:
#' \itemize{
#'   \item a named numeric vector, defining a single run,
#'   \item or a named list of numeric vectors, defining a grid of runs.
#' }
#'
#' The names of \code{eps} must correspond exactly to the constrained objective
#' aliases, that is, to all aliases in \code{aliases} except \code{primary}.
#'
#' If the constrained objectives are
#' \eqn{\mathcal{C} = \{c_1, \dots, c_q\}}, then manual mode creates a design
#' grid containing all combinations of the supplied \eqn{\epsilon}-levels for
#' the constrained objectives.
#'
#'
#' Each row of this grid defines one subproblem to be solved later.
#'
#' \strong{Important:} manual mode supports \strong{two or more objectives}.
#' In particular, it can be used with:
#' \itemize{
#'   \item 2 objectives: 1 primary + 1 constrained objective,
#'   \item 3 or more objectives: 1 primary + multiple constrained objectives.
#' }
#'
#' Thus, manual mode is the general way to use the
#' \eqn{\epsilon}-constraint method when more than two objectives are involved.
#'
#' In manual mode, the generated design grid is stored immediately in
#' \code{x$data$method$runs}. Its \eqn{\epsilon}-columns are named
#' \code{eps_<alias>}, for example \code{eps_frag}.
#'
#' \strong{Automatic mode}
#'
#' In \code{mode = "auto"}, the user omits \code{eps} and instead supplies
#' \code{n_points}.
#'
#' In this case, the \eqn{\epsilon}-grid is not built immediately. Instead, it
#' is constructed later during \code{\link{solve}} using extreme-point or
#' payoff-table information.
#'
#' In the current implementation, automatic mode supports
#' \strong{exactly two objectives only}:
#' \itemize{
#'   \item one primary objective,
#'   \item one constrained objective.
#' }
#'
#' Therefore, if \code{mode = "auto"}, then \code{aliases} must contain exactly
#' two objective aliases. Problems with three or more objectives must use
#' \code{mode = "manual"}.
#'
#' If \code{include_extremes = TRUE}, the automatically generated grid includes
#' the extreme values of the constrained objective. Otherwise, only interior
#' values are used.
#'
#' If \code{lexicographic = TRUE}, the extreme points used to generate the grid
#' are computed lexicographically. In that case, one objective is optimized
#' first, and then the second objective is optimized while constraining the
#' first to remain within \code{lexicographic_tol} of its optimum.
#'
#' \strong{Stored configuration}
#'
#' The configured method stores:
#' \itemize{
#'   \item \code{name = "epsilon_constraint"},
#'   \item \code{mode},
#'   \item \code{primary},
#'   \item \code{aliases},
#'   \item \code{constrained},
#'   \item epsilon design information,
#'   \item lexicographic configuration.
#' }
#'
#' In manual mode, \code{x$data$method$runs} contains the explicit design grid.
#' In automatic mode, \code{x$data$method$runs} is initially \code{NULL} and is
#' generated later during \code{\link{solve}}.For more than two objectives, automatic
#' grid generation is currently unavailable because the number of epsilon combinations
#' grows rapidly and requires explicit user control.
#'
#' @param x A \code{Problem} object.
#' @param primary Character string giving the alias of the primary objective to
#'   optimize directly.
#' @param eps Optional epsilon specification used only in \code{mode = "manual"}.
#'   It may be:
#'   \itemize{
#'     \item a named numeric vector, defining epsilon values for a single run,
#'     \item or a named list of numeric vectors, defining epsilon values for a
#'     grid of runs.
#'   }
#'   Names must correspond exactly to the constrained objective aliases.
#' @param aliases Optional character vector of objective aliases to include.
#'   By default, all registered objective aliases are used. The value of
#'   \code{primary} must be included in \code{aliases}.
#' @param mode Character string. Must be either \code{"manual"} or
#'   \code{"auto"}.
#' @param n_points Integer scalar used only in \code{mode = "auto"}. Number of
#'   epsilon points to generate automatically for the constrained objective.
#'   Must be at least 2.
#' @param include_extremes Logical scalar used only in \code{mode = "auto"}. If
#'   \code{TRUE}, include extreme epsilon values in the automatically generated
#'   grid.
#' @param lexicographic Logical scalar used only in \code{mode = "auto"}. If
#'   \code{TRUE}, compute extreme points lexicographically.
#' @param lexicographic_tol Numeric scalar \eqn{\ge 0}. Tolerance used in
#'   lexicographic extreme-point computation.
#'
#' @return An updated \code{Problem} object with the
#'   \eqn{\epsilon}-constraint method configuration stored in
#'   \code{x$data$method}.
#'
#' In manual mode, \code{x$data$method$runs} contains the explicit
#' \eqn{\epsilon}-design grid.
#'
#' In automatic mode, \code{x$data$method$runs} is \code{NULL} until the grid is
#' generated later during \code{\link{solve}}.
#'
#' @examples
#' \dontrun{
#' # Manual mode: one constrained objective, one run
#' p <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "cost",
#'     mode = "manual",
#'     eps = c(frag = 1000)
#'   )
#'
#' # Manual mode: one constrained objective, multiple epsilon values
#' p <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "cost",
#'     mode = "manual",
#'     eps = list(frag = c(1000, 2000, 3000))
#'   )
#'
#' # Manual mode: more than two objectives
#' p <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "benefit",
#'     aliases = c("benefit", "cost", "frag"),
#'     mode = "manual",
#'     eps = list(
#'       cost = c(100, 200, 300),
#'       frag = c(10, 20)
#'     )
#'   )
#'
#' # Automatic mode: currently only two objectives
#' p <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "cost",
#'     aliases = c("cost", "frag"),
#'     mode = "auto",
#'     n_points = 5,
#'     include_extremes = TRUE,
#'     lexicographic = TRUE,
#'     lexicographic_tol = 1e-8
#'   )
#' }
#'
#' @seealso
#' \code{\link{set_method_augmecon}},
#' \code{\link{set_method_weighted_sum}},
#' \code{\link{solve}}
#'
#' @export
set_method_epsilon_constraint <- function(
    x,
    primary,
    eps = NULL,
    aliases = NULL,
    mode = c("manual", "auto"),
    n_points = 10,
    include_extremes = TRUE,
    lexicographic = TRUE,
    lexicographic_tol = 1e-8
) {
  #x <- .pamo_as_mo(x)
  stopifnot(inherits(x, "Problem"))

  mode <- match.arg(mode)

  primary <- as.character(primary)[1]
  if (is.na(primary) || !nzchar(primary)) {
    stop("primary must be a non-empty string.", call. = FALSE)
  }

  .pamo_validate_objectives(x)

  specs_all <- .pamo_get_specs(x)
  obj_alias <- names(specs_all)
  if (!primary %in% obj_alias) {
    stop("primary alias not found: '", primary, "'.", call. = FALSE)
  }

  if (is.null(aliases)) aliases <- obj_alias
  aliases <- as.character(aliases)

  if (any(!aliases %in% obj_alias)) {
    bad <- aliases[!aliases %in% obj_alias]
    stop("Unknown aliases: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (!primary %in% aliases) {
    stop("primary must be included in aliases.", call. = FALSE)
  }

  constrained <- setdiff(aliases, primary)

  if (length(constrained) == 0L) {
    stop("At least one constrained objective is required.", call. = FALSE)
  }

  if (!is.logical(lexicographic) || length(lexicographic) != 1L || is.na(lexicographic)) {
    stop("lexicographic must be TRUE or FALSE.", call. = FALSE)
  }

  lexicographic_tol <- as.numeric(lexicographic_tol)[1]
  if (!is.finite(lexicographic_tol) || lexicographic_tol < 0) {
    stop("lexicographic_tol must be a finite non-negative number.", call. = FALSE)
  }

  if (mode == "manual") {
    if (is.null(eps)) {
      stop("In mode='manual', eps must be provided.", call. = FALSE)
    }

    if (is.numeric(eps) && !is.null(names(eps))) {
      eps_list <- as.list(eps)
      eps_list <- lapply(eps_list, function(v) c(as.numeric(v)[1]))
    } else if (is.list(eps) && length(eps) > 0 && !is.null(names(eps))) {
      eps_list <- lapply(eps, function(v) as.numeric(v))
    } else {
      stop("eps must be a named numeric vector or a named list of numeric vectors.", call. = FALSE)
    }

    miss <- setdiff(constrained, names(eps_list))
    if (length(miss) > 0) {
      stop("eps must include all constrained objectives. Missing: ", paste(miss, collapse = ", "), call. = FALSE)
    }

    extra <- setdiff(names(eps_list), constrained)
    if (length(extra) > 0) {
      stop(
        "eps contains aliases that are not constrained objectives: ",
        paste(extra, collapse = ", "),
        call. = FALSE
      )
    }

    bad_empty <- names(eps_list)[vapply(eps_list, length, integer(1)) == 0L]
    if (length(bad_empty) > 0) {
      stop("eps contains empty vectors for: ", paste(bad_empty, collapse = ", "), call. = FALSE)
    }

    bad_nonfinite <- names(eps_list)[vapply(eps_list, function(v) any(!is.finite(v)), logical(1))]
    if (length(bad_nonfinite) > 0) {
      stop("eps contains non-finite values for: ", paste(bad_nonfinite, collapse = ", "), call. = FALSE)
    }

    grid <- expand.grid(
      eps_list[constrained],
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    if (nrow(grid) == 0) {
      stop("Empty epsilon grid.", call. = FALSE)
    }

    names(grid) <- paste0("eps_", names(grid))
    grid$run_id <- seq_len(nrow(grid))

    x$data$method <- list(
      name = "epsilon_constraint",
      mode = "manual",
      primary = primary,
      aliases = aliases,
      constrained = constrained,
      eps = eps_list,
      runs = grid,
      lexicographic = isTRUE(lexicographic),
      lexicographic_tol = lexicographic_tol
    )

    return(x)
  }

  if (length(aliases) != 2L) {
    stop(
      "set_method_epsilon_constraint(mode='auto') currently supports exactly 2 objectives.\n",
      "Use mode='manual' for 3+ objectives.",
      call. = FALSE
    )
  }

  n_points <- as.integer(n_points)[1]
  if (!is.finite(n_points) || is.na(n_points) || n_points < 2L) {
    stop("n_points must be an integer >= 2.", call. = FALSE)
  }

  x$data$method <- list(
    name = "epsilon_constraint",
    mode = "auto",
    primary = primary,
    aliases = aliases,
    constrained = constrained,
    n_points = n_points,
    include_extremes = isTRUE(include_extremes),
    lexicographic = isTRUE(lexicographic),
    lexicographic_tol = lexicographic_tol,
    runs = NULL
  )

  x
}
