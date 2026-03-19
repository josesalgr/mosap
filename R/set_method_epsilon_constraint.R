#' Configure epsilon-constraint multi-objective method
#'
#' @description
#' Configure an epsilon-constraint multi-objective method for a
#' \code{MOProblem}. One objective is optimized as the \emph{primary}
#' objective, while the remaining objectives are converted into epsilon
#' constraints.
#'
#' Two operating modes are supported.
#'
#' \strong{Manual mode}
#' \itemize{
#'   \item The user provides \code{eps}.
#'   \item \code{eps} can be either:
#'   \itemize{
#'     \item a named numeric vector, defining a single run, or
#'     \item a named list of numeric vectors, defining a grid of runs.
#'   }
#'   \item Names must correspond to constrained objective aliases, that is,
#'   objective aliases other than \code{primary}.
#' }
#'
#' \strong{Automatic mode}
#' \itemize{
#'   \item The user omits \code{eps} and provides \code{n_points}.
#'   \item The epsilon grid is not built immediately. Instead, it is
#'   constructed later during \code{\link{solve}} from extreme/payoff
#'   information.
#'   \item Automatic mode currently supports exactly two objectives:
#'   one primary objective and one constrained objective.
#'   \item If \code{lexicographic = TRUE}, the extreme points are computed
#'   lexicographically: first one objective is optimized, and then the other
#'   is optimized while constraining the first one to remain within
#'   \code{lexicographic_tol} of its optimum.
#' }
#'
#' In manual mode, the generated design grid is stored inside
#' \code{x$method$runs}. In automatic mode, \code{x$method$runs} is initially
#' \code{NULL} and is created later during \code{\link{solve}}.
#'
#' @param x A \code{Problem} or \code{MOProblem} object.
#' @param primary Character scalar. Alias of the primary objective to optimize.
#' @param eps Optional epsilon specification used only in \code{mode = "manual"}.
#'   It can be either:
#'   \itemize{
#'     \item a named numeric vector, defining epsilon values for a single run, or
#'     \item a named list of numeric vectors, defining epsilon values for a grid.
#'   }
#'   Names must correspond to constrained objective aliases.
#' @param aliases Optional character vector of objective aliases to consider.
#'   By default, all registered objective aliases are used.
#' @param mode Character scalar. Either \code{"manual"} or \code{"auto"}.
#' @param n_points Integer scalar used only in \code{mode = "auto"}.
#'   Number of epsilon points to generate automatically for the constrained
#'   objective. Must be at least 2.
#' @param include_extremes Logical scalar used only in \code{mode = "auto"}.
#'   If \code{TRUE}, include the extreme epsilon values in the generated grid.
#'   If \code{FALSE}, only interior epsilon values are used.
#' @param lexicographic Logical scalar used only in \code{mode = "auto"}.
#'   If \code{TRUE}, compute extreme points lexicographically. If \code{FALSE},
#'   use single-objective extreme points directly.
#' @param lexicographic_tol Numeric scalar \eqn{\ge 0}. Tolerance used in
#'   lexicographic extreme-point computation to keep the first-stage objective
#'   within a small margin of its optimum.
#'
#' @return
#' An updated \code{MOProblem} object with the epsilon-constraint method
#' configuration stored in \code{x$method}.
#'
#' In manual mode, \code{x$method$runs} contains the epsilon design grid.
#' In automatic mode, \code{x$method$runs} is set to \code{NULL} and the design
#' is generated later during \code{\link{solve}}.
#'
#' @details
#' The configured method stores:
#' \itemize{
#'   \item \code{name = "epsilon_constraint"}
#'   \item \code{mode}
#'   \item \code{primary}
#'   \item \code{aliases}
#'   \item \code{constrained}
#'   \item epsilon design information
#'   \item lexicographic configuration
#' }
#'
#' In manual mode, epsilon columns in the stored design grid are named
#' \code{eps_<alias>}, for example \code{eps_frag}.
#'
#' @examples
#' \dontrun{
#' # Manual mode: one run
#' p_mo <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "cost",
#'     mode = "manual",
#'     eps = c(frag = 1000)
#'   )
#'
#' # Manual mode: grid of runs
#' p_mo <- p |>
#'   set_method_epsilon_constraint(
#'     primary = "cost",
#'     mode = "manual",
#'     eps = list(frag = c(1000, 2000, 3000))
#'   )
#'
#' # Automatic mode
#' p_mo <- p |>
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
