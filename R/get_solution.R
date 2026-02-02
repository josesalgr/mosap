#' @title Get planning unit results from a Solution
#'
#' @description
#' Extracts the planning-unit table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} column indicating whether each planning
#' unit is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only selected planning units.
#'   Default \code{FALSE}.
#'
#' @return A \code{data.frame} with planning-unit information and a \code{selected} column.
#' @export
#'
#' @examples
#' \dontrun{
#' sol <- solve(hola)
#' pu_tbl <- get_pu(sol)
#' pu_sel <- get_pu(sol, only_selected = TRUE)
#' }
get_pu <- function(x, only_selected = FALSE) {
  stopifnot(inherits(x, "Solution"))
  pu <- x$data$tables$pu %||% NULL
  if (is.null(pu)) stop("No PU table found in solution (x$data$tables$pu is NULL).", call. = FALSE)

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(pu))) stop("PU table has no 'selected' column.", call. = FALSE)
    pu <- pu[pu$selected == 1L, , drop = FALSE]
  }
  pu
}

#' @title Get action results from a Solution
#'
#' @description
#' Extracts the action-allocation table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} column indicating whether each feasible
#' \code{(pu, action)} pair is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only selected actions
#'   (\code{selected == 1}). Default \code{FALSE}.
#'
#' @return A \code{data.frame} with action allocation information and a \code{selected} column.
#' @export
#'
#' @examples
#' \dontrun{
#' sol <- solve(hola)
#' act_tbl <- get_actions(sol)
#' act_sel <- get_actions(sol, only_selected = TRUE)
#' }
get_actions <- function(x, only_selected = FALSE) {
  stopifnot(inherits(x, "Solution"))
  a <- x$data$tables$actions %||% NULL
  if (is.null(a)) stop("No actions table found in solution (x$data$tables$actions is NULL).", call. = FALSE)

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(a))) stop("Actions table has no 'selected' column.", call. = FALSE)
    a <- a[a$selected == 1L, , drop = FALSE]
  }
  a
}

#' @title Get feature achievement summary from a Solution
#'
#' @description
#' Extracts the feature achievement table from a [solution-class] object returned by [solve()].
#' This table typically contains:
#' \itemize{
#' \item \code{baseline_contrib}: contribution from baseline / conservation (e.g., z variables)
#' \item \code{recovery_contrib}: contribution from recovery (e.g., benefit layers times actions)
#' \item \code{total}: baseline + recovery
#' }
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A \code{data.frame} with feature achievement metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' sol <- solve(hola)
#' feat_tbl <- get_features(sol)
#' }
get_features <- function(x) {
  stopifnot(inherits(x, "Solution"))
  f <- x$data$tables$features %||% NULL
  if (is.null(f)) stop("No features table found in solution (x$data$tables$features is NULL).", call. = FALSE)
  f
}

#' @title Get target achievement table from a Solution
#'
#' @description
#' Extracts the targets table (if present) from a [solution-class] object returned by [solve()].
#' The targets table typically includes \code{target_value}, \code{achieved}, and \code{gap}
#' (achieved - target_value), plus target metadata (e.g., type).
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A \code{data.frame} with target achievement metrics, or \code{NULL} if no targets
#'   were stored/applied.
#' @export
#'
#' @examples
#' \dontrun{
#' sol <- solve(hola)
#' tgt_tbl <- get_targets(sol)
#' if (!is.null(tgt_tbl)) head(tgt_tbl)
#' }
get_targets <- function(x) {
  stopifnot(inherits(x, "Solution"))
  t <- x$data$tables$targets %||% NULL
  # targets are optional: return NULL silently if absent
  t
}

#' @title Get raw solution vector from a Solution
#'
#' @description
#' Returns the raw decision-variable vector produced by the solver (in the model variable order).
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A numeric vector with one value per model variable.
#' @export
#'
#' @examples
#' \dontrun{
#' sol <- solve(hola)
#' v <- get_solution_vector(sol)
#' length(v)
#' }
get_solution_vector <- function(x) {
  stopifnot(inherits(x, "Solution"))
  v <- x$data$sol %||% NULL
  if (is.null(v)) stop("No raw solution vector found (x$data$sol is NULL).", call. = FALSE)
  as.numeric(v)
}
