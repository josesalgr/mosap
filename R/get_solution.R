#' @title Get planning unit results from a Solution
#'
#' @description
#' Extract the planning-unit results table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} indicator (typically \code{0/1}) showing
#' whether each planning unit is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only rows where \code{selected == 1}.
#'   Default \code{FALSE}.
#'
#' @return A \code{data.frame} with planning-unit information stored in the solution and
#'   a \code{selected} column.
#'
#' @details
#' This function expects the solution object to store a planning-unit results table at
#' \code{x$data$tables$pu}. It errors if the table is missing. If \code{only_selected = TRUE},
#' it also errors when the \code{selected} column is not present.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' pu_tbl <- get_pu(sol)
#' pu_sel <- get_pu(sol, only_selected = TRUE)
#' }
#'
#' @export
#'
#' @seealso [get_actions()], [get_features()], [get_targets()], [get_solution_vector()]
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
#' Extract the action-allocation results table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} indicator (typically \code{0/1}) showing whether
#' each feasible \code{(pu, action)} pair is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only rows where \code{selected == 1}.
#'   Default \code{FALSE}.
#'
#' @return A \code{data.frame} with action-allocation information stored in the solution and
#'   a \code{selected} column.
#'
#' @details
#' This function expects the solution object to store an action-allocation table at
#' \code{x$data$tables$actions}. It errors if the table is missing. If \code{only_selected = TRUE},
#' it also errors when the \code{selected} column is not present.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' act_tbl <- get_actions(sol)
#' act_sel <- get_actions(sol, only_selected = TRUE)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_features()], [get_targets()], [get_solution_vector()]
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
#' Extract the feature achievement table from a [solution-class] object returned by [solve()].
#' This table typically summarizes, for each feature, how much is achieved by baseline selection
#' and by action effects, and their total.
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A \code{data.frame} with feature achievement metrics stored in the solution.
#'
#' @details
#' This function expects the feature achievement table at \code{x$data$tables$features} and
#' errors if it is missing. The exact columns depend on the model and reporting options, but
#' commonly include:
#' \itemize{
#' \item \code{baseline_contrib}: contribution from baseline / conservation selection.
#' \item \code{recovery_contrib}: contribution from action effects (e.g., benefits).
#' \item \code{total}: baseline + recovery.
#' }
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' feat_tbl <- get_features(sol)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_targets()]
get_features <- function(x) {
  stopifnot(inherits(x, "Solution"))
  f <- x$data$tables$features %||% NULL
  if (is.null(f)) stop("No features table found in solution (x$data$tables$features is NULL).", call. = FALSE)
  f
}


#' @title Get target achievement table from a Solution
#'
#' @description
#' Extract the target achievement table (if present) from a [solution-class] object returned by [solve()].
#' The targets table typically contains the target value, achieved value, and gap
#' (\code{achieved - target_value}), plus target metadata such as type and units.
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A \code{data.frame} with target achievement metrics, or \code{NULL} if the solution
#'   does not contain a targets table.
#'
#' @details
#' Targets are optional. If the solution does not include \code{x$data$tables$targets},
#' this function returns \code{NULL} without error.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' tgt_tbl <- get_targets(sol)
#' if (!is.null(tgt_tbl)) head(tgt_tbl)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_features()]
get_targets <- function(x) {
  stopifnot(inherits(x, "Solution"))
  t <- x$data$tables$targets %||% NULL
  # targets are optional: return NULL silently if absent
  t
}


#' @title Get raw solution vector from a Solution
#'
#' @description
#' Return the raw decision-variable vector produced by the solver, in the internal model
#' variable order used by the optimizer backend.
#'
#' @param x A [solution-class] object returned by [solve()].
#'
#' @return A numeric vector with one value per model variable.
#'
#' @details
#' This function expects the raw solution vector at \code{x$data$sol} and errors if it is missing.
#' The vector is returned as numeric and corresponds to the model's variable ordering (e.g.,
#' planning-unit selection variables, action variables, and any auxiliary variables such as
#' fragmentation variables when present).
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' v <- get_solution_vector(sol)
#' length(v)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_features()], [get_targets()]
get_solution_vector <- function(x) {
  stopifnot(inherits(x, "Solution"))
  v <- x$data$sol %||% NULL
  if (is.null(v)) stop("No raw solution vector found (x$data$sol is NULL).", call. = FALSE)
  as.numeric(v)
}
