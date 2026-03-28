#' @include internalMO.R
#'
#' @title Get planning-unit results from a solution
#'
#' @description
#' Extract the planning-unit summary table from a \code{Solution} or
#' \code{SolutionSet} object returned by \code{\link{solve}}.
#'
#' The returned table summarizes solution values at the planning-unit level and
#' typically includes a \code{selected} indicator showing whether each planning
#' unit is selected in the solution.
#'
#' @details
#' This function reads the planning-unit summary stored in
#' \code{x$summary$pu}. It does not reconstruct the table from the raw decision
#' vector; it simply returns the stored summary after optional filtering.
#'
#' Let \eqn{w_i} denote the planning-unit selection variable for planning unit
#' \eqn{i}. In standard \code{mosap} workflows, the \code{selected} column is the
#' user-facing representation of that planning-unit decision, typically coded as
#' \code{0} or \code{1}.
#'
#' If \code{x} is a \code{SolutionSet} and \code{run} is provided, only rows
#' belonging to that run are returned. This requires the summary table to
#' contain a \code{run_id} column.
#'
#' If \code{only_selected = TRUE}, only rows with \code{selected == 1} are
#' returned. This requires the summary table to contain a \code{selected}
#' column.
#'
#' This function is intended for user-facing inspection of planning-unit results.
#' For the raw model variable vector, use \code{\link{get_solution_vector}}.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object returned by
#'   \code{\link{solve}}.
#' @param only_selected Logical. If \code{TRUE}, return only rows where
#'   \code{selected == 1}. Default is \code{FALSE}.
#' @param run Optional positive integer giving the run index to extract from a
#'   \code{SolutionSet}. If \code{NULL}, all runs are returned when available.
#'
#' @return A \code{data.frame} containing the stored planning-unit summary.
#'   Typical columns include planning-unit identifiers, optional labels, and a
#'   \code{selected} indicator.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#'
#' pu_tbl <- get_pu(sol)
#' pu_sel <- get_pu(sol, only_selected = TRUE)
#' }
#'
#' @seealso
#' \code{\link{get_actions}},
#' \code{\link{get_features}},
#' \code{\link{get_targets}},
#' \code{\link{get_solution_vector}}
#'
#' @export
get_pu <- function(x, only_selected = FALSE, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  pu <- x$summary$pu %||% NULL
  if (is.null(pu)) {
    stop("No PU summary found (x$summary$pu is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(pu))) {
      stop("PU summary has no 'run_id' column.", call. = FALSE)
    }
    pu <- pu[pu$run_id == run, , drop = FALSE]
  }

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(pu))) {
      stop("PU summary has no 'selected' column.", call. = FALSE)
    }
    pu <- pu[pu$selected == 1L, , drop = FALSE]
  }

  pu
}

#' @title Get action results from a solution
#'
#' @description
#' Extract the action-allocation summary table from a \code{Solution} or
#' \code{SolutionSet} object returned by \code{\link{solve}}.
#'
#' The returned table summarizes solution values at the
#' planning unit--action level and typically includes a \code{selected}
#' indicator showing whether each feasible \code{(pu, action)} pair is selected
#' in the solution.
#'
#' @details
#' This function reads the action summary stored in \code{x$summary$actions}. It
#' does not reconstruct the table from the raw decision vector; it simply
#' returns the stored summary after optional filtering.
#'
#' Let \eqn{x_{ia}} denote the decision variable associated with selecting
#' action \eqn{a} in planning unit \eqn{i}. In standard \code{mosap} workflows,
#' the \code{selected} column is the user-facing representation of that
#' decision, typically coded as \code{0} or \code{1}.
#'
#' If \code{x} is a \code{SolutionSet} and \code{run} is provided, only rows
#' belonging to that run are returned. This requires the summary table to
#' contain a \code{run_id} column.
#'
#' If \code{only_selected = TRUE}, only rows with \code{selected == 1} are
#' returned. This requires the summary table to contain a \code{selected}
#' column.
#'
#' This function is intended for user-facing inspection of action allocations.
#' For the raw model variable vector, use \code{\link{get_solution_vector}}.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object returned by
#'   \code{\link{solve}}.
#' @param only_selected Logical. If \code{TRUE}, return only rows where
#'   \code{selected == 1}. Default is \code{FALSE}.
#' @param run Optional positive integer giving the run index to extract from a
#'   \code{SolutionSet}. If \code{NULL}, all runs are returned when available.
#'
#' @return A \code{data.frame} containing the stored action-allocation summary.
#'   Typical columns include planning-unit ids, action ids, optional labels, and
#'   a \code{selected} indicator.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#'
#' act_tbl <- get_actions(sol)
#' act_sel <- get_actions(sol, only_selected = TRUE)
#' }
#'
#' @seealso
#' \code{\link{get_pu}},
#' \code{\link{get_features}},
#' \code{\link{get_targets}},
#' \code{\link{get_solution_vector}}
#'
#' @export
get_actions <- function(x, only_selected = FALSE, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  a <- x$summary$actions %||% NULL
  if (is.null(a)) {
    stop("No actions summary found (x$summary$actions is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(a))) {
      stop("Actions summary has no 'run_id' column.", call. = FALSE)
    }
    a <- a[a$run_id == run, , drop = FALSE]
  }

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(a))) {
      stop("Actions summary has no 'selected' column.", call. = FALSE)
    }
    a <- a[a$selected == 1L, , drop = FALSE]
  }

  a
}



#' @title Get feature summary from a solution
#'
#' @description
#' Extract the per-feature summary table from a \code{Solution} or
#' \code{SolutionSet} object returned by \code{\link{solve}}.
#'
#' The returned table summarizes, for each feature, the total amount available
#' in the landscape together with the positive and negative contributions induced
#' by the selected actions in the solution.
#'
#' @details
#' This function reads the feature summary stored in \code{x$summary$features}.
#' It errors if that table is missing.
#'
#' Let \eqn{B_f} denote the total baseline amount available in the landscape for
#' feature \eqn{f}. Let \eqn{G_f} denote the total positive contribution induced
#' by selected actions, and let \eqn{L_f} denote the total negative
#' contribution. Then the returned table is intended to summarize quantities of
#' the form:
#'
#' \deqn{
#' \mathrm{net}_f = G_f - L_f,
#' }
#'
#' \deqn{
#' \mathrm{total}_f = B_f + \mathrm{net}_f.
#' }
#'
#' In the stored summary, these quantities are typically represented by the
#' columns:
#' \itemize{
#'   \item \code{total_available}, corresponding to \eqn{B_f},
#'   \item \code{benefit}, corresponding to \eqn{G_f},
#'   \item \code{loss}, corresponding to \eqn{L_f},
#'   \item \code{net}, corresponding to \eqn{G_f - L_f},
#'   \item \code{total}, corresponding to \eqn{B_f + G_f - L_f}.
#' }
#'
#' If any of the core numeric columns \code{total_available}, \code{benefit}, or
#' \code{loss} are missing, they are created and filled with zero. If
#' \code{net} is missing, it is computed as \code{benefit - loss}. If
#' \code{total} is missing, it is computed as \code{total_available + net}.
#'
#' Thus, this function guarantees that the returned table contains the columns
#' \code{total_available}, \code{benefit}, \code{loss}, \code{net}, and
#' \code{total}, even if some of them were absent from the stored summary.
#'
#' If \code{x} is a \code{SolutionSet} and \code{run} is provided, only rows
#' belonging to that run are returned. If the result contains a \code{run_id}
#' column but only a single run is present and \code{run} was not requested
#' explicitly, the \code{run_id} column is removed for convenience.
#'
#' This function summarizes feature outcomes in the solution. It is different
#' from \code{\link{get_targets}}, which focuses on target achievement rather
#' than total feature balance.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object returned by
#'   \code{\link{solve}}.
#' @param run Optional positive integer giving the run index to extract from a
#'   \code{SolutionSet}. If \code{NULL}, all runs are returned when available.
#'
#' @return A \code{data.frame} with one row per feature, or one row per
#'   feature--run combination when multiple runs are present. The returned table
#'   always includes the columns \code{total_available}, \code{benefit},
#'   \code{loss}, \code{net}, and \code{total}.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' feat_tbl <- get_features(sol)
#' }
#'
#' @seealso
#' \code{\link{get_pu}},
#' \code{\link{get_actions}},
#' \code{\link{get_targets}}
#'
#' @export
get_features <- function(x, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  f <- x$summary$features %||% NULL
  if (is.null(f)) {
    stop("No features summary found (x$summary$features is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(f))) {
      stop("Features summary has no 'run_id' column.", call. = FALSE)
    }

    runs_avail <- sort(unique(f$run_id))
    if (!(run %in% runs_avail)) {
      stop(
        "run=", run, " is out of range. Available runs: ",
        paste(runs_avail, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    f <- f[f$run_id == run, , drop = FALSE]
  }

  out <- f

  # ensure core numeric columns exist
  if (!("total_available" %in% names(out))) out$total_available <- 0
  if (!("benefit" %in% names(out))) out$benefit <- 0
  if (!("loss" %in% names(out))) out$loss <- 0

  out$total_available <- as.numeric(out$total_available)
  out$benefit <- as.numeric(out$benefit)
  out$loss <- as.numeric(out$loss)

  if (!("net" %in% names(out))) {
    out$net <- out$benefit - out$loss
  } else {
    out$net <- as.numeric(out$net)
  }

  if (!("total" %in% names(out))) {
    out$total <- out$total_available + out$net
  } else {
    out$total <- as.numeric(out$total)
  }

  keep_first <- c(
    "run_id",
    "feature",
    "feature_name",
    "total_available",
    "benefit",
    "loss",
    "net",
    "total"
  )

  keep_first <- intersect(keep_first, names(out))
  keep_rest <- setdiff(names(out), keep_first)
  out <- out[, c(keep_first, keep_rest), drop = FALSE]

  if ("run_id" %in% names(out) && length(unique(out$run_id)) <= 1L && is.null(run)) {
    out$run_id <- NULL
  }

  out
}




#' @title Get target achievement summary from a solution
#'
#' @description
#' Extract a user-facing target-achievement table from a \code{Solution} or
#' \code{SolutionSet} object returned by \code{\link{solve}}.
#'
#' The returned table summarizes, for each stored target, the target level, the
#' achieved value in the solution, the gap between achieved and required values,
#' and whether the target was met.
#'
#' @details
#' Targets are optional in \code{mosap}. If the solution object does not contain
#' a targets summary table at \code{x$summary$targets}, this function returns
#' \code{NULL} without error.
#'
#' This function reads the stored targets summary and returns a simplified
#' user-facing table. If the summary contains \code{achieved} and
#' \code{target_value}, target satisfaction is evaluated as follows.
#'
#' For lower-bound targets:
#' \deqn{
#' \mathrm{met} = (\mathrm{achieved} \ge \mathrm{target}),
#' }
#'
#' and for upper-bound targets:
#' \deqn{
#' \mathrm{met} = (\mathrm{achieved} \le \mathrm{target}).
#' }
#'
#' The interpretation of the target direction is taken from the \code{sense}
#' column when available:
#' \itemize{
#'   \item \code{"ge"}, \code{">="}, or \code{"min"} are treated as lower-bound
#'   targets,
#'   \item \code{"le"}, \code{"<="}, or \code{"max"} are treated as upper-bound
#'   targets,
#'   \item if \code{sense} is missing, the target is treated as a lower bound by
#'   default.
#' }
#'
#' The returned table is simplified and renames some internal fields for
#' readability:
#' \itemize{
#'   \item \code{target_raw} is returned as \code{target_level},
#'   \item \code{basis_total} is returned as \code{total_available},
#'   \item \code{target_value} is returned as \code{target}.
#' }
#'
#' If \code{x} is a \code{SolutionSet} and \code{run} is provided, only rows
#' belonging to that run are returned. If the result contains a \code{run_id}
#' column but only a single run is present and \code{run} was not requested
#' explicitly, the \code{run_id} column is removed for convenience.
#'
#' The \code{gap} column is expected to be part of the stored summary. When
#' present, it typically represents:
#' \deqn{
#' \mathrm{gap} = \mathrm{achieved} - \mathrm{target}.
#' }
#'
#' @param x A \code{Solution} or \code{SolutionSet} object returned by
#'   \code{\link{solve}}.
#' @param run Optional positive integer giving the run index to extract from a
#'   \code{SolutionSet}. If \code{NULL}, all runs are returned when available.
#'
#' @return A simplified \code{data.frame} target summary, or \code{NULL} if the
#'   solution does not contain targets. Typical columns include
#'   \code{feature}, \code{feature_name}, \code{target_level},
#'   \code{total_available}, \code{target}, \code{achieved}, \code{gap}, and
#'   \code{met}.
#'
#' @seealso
#' \code{\link{get_pu}},
#' \code{\link{get_actions}},
#' \code{\link{get_features}}
#'
#' @export
get_targets <- function(x, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  t <- x$summary$targets %||% NULL
  if (is.null(t)) {
    return(NULL)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(t))) {
      stop("Targets summary has no 'run_id' column.", call. = FALSE)
    }
    t <- t[t$run_id == run, , drop = FALSE]
  }

  out <- t

  if (all(c("achieved", "target_value") %in% names(out))) {
    if ("sense" %in% names(out)) {
      out$met <- ifelse(
        out$sense %in% c("ge", ">=", "min"),
        out$achieved >= out$target_value,
        ifelse(
          out$sense %in% c("le", "<=", "max"),
          out$achieved <= out$target_value,
          NA
        )
      )
    } else {
      out$met <- out$achieved >= out$target_value
    }
  }

  keep <- c(
    "run_id",
    "feature",
    "feature_name",
    "target_raw",
    "basis_total",
    "target_value",
    "achieved",
    "gap",
    "met"
  )
  keep <- intersect(keep, names(out))
  out <- out[, keep, drop = FALSE]

  names(out)[names(out) == "target_raw"] <- "target_level"
  names(out)[names(out) == "basis_total"] <- "total_available"
  names(out)[names(out) == "target_value"] <- "target"

  if ("run_id" %in% names(out) && length(unique(out$run_id)) <= 1L && is.null(run)) {
    out$run_id <- NULL
  }

  out
}

#' @title Get raw solution vector from a solution
#'
#' @description
#' Return the raw decision-variable vector produced by the solver, in the
#' internal model-variable order used by the optimization backend.
#'
#' @details
#' This function extracts the raw solution vector stored at
#' \code{x$solution$vector} for a \code{Solution} or for a selected run of a
#' \code{SolutionSet}.
#'
#' The returned vector is in the internal variable order of the optimization
#' model. Depending on the problem formulation, it may include:
#' \itemize{
#'   \item planning-unit selection variables,
#'   \item action-allocation variables,
#'   \item auxiliary variables introduced for targets, budgets, fragmentation,
#'   or other constraints/objectives,
#'   \item and potentially additional blocks created internally by the model
#'   builder.
#' }
#'
#' Therefore, this vector is primarily intended for advanced users, debugging,
#' diagnostics, or internal verification. It is not a user-facing allocation
#' table.
#'
#' To inspect selected planning units or selected actions in a more interpretable
#' form, use \code{\link{get_pu}} or \code{\link{get_actions}} instead.
#'
#' For a single solution, the returned vector corresponds to that solution.
#' For a \code{SolutionSet}, the \code{run} argument selects which run to
#' extract.
#'
#' @param x A \code{Solution} or \code{SolutionSet} object returned by
#'   \code{\link{solve}}.
#' @param run Positive integer giving the run index to extract when \code{x} is
#'   a \code{SolutionSet}. Default is \code{1L}.
#'
#' @return A numeric vector with one value per internal model variable.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' v <- get_solution_vector(sol)
#' length(v)
#' }
#'
#' @seealso
#' \code{\link{get_pu}},
#' \code{\link{get_actions}},
#' \code{\link{get_features}},
#' \code{\link{get_targets}}
#'
#' @export
get_solution_vector <- function(x, run = 1L) {
  sol <- .mo_get_solution_from(x, run = run)

  v <- sol$solution$vector %||% NULL
  if (is.null(v)) {
    stop("No raw solution vector found (x$solution$vector is NULL).", call. = FALSE)
  }

  as.numeric(v)
}
