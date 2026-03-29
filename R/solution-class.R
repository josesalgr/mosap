#' @include internal.R
#'
#' @export
if (!methods::isClass("Solution")) methods::setOldClass("Solution")
NULL

#' @name solution-class
#' @aliases Solution
#' @title Solution class
#'
#' @description
#' The \code{Solution} class stores the result of solving a single
#' \code{\link{Problem}} object in \pkg{paretoscape}.
#'
#' A \code{Solution} object contains the original problem definition, the core
#' optimization output returned by the solver, user-facing summary tables,
#' diagnostics about the solve process, and metadata describing how the solution
#' was obtained.
#'
#' Objects of this class are typically created by \code{\link{solve}}.
#'
#' @details
#' \strong{Conceptual role}
#'
#' The \code{Solution} class represents the output of one optimization run.
#'
#' It should be understood as the single-run counterpart of the modelling
#' workflow:
#' \preformatted{
#' Problem -> solve() -> Solution
#' }
#'
#' Thus, a \code{Solution} object does not replace the original
#' \code{Problem}; instead, it keeps a reference to it in the \code{problem}
#' field and augments it with optimization results.
#'
#' \strong{Single-run semantics}
#'
#' A \code{Solution} corresponds to one realized solution of one configured
#' optimization problem. This may come from:
#' \itemize{
#'   \item a single-objective solve,
#'   \item one run of a weighted multi-objective workflow,
#'   \item one \eqn{\epsilon}-constraint subproblem,
#'   \item one AUGMECON subproblem,
#'   \item or any other workflow that ultimately produces one optimizer output.
#' }
#'
#' When multiple runs are generated, they are typically collected in a separate
#' \code{SolutionSet} object rather than a single \code{Solution}.
#'
#' \strong{Internal structure}
#'
#' A \code{Solution} object separates results into several layers:
#'
#' \describe{
#'   \item{\code{problem}}{The original \code{Problem} object used to generate
#'   the solution.}
#'
#'   \item{\code{solution}}{A named \code{list} containing the core optimization
#'   output. This may include the objective value, raw model variable vector,
#'   decoded decision vectors, and evaluated objective values by alias.}
#'
#'   \item{\code{summary}}{A named \code{list} of user-facing summary tables,
#'   typically derived from the original problem and the solved decisions. These
#'   tables are intended for inspection, plotting, reporting, and downstream
#'   analysis.}
#'
#'   \item{\code{diagnostics}}{A named \code{list} containing solver diagnostics
#'   such as status, runtime, optimality gap, solver name, and runtime
#'   settings.}
#'
#'   \item{\code{method}}{A named \code{list} describing the optimization method
#'   used to obtain the solution.}
#'
#'   \item{\code{meta}}{A named \code{list} containing additional metadata.}
#'
#'   \item{\code{name}}{A \code{character(1)} identifier for the solution
#'   object.}
#' }
#'
#' \strong{Core optimization output}
#'
#' The \code{solution} field stores the solver-facing result. Typical entries may
#' include:
#' \describe{
#'   \item{\code{objective}}{The scalar objective value returned by the solver
#'   for this run.}
#'   \item{\code{vector}}{The raw internal solution vector in model-variable
#'   order.}
#'   \item{\code{alias_values}}{Objective values evaluated for registered
#'   aliases, when available.}
#' }
#'
#' The raw solution vector may contain not only user-facing decision variables
#' such as planning-unit or action decisions, but also auxiliary variables
#' introduced internally by the model builder.
#'
#' \strong{User-facing summaries}
#'
#' The \code{summary} field stores derived tables intended for interpretation
#' rather than solver interaction. Typical entries include:
#' \describe{
#'   \item{\code{pu}}{Planning-unit summary table.}
#'   \item{\code{actions}}{Planning unit--action allocation summary table.}
#'   \item{\code{features}}{Feature-level outcome summary table.}
#'   \item{\code{targets}}{Target-achievement summary table, when targets were
#'   part of the problem.}
#' }
#'
#' These tables are the main source used by user-facing accessors such as
#' \code{\link{get_pu}}, \code{\link{get_actions}},
#' \code{\link{get_features}}, and \code{\link{get_targets}}.
#'
#' \strong{Diagnostics}
#'
#' The \code{diagnostics} field stores metadata about the optimization process,
#' including solver status and runtime information. Typical entries may include:
#' \itemize{
#'   \item solver name,
#'   \item runtime in seconds,
#'   \item optimality gap,
#'   \item number of cores,
#'   \item time limit,
#'   \item status code or status label.
#' }
#'
#' These values describe how the solution was obtained, not the content of the
#' solution itself.
#'
#' \strong{Printing}
#'
#' The \code{print()} method is intended as a concise diagnostic summary. It
#' reports:
#' \itemize{
#'   \item solver status,
#'   \item objective value,
#'   \item optimality gap,
#'   \item runtime,
#'   \item counts of selected planning units and actions,
#'   \item target fulfillment summary when available,
#'   \item evaluated objective alias values when available,
#'   \item and basic solver information.
#' }
#'
#' This summary is intended for quick inspection. More detailed exploration
#' should use the stored \code{summary}, \code{solution}, and
#' \code{diagnostics} fields directly, or the dedicated accessor functions.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{problem}}{The \code{Problem} object used to generate the
#'   solution.}
#'   \item{\code{solution}}{A named \code{list} containing the core optimization
#'   result.}
#'   \item{\code{summary}}{A named \code{list} of user-facing summary tables.}
#'   \item{\code{diagnostics}}{A named \code{list} of solver diagnostics.}
#'   \item{\code{method}}{A named \code{list} describing the method used.}
#'   \item{\code{meta}}{A named \code{list} of additional metadata.}
#'   \item{\code{name}}{A \code{character(1)} name for the solution object.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{print()}}{Print a concise summary of the solution, including
#'   status, objective value, runtime, selection counts, and target
#'   fulfillment.}
#'
#'   \item{\code{show()}}{Alias of \code{print()}.}
#'
#'   \item{\code{repr()}}{Return a short one-line representation of the
#'   solution.}
#' }
#'
#' @return No return value. This page documents the \code{Solution} class.
#'
#' @seealso
#' \code{\link{problem-class}},
#' \code{\link{get_pu}},
#' \code{\link{get_actions}},
#' \code{\link{get_features}},
#' \code{\link{get_targets}},
#' \code{\link{solve}}
NULL

# internal helper
.pa_solution_status_class <- function(status_txt) {
  status_txt <- as.character(status_txt %||% "unknown")[1]

  if (status_txt %in% c("optimal")) {
    return("ok")
  }

  if (status_txt %in% c("time_limit_feasible", "solution_limit")) {
    return("warn")
  }

  if (status_txt %in% c("infeasible_or_unbounded", "time_limit_no_solution", "unknown")) {
    return("bad")
  }

  "muted"
}

# internal helper
.pa_solution_status_inline <- function(status_txt) {
  cls <- .pa_solution_status_class(status_txt)
  paste0("{.", cls, " ", status_txt, "}")
}

# internal helper
.pa_pct_text <- function(x, digits = 3) {
  if (!is.numeric(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  paste0(round(100 * x, digits), "%")
}

# internal helper
.pa_num_text <- function(x, digits = 6) {
  if (!is.numeric(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  format(round(x, digits), trim = TRUE, scientific = FALSE)
}

# internal helper
.pa_n_of_total_text <- function(n, total) {
  if (is.null(n) || length(n) == 0 || is.na(n)) n <- 0L
  if (is.null(total) || length(total) == 0 || is.na(total)) {
    return(as.character(n))
  }
  paste0(n, " of ", total)
}

# internal helper
.pa_solution_alias_values_summary <- function(self, max_show = 6L) {
  av <- self$solution$alias_values %||% NULL

  if (is.null(av) || length(av) == 0) {
    return(NULL)
  }

  av <- unlist(av, use.names = TRUE)
  nm <- names(av) %||% rep("objective", length(av))

  if (length(av) > max_show) {
    keep <- seq_len(max_show)
    txt <- paste0(
      nm[keep], ": ", vapply(av[keep], .pa_num_text, character(1)),
      collapse = ", "
    )
    txt <- paste0(txt, ", ...")
  } else {
    txt <- paste0(
      nm, ": ", vapply(av, .pa_num_text, character(1)),
      collapse = ", "
    )
  }

  txt
}

#' @export
Solution <- pproto(
  "Solution",
  problem = NULL,
  solution = list(),
  summary = list(),
  diagnostics = list(),
  method = list(),
  meta = list(),
  name = "sol",

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    cli::cli_text("A paretoscape solution ({.cls Solution})")

    obj <- self$solution$objective %||% NA_real_
    gap <- self$diagnostics$gap %||% NA_real_
    rt  <- self$diagnostics$runtime %||% NA_real_
    st  <- getStatus(self)

    sm <- self$summary %||% list()
    dg <- self$diagnostics %||% list()
    pr <- self$problem %||% NULL

    # ---- totals from problem
    n_pu_total <- if (!is.null(pr) && !is.null(pr$data$pu) && inherits(pr$data$pu, "data.frame")) {
      nrow(pr$data$pu)
    } else {
      NA_integer_
    }

    n_act_total <- if (!is.null(pr) && !is.null(pr$data$dist_actions) && inherits(pr$data$dist_actions, "data.frame")) {
      nrow(pr$data$dist_actions)
    } else {
      NA_integer_
    }

    n_tgt_total <- if (!is.null(sm$targets) && inherits(sm$targets, "data.frame")) {
      nrow(sm$targets)
    } else {
      NA_integer_
    }

    # ---- selected counts
    n_pu_sel <- if (!is.null(sm$pu) && inherits(sm$pu, "data.frame") && "selected" %in% names(sm$pu)) {
      sum(sm$pu$selected %in% 1L, na.rm = TRUE)
    } else {
      NA_integer_
    }

    n_act_sel <- if (!is.null(sm$actions) && inherits(sm$actions, "data.frame") && "selected" %in% names(sm$actions)) {
      sum(sm$actions$selected %in% 1L, na.rm = TRUE)
    } else {
      NA_integer_
    }

    n_tgt_met <- if (!is.null(sm$targets) && inherits(sm$targets, "data.frame") && "gap" %in% names(sm$targets)) {
      sum(sm$targets$gap >= 0, na.rm = TRUE)
    } else {
      NA_integer_
    }

    alias_txt <- .pa_solution_alias_values_summary(self)

    # ---- RESULT
    cli::cli_text("{ch$j}{ch$b}{.h result}", .envir = environment())
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}status:          {.eval .pa_solution_status_inline(st)}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}objective value: {.strong {.val { .pa_num_text(obj) }}}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}gap:             { .pa_pct_text(gap) }",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$l}{ch$b}runtime:         { .pa_num_text(rt, digits = 3) } sec",
      .envir = environment()
    )

    # ---- SELECTION
    cli::cli_text("{ch$l}{ch$b}{.h selection}", .envir = environment())

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}planning units:  { .pa_n_of_total_text(n_pu_sel, n_pu_total) } selected",
      .envir = environment()
    )

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}actions:         { .pa_n_of_total_text(n_act_sel, n_act_total) } selected",
      .envir = environment()
    )

    if (!is.na(n_tgt_total)) {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}targets met:     { .pa_n_of_total_text(n_tgt_met, n_tgt_total) }",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}targets met:     {.muted none}",
        .envir = environment()
      )
    }

    # ---- OBJECTIVE VALUES
    if (!is.null(alias_txt)) {
      cli::cli_text("{ch$l}{ch$b}{.h objective values}", .envir = environment())
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}{alias_txt}",
        .envir = environment()
      )
    }

    # ---- SOLVER
    cli::cli_text("{ch$l}{ch$b}{.h solver}", .envir = environment())

    solver_txt <- as.character(dg$solver %||% dg$solver_name %||% "unknown")[1]
    cores_txt  <- as.character(dg$cores %||% NA)[1]
    tl_txt     <- as.character(dg$timelimit %||% dg$time_limit %||% NA)[1]

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}name:            {.code {solver_txt}}",
      .envir = environment()
    )
    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}cores:           {cores_txt}",
      .envir = environment()
    )
    cli::cli_text(
      " {ch$v}{ch$l}{ch$b}time limit:      {tl_txt}",
      .envir = environment()
    )

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(
      cli::col_grey(
        paste0("# ", info_sym, " Use {.code x$summary} to inspect user-facing solution summaries.")
      )
    )

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    st <- tryCatch(getStatus(self), error = function(e) "unknown")
    obj <- self$solution$objective %||% NA_real_
    paste0("<Solution> status=", st, ", objective=", .pa_num_text(obj))
  }
)
