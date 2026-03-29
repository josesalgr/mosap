#' @include internal.R
#'
#' @export
if (!methods::isClass("SolutionSet")) methods::setOldClass("SolutionSet")
NULL

#' @name solutionset-class
#' @aliases SolutionSet
#' @title SolutionSet class
#'
#' @description
#' The \code{SolutionSet} class stores the result of solving a
#' \code{\link{Problem}} object when multiple runs are produced.
#'
#' A \code{SolutionSet} object is the multi-run counterpart of
#' \code{\link{solution-class}}. It contains the original problem, run-level
#' design and outcome tables, the list of individual \code{Solution} objects,
#' diagnostics summarizing the full set of runs, method metadata, and additional
#' metadata.
#'
#' Objects of this class are typically created by \code{\link{solve}} in
#' workflows that generate more than one optimization run, such as weighted-sum
#' scans, \eqn{\epsilon}-constraint designs, or AUGMECON grids.
#'
#' @details
#' \strong{Conceptual role}
#'
#' The \code{SolutionSet} class represents the result of a multi-run solving
#' workflow:
#' \preformatted{
#' Problem -> solve() -> SolutionSet
#' }
#'
#' Each run corresponds to one specific optimization subproblem, parameter
#' setting, or trade-off configuration. The \code{SolutionSet} object keeps all
#' these runs together in a structured form.
#'
#' Thus, a \code{SolutionSet} is not a single solution with multiple labels, but
#' a collection of distinct \code{Solution} objects linked to a shared problem
#' and a shared multi-run design.
#'
#' \strong{Typical use cases}
#'
#' A \code{SolutionSet} is typically returned when the chosen solution method
#' generates multiple runs, for example:
#' \itemize{
#'   \item several weighted-sum configurations,
#'   \item a grid of \eqn{\epsilon}-constraint runs,
#'   \item an AUGMECON exploration of multiple
#'   \eqn{\epsilon}-combinations,
#'   \item or any other workflow producing more than one optimizer call.
#' }
#'
#' \strong{Internal structure}
#'
#' A \code{SolutionSet} object separates information into several layers:
#'
#' \describe{
#'   \item{\code{problem}}{The original \code{Problem} object shared by all
#'   runs.}
#'
#'   \item{\code{solution}}{A named \code{list} containing the core multi-run
#'   optimization outputs. This typically includes the run design, the run
#'   summary table, and the list of individual \code{Solution} objects.}
#'
#'   \item{\code{summary}}{A named \code{list} containing user-facing summaries
#'   aggregated across runs when such summaries are available.}
#'
#'   \item{\code{diagnostics}}{A named \code{list} containing diagnostics about
#'   the solution set as a whole.}
#'
#'   \item{\code{method}}{A named \code{list} describing the multi-run
#'   optimization method used.}
#'
#'   \item{\code{meta}}{A named \code{list} containing additional metadata.}
#'
#'   \item{\code{name}}{A \code{character(1)} identifier for the solution set.}
#' }
#'
#' \strong{Run-level content}
#'
#' The \code{solution} field is the main entry point for run-level information.
#' Typical entries include:
#' \describe{
#'   \item{\code{design}}{A \code{data.frame} describing the experimental or
#'   optimization design, for example weights or \eqn{\epsilon}-levels.}
#'
#'   \item{\code{runs}}{A \code{data.frame} summarizing the outcome of each run.
#'   This typically includes run identifiers, solver status, runtime, gap, and
#'   objective values.}
#'
#'   \item{\code{solutions}}{A list of individual \code{Solution} objects, one
#'   per run.}
#' }
#'
#' In many workflows, the \code{runs} table is the most important compact
#' representation of the solution set, while \code{solutions[[i]]} provides the
#' full detailed output for run \eqn{i}.
#'
#' \strong{Objective values and run tables}
#'
#' In multi-objective workflows, the run table often stores objective values in
#' columns named \code{value_<alias>}, where \code{<alias>} is the alias of a
#' registered objective. For example:
#' \itemize{
#'   \item \code{value_cost},
#'   \item \code{value_frag},
#'   \item \code{value_benefit}.
#' }
#'
#' This naming convention is used by downstream functions such as
#' \code{\link{plot_tradeoff}}.
#'
#' Depending on the solving method, the run table may also contain design
#' columns such as:
#' \itemize{
#'   \item \code{weight_<alias>} for weighted-sum runs,
#'   \item \code{eps_<alias>} for \eqn{\epsilon}-constraint or AUGMECON runs.
#' }
#'
#' \strong{Relationship with \code{Solution}}
#'
#' A \code{SolutionSet} is conceptually a collection of \code{Solution} objects.
#' The individual runs are typically stored in:
#' \deqn{
#' \code{x$solution$solutions[[i]]}
#' }
#'
#' where each element is itself a full \code{Solution} object.
#'
#' Therefore:
#' \itemize{
#'   \item use \code{SolutionSet} when working with the full set of runs,
#'   \item use an individual \code{Solution} when inspecting one particular run
#'   in detail.
#' }
#'
#' \strong{Diagnostics}
#'
#' The \code{diagnostics} field stores metadata about the multi-run solve
#' process. Depending on the implementation, it may summarize:
#' \itemize{
#'   \item number of runs,
#'   \item status frequencies,
#'   \item runtime ranges,
#'   \item gap ranges,
#'   \item and other aggregate information about the set of runs.
#' }
#'
#' \strong{Printing}
#'
#' The \code{print()} method provides a concise summary of the full solution set.
#' It reports:
#' \itemize{
#'   \item the optimization method name,
#'   \item the participating objective aliases,
#'   \item the number of design rows, runs, and stored solutions,
#'   \item run-level status summaries,
#'   \item runtime and gap ranges,
#'   \item and the names of design and objective-value columns when available.
#' }
#'
#' This printed output is intended as a quick overview. Detailed inspection
#' should use:
#' \itemize{
#'   \item \code{x$solution$runs},
#'   \item \code{x$solution$design},
#'   \item \code{x$solution$solutions[[i]]},
#'   \item or the accessor methods documented below.
#' }
#'
#' @section Fields:
#' \describe{
#'   \item{\code{problem}}{The \code{Problem} object used to generate the full
#'   solution set.}
#'   \item{\code{solution}}{A named \code{list} containing the core multi-run
#'   outputs, typically including \code{design}, \code{runs}, and
#'   \code{solutions}.}
#'   \item{\code{summary}}{A named \code{list} containing user-facing summaries
#'   associated with the solution set.}
#'   \item{\code{diagnostics}}{A named \code{list} containing diagnostics about
#'   the solution set as a whole.}
#'   \item{\code{method}}{A named \code{list} describing the optimization method
#'   used.}
#'   \item{\code{meta}}{A named \code{list} of additional metadata.}
#'   \item{\code{name}}{A \code{character(1)} name for the solution set
#'   object.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{print()}}{Print a concise summary of the solution set,
#'   including method name, number of runs, and run-level diagnostics.}
#'
#'   \item{\code{show()}}{Alias of \code{print()}.}
#'
#'   \item{\code{repr()}}{Return a short one-line representation of the
#'   solution set.}
#'
#'   \item{\code{getMethod()}}{Return the method specification stored in
#'   \code{self$method}.}
#'
#'   \item{\code{getDesign()}}{Return the design table stored in
#'   \code{self$solution$design}.}
#'
#'   \item{\code{getRuns()}}{Return the run summary table stored in
#'   \code{self$solution$runs}.}
#'
#'   \item{\code{getSolutions()}}{Return the list of individual
#'   \code{Solution} objects stored in \code{self$solution$solutions}.}
#' }
#'
#' @return No return value. This page documents the \code{SolutionSet} class.
#'
#' @seealso
#' \code{\link{solution-class}},
#' \code{\link{plot_tradeoff}},
#' \code{\link{get_pu}},
#' \code{\link{get_actions}},
#' \code{\link{get_features}},
#' \code{\link{get_targets}},
#' \code{\link{solve}}
NULL

.pa_solutionset_method_label <- function(method) {
  if (is.null(method) || !is.list(method)) return("unknown")
  as.character(method$name %||% method$type %||% "unknown")[1]
}

.pa_solutionset_aliases <- function(method) {
  if (is.null(method) || !is.list(method)) return(character(0))
  aliases <- as.character(method$aliases %||% character(0))
  aliases[!is.na(aliases) & nzchar(aliases)]
}

.pa_solutionset_preview_runs <- function(runs, max_show = 6L) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  val_cols <- grep("^value_", names(runs), value = TRUE)

  if (length(val_cols) == 0L) {
    return(NULL)
  }

  rr <- runs[1, , drop = FALSE]

  txt <- paste0(
    sub("^value_", "", val_cols),
    ": ",
    vapply(rr[val_cols], .pa_num_text, character(1)),
    collapse = ", "
  )

  txt
}

.pa_solutionset_status_summary <- function(runs) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L || !("status" %in% names(runs))) {
    return("none")
  }

  st <- as.character(runs$status)
  st <- st[!is.na(st) & nzchar(st)]

  if (length(st) == 0L) return("none")

  tb <- table(st)
  paste0(names(tb), ": ", as.integer(tb), collapse = ", ")
}

.pa_solutionset_range_text <- function(x, digits = 3) {
  if (is.null(x)) return("none")
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0L) return("none")
  if (length(x) == 1L) return(as.character(round(x, digits)))
  paste0(round(min(x), digits), "..", round(max(x), digits))
}

.pa_solutionset_run_columns <- function(runs) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(list(design = character(0), values = character(0), core = character(0)))
  }

  nm <- names(runs)

  core_cols <- intersect(c("run_id", "status", "runtime", "gap"), nm)
  design_cols <- grep("^(weight_|eps_)", nm, value = TRUE)
  value_cols <- grep("^value_", nm, value = TRUE)

  list(
    core = core_cols,
    design = design_cols,
    values = value_cols
  )
}

.pa_solutionset_preview_table <- function(runs, max_rows = 6L, digits = 3) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  cols <- .pa_solutionset_run_columns(runs)
  keep <- c(cols$core, cols$design, cols$values)
  keep <- unique(keep[keep %in% names(runs)])

  if (length(keep) == 0L) return(NULL)

  out <- runs[seq_len(min(nrow(runs), max_rows)), keep, drop = FALSE]

  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) {
      out[[nm]] <- round(out[[nm]], digits)
    }
  }

  out
}

.pa_solutionset_best_run <- function(runs, method = NULL) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  ok <- rep(TRUE, nrow(runs))
  if ("status" %in% names(runs)) {
    ok <- runs$status %in% c("optimal", "time_limit_feasible", "solution_limit")
    ok[is.na(ok)] <- FALSE
  }

  rr <- runs[ok, , drop = FALSE]
  if (nrow(rr) == 0L) return(NULL)

  mname <- .pa_solutionset_method_label(method)

  if (identical(mname, "weighted")) {
    val_cols <- grep("^value_", names(rr), value = TRUE)
    if (length(val_cols) > 0L) {
      idx <- 1L
      out <- rr[idx, , drop = FALSE]
      return(out)
    }
  }

  rr[1, , drop = FALSE]
}

#' @export
SolutionSet <- pproto(
  "SolutionSet",
  problem = NULL,
  solution = list(),
  summary = list(),
  diagnostics = list(),
  method = list(),
  meta = list(),
  name = "solset",

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    method_name <- .pa_solutionset_method_label(self$method)
    aliases <- .pa_solutionset_aliases(self$method)

    design <- self$solution$design %||% NULL
    runs <- self$solution$runs %||% NULL
    sols <- self$solution$solutions %||% list()

    n_design <- if (inherits(design, "data.frame")) nrow(design) else 0L
    n_runs <- if (inherits(runs, "data.frame")) nrow(runs) else 0L
    n_solutions <- length(sols)

    alias_txt <- if (length(aliases) == 0L) {
      "none"
    } else {
      .pa_preview_text(aliases, max_show = 6L, quote = FALSE)
    }

    status_txt <- .pa_solutionset_status_summary(runs)

    runtime_txt <- if (!is.null(runs) && "runtime" %in% names(runs)) {
      .pa_solutionset_range_text(runs$runtime, digits = 3)
    } else {
      "none"
    }

    gap_txt <- if (!is.null(runs) && "gap" %in% names(runs)) {
      .pa_solutionset_range_text(runs$gap, digits = 4)
    } else {
      "none"
    }

    run_cols <- .pa_solutionset_run_columns(runs)
    design_txt <- if (length(run_cols$design) == 0L) "none" else paste(run_cols$design, collapse = ", ")
    value_txt  <- if (length(run_cols$values) == 0L) "none" else paste(run_cols$values, collapse = ", ")

    cli::cli_text("A paretoscape solution set ({.cls SolutionSet})")

    # ---- METHOD
    cli::cli_text("{ch$j}{ch$b}{.h method}", .envir = environment())
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}name:            {.code {method_name}}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$l}{ch$b}objectives:      {length(aliases)} ({alias_txt})",
      .envir = environment()
    )

    # ---- CONTENT
    cli::cli_text("{ch$l}{ch$b}{.h content}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}design rows:     {n_design}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}runs:            {n_runs}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$l}{ch$b}solutions:       {n_solutions}", .envir = environment())

    # ---- RUN SUMMARY
    cli::cli_text("{ch$l}{ch$b}{.h run summary}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}statuses:        {status_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}runtime:         {runtime_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}gap:             {gap_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}design cols:     {design_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$l}{ch$b}value cols:      {value_txt}", .envir = environment())

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(
      cli::col_grey(
        paste0(
          "# ", info_sym,
          " Use {.code x$solution$runs}, {.code x$solution$design}, ",
          "and {.code x$solution$solutions[[i]]} to inspect details."
        )
      )
    )

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    method_name <- .pa_solutionset_method_label(self$method)
    runs <- self$solution$runs %||% NULL
    n_runs <- if (inherits(runs, "data.frame")) nrow(runs) else 0L
    paste0("<SolutionSet> method=", method_name, ", runs=", n_runs)
  },

  getMethod = function(self) self$method,
  getDesign = function(self) self$solution$design %||% NULL,
  getRuns = function(self) self$solution$runs %||% NULL,
  getSolutions = function(self) self$solution$solutions %||% list()
)
