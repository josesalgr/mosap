#' @include internal.R
#'
#' @export
if (!methods::isClass("SolutionSet")) methods::setOldClass("SolutionSet")
NULL

#' SolutionSet class
#'
#' @description
#' The `SolutionSet` class stores the output of solving a `Problem` object in
#' `mosap`. It contains the optimization status, objective value, solver
#' metadata, decoded decision vectors, and human-readable result tables.
#'
#' Objects of this class are typically created with [solve()].
#'
#' @section Fields:
#' \describe{
#'   \item{data}{A named `list` containing optimization outputs, such as the
#'   objective value, raw solution vector, optimality gap, runtime, solver
#'   arguments, decoded decision vectors, and result tables.}
#'   \item{Problem}{The `Problem` object used to generate the solution.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{print()}{Print a concise summary of the solution, including solver
#'   status, objective value, runtime, selection summary, and target fulfillment.}
#'   \item{show()}{Alias of `print()`.}
#'   \item{repr()}{Returns a short string representation.}
#' }
#'
#' @return No return value.
#'
#' @name solutionset-class
#' @aliases SolutionSet
NULL

.pa_solutionset_method_label <- function(method) {
  if (is.null(method) || !is.list(method)) return("unknown")
  as.character(method$name %||% "unknown")[1]
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
    # for weighted, use first value_* column as a simple preview criterion if present
    val_cols <- grep("^value_", names(rr), value = TRUE)
    if (length(val_cols) > 0L) {
      idx <- 1L
      out <- rr[idx, , drop = FALSE]
      return(out)
    }
  }

  # fallback: first feasible row
  rr[1, , drop = FALSE]
}

#' @export
SolutionSet <- pproto(
  "SolutionSet",
  data = NULL,
  method = NULL,
  design = NULL,
  runs = NULL,
  solutions = list(),
  extras = list(),
  meta = list(),

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    method_name <- .pa_solutionset_method_label(self$method)
    aliases <- .pa_solutionset_aliases(self$method)

    n_design <- if (inherits(self$design, "data.frame")) nrow(self$design) else 0L
    n_runs <- if (inherits(self$runs, "data.frame")) nrow(self$runs) else 0L
    n_solutions <- length(self$solutions)
    n_extras <- length(self$extras %||% list())

    alias_txt <- if (length(aliases) == 0L) {
      "none"
    } else {
      .pa_preview_text(aliases, max_show = 6L, quote = FALSE)
    }

    status_txt <- .pa_solutionset_status_summary(self$runs)

    runtime_txt <- if (!is.null(self$runs) && "runtime" %in% names(self$runs)) {
      .pa_solutionset_range_text(self$runs$runtime, digits = 3)
    } else {
      "none"
    }

    gap_txt <- if (!is.null(self$runs) && "gap" %in% names(self$runs)) {
      .pa_solutionset_range_text(self$runs$gap, digits = 4)
    } else {
      "none"
    }

    run_cols <- .pa_solutionset_run_columns(self$runs)
    design_txt <- if (length(run_cols$design) == 0L) "none" else paste(run_cols$design, collapse = ", ")
    value_txt  <- if (length(run_cols$values) == 0L) "none" else paste(run_cols$values, collapse = ", ")

    cli::cli_text("A mosap solution set ({.cls SolutionSet})")

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
    cli::cli_text(" {ch$v}{ch$j}{ch$b}solutions:       {n_solutions}", .envir = environment())
    cli::cli_text(
      " {ch$v}{ch$l}{ch$b}extras:          {if (n_extras > 0L) paste(names(self$extras), collapse = ', ') else '{.muted none}'}",
      .envir = environment()
    )

    # ---- RUN SUMMARY
    cli::cli_text("{ch$l}{ch$b}{.h run summary}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}statuses:        {status_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}runtime:         {runtime_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}gap:             {gap_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}design cols:     {design_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$l}{ch$b}value cols:      {value_txt}", .envir = environment())

    # ---- RUN PREVIEW
    # prev <- .pa_solutionset_preview_table(self$runs, max_rows = 6L, digits = 3)
    #
    # cli::cli_text("{ch$l}{ch$b}{.h run preview}", .envir = environment())
    #
    # if (is.null(prev) || nrow(prev) == 0L) {
    #   cli::cli_text(" {ch$v}{ch$l}{ch$b}{.muted none}", .envir = environment())
    # } else {
    #   prev_df <- as.data.frame(prev, stringsAsFactors = FALSE)
    #   class(prev_df) <- "data.frame"
    #
    #   txt <- utils::capture.output(
    #     print(prev_df)
    #   )
    #
    #   cli::cli_verbatim(paste(txt, collapse = "\n"))
    # }

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(
      cli::col_grey(
        paste0("# ", info_sym, " Use {.code x$runs}, {.code x$design}, and {.code x$solutions[[i]]} to inspect details.")
      )
    )

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    method_name <- .pa_solutionset_method_label(self$method)
    n_runs <- if (inherits(self$runs, "data.frame")) nrow(self$runs) else 0L
    paste0("<SolutionSet> method=", method_name, ", runs=", n_runs)
  },

  getMethod = function(self) self$method,
  getDesign = function(self) self$design,
  getRuns = function(self) self$runs,
  getSolutions = function(self) self$solutions
)
