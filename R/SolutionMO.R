#' @export
if (!methods::isClass("SolutionMO")) methods::setOldClass("SolutionMO")
NULL

#' @export
SolutionMO <- pproto(
  "SolutionMO",
  method = NULL,
  design = NULL,
  runs = NULL,
  solutions = list(),
  extras = list(),
  meta = list(),

  print = function(self) {
    n_runs <- if (is.null(self$runs)) 0L else nrow(self$runs)
    n_design <- if (is.null(self$design)) 0L else nrow(self$design)
    n_solutions <- length(self$solutions)

    meth <- self$method$name %||% "(unknown)"
    aliases <- self$method$aliases %||% character(0)

    message(
      "A mosap multi-objective result (SolutionMO)\n",
      "  method:        ", meth, "\n",
      "  aliases:       ", if (length(aliases) > 0L) paste(aliases, collapse = ", ") else "(none)", "\n",
      "  design rows:   ", n_design, "\n",
      "  runs:          ", n_runs, "\n",
      "  solutions:     ", n_solutions, "\n",
      "  extras:        ", if (length(self$extras) > 0L) paste(names(self$extras), collapse = ", ") else "(none)"
    )
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    meth <- self$method$name %||% "(unknown)"
    paste0("SolutionMO<", meth, ">")
  },

  getMethod = function(self) self$method,
  getDesign = function(self) self$design,
  getRuns = function(self) self$runs,
  getSolutions = function(self) self$solutions
)
