#' @include internal.R
NULL

# =========================================================
# S4 registration for pproto class
# =========================================================
#' @export
if (!methods::isClass("MOResult")) methods::setOldClass("MOResult")
NULL

# =========================================================
#' MOResult class
#'
#' Container for results produced by multi-objective methods in prioriactionsMO.
#'
#' An `MOResult` stores:
#' 1) A run table (one row per run/solution),
#' 2) The list of underlying single-objective `prioriactions::Solution` objects,
#' 3) Metadata (method config, call, timestamps, etc.).
#'
#' @section Fields:
#' \describe{
#'   \item{runs}{A `data.frame` with one row per run (weights/epsilons, status, runtime, gap, objective values, etc.).}
#'   \item{solutions}{A list of `prioriactions::Solution` objects (one per run).}
#'   \item{meta}{A list with method configuration, call, and any additional bookkeeping.}
#' }
#'
#' @name mo-result-class
#' @aliases MOResult
#' @keywords internal
NULL

#' @export
MOResult <- pproto(
  "MOResult",

  runs = NULL,
  solutions = NULL,
  meta = list(),

  print = function(self) {

    n_runs <- if (is.null(self$runs)) 0L else nrow(self$runs)
    n_sol  <- if (is.null(self$solutions)) 0L else length(self$solutions)

    meth <- "(unknown)"
    if (!is.null(self$meta$method) && is.list(self$meta$method)) {
      meth <- self$meta$method$name %||% "(unnamed)"
    }

    msg <- paste0(
      "A prioriactionsMO object (MOResult)\n",
      "  method:        ", meth, "\n",
      "  runs:          ", n_runs, "\n",
      "  solutions:     ", n_sol, "\n",
      "  has meta:      ", if (length(self$meta) > 0) "yes" else "no"
    )
    message(msg)
    invisible(TRUE)
  },

  show = function(self) self$print(),
  repr = function(self) "MOResult object",

  getRuns = function(self) self$runs,
  getSolutions = function(self) self$solutions,

  getSolution = function(self, run_id = 1L) {
    run_id <- as.integer(run_id)[1]
    if (is.null(self$solutions) || length(self$solutions) == 0) return(NULL)
    if (run_id < 1L || run_id > length(self$solutions)) return(NULL)
    self$solutions[[run_id]]
  }
)
