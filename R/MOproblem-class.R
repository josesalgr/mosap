#' @include internal.R
NULL

# =========================================================
# S4 registration for pproto class
# =========================================================
#' @export
if (!methods::isClass("MOProblem")) methods::setOldClass("MOProblem")
NULL

# =========================================================
#' MOProblem class
#'
#' A container for multi-objective optimization workflows built on top of a
#' prioriactions `Data` object.
#'
#' The goal of `MOProblem` is to keep `prioriactions` focused on defining data,
#' constraints and *atomic objectives*, while `prioriactionsMO` orchestrates
#' *multi-objective methods* (weighted sum, epsilon-constraint, AUGMECON,
#' goal programming, interactive, etc.).
#'
#' Conceptually, an `MOProblem` wraps:
#'
#' 1) A base `Data` object (coming from `prioriactions::inputData()`),
#' 2) A registry of atomic objectives (each with alias, direction and builders),
#' 3) A multi-objective "method" configuration (e.g., weights, epsilons),
#' 4) Results (solutions, objective values, solver logs).
#'
#' @section Fields:
#' \describe{
#'   \item{data}{A `Data` object from `prioriactions`.}
#'   \item{objectives}{A named list of registered objectives (by alias).}
#'   \item{method}{A list describing the selected multi-objective method and its parameters.}
#'   \item{results}{A list storing solutions, objective values and diagnostics.}
#'   \item{meta}{A list for bookkeeping (package versions, creation time, etc.).}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{print():}{Print a compact summary.}
#'   \item{show():}{Alias of `print()`.}
#'   \item{repr():}{Short representation.}
#'   \item{getData():}{Return the underlying `Data` object.}
#'   \item{listObjectives():}{Return objective aliases currently registered.}
#'   \item{getObjective(alias):}{Return an objective definition by alias.}
#' }
#'
#' @name mo-problem-class
#' @aliases MOProblem
NULL

#' @export
MOProblem <- pproto(
  "MOProblem",

  data = NULL,
  objectives = list(),
  method = NULL,
  results = NULL,
  meta = list(),

  print = function(self) {

    # basic summary without requiring cli
    cls_data <- if (is.null(self$data)) "NULL" else class(self$data)[1]
    n_obj <- length(self$objectives)
    obj_alias <- names(self$objectives)
    meth <- if (is.null(self$method)) "(none)" else self$method$name %||% "(unnamed)"

    msg <- paste0(
      "A prioriactionsMO object (MOProblem)\n",
      "  base data:     ", cls_data, "\n",
      "  objectives:    ", n_obj, if (n_obj > 0) paste0(" (", paste(obj_alias, collapse = ", "), ")") else "", "\n",
      "  method:        ", meth, "\n",
      "  results:       ", if (is.null(self$results)) "(none)" else "(available)"
    )
    message(msg)
    invisible(TRUE)
  },

  show = function(self) self$print(),
  repr = function(self) "MOProblem object",

  getData = function(self) self$data,

  listObjectives = function(self) {
    if (length(self$objectives) == 0) return(character(0))
    names(self$objectives)
  },

  getObjective = function(self, alias) {
    stopifnot(is.character(alias), length(alias) == 1L)
    if (!alias %in% names(self$objectives)) return(NULL)
    self$objectives[[alias]]
  }
)


# =========================================================
# Constructor / coercion
# =========================================================

#' Create a multi-objective problem wrapper
#'
#' @param x A `Data` object created by `prioriactions::inputData()` (or a compatible object).
#' @return An object of class `MOProblem`.
#' @export
as_mo_problem <- function(x) {

  if (!inherits(x, "Data")) {
    stop("as_mo_problem(): `x` must inherit from class 'Data'.", call. = FALSE)
  }

  pproto(
    NULL, MOProblem,
    data = x,
    objectives = list(),
    method = NULL,
    results = NULL,
    meta = list(created = Sys.time())
  )
}

# =========================================================
# DSL support: x + f(...)
# =========================================================
# This matches the style typically used in these packages:
# e2 must be a function that takes e1 and returns an updated object.
# e.g. x + add_objective_min_cost(alias="cost")
#
# In practice, add_* functions in prioriactions often return such a function.

#' @export
Ops.MOProblem <- function(e1, e2) {
  op <- .Generic
  if (op != "+") stop("Only '+' is supported for MOProblem DSL.", call. = FALSE)

  if (!is.function(e2)) {
    stop("Right-hand side of '+' must be a function.", call. = FALSE)
  }
  e2(e1)
}

# =========================================================
# Objective registry (minimal, atomic objective definitions)
# =========================================================

#' Register an atomic objective in an MOProblem
#'
#' This does not solve anything. It only records how to evaluate/build the objective.
#' Multi-objective methods (weighted, epsilon, etc.) will use this registry later.
#'
#' @param x `MOProblem`.
#' @param alias `character(1)` unique objective identifier used by MO methods.
#' @param sense `character(1)` either "min" or "max".
#' @param build_fun A function `(data, ...) -> data_or_problem` that activates this objective.
#'   For now you can store it and decide later whether it returns a new model pointer,
#'   an objective vector, or a configured `OptimizationProblem`.
#' @param eval_fun Optional function `(solution, data, ...) -> numeric(1)` to evaluate objective value.
#' @param ... Reserved for future extensions.
#'
#' @return A function that takes an `MOProblem` (for DSL `x + ...`) and returns the updated object.
#' @export
register_objective <- function(alias, sense = c("min", "max"), build_fun, eval_fun = NULL, ...) {
  sense <- match.arg(sense)

  if (!is.character(alias) || length(alias) != 1L || nchar(alias) == 0) {
    stop("register_objective(): `alias` must be a non-empty character(1).", call. = FALSE)
  }
  if (!is.function(build_fun)) {
    stop("register_objective(): `build_fun` must be a function.", call. = FALSE)
  }
  if (!is.null(eval_fun) && !is.function(eval_fun)) {
    stop("register_objective(): `eval_fun` must be NULL or a function.", call. = FALSE)
  }

  dots <- list(...)

  function(x) {
    if (!inherits(x, "MOProblem")) {
      stop("register_objective(): left-hand side must be an MOProblem.", call. = FALSE)
    }
    if (alias %in% names(x$objectives)) {
      stop("Objective alias already exists: '", alias, "'.", call. = FALSE)
    }

    x$objectives[[alias]] <- list(
      alias = alias,
      sense = sense,
      build_fun = build_fun,
      eval_fun = eval_fun,
      args = dots
    )

    x
  }
}
