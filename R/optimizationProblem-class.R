#' @include internal.R

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' This class encodes the corresponding optimization model. It is created
#' using `problem()` function.
#'
#' @section Fields: \describe{
#'
#'   \item{$data}{`list` object containing data
#'   of the mathematical model.}
#'
#'   \item{$ConservationClass}{object of class
#'   [data-class()] that contains the data input.}
#'   }
#'
#' @section Methods: \describe{
#'  \item{getData(`character` name)}{
#'   [vector()]. Object stored in the `data` field with the
#'   corresponding `name`. The data correspond to the different parts of
#'   the mathematical model. The argument `name` can be made to the
#'   following: "obj", "rhs", "sense", "vtype", "A", "bounds" or "modelsense".}
#'
#'   \item{getDataList()}{
#'    [list()] of
#'   [vector()]. Object stored in the `data`. It contains all information relative
#'   to the mathematical model, such as "obj", "rhs", etc.}
#'
#'   \item{print()}{
#'   Print basic information of the optimization model.}
#'
#'   \item{show()}{
#'   Call print method.}
#'
#'   }
#' @return No return value.
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' ## Use class methods
#' head(problem_model$getData("obj"))
#'
#' problem_model$print()
#' @name optimizationProblem-class
#'
#' @aliases OptimizationProblem
NULL

# ---- helpers para imprimir (puedes mover a internal.R) ----
.pa_op_cli_theme <- function() {
  list(
    .h     = list("font-weight" = "bold", color = "#569746"),
    .cls   = list("font-weight" = "bold", color = "blue"),
    .code  = list(color = "green"),
    .muted = list(color = "grey60")
  )
}

.pa_dim <- function(A, size) {
  if (!is.null(A) && inherits(A, "Matrix")) {
    return(c(nrow(A), ncol(A), length(A@x)))
  }
  if (!is.null(size)) return(size)
  c(0, 0, 0)
}

.pa_safe <- function(x, default = NULL) if (is.null(x)) default else x

#' @export
OptimizationProblem <- pproto(
  "OptimizationProblem",
  data = list(),
  ConservationClass = NULL,

  print = function(self) {

    args <- .pa_safe(self$data$args, list())
    idx  <- .pa_safe(self$data$index, list())

    modelsense <- .pa_safe(self$data$modelsense, .pa_safe(args$modelsense, NA_character_))
    model_type <- .pa_safe(args$model_type, NA_character_)
    objective_id <- .pa_safe(args$objective_id, NA_character_)

    # variables
    n_w <- if (!is.null(self$data$var_sizes$n_w)) self$data$var_sizes$n_w else if (!is.null(idx$w_index)) length(idx$w_index) else NA_integer_
    n_x <- if (!is.null(self$data$var_sizes$n_x)) self$data$var_sizes$n_x else if (!is.null(idx$x_index)) length(idx$x_index) else NA_integer_
    n_z <- if (!is.null(self$data$var_sizes$n_z)) self$data$var_sizes$n_z else if (!is.null(idx$z_index)) length(idx$z_index) else 0L

    # dims
    A <- self$data$A
    dims <- .pa_dim(A, self$data$size)
    n_con <- if (!is.null(self$data$rhs)) length(self$data$rhs) else dims[1]
    n_var <- if (!is.null(self$data$obj)) length(self$data$obj) else dims[2]
    nnz   <- dims[3]

    if (!requireNamespace("cli", quietly = TRUE)) {
      message(
        "Optimization Problem",
        "\n  model sense:  ", modelsense,
        "\n  model type:   ", model_type,
        "\n  objective:    ", objective_id,
        "\n  dimensions:   ", n_con, " constraints, ", n_var, " variables, ", nnz, " nonzeros",
        "\n  variables:    w=", n_w, ", x=", n_x, ", z=", n_z,
        if (!is.null(args$budget)) paste0("\n  budget:       ", args$budget) else "",
        if (!is.null(args$blm)) paste0("\n  blm:          ", args$blm) else "",
        if (!is.null(args$curve)) paste0("\n  curve:        ", args$curve) else "",
        if (!is.null(args$segments)) paste0("\n  segments:     ", args$segments) else ""
      )
      return(invisible(TRUE))
    }

    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_op_cli_theme())

    cli::cli_text("A prioriactions optimization model ({.cls OptimizationProblem})")

    cli::cli_text("{ch$j}{ch$b}{.h model}", .envir = environment())
    cli::cli_text("{ch$v}{ch$j}{ch$b}model sense:   {.code {modelsense}}", .envir = environment())
    if (!is.na(model_type))  cli::cli_text("{ch$v}{ch$j}{ch$b}model type:    {.code {model_type}}", .envir = environment())
    if (!is.na(objective_id)) cli::cli_text("{ch$v}{ch$l}{ch$b}objective:     {.code {objective_id}}", .envir = environment())

    cli::cli_text("{ch$j}{ch$b}{.h dimensions}", .envir = environment())
    cli::cli_text("{ch$v}{ch$j}{ch$b}constraints:  {n_con}", .envir = environment())
    cli::cli_text("{ch$v}{ch$j}{ch$b}variables:    {n_var}", .envir = environment())
    cli::cli_text("{ch$v}{ch$l}{ch$b}nonzeros:     {nnz}", .envir = environment())

    cli::cli_text("{ch$j}{ch$b}{.h variables}", .envir = environment())
    cli::cli_text("{ch$v}{ch$j}{ch$b}w (PU select):     {n_w}", .envir = environment())
    cli::cli_text("{ch$v}{ch$j}{ch$b}x (actions):       {n_x}", .envir = environment())
    cli::cli_text("{ch$v}{ch$l}{ch$b}z (conservation):  {n_z}", .envir = environment())

    cli::cli_text("{ch$l}{ch$b}{.h parameters}", .envir = environment())
    if (!is.null(args$budget))   cli::cli_text(" {ch$v}{ch$j}{ch$b}budget:   {args$budget}", .envir = environment())
    if (!is.null(args$blm))      cli::cli_text(" {ch$v}{ch$j}{ch$b}blm:      {args$blm}", .envir = environment())
    if (!is.null(args$curve))    cli::cli_text(" {ch$v}{ch$j}{ch$b}curve:    {args$curve}", .envir = environment())
    if (!is.null(args$segments)) cli::cli_text(" {ch$v}{ch$l}{ch$b}segments: {args$segments}", .envir = environment())

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(cli::col_grey(
      paste0("# ", info_sym, " Use {.code model$getDataList()} to inspect matrices/vectors.")
    ))

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) "OptimizationProblem object",

  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) return("x object do not found")
    self$data[[x]]
  },

  getDataList = function(self) self$data
)
