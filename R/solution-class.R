#' @include internal.R

#' @export
if (!methods::isClass("Solution")) methods::setOldClass("Solution")
NULL

#' Solution class
#'
#' This class is used to represent the solution of the MIP (Mixed-Integer Programming) model.
#' This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and actions. It is created using the [solve()]
#' function.
#'
#' @section Fields:
#' \describe{
#' \item{$data}{
#' `list`. Object containing data on the results of the optimization process.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{print()}{
#' Print basic information of the model solution.}
#'
#' \item{show()}{
#' Call print method.}
#'
#' }
#' @return No return value.
#'
#' @examples
#' \donttest{
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
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 5, output_file = FALSE, cores = 2)
#'
#' ## Use class methods
#'
#' s$print()
#' }
#'
#' @name solution-class
#'
#' @aliases Solution
NULL

#' @export
#' @export
Solution <- pproto(
  "Solution",
  data = list(),
  Data = NULL,   # en vez de OptimizationClass
  name = "sol",

  print = function(self) {
    if (!requireNamespace("cli", quietly = TRUE)) {
      message(
        "Solution overview",
        "\n  name: ", self$name,
        "\n  objective value: ", round(self$data$objval, 3),
        "\n  gap: ", if (is.numeric(self$data$gap)) paste0(round(self$data$gap * 100, 3), "%") else self$data$gap,
        "\n  status: ", getStatus(self),
        "\n  runtime: ", paste0(round(self$data$runtime, 3), " sec")
      )
      return(invisible(TRUE))
    }

    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    cli::cli_text("{.h Solution} ({.code {self$name}})")

    # --- core metrics
    obj <- self$data$objval
    gap <- self$data$gap
    rt  <- self$data$runtime
    st  <- getStatus(self)

    cli::cli_text("{ch$j}{ch$b}{.h optimization}")
    cli::cli_text("{ch$v}{ch$j}{ch$b}status:        {.strong {st}}")
    cli::cli_text("{ch$v}{ch$j}{ch$b}objective:     {round(obj, 6)}")
    cli::cli_text("{ch$v}{ch$j}{ch$b}gap:           {if (is.numeric(gap)) paste0(round(gap*100,3),'%') else gap}")
    cli::cli_text("{ch$v}{ch$l}{ch$b}runtime:       {round(rt, 3)} sec")

    # --- solver info
    sa <- self$data$args %||% list()
    cli::cli_text("{ch$l}{ch$b}{.h solver}")
    cli::cli_text(" {ch$v}{ch$j}{ch$b}name:          {.code {sa$solver %||% 'unknown'}}")
    cli::cli_text(" {ch$v}{ch$j}{ch$b}cores:         {sa$cores %||% NA}")
    cli::cli_text(" {ch$v}{ch$l}{ch$b}time_limit:    {sa$timelimit %||% NA}")

    # --- model meta (if stored)
    mm <- self$data$model_meta %||% NULL
    if (!is.null(mm)) {
      cli::cli_text("{ch$l}{ch$b}{.h model}")
      cli::cli_text(" {ch$v}{ch$j}{ch$b}type:          {.code {mm$model_type %||% 'unknown'}}")
      cli::cli_text(" {ch$v}{ch$j}{ch$b}objective_id:  {.code {mm$objective_id %||% 'unknown'}} ({mm$modelsense %||% ''})")
      cli::cli_text(" {ch$v}{ch$l}{ch$b}dimensions:    {mm$n_constraints %||% NA} cons, {mm$n_variables %||% NA} vars, {mm$nnz %||% NA} nnz")
    }

    # --- solution sizes (from decoded tables)
    tb <- self$data$tables %||% NULL
    if (!is.null(tb)) {
      cli::cli_text("{ch$l}{ch$b}{.h solution}")
      if (!is.null(tb$pu) && "selected" %in% names(tb$pu)) {
        nsel <- sum(tb$pu$selected %in% 1L, na.rm = TRUE)
        cli::cli_text(" {ch$v}{ch$j}{ch$b}planning units selected:  {nsel}")
      }
      if (!is.null(tb$actions) && "selected" %in% names(tb$actions)) {
        nsel <- sum(tb$actions$selected %in% 1L, na.rm = TRUE)
        cli::cli_text(" {ch$v}{ch$j}{ch$b}actions selected:        {nsel}")
      }
      if (!is.null(tb$targets) && "gap" %in% names(tb$targets)) {
        ok <- sum(tb$targets$gap >= 0, na.rm = TRUE)
        tot <- nrow(tb$targets)
        cli::cli_text(" {ch$v}{ch$l}{ch$b}targets met:             {ok}/{tot}")
      }
    }

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),
  repr = function(self) "Solution object"
)
