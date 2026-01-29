#' @include internal.R
#'
#' @title Configure mathematical model (legacy-compatible preset)
#'
#' @description
#' Legacy-compatible helper that stores a model preset inside the `Data` object.
#' In the new pipeline, the MILP is built inside `solve()`. This function does NOT
#' create a C++ model pointer; it only configures `x$data$model_args` for `.pa_build_model()`.
#'
#' @param x Data object (class "Data") created with inputData()/inputDataSpatial().
#' @param model_type character. "minimizeCosts" or "maximizeBenefits".
#' @param budget numeric. Stored in model_args (compat/printing).
#' @param blm numeric. Stored in model_args (compat/printing).
#' @param curve integer. Stored in model_args (compat/printing).
#' @param segments integer. Stored in model_args (compat/printing).
#'
#' @return Updated `Data` object with preset stored in `x$data$model_args`.
#' @export
problem <- function(x,
                    model_type = c("minimizeCosts", "maximizeBenefits"),
                    budget = 0,
                    blm = 0,
                    curve = 1,
                    segments = 3) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  model_type <- match.arg(model_type)

  stopifnot(inherits(x, "Data"))
  stopifnot(!is.null(x$data$pu))
  stopifnot(!is.null(x$data$dist_features))

  # ------------------------------------------------------------
  # 0) Clear any previously built model pointer (new pipeline)
  # ------------------------------------------------------------
  x$data$model_ptr   <- NULL
  x$data$model_index <- NULL
  x$data$model_list  <- NULL
  x$data$has_model   <- FALSE

  # ------------------------------------------------------------
  # 1) Mark preset source (helps solve decide defaults/messages)
  # ------------------------------------------------------------
  if (is.null(x$data$meta) || !is.list(x$data$meta)) x$data$meta <- list()
  x$data$meta$used_problem <- TRUE

  # ------------------------------------------------------------
  # 2) Write preset to model_args (source of truth for builder)
  # ------------------------------------------------------------
  if (is.null(x$data$model_args) || !is.list(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args <- modifyList(
    x$data$model_args,
    list(
      model_type = model_type,
      # objective_id is informational; builder will resolve again
      objective_id = if (identical(model_type, "minimizeCosts")) "min_cost" else "max_benefit",
      modelsense   = if (identical(model_type, "minimizeCosts")) "min" else "max",
      budget   = base::round(as.numeric(budget), 3),
      blm      = as.numeric(blm),
      benefit_exponent = as.integer(curve),
      curve_segments = as.integer(segments),
      configured_by = "problem()"
    )
  )

  # (optional metadata mirror)
  x$data$model_spec <- modifyList(
    x$data$model_spec %||% list(),
    c(x$data$model_args, list(configured_by = "problem()"))
  )

  x
}
