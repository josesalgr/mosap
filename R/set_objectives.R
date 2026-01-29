#' Set objective: minimize costs
#'
#' @description
#' Stores the objective specification inside the [`Data`] object so it can be
#' materialized later when the optimization model is built (typically from `solve()`).
#'
#' This objective minimizes the total cost associated with planning units and/or actions,
#' depending on the flags provided.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param include_pu_cost logical. If `TRUE`, include planning-unit costs in the objective.
#' @param include_action_cost logical. If `TRUE`, include action costs in the objective.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It only updates `x$data$model_args`:
#' \describe{
#'   \item{`model_type`}{`"minimizeCosts"`}
#'   \item{`objective_id`}{`"min_cost"`}
#'   \item{`objective_args`}{a list with `include_pu_cost` and `include_action_cost`}
#' }
#'
#' If you call another objective setter afterwards, it will overwrite the previous objective.
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   set_objective_min_cost()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
set_objective_min_cost <- function(x, include_pu_cost = TRUE, include_action_cost = TRUE) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "minimizeCosts"
  x$data$model_args$objective_id <- "min_cost"
  x$data$model_args$objective_args <- list(
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )
  x
}

#' Set objective: maximize benefits
#'
#' @description
#' Stores the objective specification inside the [`Data`] object so it can be
#' materialized later when the optimization model is built (typically from `solve()`).
#'
#' This objective maximizes the total benefit delivered by selected actions, using the
#' benefit values stored in `dist_benefit`.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param benefit_col character. Name of the column in the benefit table (model-ready
#' `dist_benefit_model`) containing numeric benefit values. Default is `"benefit"`.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It only updates `x$data$model_args`:
#' \describe{
#'   \item{`model_type`}{`"maximizeBenefits"`}
#'   \item{`objective_id`}{`"max_benefit"`}
#'   \item{`objective_args`}{a list with `benefit_col`}
#' }
#'
#' Notes:
#' \itemize{
#'   \item The model builder will require `dist_benefit` to exist (typically created by
#'   [add_benefits()]) and will error if benefits are missing.
#'   \item If you call another objective setter afterwards, it will overwrite the previous objective.
#' }
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   add_benefits(benefits_df) %>%
#'   set_objective_max_benefit()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
set_objective_max_benefit <- function(x, benefit_col = "benefit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "maximizeBenefits"
  x$data$model_args$objective_id <- "max_benefit"
  x$data$model_args$objective_args <- list(
    benefit_col = as.character(benefit_col)[1]
  )
  x
}

#' Set objective: minimize costs
#'
#' @description
#' Stores the objective specification inside the [`Data`] object so it can be
#' materialized later when the optimization model is built (typically from `solve()`).
#'
#' This objective minimizes the total cost associated with planning units and/or actions,
#' depending on the flags provided.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param include_pu_cost logical. If `TRUE`, include planning-unit costs in the objective.
#' @param include_action_cost logical. If `TRUE`, include action costs in the objective.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It only updates `x$data$model_args`:
#' \describe{
#'   \item{`model_type`}{`"minimizeCosts"`}
#'   \item{`objective_id`}{`"min_cost"`}
#'   \item{`objective_args`}{a list with `include_pu_cost` and `include_action_cost`}
#' }
#'
#' If you call another objective setter afterwards, it will overwrite the previous objective.
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   set_objective_min_cost()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
set_objective_min_cost <- function(x, include_pu_cost = TRUE, include_action_cost = TRUE) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "minimizeCosts"
  x$data$model_args$objective_id <- "min_cost"
  x$data$model_args$objective_args <- list(
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )
  x
}

#' Set objective: maximize benefits
#'
#' @description
#' Stores the objective specification inside the [`Data`] object so it can be
#' materialized later when the optimization model is built (typically from `solve()`).
#'
#' This objective maximizes the total benefit delivered by selected actions, using the
#' benefit values stored in `dist_benefit`.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param benefit_col character. Name of the column in the benefit table (model-ready
#' `dist_benefit_model`) containing numeric benefit values. Default is `"benefit"`.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It only updates `x$data$model_args`:
#' \describe{
#'   \item{`model_type`}{`"maximizeBenefits"`}
#'   \item{`objective_id`}{`"max_benefit"`}
#'   \item{`objective_args`}{a list with `benefit_col`}
#' }
#'
#' Notes:
#' \itemize{
#'   \item The model builder will require `dist_benefit` to exist (typically created by
#'   [add_benefits()]) and will error if benefits are missing.
#'   \item If you call another objective setter afterwards, it will overwrite the previous objective.
#' }
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   add_benefits(benefits_df) %>%
#'   set_objective_max_benefit()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
set_objective_max_benefit <- function(x, benefit_col = "benefit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "maximizeBenefits"
  x$data$model_args$objective_id <- "max_benefit"
  x$data$model_args$objective_args <- list(
    benefit_col = as.character(benefit_col)[1]
  )
  x
}

#' Set objective: maximize profit
#'
#' @description
#' Stores an objective specification inside the [`Data`] object to later build an
#' optimization model that **maximizes economic profit** from selected (pu, action) pairs.
#'
#' This objective uses `x$data$dist_profit` (created with [add_profit()]).
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding C++ objective.
#'
#' @return Updated [`Data`] object.
#' @export
set_objective_max_profit <- function(x, profit_col = "profit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "maximizeProfit"
  x$data$model_args$objective_id <- "max_profit"
  x$data$model_args$objective_args <- list(
    profit_col = as.character(profit_col)[1]
  )

  x
}

#' Set objective: maximize net profit (profit - total cost)
#'
#' @description
#' Stores an objective specification inside the [`Data`] object to later build an
#' optimization model that **maximizes net profit**, defined as:
#' \deqn{\sum profit \cdot x \;-\; \left(\sum pu\_cost \cdot w + \sum action\_cost \cdot x\right).}
#'
#' Profit is taken from `x$data$dist_profit` (created with [add_profit()]).
#' Costs are taken from `x$data$pu` (planning-unit costs) and `x$data$dist_actions` (action costs).
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#' @param include_pu_cost logical. If `TRUE`, subtract planning-unit costs.
#' @param include_action_cost logical. If `TRUE`, subtract action costs.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding objective
#' (typically via one C++ routine that sets all objective coefficients).
#'
#' @return Updated [`Data`] object.
#' @export
set_objective_max_net_profit <- function(
    x,
    profit_col = "profit",
    include_pu_cost = TRUE,
    include_action_cost = TRUE
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "maximizeNetProfit"
  x$data$model_args$objective_id <- "max_net_profit"
  x$data$model_args$objective_args <- list(
    profit_col = as.character(profit_col)[1],
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )

  x
}


#' Set objective: maximize profit
#'
#' @description
#' Stores an objective specification inside the [`Data`] object to later build an
#' optimization model that **maximizes economic profit** from selected (pu, action) pairs.
#'
#' This objective uses `x$data$dist_profit` (created with [add_profit()]).
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding C++ objective.
#'
#' @return Updated [`Data`] object.
#' @export
set_objective_max_profit <- function(x, profit_col = "profit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "maximizeProfit"
  x$data$model_args$objective_id <- "max_profit"
  x$data$model_args$objective_args <- list(
    profit_col = as.character(profit_col)[1]
  )

  x
}

#' Set objective: maximize net profit (profit - total cost)
#'
#' @description
#' Stores an objective specification inside the [`Data`] object to later build an
#' optimization model that **maximizes net profit**, defined as:
#' \deqn{\sum profit \cdot x \;-\; \left(\sum pu\_cost \cdot w + \sum action\_cost \cdot x\right).}
#'
#' Profit is taken from `x$data$dist_profit` (created with [add_profit()]).
#' Costs are taken from `x$data$pu` (planning-unit costs) and `x$data$dist_actions` (action costs).
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#' @param include_pu_cost logical. If `TRUE`, subtract planning-unit costs.
#' @param include_action_cost logical. If `TRUE`, subtract action costs.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding objective
#' (typically via one C++ routine that sets all objective coefficients).
#'
#' @return Updated [`Data`] object.
#' @export
set_objective_max_net_profit <- function(
    x,
    profit_col = "profit",
    include_pu_cost = TRUE,
    include_action_cost = TRUE
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "maximizeNetProfit"
  x$data$model_args$objective_id <- "max_net_profit"
  x$data$model_args$objective_args <- list(
    profit_col = as.character(profit_col)[1],
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )

  x
}

