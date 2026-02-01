#' @include internal.R
#'
#' add objective: minimize costs
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
#'   add_objective_min_cost()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
add_objective_min_cost <- function(x, include_pu_cost = TRUE, include_action_cost = TRUE) {
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

#' add objective: maximize benefits
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
#'   add_objective_max_benefit()
#'
#' # Later:
#' # sol <- solve(x)
#' }
#'
#' @export
add_objective_max_benefit <- function(x, benefit_col = "benefit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "maximizeBenefits"
  x$data$model_args$objective_id <- "max_benefit"
  x$data$model_args$objective_args <- list(
    benefit_col = as.character(benefit_col)[1]
  )
  x
}

#' add objective: maximize profit
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
add_objective_max_profit <- function(x, profit_col = "profit") {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "maximizeProfit"
  x$data$model_args$objective_id <- "max_profit"
  x$data$model_args$objective_args <- list(
    profit_col = as.character(profit_col)[1]
  )

  x
}

#' add objective: maximize net profit (profit - total cost)
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
add_objective_max_net_profit <- function(
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

#' add objective: minimize fragmentation (perimeter cut)
#'
#' @description
#' Stores an objective specification to later build a model that minimizes spatial
#' fragmentation measured as the weighted cut between selected vs non-selected PUs:
#' \deqn{\sum_{(i,j)} w_{ij} |z_i - z_j|.}
#' Internally this is linearized with auxiliary edge variables.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param relation_name character. Name of the stored relation in `x$data$spatial_relations`
#'   (e.g. `"boundary"`, `"rook"`, `"queen"`, `"knn"`). Default `"boundary"`.
#' @param weight_multiplier numeric. Multiplies all relation weights (BLM-like). Default 1.
#'
#' @return Updated [`Data`] object.
#' @export
add_objective_min_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1
) {
  stopifnot(inherits(x, "Data"))

  relation_name <- as.character(relation_name)[1]
  weight_multiplier <- as.numeric(weight_multiplier)[1]
  if (!is.finite(weight_multiplier) || weight_multiplier < 0) {
    stop("weight_multiplier must be a finite number >= 0.", call. = FALSE)
  }

  # chequeo suave: que exista la relación (mejor fallar aquí que en build_model)
  rels <- x$data$spatial_relations
  if (is.null(rels) || !is.list(rels) || is.null(rels[[relation_name]])) {
    stop(
      "Spatial relation '", relation_name, "' not found in x$data$spatial_relations. ",
      "Add it first (e.g. add_spatial_boundary()/rook/queen/knn/distance).",
      call. = FALSE
    )
  }

  if (is.null(x$data$model_args)) x$data$model_args <- list()
  x$data$model_args$model_type <- "minimizeFragmentation"
  x$data$model_args$objective_id <- "min_fragmentation"
  x$data$model_args$objective_args <- list(
    relation_name = relation_name,
    weight_multiplier = weight_multiplier
  )

  x
}


#' @export
add_objective_min_action_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    action_weights = NULL,  # named numeric o data.frame(action, weight)
    actions = NULL          # subset opcional de acciones (ids)
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "minimizeActionFragmentation"
  x$data$model_args$objective_id <- "min_action_fragmentation"
  x$data$model_args$objective_args <- list(
    relation_name = as.character(relation_name)[1],
    weight_multiplier = as.numeric(weight_multiplier)[1],
    action_weights = action_weights,
    actions = actions
  )
  x
}

#' @export
add_objective_min_intervention_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  x$data$model_args$model_type <- "minimizeInterventionFragmentation"
  x$data$model_args$objective_id <- "min_intervention_fragmentation"
  x$data$model_args$objective_args <- list(
    relation_name = as.character(relation_name)[1],
    weight_multiplier = as.numeric(weight_multiplier)[1]
  )
  x
}



