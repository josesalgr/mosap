#' @include internal.R
#'
#' @title Register an atomic objective (internal)
#'
#' @description
#' Internal helper used by objective setter functions to optionally register an objective
#' as an \emph{atomic objective} for multi-objective workflows.
#'
#' If \code{alias} is non-\code{NULL}, the objective definition is stored in
#' \code{x$data$objectives[[alias]]}. This allows external multi-objective orchestration
#' (e.g., weighted sum, \eqn{\epsilon}-constraint, AUGMECON, interactive methods) to refer to
#' objectives by a stable user-facing identifier.
#'
#' The function is fully backward compatible with single-objective workflows: when
#' \code{alias} is \code{NULL}, no entry is added to \code{x$data$objectives} and only the
#' active single-objective specification stored in \code{x$data$model_args} (set by the calling
#' objective setter) is used.
#'
#' @param x A \code{Data} object.
#' @param alias Character scalar or \code{NULL}. Unique identifier to register the objective.
#' @param objective_id Character. Stable objective identifier (e.g., \code{"min_cost"}).
#' @param model_type Character. Model type label used by the model builder (e.g., \code{"minimizeCosts"}).
#' @param objective_args List. Objective-specific arguments to be stored with the objective.
#' @param sense Character. Either \code{"min"} or \code{"max"}.
#'
#' @return The updated \code{Data} object.
#'
#' @keywords internal
#'
#' @title Add objective: minimize costs
#'
#' @description
#' Specify an objective that minimizes total costs associated with the solution.
#' Costs may include planning-unit costs and/or action costs. When `actions` is provided,
#' the action-cost component is restricted to that subset of actions, while planning-unit
#' costs remain global.
#'
#' @param x A `Data` object.
#' @param include_pu_cost Logical. If `TRUE`, include planning-unit costs.
#' @param include_action_cost Logical. If `TRUE`, include action costs.
#' @param actions Optional subset of actions to include in the action-cost component.
#'   Values may match `actions$id` and, if present, `actions$action_set`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_min_cost <- function(
    x,
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  action_subset <- NULL
  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  args <- list(
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost),
    actions = if (is.null(action_subset)) NULL else as.character(action_subset$id)
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "minimizeCosts",
    objective_id = "min_cost",
    objective_args = args,
    sense = "min",
    alias = alias
  )
}

#' @title Add objective: maximize benefit
#'
#' @description
#' Specify an objective that maximizes total benefit delivered by selected actions.
#' The objective can optionally be restricted to subsets of actions and/or features.
#'
#' @param x A `Data` object.
#' @param benefit_col Character. Benefit column name in the model-ready effects table.
#' @param actions Optional subset of actions to include. Values may match `actions$id`
#'   and, if present, `actions$action_set`.
#' @param features Optional subset of features to include. Values may match
#'   `features$id` and, if present, `features$name`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_max_benefit <- function(
    x,
    benefit_col = "benefit",
    actions = NULL,
    features = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  action_subset <- NULL
  feature_subset <- NULL

  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }
  if (!is.null(features)) {
    feature_subset <- .pa_resolve_feature_subset(x, features)
  }

  args <- list(
    benefit_col = as.character(benefit_col)[1],
    actions = if (is.null(action_subset)) NULL else as.character(action_subset$id),
    features = if (is.null(feature_subset)) NULL else feature_subset$id
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "maximizeBenefits",
    objective_id = "max_benefit",
    objective_args = args,
    sense = "max",
    alias = alias
  )
}

#' @title Add objective: maximize profit
#'
#' @description
#' Specify an objective that maximizes total profit from selected `(pu, action)` pairs.
#' The objective can optionally be restricted to a subset of actions.
#'
#' @param x A `Data` object.
#' @param profit_col Character. Profit column in `x$data$dist_profit`.
#' @param actions Optional subset of actions to include. Values may match `actions$id`
#'   and, if present, `actions$action_set`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_max_profit <- function(
    x,
    profit_col = "profit",
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  action_subset <- NULL
  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  args <- list(
    profit_col = as.character(profit_col)[1],
    actions = if (is.null(action_subset)) NULL else as.character(action_subset$id)
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "maximizeProfit",
    objective_id = "max_profit",
    objective_args = args,
    sense = "max",
    alias = alias
  )
}

#' @title Add objective: maximize net profit
#'
#' @description
#' Specify an objective that maximizes net profit, optionally restricting the
#' profit and action-cost components to a subset of actions.
#'
#' @param x A `Data` object.
#' @param profit_col Character. Profit column in `x$data$dist_profit`.
#' @param include_pu_cost Logical. If `TRUE`, subtract planning-unit costs.
#' @param include_action_cost Logical. If `TRUE`, subtract action costs.
#' @param actions Optional subset of actions to include in the profit and action-cost terms.
#'   Values may match `actions$id` and, if present, `actions$action_set`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_max_net_profit <- function(
    x,
    profit_col = "profit",
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  action_subset <- NULL
  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  args <- list(
    profit_col = as.character(profit_col)[1],
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost),
    actions = if (is.null(action_subset)) NULL else as.character(action_subset$id)
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "maximizeNetProfit",
    objective_id = "max_net_profit",
    objective_args = args,
    sense = "max",
    alias = alias
  )
}

#' @title Add objective: minimize fragmentation
#'
#' @description
#' Specify an objective that minimizes planning-unit fragmentation over a spatial relation.
#'
#' @param x A `Data` object.
#' @param relation_name Character. Name of the spatial relation.
#' @param weight_multiplier Numeric >= 0. Multiplier applied to relation weights.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_min_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  relation_name <- as.character(relation_name)[1]
  weight_multiplier <- as.numeric(weight_multiplier)[1]

  if (!is.finite(weight_multiplier) || weight_multiplier < 0) {
    stop("weight_multiplier must be a finite number >= 0.", call. = FALSE)
  }

  rels <- x$data$spatial_relations
  if (is.null(rels) || !is.list(rels) || is.null(rels[[relation_name]])) {
    stop(
      "Spatial relation '", relation_name, "' not found in x$data$spatial_relations. ",
      "Add it first.",
      call. = FALSE
    )
  }

  args <- list(
    relation_name = relation_name,
    weight_multiplier = weight_multiplier
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "minimizeFragmentation",
    objective_id = "min_fragmentation",
    objective_args = args,
    sense = "min",
    alias = alias
  )
}

#' @title Add objective: minimize action fragmentation
#'
#' @description
#' Specify an objective that minimizes fragmentation at the action level over
#' a spatial relation. The objective can optionally be restricted to a subset
#' of actions and/or weighted by action.
#'
#' @param x A `Data` object.
#' @param relation_name Character. Name of the spatial relation.
#' @param weight_multiplier Numeric >= 0. Multiplier applied to relation weights.
#' @param action_weights Optional action weights. Either a named numeric vector
#'   (names = action ids) or a `data.frame(action, weight)`.
#' @param actions Optional subset of actions to include. Values may match `actions$id`
#'   and, if present, `actions$action_set`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_min_action_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    action_weights = NULL,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  action_subset <- NULL
  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  args <- list(
    relation_name = as.character(relation_name)[1],
    weight_multiplier = as.numeric(weight_multiplier)[1],
    action_weights = action_weights,
    actions = if (is.null(action_subset)) NULL else as.character(action_subset$id)
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "minimizeActionFragmentation",
    objective_id = "min_action_fragmentation",
    objective_args = args,
    sense = "min",
    alias = alias
  )
}


#' @title Add objective: maximize representation
#'
#' @description
#' Specify an objective that maximizes total representation across a subset of features.
#'
#' @param x A `Data` object.
#' @param amount_col Character. Column in `dist_features` containing amounts.
#' @param features Optional subset of features to include. Values may match
#'   `features$id` and, if present, `features$name`.
#' @param alias Optional identifier for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_max_representation <- function(
    x,
    amount_col = "amount",
    features = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  feature_subset <- NULL
  if (!is.null(features)) {
    feature_subset <- .pa_resolve_feature_subset(x, features)
  }

  args <- list(
    amount_col = as.character(amount_col)[1],
    features = if (is.null(feature_subset)) NULL else feature_subset$id
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "maximizeRepresentation",
    objective_id = "max_representation",
    objective_args = args,
    sense = "max",
    alias = alias
  )
}


#' @title Add objective: minimize intervention impact
#'
#' @description
#' Specify an objective that minimizes the impact associated with selecting
#' planning units for intervention. This objective uses planning-unit selection
#' variables (`w`) so that each planning unit contributes at most once,
#' regardless of how many feasible actions exist in that unit.
#'
#' Impact values are taken from the baseline feature distribution in
#' `x$data$dist_features`. Optionally, the objective can be restricted to a
#' subset of features.
#'
#' @param x A `Data` object.
#' @param impact_col Character. Column in `x$data$dist_features` containing the
#'   per-(pu,feature) impact amount. Default `"amount"`.
#' @param features Optional subset of features to include. Can be feature ids
#'   and, if available, feature names.
#' @param alias Optional alias for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_min_intervention_impact <- function(
    x,
    impact_col = "amount",
    features = NULL,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))

  impact_col <- as.character(impact_col)[1]
  if (is.na(impact_col) || !nzchar(impact_col)) {
    stop("impact_col must be a non-empty string.", call. = FALSE)
  }

  if (is.null(x$data$dist_features) ||
      !inherits(x$data$dist_features, "data.frame") ||
      nrow(x$data$dist_features) == 0) {
    stop("x$data$dist_features is missing or empty.", call. = FALSE)
  }

  if (!(impact_col %in% names(x$data$dist_features))) {
    stop(
      "impact_col '", impact_col, "' was not found in x$data$dist_features.",
      call. = FALSE
    )
  }

  feat_subset <- .pa_resolve_feature_subset(x, features = features)
  act_subset  <- .pa_resolve_action_subset(x, subset = actions)

  args <- list(
    impact_col = impact_col,
    features = feat_subset$id,
    actions = act_subset$id
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "minimizeInterventionImpact",
    objective_id = "min_intervention_impact",
    objective_args = args,
    sense = "min",
    alias = alias
  )
}
