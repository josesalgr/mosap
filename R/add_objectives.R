#' @include internal.R
#'
#' Register an atomic objective (for multi-objective workflows)
#'
#' @description
#' Internal helper used by objective setters. If `alias` is provided (non-NULL),
#' the objective is stored in `x$data$objectives[[alias]]` as an *atomic objective*
#' that can later be combined by multi-objective methods (e.g., weighted sum).
#'
#' This is fully backward compatible with the single-objective workflow:
#' if `alias` is NULL, nothing is registered and only `x$data$model_args` is updated.
#'
#' @param x A [`Data`] object.
#' @param alias character or NULL. Unique identifier for the atomic objective.
#' @param objective_id character. Stable objective identifier (e.g., "min_cost").
#' @param model_type character. Model type used by the builder (e.g., "minimizeCosts").
#' @param objective_args list. Objective-specific arguments.
#' @param sense character. Either `"min"` or `"max"`.
#'
#' @return Updated [`Data`] object.
#' @keywords internal

#' add objective: minimize costs
#'
#' @description
#' Stores the objective specification inside the [`Data`] object so it can be
#' materialized later when the optimization model is built (typically from `solve()`).
#'
#' This objective minimizes the total cost associated with planning units and/or actions,
#' depending on the flags provided.
#'
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an *atomic objective* for multi-objective workflows, while preserving the
#' single-objective behavior (the last objective set overwrites the active one).
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param include_pu_cost logical. If `TRUE`, include planning-unit costs in the objective.
#' @param include_action_cost logical. If `TRUE`, include action costs in the objective.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It updates `x$data$model_args`:
#' \describe{
#'   \item{`model_type`}{`"minimizeCosts"`}
#'   \item{`objective_id`}{`"min_cost"`}
#'   \item{`objective_args`}{a list with `include_pu_cost` and `include_action_cost`}
#' }
#'
#' If you call another objective setter afterwards, it will overwrite the previous
#' active (single-objective) objective in `x$data$model_args`.
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   add_objective_min_cost()
#'
#' # atomic registration (for MO workflows)
#' x <- x %>% add_objective_min_cost(alias = "cost")
#' }
#'
#' @export
add_objective_min_cost <- function(
    x,
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "minimizeCosts"
  x$data$model_args$objective_id <- "min_cost"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "min_cost",
    model_type = "minimizeCosts",
    objective_args = args,
    sense = "min"
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
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param benefit_col character. Name of the column in the benefit table (model-ready
#' `dist_benefit_model`) containing numeric benefit values. Default is `"benefit"`.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @details
#' This function does **not** build or solve the optimization model.
#' It updates `x$data$model_args`:
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
#'   \item If you call another objective setter afterwards, it will overwrite the previous
#'   active (single-objective) objective in `x$data$model_args`.
#' }
#'
#' @return Updated [`Data`] object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") %>%
#'   add_actions(actions_df) %>%
#'   add_benefits(benefits_df) %>%
#'   add_objective_max_benefit(alias = "benefit")
#' }
#'
#' @export
add_objective_max_benefit <- function(x, benefit_col = "benefit", alias = NULL) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    benefit_col = as.character(benefit_col)[1]
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "maximizeBenefits"
  x$data$model_args$objective_id <- "max_benefit"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "max_benefit",
    model_type = "maximizeBenefits",
    objective_args = args,
    sense = "max"
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
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding C++ objective.
#'
#' If you call another objective setter afterwards, it will overwrite the previous
#' active (single-objective) objective in `x$data$model_args`.
#'
#' @return Updated [`Data`] object.
#' @export
add_objective_max_profit <- function(x, profit_col = "profit", alias = NULL) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    profit_col = as.character(profit_col)[1]
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "maximizeProfit"
  x$data$model_args$objective_id <- "max_profit"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "max_profit",
    model_type = "maximizeProfit",
    objective_args = args,
    sense = "max"
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
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param profit_col character. Column name in `dist_profit` containing numeric profits.
#'   Default `"profit"`.
#' @param include_pu_cost logical. If `TRUE`, subtract planning-unit costs.
#' @param include_action_cost logical. If `TRUE`, subtract action costs.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @details
#' This function does **not** build or solve the model. It only updates `x$data$model_args`.
#' The model builder (called from `solve()`) must implement the corresponding objective
#' (typically via one C++ routine that sets all objective coefficients).
#'
#' If you call another objective setter afterwards, it will overwrite the previous
#' active (single-objective) objective in `x$data$model_args`.
#'
#' @return Updated [`Data`] object.
#' @export
add_objective_max_net_profit <- function(
    x,
    profit_col = "profit",
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    profit_col = as.character(profit_col)[1],
    include_pu_cost = isTRUE(include_pu_cost),
    include_action_cost = isTRUE(include_action_cost)
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "maximizeNetProfit"
  x$data$model_args$objective_id <- "max_net_profit"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "max_net_profit",
    model_type = "maximizeNetProfit",
    objective_args = args,
    sense = "max"
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
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param relation_name character. Name of the stored relation in `x$data$spatial_relations`
#'   (e.g. `"boundary"`, `"rook"`, `"queen"`, `"knn"`). Default `"boundary"`.
#' @param weight_multiplier numeric. Multiplies all relation weights (BLM-like). Default 1.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return Updated [`Data`] object.
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

  # soft check: relation exists (fail early)
  rels <- x$data$spatial_relations
  if (is.null(rels) || !is.list(rels) || is.null(rels[[relation_name]])) {
    stop(
      "Spatial relation '", relation_name, "' not found in x$data$spatial_relations. ",
      "Add it first (e.g. add_spatial_boundary()/rook/queen/knn/distance).",
      call. = FALSE
    )
  }

  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    relation_name = relation_name,
    weight_multiplier = weight_multiplier
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "minimizeFragmentation"
  x$data$model_args$objective_id <- "min_fragmentation"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "min_fragmentation",
    model_type = "minimizeFragmentation",
    objective_args = args,
    sense = "min"
  )

  x
}

#' add objective: minimize action fragmentation
#'
#' @description
#' Stores an objective specification to later build a model that minimizes *action-level*
#' fragmentation over a given spatial relation.
#'
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param relation_name character. Name of the stored relation in `x$data$spatial_relations`.
#' @param weight_multiplier numeric. Multiplies all relation weights (BLM-like). Default 1.
#' @param action_weights named numeric or data.frame(action, weight). Optional weights per action.
#' @param actions optional subset of actions (ids) to include.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return Updated [`Data`] object.
#' @export
add_objective_min_action_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    action_weights = NULL,  # named numeric or data.frame(action, weight)
    actions = NULL,         # optional subset of actions (ids)
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    relation_name = as.character(relation_name)[1],
    weight_multiplier = as.numeric(weight_multiplier)[1],
    action_weights = action_weights,
    actions = actions
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "minimizeActionFragmentation"
  x$data$model_args$objective_id <- "min_action_fragmentation"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "min_action_fragmentation",
    model_type = "minimizeActionFragmentation",
    objective_args = args,
    sense = "min"
  )

  x
}

#' add objective: minimize intervention fragmentation
#'
#' @description
#' Stores an objective specification to later build a model that minimizes *intervention-level*
#' fragmentation over a given spatial relation.
#'
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A [`Data`] object created with [inputData()] / [inputDataSpatial()].
#' @param relation_name character. Name of the stored relation in `x$data$spatial_relations`.
#' @param weight_multiplier numeric. Multiplies all relation weights (BLM-like). Default 1.
#' @param alias character or NULL. Optional unique identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return Updated [`Data`] object.
#' @export
add_objective_min_intervention_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  args <- list(
    relation_name = as.character(relation_name)[1],
    weight_multiplier = as.numeric(weight_multiplier)[1]
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "minimizeInterventionFragmentation"
  x$data$model_args$objective_id <- "min_intervention_fragmentation"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "min_intervention_fragmentation",
    model_type = "minimizeInterventionFragmentation",
    objective_args = args,
    sense = "min"
  )

  x
}
