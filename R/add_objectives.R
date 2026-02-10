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
#' Costs may include planning-unit costs and/or action costs depending on the flags provided.
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built
#' (typically when calling \code{solve()}).
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows, while preserving the legacy
#' single-objective behavior (the most recently set objective remains the active one in
#' \code{x$data$model_args}).
#'
#' @details
#' The function updates \code{x$data$model_args} with:
#' \describe{
#'   \item{\code{model_type}}{\code{"minimizeCosts"}}
#'   \item{\code{objective_id}}{\code{"min_cost"}}
#'   \item{\code{objective_args}}{a list with \code{include_pu_cost} and \code{include_action_cost}}
#' }
#'
#' The model builder must interpret these fields to set the objective coefficients.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param include_pu_cost Logical. If \code{TRUE}, include planning-unit costs in the objective.
#' @param include_action_cost Logical. If \code{TRUE}, include action costs in the objective.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") |>
#'   add_actions(actions_df) |>
#'   add_objective_min_cost()
#'
#' # Register as atomic objective for multi-objective workflows
#' x <- x |> add_objective_min_cost(alias = "cost")
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

#' @title Add objective: maximize benefit
#'
#' @description
#' Specify an objective that maximizes total benefit delivered by selected actions.
#' Benefit values are taken from the benefit table produced by \code{\link{add_benefits}}
#' (stored in \code{x$data$dist_benefit} and/or its model-ready variant).
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built
#' (typically when calling \code{solve()}).
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @details
#' The function updates \code{x$data$model_args} with:
#' \describe{
#'   \item{\code{model_type}}{\code{"maximizeBenefits"}}
#'   \item{\code{objective_id}}{\code{"max_benefit"}}
#'   \item{\code{objective_args}}{a list with \code{benefit_col}}
#' }
#'
#' The model builder will require benefit data to exist and will error if benefits are missing.
#' If another objective setter is called afterwards, it overwrites the active single-objective
#' specification in \code{x$data$model_args}.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param benefit_col Character. Column name in the model-ready benefit table containing numeric benefits.
#'   Default \code{"benefit"}.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
#' @examples
#' \dontrun{
#' x <- inputDataSpatial(pu = pu_sf, cost = "cost", features = feat_sf, pu_id_col = "id") |>
#'   add_actions(actions_df) |>
#'   add_benefits(benefits_df) |>
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

#' @title Add objective: maximize profit
#'
#' @description
#' Specify an objective that maximizes economic profit from selected \code{(pu, action)} pairs.
#' Profit values are taken from \code{x$data$dist_profit}, typically created with
#' \code{\link{add_profit}}.
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built
#' (typically when calling \code{solve()}).
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @details
#' The function updates \code{x$data$model_args} with:
#' \describe{
#'   \item{\code{model_type}}{\code{"maximizeProfit"}}
#'   \item{\code{objective_id}}{\code{"max_profit"}}
#'   \item{\code{objective_args}}{a list with \code{profit_col}}
#' }
#'
#' If another objective setter is called afterwards, it overwrites the active single-objective
#' specification in \code{x$data$model_args}.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param profit_col Character. Column name in \code{x$data$dist_profit} containing numeric profits.
#'   Default \code{"profit"}.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
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

#' @title Add objective: maximize net profit
#'
#' @description
#' Specify an objective that maximizes net profit, defined as total profit from selected
#' \code{(pu, action)} pairs minus total costs:
#' \deqn{\sum \mathrm{profit}\,x \;-\; \left(\sum \mathrm{pu\_cost}\,w + \sum \mathrm{action\_cost}\,x\right).}
#'
#' Profit is taken from \code{x$data$dist_profit} (created with \code{\link{add_profit}}).
#' Planning-unit costs are taken from \code{x$data$pu}; action costs are taken from
#' \code{x$data$dist_actions}.
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built.
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @details
#' The function updates \code{x$data$model_args} with:
#' \describe{
#'   \item{\code{model_type}}{\code{"maximizeNetProfit"}}
#'   \item{\code{objective_id}}{\code{"max_net_profit"}}
#'   \item{\code{objective_args}}{a list with \code{profit_col}, \code{include_pu_cost}, and \code{include_action_cost}}
#' }
#'
#' If another objective setter is called afterwards, it overwrites the active single-objective
#' specification in \code{x$data$model_args}.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param profit_col Character. Column name in \code{x$data$dist_profit} containing numeric profits.
#'   Default \code{"profit"}.
#' @param include_pu_cost Logical. If \code{TRUE}, subtract planning-unit costs.
#' @param include_action_cost Logical. If \code{TRUE}, subtract action costs.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
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

#' @title Add objective: minimize fragmentation (PU cut)
#'
#' @description
#' Specify an objective that minimizes spatial fragmentation measured as the weighted cut
#' between selected and non-selected planning units over a spatial relation:
#' \deqn{\sum_{(i,j)} w_{ij}\,|z_i - z_j|.}
#'
#' In the model builder, this objective is linearized using auxiliary edge variables.
#' Relation weights \eqn{w_{ij}} are read from \code{x$data$spatial_relations[[relation_name]]}
#' and can be scaled by \code{weight_multiplier} (BLM-like scaling).
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built.
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param relation_name Character. Name of the spatial relation in \code{x$data$spatial_relations}
#'   (e.g., \code{"boundary"}, \code{"rook"}, \code{"queen"}, \code{"knn"}). Default \code{"boundary"}.
#' @param weight_multiplier Numeric \eqn{\ge 0}. Multiplier applied to all relation weights. Default \code{1}.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
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

#' @title Add objective: minimize action fragmentation
#'
#' @description
#' Specify an objective that minimizes fragmentation at the action level over a spatial relation.
#' This objective is intended for models where action allocations (rather than only PU selection)
#' drive spatial cohesion.
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built.
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @details
#' Action-level fragmentation can optionally weight actions differently via \code{action_weights}
#' and can optionally restrict the objective to a subset of actions via \code{actions}.
#' The exact linearization and interpretation are implemented in the model builder.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param relation_name Character. Name of the spatial relation in \code{x$data$spatial_relations}.
#' @param weight_multiplier Numeric \eqn{\ge 0}. Multiplier applied to all relation weights. Default \code{1}.
#' @param action_weights Optional action weights. Either a named numeric vector (names = action ids)
#'   or a \code{data.frame(action, weight)}.
#' @param actions Optional subset of action ids to include in the objective.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
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

#' @title Add objective: minimize intervention fragmentation
#'
#' @description
#' Specify an objective that minimizes fragmentation at the intervention level over a spatial relation.
#' This objective is intended for models where interventions represent a coarser grouping of actions,
#' and spatial cohesion is evaluated at that grouping level.
#'
#' This function is \strong{data-only}: it stores the objective specification inside the
#' \code{Data} object so it can be materialized later when the optimization model is built.
#'
#' If \code{alias} is provided, the objective is also registered in \code{x$data$objectives}
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#' @param relation_name Character. Name of the spatial relation in \code{x$data$spatial_relations}.
#' @param weight_multiplier Numeric \eqn{\ge 0}. Multiplier applied to all relation weights. Default \code{1}.
#' @param alias Character scalar or \code{NULL}. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return The updated \code{Data} object.
#'
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

#' @title Add objective: maximize representation
#'
#' @description
#' Specify an objective that maximizes total representation across features,
#' using the z variables associated with (pu, feature) rows in `dist_features`.
#'
#' This is a data-only setter: it stores the objective specification inside the `Data`
#' object so it can be materialized later when the optimization model is built.
#'
#' If `alias` is provided, the objective is also registered in `x$data$objectives`
#' as an atomic objective for multi-objective workflows.
#'
#' @param x A `Data` object created with [inputData()] or [inputDataSpatial()].
#' @param amount_col Character. Column name in `dist_features` containing non-negative amounts.
#' @param features Optional subset of feature ids to include in the objective.
#'   Can be:
#'   - integer/numeric feature ids (matching `x$data$features$id`), or
#'   - character feature ids (matching `x$data$features$id` if those are character), or
#'   - internal feature indices (1..n_features) if `internal = TRUE`.
#' @param internal Logical. If `TRUE`, interpret `features` as internal feature indices.
#' @param alias Character scalar or `NULL`. Optional identifier to register this objective
#'   as an atomic objective for multi-objective workflows.
#'
#' @return Updated `Data` object.
#' @export
add_objective_max_representation <- function(
    x,
    amount_col = "amount",
    features = NULL,
    internal = FALSE,
    alias = NULL
) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$model_args)) x$data$model_args <- list()

  amount_col <- as.character(amount_col)[1]
  if (is.na(amount_col) || !nzchar(amount_col)) {
    stop("`amount_col` must be a non-empty string.", call. = FALSE)
  }

  # ---- normalize/validate feature subset
  feat_ids <- NULL

  if (!is.null(features)) {
    if (!is.logical(internal) || length(internal) != 1L || is.na(internal)) {
      stop("`internal` must be TRUE or FALSE.", call. = FALSE)
    }
    internal <- isTRUE(internal)

    if (internal) {
      idx <- as.integer(features)
      if (anyNA(idx) || any(idx < 1L)) stop("`features` (internal=TRUE) must be valid indices >= 1.", call. = FALSE)

      nF <- nrow(x$data$features %||% data.frame())
      if (nF <= 0) stop("No features found in x$data$features.", call. = FALSE)
      if (any(idx > nF)) stop("Some internal feature indices are out of range (1..n_features).", call. = FALSE)

      feat_ids <- x$data$features$id[idx]
    } else {
      # treat as feature ids
      feat_ids <- features
    }

    feat_ids <- unique(feat_ids)
    if (length(feat_ids) == 0L) stop("`features` subset is empty after processing.", call. = FALSE)

    # must exist in x$data$features$id
    if (is.null(x$data$features) || !inherits(x$data$features, "data.frame") || nrow(x$data$features) == 0) {
      stop("x$data$features is missing/empty; cannot validate `features` subset.", call. = FALSE)
    }
    ok <- feat_ids %in% x$data$features$id
    if (any(!ok)) {
      stop(
        "Some feature ids in `features` were not found in x$data$features$id: ",
        paste(feat_ids[!ok], collapse = ", "),
        call. = FALSE
      )
    }
  }

  args <- list(
    amount_col = amount_col,
    features = feat_ids
  )

  # single-objective (legacy) behavior
  x$data$model_args$model_type <- "maximizeRepresentation"
  x$data$model_args$objective_id <- "max_representation"
  x$data$model_args$objective_args <- args

  # atomic registration (MO)
  x <- .pa_register_objective(
    x = x,
    alias = alias,
    objective_id = "max_representation",
    model_type = "maximizeRepresentation",
    objective_args = args,
    sense = "max"
  )

  x
}



