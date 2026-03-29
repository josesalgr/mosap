#' @include internal.R
#'
#' @title Register an atomic objective (internal)
#' @name register_atomic_objective_internal
#'
#' @description
#' Internal helper used by objective setter functions to define the active
#' single-objective configuration of a \code{Problem} object and, optionally,
#' register that objective as an atomic objective for multi-objective workflows.
#'
#' @details
#' In \code{paretoscape}, an \emph{atomic objective} is a fully specified objective
#' definition that can later be reused by a multi-objective method such as a
#' weighted-sum formulation, an \eqn{\epsilon}-constraint method, AUGMECON, or
#' other objective-orchestration procedures.
#'
#' Each atomic objective is identified by:
#' \itemize{
#'   \item a stable internal identifier \code{objective_id},
#'   \item a solver-facing model label \code{model_type},
#'   \item a list of objective-specific arguments stored in
#'   \code{objective_args},
#'   \item an optimization sense, either \code{"min"} or \code{"max"},
#'   \item and, optionally, a user-facing identifier \code{alias}.
#' }
#'
#' If \code{alias} is not \code{NULL}, the objective is stored in
#' \code{x$data$objectives[[alias]]}. This makes it possible to refer to the
#' same objective later by a stable user-facing name. For example, a user may
#' register objectives under aliases such as \code{"cost"},
#' \code{"benefit"}, or \code{"frag"} and then pass those aliases to a
#' multi-objective method.
#'
#' If \code{alias} is \code{NULL}, no atomic-objective entry is created in
#' \code{x$data$objectives}. In that case, the calling function still defines
#' the currently active single-objective configuration through
#' \code{x$data$model_args}, but no reusable multi-objective registration is
#' created.
#'
#' Thus, this helper supports two complementary modes:
#' \itemize{
#'   \item \strong{single-objective mode}: only the active objective is stored,
#'   \item \strong{multi-objective-ready mode}: the active objective is stored
#'   and also registered under an alias for later reuse.
#' }
#'
#' Conceptually, if an objective function is denoted by \eqn{f(x)}, this helper
#' does not itself define the mathematical form of \eqn{f}; rather, it stores
#' the metadata required so that downstream code can reconstruct the correct
#' objective expression, its direction of optimization, and its user-visible
#' identity.
#'
#' @param x A \code{Problem} object.
#' @param alias Optional character scalar used to register the objective as an
#'   atomic objective. If \code{NULL}, no registration entry is created.
#' @param objective_id Character string giving the stable internal identifier of
#'   the objective, for example \code{"min_cost"} or \code{"max_benefit"}.
#' @param model_type Character string giving the model-builder label associated
#'   with this objective, for example \code{"minimizeCosts"}.
#' @param objective_args A list of objective-specific arguments to be stored with
#'   the objective definition.
#' @param sense Character string giving the optimization direction. Must be
#'   either \code{"min"} or \code{"max"}.
#'
#' @return An updated \code{Problem} object.
#'
#' @keywords internal
NULL

#' @title Add objective: minimize cost
#'
#' @description
#' Define an objective that minimizes the total cost of the solution.
#'
#' Depending on the function arguments, the objective may include planning-unit
#' costs, action costs, or both. Action costs can optionally be restricted to a
#' subset of actions.
#'
#' @details
#' Let \eqn{\mathcal{P}} be the set of planning units and let
#' \eqn{\mathcal{A}_i} be the set of feasible actions in planning unit
#' \eqn{i \in \mathcal{P}}.
#'
#' Let:
#' \itemize{
#'   \item \eqn{w_i \in \{0,1\}} denote whether planning unit \eqn{i} is
#'   selected,
#'   \item \eqn{x_{ia} \in \{0,1\}} denote whether action \eqn{a} is selected
#'   in planning unit \eqn{i},
#'   \item \eqn{c_i^{PU} \ge 0} denote the planning-unit cost of unit \eqn{i},
#'   \item \eqn{c_{ia}^{A} \ge 0} denote the cost of selecting action
#'   \eqn{a} in planning unit \eqn{i}.
#' }
#'
#' The most general form of this objective is:
#'
#' \deqn{
#' \min \left(
#' \sum_{i \in \mathcal{P}} c_i^{PU} w_i
#' +
#' \sum_{i \in \mathcal{P}} \sum_{a \in \mathcal{A}_i^\star} c_{ia}^{A} x_{ia}
#' \right),
#' }
#'
#' where \eqn{\mathcal{A}_i^\star} denotes the subset of feasible actions that
#' contribute to the action-cost term.
#'
#' If \code{include_pu_cost = FALSE}, the planning-unit cost term is omitted.
#'
#' If \code{include_action_cost = FALSE}, the action-cost term is omitted.
#'
#' If \code{actions = NULL}, all feasible actions contribute to the action-cost
#' term. If \code{actions} is supplied, only the selected subset contributes to
#' that term. Planning-unit costs are never subset by \code{actions}; they are
#' always global whenever \code{include_pu_cost = TRUE}.
#'
#' This objective is useful when the decision problem is framed primarily as a
#' cost-minimization problem, optionally combining fixed planning-unit costs
#' with action-specific implementation costs.
#'
#' @param x A \code{Problem} object.
#' @param include_pu_cost Logical. If \code{TRUE}, include planning-unit costs
#'   in the objective.
#' @param include_action_cost Logical. If \code{TRUE}, include action costs in
#'   the objective.
#' @param actions Optional subset of actions to include in the action-cost
#'   component. Values may match \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}. If \code{NULL}, all feasible actions are
#'   included in the action-cost term.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' # Minimize both planning-unit and action costs
#' p <- add_objective_min_cost(p)
#'
#' # Minimize only action costs
#' p <- add_objective_min_cost(
#'   p,
#'   include_pu_cost = FALSE,
#'   include_action_cost = TRUE
#' )
#'
#' # Minimize costs considering only a subset of actions
#' p <- add_objective_min_cost(
#'   p,
#'   actions = c("restoration", "conservation")
#' )
#' }
#'
#' @seealso
#' \code{\link{add_objective_max_profit}},
#' \code{\link{add_objective_max_net_profit}}
#'
#' @export
add_objective_min_cost <- function(
    x,
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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
#' Define an objective that maximizes the total positive effects generated by
#' selected actions on selected features.
#'
#' This objective is based on the canonical effects table stored in
#' \code{x$data$dist_effects} and uses only the non-negative
#' \code{benefit} component.
#'
#' @details
#' Let \eqn{\mathcal{E}} denote the set of rows in
#' \code{x$data$dist_effects}. For each row associated with planning unit
#' \eqn{i}, action \eqn{a}, and feature \eqn{f}, let
#' \eqn{b_{iaf} \ge 0} denote the stored value in the \code{benefit} column.
#'
#' Since \code{dist_effects} is already expressed in canonical form,
#' \eqn{b_{iaf}} represents the positive part of the net effect associated with
#' the corresponding selected action decision.
#'
#' If no subsets are supplied, the objective can be written as:
#'
#' \deqn{
#' \max \sum_{(i,a,f) \in \mathcal{E}} b_{iaf} \, x_{ia},
#' }
#'
#' where \eqn{x_{ia} \in \{0,1\}} indicates whether action \eqn{a} is selected
#' in planning unit \eqn{i}.
#'
#' If \code{actions} is provided, only rows whose action belongs to the selected
#' subset contribute to the objective.
#'
#' If \code{features} is provided, only rows whose feature belongs to the
#' selected subset contribute to the objective.
#'
#' More generally, letting \eqn{\mathcal{E}^{\star}} be the subset induced by
#' the selected actions and features, the objective is:
#'
#' \deqn{
#' \max \sum_{(i,a,f) \in \mathcal{E}^{\star}} b_{iaf} \, x_{ia}.
#' }
#'
#' This objective maximizes gains only. It does not subtract losses. If the user
#' wishes to account for harmful effects as well, losses should be handled
#' separately through additional objectives or constraints.
#'
#' @param x A \code{Problem} object.
#' @param actions Optional subset of actions to include in the objective. Values
#'   may match \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}. If \code{NULL}, all actions are included.
#' @param features Optional subset of features to include in the objective.
#'   Values may match \code{x$data$features$id} and, if present,
#'   \code{x$data$features$name}. If \code{NULL}, all features are included.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_objective_max_benefit(p)
#'
#' p <- add_objective_max_benefit(
#'   p,
#'   actions = c("restoration", "conservation")
#' )
#'
#' p <- add_objective_max_benefit(
#'   p,
#'   features = c("sp1", "sp3")
#' )
#'
#' p <- add_objective_max_benefit(
#'   p,
#'   actions = "restoration",
#'   features = c("sp1", "sp2")
#' )
#' }
#'
#' @seealso
#' \code{\link{add_objective_min_loss}},
#' \code{\link{add_effects}}
#'
#' @export
add_objective_max_benefit <- function(
    x,
    actions = NULL,
    features = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

  action_subset <- NULL
  feature_subset <- NULL

  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  if (!is.null(features)) {
    feature_subset <- .pa_resolve_feature_subset(x, features)
  }

  args <- list(
    actions = if (is.null(action_subset)) NULL else as.integer(action_subset$internal_id),
    features = if (is.null(feature_subset)) NULL else as.integer(feature_subset$internal_id)
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

#' @title Add objective: minimize loss
#'
#' @description
#' Define an objective that minimizes the total negative effects generated by
#' selected actions on selected features.
#'
#' This objective is based on the canonical effects table stored in
#' \code{x$data$dist_effects} and uses only the non-negative \code{loss}
#' component.
#'
#' @details
#' Let \eqn{\mathcal{E}} denote the set of rows in
#' \code{x$data$dist_effects}. For each row associated with planning unit
#' \eqn{i}, action \eqn{a}, and feature \eqn{f}, let
#' \eqn{\ell_{iaf} \ge 0} denote the stored value in the \code{loss} column.
#'
#' If no subsets are supplied, the objective can be written as:
#'
#' \deqn{
#' \min \sum_{(i,a,f) \in \mathcal{E}} \ell_{iaf} \, x_{ia}.
#' }
#'
#' If \code{actions} is provided, only rows whose action belongs to the selected
#' subset contribute to the objective.
#'
#' If \code{features} is provided, only rows whose feature belongs to the
#' selected subset contribute to the objective.
#'
#' More generally, letting \eqn{\mathcal{E}^{\star}} be the subset induced by
#' the selected actions and features, the objective is:
#'
#' \deqn{
#' \min \sum_{(i,a,f) \in \mathcal{E}^{\star}} \ell_{iaf} \, x_{ia}.
#' }
#'
#' This objective minimizes harmful effects only. It does not offset losses
#' against benefits unless benefits are handled elsewhere through additional
#' objectives or constraints.
#'
#' @param x A \code{Problem} object.
#' @param actions Optional subset of actions to include in the objective. Values
#'   may match \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}. If \code{NULL}, all actions are included.
#' @param features Optional subset of features to include in the objective.
#'   Values may match \code{x$data$features$id} and, if present,
#'   \code{x$data$features$name}. If \code{NULL}, all features are included.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_objective_min_loss(p)
#'
#' p <- add_objective_min_loss(
#'   p,
#'   actions = c("restoration", "harvest")
#' )
#'
#' p <- add_objective_min_loss(
#'   p,
#'   features = c("sp1", "sp3")
#' )
#'
#' p <- add_objective_min_loss(
#'   p,
#'   actions = "harvest",
#'   features = c("sp1", "sp2")
#' )
#' }
#'
#' @seealso
#' \code{\link{add_objective_max_benefit}},
#' \code{\link{add_effects}}
#'
#' @export
add_objective_min_loss <- function(
    x,
    actions = NULL,
    features = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

  action_subset <- NULL
  feature_subset <- NULL

  if (!is.null(actions)) {
    action_subset <- .pa_resolve_action_subset(x, actions)
  }

  if (!is.null(features)) {
    feature_subset <- .pa_resolve_feature_subset(x, features)
  }

  args <- list(
    actions = if (is.null(action_subset)) NULL else as.integer(action_subset$internal_id),
    features = if (is.null(feature_subset)) NULL else as.integer(feature_subset$internal_id)
  )

  .pa_set_active_and_register_objective(
    x = x,
    model_type = "minimizeLosses",
    objective_id = "min_loss",
    objective_args = args,
    sense = "min",
    alias = alias
  )
}

#' @title Add objective: maximize profit
#'
#' @description
#' Define an objective that maximizes total profit from selected planning
#' unit--action decisions.
#'
#' @details
#' Let \eqn{x_{ia} \in \{0,1\}} denote whether action \eqn{a} is selected in
#' planning unit \eqn{i}, and let \eqn{\pi_{ia}} denote the profit associated
#' with that decision, as taken from column \code{profit_col} in
#' \code{x$data$dist_profit}.
#'
#' If all actions are included, the objective is:
#'
#' \deqn{
#' \max \sum_{(i,a) \in \mathcal{F}} \pi_{ia} x_{ia},
#' }
#'
#' where \eqn{\mathcal{F}} denotes the set of feasible planning unit--action
#' pairs.
#'
#' If \code{actions} is provided, only the selected subset contributes to the
#' objective. Letting \eqn{\mathcal{F}^{\star}} denote the feasible pairs whose
#' action belongs to the selected subset, the objective becomes:
#'
#' \deqn{
#' \max \sum_{(i,a) \in \mathcal{F}^{\star}} \pi_{ia} x_{ia}.
#' }
#'
#' This objective considers profit only. It does not subtract planning-unit
#' costs or action costs. For a net-profit formulation, use
#' \code{\link{add_objective_max_net_profit}}.
#'
#' @param x A \code{Problem} object.
#' @param profit_col Character string giving the profit column in
#'   \code{x$data$dist_profit}.
#' @param actions Optional subset of actions to include. Values may match
#'   \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}. If \code{NULL}, all actions are included.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @seealso
#' \code{\link{add_objective_min_cost}},
#' \code{\link{add_objective_max_net_profit}}
#'
#' @export
add_objective_max_profit <- function(
    x,
    profit_col = "profit",
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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
#' Define an objective that maximizes net profit by combining profits with
#' optional planning-unit and action-cost penalties.
#'
#' @details
#' Let:
#' \itemize{
#'   \item \eqn{x_{ia} \in \{0,1\}} denote whether action \eqn{a} is selected
#'   in planning unit \eqn{i},
#'   \item \eqn{w_i \in \{0,1\}} denote whether planning unit \eqn{i} is
#'   selected,
#'   \item \eqn{\pi_{ia}} denote the profit associated with decision
#'   \eqn{(i,a)},
#'   \item \eqn{c_i^{PU} \ge 0} denote the planning-unit cost,
#'   \item \eqn{c_{ia}^{A} \ge 0} denote the action cost.
#' }
#'
#' In its most general form, the objective is:
#'
#' \deqn{
#' \max \left(
#' \sum_{(i,a) \in \mathcal{F}^{\star}} \pi_{ia} x_{ia}
#' -
#' \sum_{i \in \mathcal{P}} c_i^{PU} w_i
#' -
#' \sum_{(i,a) \in \mathcal{F}^{\star}} c_{ia}^{A} x_{ia}
#' \right),
#' }
#'
#' where \eqn{\mathcal{F}^{\star}} denotes the subset of feasible
#' planning unit--action pairs included in the objective.
#'
#' If \code{actions = NULL}, all feasible actions contribute to both the profit
#' term and the action-cost term.
#'
#' If \code{actions} is provided, the profit term and the action-cost term are
#' restricted to that subset. The planning-unit cost term, if included, remains
#' global.
#'
#' If \code{include_pu_cost = FALSE}, the planning-unit cost term is omitted.
#'
#' If \code{include_action_cost = FALSE}, the action-cost term is omitted.
#'
#' This objective is appropriate when decisions generate revenues or returns and
#' the analyst wishes to optimize the resulting net balance after accounting for
#' selected cost components.
#'
#' @param x A \code{Problem} object.
#' @param profit_col Character string giving the profit column in
#'   \code{x$data$dist_profit}.
#' @param include_pu_cost Logical. If \code{TRUE}, subtract planning-unit costs.
#' @param include_action_cost Logical. If \code{TRUE}, subtract action costs.
#' @param actions Optional subset of actions to include in the profit and
#'   action-cost terms. Values may match \code{x$data$actions$id} and, if
#'   present, \code{x$data$actions$action_set}.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @seealso
#' \code{\link{add_objective_max_profit}},
#' \code{\link{add_objective_min_cost}}
#'
#' @export
add_objective_max_net_profit <- function(
    x,
    profit_col = "profit",
    include_pu_cost = TRUE,
    include_action_cost = TRUE,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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
#' Define an objective that minimizes planning-unit fragmentation over a stored
#' spatial relation.
#'
#' This objective acts on the planning-unit selection pattern through the binary
#' planning-unit variables \eqn{w_i}. It is therefore appropriate when spatial
#' cohesion is to be encouraged at the level of the selected planning-unit set
#' as a whole.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{w_i \in \{0,1\}} indicate whether planning unit \eqn{i \in \mathcal{P}}
#' is selected.
#'
#' Let the chosen spatial relation
#' \code{x$data$spatial_relations[[relation_name]]} define a set of weighted
#' pairs with weights \eqn{\omega_{ij} \ge 0}. These relation weights are
#' interpreted by the model builder after scaling by
#' \eqn{\lambda = \code{weight_multiplier}}.
#'
#' The internal preparation step constructs one auxiliary variable
#' \eqn{y_{ij} \in [0,1]} for each unique non-diagonal undirected edge
#' \eqn{(i,j)} with \eqn{i < j}. Diagonal entries of the relation, if present,
#' are not used to create auxiliary variables.
#'
#' Each auxiliary variable is constrained to represent the logical conjunction of
#' the two incident planning-unit selections:
#' \deqn{
#' y_{ij} = w_i \land w_j.
#' }
#'
#' This is enforced by the standard linearization:
#' \deqn{
#' y_{ij} \le w_i,
#' }
#' \deqn{
#' y_{ij} \le w_j,
#' }
#' \deqn{
#' y_{ij} \ge w_i + w_j - 1.
#' }
#'
#' Thus, \eqn{y_{ij}=1} if and only if both planning units \eqn{i} and
#' \eqn{j} are selected, and \eqn{y_{ij}=0} otherwise.
#'
#' The exact objective coefficients are assembled later by the model builder
#' from:
#' \itemize{
#'   \item the planning-unit variables \eqn{w_i},
#'   \item the edge-conjunction variables \eqn{y_{ij}},
#'   \item the stored relation weights \eqn{\omega_{ij}},
#'   \item and the multiplier \eqn{\lambda}.
#' }
#'
#' Conceptually, the resulting objective is a boundary- or relation-based
#' compactness functional that penalizes exposed or weakly connected selected
#' patterns while rewarding adjacency among selected planning units.
#'
#' In the common case where \code{relation_name = "boundary"} and the relation
#' was built with \code{\link{add_spatial_boundary}}, the objective corresponds
#' to a boundary-length-style fragmentation penalty. In that setting:
#' \itemize{
#'   \item off-diagonal relation rows encode shared boundary,
#'   \item diagonal relation rows may encode exposed boundary,
#'   \item the objective is evaluated from the combination of selected planning
#'   units and selected adjacent pairs.
#' }
#'
#' A key implementation detail is that symmetric duplicates in the relation are
#' canonicalized internally before the auxiliary block is created. For each
#' unordered non-diagonal pair \eqn{\{i,j\}}, only one edge is retained, using
#' the maximum weight among duplicates.
#'
#' Setting \code{weight_multiplier = 0} removes the contribution of the spatial
#' relation from the objective after scaling.
#'
#' This objective does not distinguish between different actions within the same
#' planning unit. If action-specific spatial cohesion is required, use
#' \code{\link{add_objective_min_action_fragmentation}} instead.
#'
#' @param x A \code{Problem} object.
#' @param relation_name Character string giving the name of the spatial relation
#'   to use. The relation must already exist in
#'   \code{x$data$spatial_relations}.
#' @param weight_multiplier Numeric scalar greater than or equal to zero. Global
#'   multiplier applied to the relation weights when the objective is built.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @seealso
#' \code{\link{add_spatial_boundary}},
#' \code{\link{add_spatial_relations}},
#' \code{\link{add_objective_min_action_fragmentation}}
#'
#' @export
add_objective_min_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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
#' Define an objective that minimizes fragmentation at the action level over a
#' stored spatial relation.
#'
#' Unlike \code{\link{add_objective_min_fragmentation}}, which acts on the
#' selected planning-unit set through \eqn{w_i}, this objective acts on the
#' spatial arrangement of individual action decisions through the action
#' variables \eqn{x_{ia}}.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{\mathcal{A}} denote the set of actions.
#'
#' Let \eqn{x_{ia} \in \{0,1\}} indicate whether action \eqn{a \in \mathcal{A}}
#' is selected in planning unit \eqn{i \in \mathcal{P}}.
#'
#' Let the chosen spatial relation
#' \code{x$data$spatial_relations[[relation_name]]} define weighted pairs with
#' weights \eqn{\omega_{ij} \ge 0}, and let
#' \eqn{\lambda = \code{weight_multiplier}} be the global scaling factor applied
#' to these weights.
#'
#' If \code{actions} is supplied, only the selected subset
#' \eqn{\mathcal{A}^{\star} \subseteq \mathcal{A}} contributes to the final
#' objective. If \code{actions = NULL}, all actions are included.
#'
#' The internal preparation step constructs one auxiliary variable
#' \eqn{b_{ija} \in [0,1]} for each unique non-diagonal undirected edge
#' \eqn{(i,j)} with \eqn{i < j} and for each action \eqn{a}. These variables are
#' created globally for all actions.
#'
#' For each edge--action combination, the intended semantics is:
#' \deqn{
#' b_{ija} = x_{ia} \land x_{ja}.
#' }
#'
#' Whenever both decision variables \eqn{x_{ia}} and \eqn{x_{ja}} exist in the
#' model, this conjunction is enforced by the linearization:
#' \deqn{
#' b_{ija} \le x_{ia},
#' }
#' \deqn{
#' b_{ija} \le x_{ja},
#' }
#' \deqn{
#' b_{ija} \ge x_{ia} + x_{ja} - 1.
#' }
#'
#' If one of the two action variables does not exist because the corresponding
#' \code{(pu, action)} pair is not feasible, the auxiliary variable is forced to
#' zero:
#' \deqn{
#' b_{ija} = 0.
#' }
#'
#' Therefore, \eqn{b_{ija}=1} if and only if action \eqn{a} is selected in both
#' adjacent planning units \eqn{i} and \eqn{j}; otherwise \eqn{b_{ija}=0}.
#'
#' The exact objective coefficients are assembled later by the model builder
#' from:
#' \itemize{
#'   \item the action decision variables \eqn{x_{ia}},
#'   \item the edge-conjunction variables \eqn{b_{ija}},
#'   \item the relation weights \eqn{\omega_{ij}},
#'   \item the multiplier \eqn{\lambda},
#'   \item and, if supplied, the action-specific weights.
#' }
#'
#' If action-specific weights are provided, let \eqn{\alpha_a \ge 0} denote the
#' weight associated with action \eqn{a}. Then the resulting objective can be
#' interpreted as an action-wise compactness or fragmentation functional of the
#' form:
#' \deqn{
#' \min \sum_{a \in \mathcal{A}^{\star}} \alpha_a \,
#' F_a(x_{\cdot a}, b_{\cdot\cdot a}; \lambda \omega),
#' }
#' where \eqn{F_a} is the fragmentation expression induced by the selected
#' relation and the internal coefficient construction for action \eqn{a}.
#'
#' In practical terms, this objective penalizes solutions in which the same
#' action is spatially scattered or broken into separate patches, while allowing
#' different actions to form different spatial patterns.
#'
#' This differs from planning-unit fragmentation:
#' \itemize{
#'   \item \code{add_objective_min_fragmentation()} encourages cohesion of the
#'   union of selected planning units,
#'   \item \code{add_objective_min_action_fragmentation()} encourages cohesion of
#'   each selected action pattern separately.
#' }
#'
#' A key implementation detail is that the auxiliary block is built over the
#' global set of unique undirected non-diagonal edges and all actions, even if a
#' later subset of actions is used in the objective. Missing edge--action
#' combinations induced by infeasible decisions are fixed to zero.
#'
#' Setting \code{weight_multiplier = 0} removes the contribution of the spatial
#' relation from the objective after scaling.
#'
#' @param x A \code{Problem} object.
#' @param relation_name Character string giving the name of the spatial relation
#'   to use. The relation must already exist in
#'   \code{x$data$spatial_relations}.
#' @param weight_multiplier Numeric scalar greater than or equal to zero. Global
#'   multiplier applied to the relation weights when the objective is built.
#' @param action_weights Optional action weights. Either a named numeric vector
#'   with names equal to action ids, or a \code{data.frame} with columns
#'   \code{action} and \code{weight}. These weights scale the contribution of
#'   each action to the final objective.
#' @param actions Optional subset of actions to include. Values may match
#'   \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}. If \code{NULL}, all actions are included.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @seealso
#' \code{\link{add_objective_min_fragmentation}},
#' \code{\link{add_spatial_boundary}},
#' \code{\link{add_spatial_relations}}
#'
#' @export
add_objective_min_action_fragmentation <- function(
    x,
    relation_name = "boundary",
    weight_multiplier = 1,
    action_weights = NULL,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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

#' @title Add objective: minimize intervention impact
#'
#' @description
#' Define an objective that minimizes the impact associated with selecting
#' planning units for intervention.
#'
#' This objective uses planning-unit selection variables rather than summing the
#' same impact repeatedly over multiple actions. As a result, each planning unit
#' contributes at most once to the objective, regardless of how many feasible
#' actions exist in that unit.
#'
#' @details
#' Let \eqn{w_i \in \{0,1\}} denote whether planning unit \eqn{i} is selected
#' for intervention. Let \eqn{q_{if}} denote the impact amount associated with
#' planning unit \eqn{i} and feature \eqn{f}, taken from column
#' \code{impact_col} in \code{x$data$dist_features}.
#'
#' If all features are included, the objective can be interpreted as:
#'
#' \deqn{
#' \min \sum_{i \in \mathcal{P}} \left( \sum_{f \in \mathcal{F}^{\star}} q_{if}
#' \right) w_i,
#' }
#'
#' where \eqn{\mathcal{F}^{\star}} denotes the selected subset of features.
#'
#' Thus, the coefficient attached to \eqn{w_i} is the aggregated impact of the
#' selected features in planning unit \eqn{i}.
#'
#' The role of \code{actions} in this objective is not to make impact additive
#' over actions, but to restrict the notion of intervention to planning units
#' that are relevant for the selected subset of actions in downstream model
#' construction. Even when multiple feasible actions exist in a planning unit,
#' the planning unit contributes at most once through \eqn{w_i}.
#'
#' This objective is useful when intervention itself has a baseline ecological,
#' social, or operational impact that should be minimized independently of the
#' detailed gain or loss generated by particular actions.
#'
#' @param x A \code{Problem} object.
#' @param impact_col Character string giving the column in
#'   \code{x$data$dist_features} that contains the per-\code{(pu, feature)}
#'   impact amount. The default is \code{"amount"}.
#' @param features Optional subset of features to include. Values may match
#'   \code{x$data$features$id} and, if present, \code{x$data$features$name}.
#' @param actions Optional subset of actions used to define the intervention
#'   context. Values may match \code{x$data$actions$id} and, if present,
#'   \code{x$data$actions$action_set}.
#' @param alias Optional identifier used to register this objective for
#'   multi-objective workflows.
#'
#' @return An updated \code{Problem} object.
#'
#' @seealso
#' \code{\link{add_objective_max_benefit}},
#' \code{\link{add_objective_min_loss}}
#'
#' @export
add_objective_min_intervention_impact <- function(
    x,
    impact_col = "amount",
    features = NULL,
    actions = NULL,
    alias = NULL
) {
  stopifnot(inherits(x, "Problem"))

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
