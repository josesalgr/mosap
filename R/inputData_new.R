#' @include internal.R
NULL

#' @title Creates the multi-action planning problem
#'
#' @description
#' Create the [data-class] object with information about the multi-action
#' conservation planning problem. This function is used to specify all the data
#' that defines the spatial prioritization problem (planning units data, feature
#' data, threats data, and their spatial distributions.)
#'
#' @param pu Object of class [data.frame()] that specifies the planning units (PU)
#' of the corresponding instance and their corresponding monitoring cost and status. Each
#' row corresponds to a different planning unit. This file is inherited from the
#' *pu.dat* in *Marxan*. It must contain the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each planning unit.}
#'    \item{`monitoring_cost`}{`numeric` cost of including each planning unit in the reserve system.}
#'    \item{`status`}{`integer` (**optional**) value that indicate if each planning unit
#'    should be available to be selected (0), *locked-in* (2) as part of the
#'    solution, or *locked-out* (3) and excluded from the solution.}
#'    }
#'
#' @param features Object of class [data.frame()] that specifies the conservation
#' features to consider in the optimization problem. Each row corresponds to a different
#' feature. This file is inherited from marxan's *spec.dat*.
#'
#' The `prioriactions` package supports two types of purposes when optimizing: focus on
#' recovery of features threatened (through the **recovery target**), where only
#' take into account benefits when taking action against threats and there is no benefit
#' when selecting planning units where the features are not threatened;
#' or include the benefits of the features sites where they are not threatened
#' (through the **conservation target**).
#'
#' Note that by default only information on recovery targets is necessary,
#' while conservation targets equal to zero are assumed. The maximum values of
#' benefits to achieve both recovery and conservation per feature can be verified
#' with the `getPotentialBenefit()` function.
#' For more information on the implications of these targets in the solutions see
#' the [recovery](https://prioriactions.github.io/prioriactions/articles/objectives.html)
#' vignette.
#'
#' This file must contain
#' the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each conservation feature.}
#'    \item{`target_recovery`}{`numeric` amount of recovery target to achieve for each conservation feature.
#'    This field is **required** if a `minimizeCosts` model is used.}
#'    \item{`target_conservation`}{`numeric` (**optional**) amount of conservation target to achieve
#'    for each conservation feature.
#'    This field is used only if a model of the type `minimizeCosts` is applied.}
#'    \item{`name`}{`character` (**optional**) name for each conservation feature.}
#'    }
#'
#' @param dist_features Object of class [data.frame()] that specifies the spatial
#' distribution of conservation features across planning units. Each row corresponds
#' to a combination of planning unit and feature. This file is inherited from marxan's
#' *puvspr.dat*. It must contain the following columns:
#' \describe{
#'    \item{`pu`}{`integer` *id* of a planning unit where the conservation feature
#'    listed on the same row occurs.}
#'    \item{`feature`}{`integer` *id* of each conservation feature.}
#'    \item{`amount`}{`numeric` amount of the feature in the planning unit. Set
#'    to 1 to work with presence/absence.}
#'    }
#'
#' @param threats Object of class [data.frame()] that specifies the threats to consider in
#' the optimization exercise. Each row corresponds to a different threats. It must contain
#' the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each threat.}
#'    \item{`blm_actions`}{`numeric` (**optional**) penalty of connectivity between threats.
#'    Default is 0.}
#'    \item{`name`}{`character` (**optional**) name for each threat.}
#'    }
#'
#' @param dist_threats Object of class [data.frame()] that specifies the spatial
#' distribution of threats across planning units. Each row corresponds
#' to a combination of planning unit and threat. It must contain the following
#' columns:
#' \describe{
#'    \item{`pu`}{`integer` *id* of a planning unit where the threat listed on the
#'    same row occurs.}
#'    \item{`threat`}{`integer` *id* of each threat.}
#'    \item{`amount`}{`numeric` amount of the threat in the planning unit. Set
#'    to 1 to work with presence/absence. Continuous amount values require
#'    that feature sensitivities to threats be established (more info in
#'    [sensitivities](https://prioriactions.github.io/prioriactions/articles/sensitivities.html)
#'    vignette).}
#'    \item{`action_cost`}{`numeric` cost of an action to abate the threat
#'    in each planning unit.}
#'    \item{`status`}{`integer` (**optional**) value that indicates if each action
#'    to abate the threat is available to be selected (0), *locked-in* (2)
#'    as part of the solution, or *locked-out* (3) and therefore excluded from the solution.}
#'    }
#'
#' @param sensitivity (**optional**) Object of class [data.frame()] that specifies
#' the sensitivity of each feature to each threat. Each row corresponds
#' to a combination of feature and threat. If not informed, all features
#' are assumed to be sensitive to all threats.
#'
#' Sensitivity can be parameterized in two ways: **binary**; the feature is
#' sensitive or not, or **continuous**; with response curves of the probability of
#' persistence of the features to threats. For the first case, it is only necessary
#' to indicate the ids of the threats and the respective features sensitive to them.
#' In the second case, the response can be parameterized through four values: *\eqn{\delta_1}*, *\eqn{\delta_2}*, *\eqn{\delta_3}*
#' and *\eqn{\delta_4}*. See
#' [sensitivities](https://prioriactions.github.io/prioriactions/articles/sensitivities.html)
#' vignette for more information on continuous sensitivities. Then, the sensitivity input must contain the following columns:
#'    \describe{
#'    \item{`feature`}{`integer` *id* of each conservation feature.}
#'    \item{`threat`}{`integer` *id* of each threat.}
#'    \item{`delta1`}{`numeric` (**optional**) the minimum intensity of the threat at
#'    which the features probability of persistence starts to decline. The more
#'    sensitive the feature is to the threat, the lowest this value will be. Default
#'    is 0.}
#'    \item{`delta2`}{`numeric` (**optional**) the value of intensity of the threat
#'    over which the feature has a probability of persistence of 0. If it is not
#'    established,it is assumed as the **maximum value of the threat across all planning units**
#'    in the study area.
#'    Note that this might overestimate the sensitivity of features to threats,
#'    as they will only be assumed to disappear from planning units if the
#'    threats reach the maximum intensity value in the study area.}
#'    \item{`delta3`}{`numeric` (**optional**) minimum probability of persistence of a
#'    features when a threat reaches its maximum intensity value. Default is 0.}
#'    \item{`delta4`}{`numeric` (**optional**) maximum probability of persistence of a
#'    features in absence threats. Default is 1.}
#'    }
#'  Note that optional parameters *delta1*, *delta2*, *delta3* and *delta4* can be provided independently.
#'
#' @param boundary (**optional**) Object of class [data.frame()] that specifies
#' the spatial relationship between pair of planning units. Each row corresponds
#' to a combination of planning unit. This file is inherited from marxan's
#' *bound.dat*. It must contain the following columns:
#'   \describe{
#'   \item{`id1`}{`integer` *id* of each planning unit.}
#'   \item{`id2`}{`integer` *id* of each planning unit.}
#'   \item{`boundary`}{`numeric` penalty applied in the objective function
#'   when only one of the planning units is present in the solution.}
#'   }
#'
#' @param ... Unused arguments, reserved for future expansion.
#'
#' @name inputData_new
#'
#' @return An object of class [data-class].
#'
#' @seealso For more information on the correct format for *Marxan* input data, see the
#' [official *Marxan* website](https://marxansolutions.org) and Ball *et al.* (2009).
#'
#' @examples
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Set prioriactions path
#' prioriactions_path <- system.file("extdata/example_input/", package = "prioriactions")
#'
#' ## Load in planning unit data
#' pu_data <- data.table::fread(paste0(prioriactions_path,"/pu.dat"),
#'                              data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' features_data <- data.table::fread(paste0(prioriactions_path,"/features.dat"),
#'                                    data.table = FALSE)
#' head(features_data)
#'
#' ## Load in planning unit vs feature data
#' dist_features_data <- data.table::fread(paste0(prioriactions_path,"/dist_features.dat"),
#'                                         data.table = FALSE)
#' head(dist_features_data)
#'
#' ## Load in the threats data
#' threats_data <- data.table::fread(paste0(prioriactions_path,"/threats.dat"),
#'                                   data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the threats distribution data
#' dist_threats_data <- data.table::fread(paste0(prioriactions_path,"/dist_threats.dat"),
#'                                        data.table = FALSE)
#' head(dist_threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_data <- data.table::fread(paste0(prioriactions_path,"/sensitivity.dat"),
#'                                       data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' boundary_data <- data.table::fread(paste0(prioriactions_path,"/boundary.dat"),
#'                                    data.table = FALSE)
#' head(boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData_new(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Summary
#' print(problem_data)
#'
#' @references
#' \itemize{
#' \item Ball I, Possingham H, Watts, M. *Marxan and relatives: software for spatial
#' conservation prioritization*. Spatial conservation prioritisation: quantitative
#' methods and computational tools 2009.
#' }
#'
#' @export
#' @rdname inputData_new
#'
#' @export
methods::setGeneric(
  "inputData_new",
  signature = methods::signature("pu", "features", "dist_features"),
  function(pu, features, dist_features, boundary = NULL, ...) {
    standardGeneric("inputData_new")
  }
)

methods::setMethod(
  "inputData_new",
  methods::signature(
    pu = "data.frame",
    features = "data.frame",
    dist_features = "data.frame"
  ),
  function(pu, features, dist_features, boundary = NULL, ...) {

    # helper: coerce ids to integer safely
    .as_int_id <- function(x, what) {
      if (is.factor(x)) x <- as.character(x)
      if (is.character(x)) {
        if (any(grepl("[^0-9\\-]", x))) {
          stop(what, " must be numeric/integer ids (got non-numeric strings).", call. = FALSE)
        }
        x <- as.integer(x)
      } else {
        x <- as.integer(x)
      }
      if (anyNA(x)) stop(what, " contains NA after coercion to integer.", call. = FALSE)
      x
    }

    # =========================
    # PU: validate + normalize
    # =========================
    assertthat::assert_that(
      inherits(pu, "data.frame"),
      assertthat::has_name(pu, "id"),
      nrow(pu) > 0,
      assertthat::noNA(pu$id)
    )

    pu$id <- .as_int_id(pu$id, "pu$id")
    if (anyDuplicated(pu$id) != 0) stop("pu$id must be unique.", call. = FALSE)

    # accept cost or monitoring_cost -> normalize to cost
    if ("cost" %in% names(pu)) {
      assertthat::assert_that(is.numeric(pu$cost), assertthat::noNA(pu$cost))
    } else if ("monitoring_cost" %in% names(pu)) {
      assertthat::assert_that(is.numeric(pu$monitoring_cost), assertthat::noNA(pu$monitoring_cost))
      pu$cost <- pu$monitoring_cost
    } else {
      stop("pu must contain either a 'cost' column or a 'monitoring_cost' column.", call. = FALSE)
    }

    # locks: accept locked_in/locked_out or status (Marxan style)
    has_locked_cols <- ("locked_in" %in% names(pu)) || ("locked_out" %in% names(pu))
    if (has_locked_cols) {
      if (!("locked_in" %in% names(pu))) pu$locked_in <- FALSE
      if (!("locked_out" %in% names(pu))) pu$locked_out <- FALSE
      pu$locked_in  <- as.logical(pu$locked_in)
      pu$locked_out <- as.logical(pu$locked_out)
    } else if ("status" %in% names(pu)) {
      pu$status <- as.integer(pu$status)
      pu$locked_in  <- pu$status == 2L
      pu$locked_out <- pu$status == 3L
    } else {
      pu$locked_in  <- FALSE
      pu$locked_out <- FALSE
    }

    if (any(pu$locked_in & pu$locked_out, na.rm = TRUE)) {
      stop("Some planning units are both locked_in and locked_out. Please fix pu input.", call. = FALSE)
    }

    pu <- pu[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]
    pu <- pu[order(pu$id), , drop = FALSE]

    # internal ids + lookup (names are character, values are integer)
    pu$internal_id <- seq_len(nrow(pu))
    pu_index <- stats::setNames(pu$internal_id, as.character(pu$id))

    # =========================
    # FEATURES: validate + normalize
    # =========================
    assertthat::assert_that(
      inherits(features, "data.frame"),
      assertthat::has_name(features, "id"),
      nrow(features) > 0,
      assertthat::noNA(features$id)
    )

    features$id <- .as_int_id(features$id, "features$id")
    if (anyDuplicated(features$id) != 0) stop("features$id must be unique.", call. = FALSE)

    if (!("name" %in% names(features))) {
      features$name <- paste0("feature.", seq_len(nrow(features)))
    } else {
      features$name <- as.character(features$name)
      assertthat::assert_that(assertthat::noNA(features$name))
      if (anyDuplicated(features$name) != 0) stop("features$name must be unique.", call. = FALSE)
    }

    features <- features[, c("id", "name", setdiff(names(features), c("id", "name"))), drop = FALSE]
    features <- features[order(features$id), , drop = FALSE]

    features$internal_id <- seq_len(nrow(features))
    feature_index <- stats::setNames(features$internal_id, as.character(features$id))

    # =========================
    # DIST_FEATURES: validate + normalize
    # =========================
    assertthat::assert_that(
      inherits(dist_features, "data.frame"),
      assertthat::has_name(dist_features, "pu"),
      assertthat::has_name(dist_features, "feature"),
      assertthat::has_name(dist_features, "amount"),
      nrow(dist_features) > 0,
      assertthat::noNA(dist_features$pu),
      assertthat::noNA(dist_features$feature),
      assertthat::noNA(dist_features$amount),
      is.numeric(dist_features$amount),
      all(dist_features$amount >= 0)
    )

    dist_features$pu      <- .as_int_id(dist_features$pu, "dist_features$pu")
    dist_features$feature <- .as_int_id(dist_features$feature, "dist_features$feature")

    # check membership against pu/features ids
    if (!all(dist_features$pu %in% pu$id)) {
      bad <- unique(dist_features$pu[!dist_features$pu %in% pu$id])
      stop("dist_features contains unknown PU ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    if (!all(dist_features$feature %in% features$id)) {
      bad <- unique(dist_features$feature[!dist_features$feature %in% features$id])
      stop("dist_features contains unknown feature ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }

    # drop zeros
    dist_features <- dist_features[dist_features$amount != 0, , drop = FALSE]

    # duplicates check on (pu, feature)
    key <- paste(dist_features$pu, dist_features$feature, sep = "||")
    if (anyDuplicated(key) != 0) {
      stop("There are duplicate (pu, feature) pairs in dist_features.", call. = FALSE)
    }

    # internal mapping (fast + stable)
    dist_features$internal_pu      <- unname(pu_index[as.character(dist_features$pu)])
    dist_features$internal_feature <- unname(feature_index[as.character(dist_features$feature)])

    dist_features <- dist_features[order(dist_features$internal_pu, dist_features$internal_feature), , drop = FALSE]
    dist_features$internal_row <- seq_len(nrow(dist_features))

    # =========================
    # BOUNDARY: validate + normalize
    # =========================
    assertthat::assert_that(inherits(boundary, c("NULL", "data.frame")))
    if (inherits(boundary, "data.frame")) {
      assertthat::assert_that(
        assertthat::has_name(boundary, "id1"),
        assertthat::has_name(boundary, "id2"),
        assertthat::has_name(boundary, "boundary"),
        assertthat::noNA(boundary$id1),
        assertthat::noNA(boundary$id2),
        assertthat::noNA(boundary$boundary),
        is.numeric(boundary$boundary)
      )

      boundary$id1 <- .as_int_id(boundary$id1, "boundary$id1")
      boundary$id2 <- .as_int_id(boundary$id2, "boundary$id2")
      boundary$boundary <- base::round(as.numeric(boundary$boundary), 3)

      # drop unknown pu ids (warn)
      if (!all(boundary$id1 %in% pu$id) || !all(boundary$id2 %in% pu$id)) {
        warning("boundary contains PU ids not present in pu; they will be removed.",
                call. = FALSE, immediate. = TRUE)
        keep <- boundary$id1 %in% pu$id & boundary$id2 %in% pu$id
        boundary <- boundary[keep, , drop = FALSE]
      }

      if (nrow(boundary) == 0) {
        boundary <- NULL
      } else {
        boundary$internal_id1 <- unname(pu_index[as.character(boundary$id1)])
        boundary$internal_id2 <- unname(pu_index[as.character(boundary$id2)])
      }
    }

    # =========================
    # rounding (conservative)
    # =========================
    pu$cost <- base::round(pu$cost, 3)
    dist_features$amount <- base::round(dist_features$amount, 3)

    # =========================
    # useful warnings
    # =========================
    dif_pu <- setdiff(unique(pu$id), unique(dist_features$pu))
    if (length(dif_pu) != 0L) {
      warning(
        paste0("The following pu's do not contain features: ", paste(dif_pu, collapse = " ")),
        call. = FALSE, immediate. = TRUE
      )
    }

    dif_features <- setdiff(unique(features$id), unique(dist_features$feature))
    if (length(dif_features) != 0L) {
      warning(
        paste0("The following features are not represented in dist_features: ", paste(dif_features, collapse = " ")),
        call. = FALSE, immediate. = TRUE
      )
    }

    # =========================
    # build Data object
    # =========================
    pproto(
      NULL, Data,
      data = list(
        pu = pu,
        features = features,
        dist_features = dist_features,
        boundary = boundary,

        index = list(
          pu = pu_index,
          feature = feature_index,
          feature_name_to_id = stats::setNames(features$id, features$name)
        ),

        # meta (útil para tu workflow nuevo)
        meta = list(
          dist_features_meaning = "baseline_amount",
          dist_benefit_meaning  = "delta_by_default"
        ),

        actions = NULL,
        dist_actions = NULL,
        dist_benefit = NULL,
        locked_actions = NULL,
        targets = NULL,
        objective = NULL,
        decisions = NULL,
        solver = NULL
      )
    )
  }
)
