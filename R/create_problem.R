#' @include internal.R
NULL

#' @name create_problem
#' @title Create a planning problem input object
#'
#' @description
#' Create a \code{Problem} object from tabular or spatial inputs.
#'
#' \code{create_problem()} is the main entry point to the \code{multiscape}
#' workflow. Its role is to standardize heterogeneous planning inputs into a
#' common internal representation that can later be used by actions, effects,
#' targets, constraints, spatial relations, objectives, and solvers.
#'
#' In all supported workflows, the result is a \code{Problem} object with a
#' canonical tabular core, optionally enriched with aligned spatial metadata
#' such as coordinates, geometry, raster references, or raw planning-unit
#' attributes.
#'
#' @details
#'
#' A \code{Problem} object created by \code{create_problem()} is the basic input
#' structure used throughout downstream \code{multiscape} workflows.
#'
#' The returned object always contains a canonical tabular planning core and may
#' additionally store aligned spatial metadata depending on the input workflow.
#' This is the internal representation on which later functions such as
#' \code{add_actions()}, \code{add_effects()},
#' \code{add_constraint_targets()}, \code{add_spatial_relations()}, and
#' \code{solve()} operate.
#'
#' \strong{Conceptual role of \code{create_problem()}}
#'
#' Regardless of the input workflow, \code{create_problem()} aims to produce a
#' consistent internal representation containing at least:
#' \itemize{
#'   \item a planning-unit table,
#'   \item a feature table,
#'   \item a feature-distribution table.
#' }
#'
#' Internally, the problem is always reduced to a tabular core of the form:
#' \itemize{
#'   \item planning units \eqn{i \in \mathcal{I}},
#'   \item features \eqn{f \in \mathcal{F}},
#'   \item non-negative baseline amounts \eqn{a_{if} \ge 0},
#'   \item and, when available, spatial metadata associated with planning units.
#' }
#'
#' The feature-distribution table provides the canonical sparse representation of
#' these baseline amounts. Thus, after \code{create_problem()} has run, the
#' landscape is internally represented by baseline feature amounts
#' \eqn{a_{if}}, where \eqn{a_{if}} denotes the amount of feature \eqn{f} in
#' planning unit \eqn{i}. These baseline amounts are later combined with
#' actions, effects, targets, objectives, and constraints to build the
#' optimization model.
#'
#' \strong{Which input mode should I use?}
#'
#' \code{create_problem()} supports four main workflows. The best choice depends
#' on the form of your data and on whether you want to preserve geometry.
#'
#' \itemize{
#'   \item \strong{Tabular mode.} Use this when \code{pu}, \code{features}, and
#'   \code{dist_features} are already available as \code{data.frame} objects and
#'   no spatial derivation is needed. This is the simplest workflow: all problem
#'   components are already tabular, so \code{create_problem()} only
#'   standardizes them into the canonical internal representation.
#'
#'   \item \strong{Vector-PU spatial mode.} Use this when planning units are
#'   polygons and feature information is stored in one or more raster layers. In
#'   this mode, \code{dist_features} is derived by aggregating raster values
#'   over planning-unit polygons.
#'
#'   \item \strong{Raster-cell fast mode.} Use this when both \code{pu} and
#'   \code{features} are rasters and each valid raster cell should become one
#'   planning unit. This mode avoids raster-to-polygon conversion, treats
#'   \code{NA} feature values as zero before building \code{dist_features},
#'   keeps only strictly positive amounts in the stored distribution table, and
#'   is generally the preferred option for large regular grids.
#'
#'   \item \strong{Hybrid sf + tabular mode.} Use this when you already have a
#'   curated tabular \code{dist_features} table but still want to preserve
#'   planning-unit geometry and attributes for later plotting, spatial
#'   relations, or feasibility specifications. Here, the feature distribution is
#'   already tabular, but geometry and raw attributes are preserved from the
#'   \code{sf} planning-unit object for later spatial operations.
#' }
#'
#' \strong{How cost is interpreted across modes}
#'
#' \code{cost} handling depends on the selected workflow.
#'
#' \itemize{
#'   \item \strong{Tabular mode.} In purely tabular mode, \code{cost} is not
#'   used by this generic method. Planning-unit costs are expected to already be
#'   present in the \code{pu} table supplied to the internal tabular builder.
#'
#'   \item \strong{Vector-PU spatial mode.} In vector-PU mode, \code{cost} is
#'   required and may be either:
#'   \itemize{
#'     \item the name of a numeric attribute column in the planning-unit layer,
#'     \item or a cost raster to be aggregated over polygons using the
#'     \code{cost_aggregation} argument.
#'   }
#'
#'   \item \strong{Raster-cell fast mode.} In raster-cell mode, \code{cost}
#'   must be a single-layer raster aligned with \code{pu} and \code{features}.
#'   A raster cell becomes a planning unit only if the mask cell is not missing
#'   and the corresponding cost value is finite and strictly positive. In other
#'   words, if \eqn{m_i} denotes the mask value of cell \eqn{i} and \eqn{c_i}
#'   its cost value, then cell \eqn{i} is retained only when the mask is
#'   observed and \eqn{c_i > 0}.
#'
#'   \item \strong{Hybrid sf + tabular mode.} In hybrid mode, \code{cost} must
#'   be either:
#'   \itemize{
#'     \item the name of a numeric attribute column in the \code{sf} layer,
#'     \item or omitted if the \code{sf} attributes already contain a column
#'     literally named \code{cost}.
#'   }
#' }
#'
#' \strong{Feature identifiers}
#'
#' Features are internally standardized to an \code{id}-based representation.
#' In spatial modes where raster layers are used, one feature is created per
#' raster layer, with:
#' \itemize{
#'   \item \code{id = 1, 2, \dots, nlyr(features)},
#'   \item \code{name} equal to the raster layer name, if available,
#'   \item otherwise a generated name of the form \code{"feature.1"},
#'   \code{"feature.2"}, and so on.
#' }
#'
#' \strong{Planning-unit identifiers}
#'
#' In vector and hybrid modes, planning-unit ids are taken from the column named
#' by \code{pu_id_col}. If that column is missing and \code{pu_id_col = "id"},
#' sequential ids are created with a warning.
#'
#' In raster-cell mode, planning-unit ids are always created sequentially from
#' the valid raster cells retained after masking and cost filtering.
#'
#' \strong{Ordering and alignment}
#'
#' In spatial modes, planning units, derived coordinates, raw attributes,
#' geometry, and extracted feature amounts are aligned to the same planning-unit
#' id order before the final \code{Problem} object is created.
#'
#' This alignment is critical because later functions assume that all stored
#' planning-unit components refer to the same ordered set of planning units.
#'
#' After \code{create_problem()}, typical next steps include adding actions,
#' spatial relations, targets or other constraints, objectives, and then solving
#' the resulting problem.
#'
#' @param pu Planning-units input. Depending on the selected method, this may be:
#'   \itemize{
#'     \item a \code{data.frame} with planning-unit information,
#'     \item an \code{sf} object,
#'     \item a \code{terra::SpatVector},
#'     \item a vector file path readable by \pkg{terra},
#'     \item a \code{terra::SpatRaster},
#'     \item or a raster file path.
#'   }
#' @param features Feature input. Depending on the selected workflow, this may be:
#'   \itemize{
#'     \item a \code{data.frame} with at least an \code{id} column,
#'     \item a \code{terra::SpatRaster} with one layer per feature,
#'     \item or a raster file path.
#'   }
#' @param dist_features Feature-distribution input. In tabular and hybrid
#'   workflows, this must be a \code{data.frame} with columns \code{pu},
#'   \code{feature}, and \code{amount}. In spatial modes it may be omitted or
#'   set to \code{NULL}, in which case it is derived automatically.
#' @param cost In spatial modes, planning-unit cost information. Depending on the
#'   input mode, this may be:
#'   \itemize{
#'     \item a column name in the planning-unit attribute table,
#'     \item a raster object,
#'     \item or a raster file path.
#'   }
#'   In raster-cell mode, \code{cost} is required and valid planning units are
#'   defined only where cost is finite and strictly positive.
#' @param pu_id_col Character string giving the name of the planning-unit id
#'   column in vector or \code{sf} inputs. Ignored in purely tabular mode and in
#'   raster-cell mode.
#' @param cost_aggregation Character string used in vector-PU mode when
#'   \code{cost} is a raster and must be aggregated over polygons. Must be one
#'   of \code{"mean"} or \code{"sum"}.
#' @param ... Additional arguments forwarded to internal builders.
#'
#' @return A \code{Problem} object for downstream \code{multiscape} workflows.
#'   The returned object always contains a canonical tabular core with planning
#'   units, features, and feature-distribution data, and may additionally
#'   contain aligned spatial metadata depending on the input mode.
#'
#' @examples
#' # ------------------------------------------------------
#' # 1) Tabular mode
#' # ------------------------------------------------------
#' pu_tbl <- data.frame(
#'   id = 1:4,
#'   cost = c(1, 2, 3, 4)
#' )
#'
#' feat_tbl <- data.frame(
#'   id = 1:2,
#'   name = c("feature_1", "feature_2")
#' )
#'
#' dist_feat_tbl <- data.frame(
#'   pu = c(1, 1, 2, 3, 4),
#'   feature = c(1, 2, 2, 1, 2),
#'   amount = c(5, 2, 3, 4, 1)
#' )
#'
#' p1 <- create_problem(
#'   pu = pu_tbl,
#'   features = feat_tbl,
#'   dist_features = dist_feat_tbl
#' )
#'
#' print(p1)
#'
#' # ------------------------------------------------------
#' # 2) Hybrid sf + tabular mode using package data
#' # ------------------------------------------------------
#'
#' p2 <- create_problem(
#'   pu = sim_pu_sf,
#'   features = sim_features,
#'   dist_features = sim_dist_features,
#'   cost = "cost"
#' )
#'
#' print(p2)
#'
#' @seealso
#' \code{\link{add_actions}},
#' \code{\link{add_constraint_locked_pu}},
#' \code{\link{add_spatial_boundary}},
#' \code{\link{add_spatial_knn}},
#' \code{\link{solve}}
NULL

#' @export
#' @rdname create_problem
methods::setGeneric(
  "create_problem",
  signature = methods::signature("pu", "features", "dist_features"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    standardGeneric("create_problem")
  }
)

# =========================================================
# Internal helpers (minimal, self-contained for spatial paths)
# =========================================================

.pa_is_raster_path <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && file.exists(x) &&
    grepl("\\.(tif|tiff|grd|img|nc|asc|sdat)$", tolower(x))
}

.pa_is_vector_path <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && file.exists(x) &&
    grepl("\\.(gpkg|shp|geojson|json|gml|kml)$", tolower(x))
}

.pa_read_rast <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  if (inherits(x, "SpatRaster")) return(x)
  if (.pa_is_raster_path(x)) {
    return(tryCatch(terra::rast(x), error = function(e) NULL))
  }
  NULL
}

.pa_read_vect <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  if (inherits(x, "SpatVector")) return(x)
  if (inherits(x, "sf")) return(tryCatch(terra::vect(x), error = function(e) NULL))
  if (.pa_is_vector_path(x)) {
    return(tryCatch(terra::vect(x), error = function(e) NULL))
  }
  NULL
}

.pa_fun_from_name <- function(x) {
  x <- match.arg(x, c("mean", "sum"))
  if (identical(x, "mean")) return(function(v) mean(v, na.rm = TRUE))
  function(v) sum(v, na.rm = TRUE)
}

# =========================================================
# Internal: fast raster-cell implementation (1 PU per valid cell)
# =========================================================
.pa_create_problem_raster_cells_impl <- function(
    pu,
    features,
    cost,
    ...
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Raster-cell mode requires the 'terra' package.", call. = FALSE)
  }

  pu_r   <- .pa_read_rast(pu)
  feat_r <- .pa_read_rast(features)
  cost_r <- .pa_read_rast(cost)

  if (is.null(pu_r) || is.null(feat_r) || is.null(cost_r)) {
    stop(
      "Raster-cell mode requires `pu`, `features`, and `cost` to be terra::SpatRaster ",
      "objects or valid raster file paths.",
      call. = FALSE
    )
  }
  if (terra::nlyr(cost_r) != 1) stop("`cost` raster must have exactly 1 layer.", call. = FALSE)

  if (!terra::compareGeom(pu_r, cost_r, stopOnError = FALSE) ||
      !terra::compareGeom(pu_r, feat_r, stopOnError = FALSE)) {
    stop("`pu`, `features`, and `cost` rasters must share extent/resolution/CRS.", call. = FALSE)
  }

  cvals <- terra::values(cost_r, mat = FALSE)
  ok <- !is.na(terra::values(pu_r, mat = FALSE)) &
    !is.na(cvals) & is.finite(cvals) & (cvals > 0)

  idx_cells <- which(ok)
  if (!length(idx_cells)) {
    stop("No valid cells found after applying mask and cost rules (cost must be finite and > 0).", call. = FALSE)
  }

  pu_df <- data.frame(
    id = seq_along(idx_cells),
    cost = cvals[idx_cells],
    locked_in = FALSE,
    locked_out = FALSE,
    stringsAsFactors = FALSE
  )

  xy <- terra::xyFromCell(cost_r, idx_cells)
  pu_coords <- data.frame(id = pu_df$id, x = xy[, 1], y = xy[, 2], stringsAsFactors = FALSE)

  feat_names <- names(feat_r)
  if (is.null(feat_names) || any(feat_names == "")) {
    feat_names <- paste0("feature.", seq_len(terra::nlyr(feat_r)))
  }

  features_df <- data.frame(
    id = seq_len(terra::nlyr(feat_r)),
    name = feat_names,
    stringsAsFactors = FALSE
  )

  M <- terra::values(feat_r, mat = TRUE)
  M <- M[idx_cells, , drop = FALSE]
  M[is.na(M)] <- 0

  w <- which(M > 0, arr.ind = TRUE)
  dist_features_df <- if (!nrow(w)) {
    data.frame(pu = integer(0), feature = integer(0), amount = numeric(0))
  } else {
    data.frame(
      pu = w[, 1],
      feature = w[, 2],
      amount = M[w],
      stringsAsFactors = FALSE
    )
  }

  x <- .pa_create_problem_tabular_impl(
    pu = pu_df,
    features = features_df,
    dist_features = dist_features_df,
    boundary = NULL,
    ...
  )

  x$data$pu_coords <- pu_coords
  x$data$cell_index <- idx_cells
  x$data$pu_raster_mask <- pu_r
  x$data$cost_raster <- cost_r
  x$data$features_raster <- feat_r

  x
}

# =========================================================
# Method: TABULAR inputs
# =========================================================
#' @export
#' @rdname create_problem
methods::setMethod(
  "create_problem",
  methods::signature(pu = "data.frame", features = "data.frame", dist_features = "data.frame"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    x <- .pa_create_problem_tabular_impl(
      pu = pu,
      features = features,
      dist_features = dist_features,
      boundary = NULL,
      ...
    )

    # preserve raw PU attributes for downstream helpers such as add_locked_pu()
    x$data$pu_data_raw <- pu

    x
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features missing)
# =========================================================
#' @export
#' @rdname create_problem
methods::setMethod(
  "create_problem",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "missing"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    cost_aggregation <- match.arg(cost_aggregation)

    if (is.null(cost)) {
      stop(
        "Spatial mode: `dist_features` is missing, so you must provide `cost` ",
        "(either a PU column name for vector PUs, or a raster/file path for spatial rasters).",
        call. = FALSE
      )
    }
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Spatial mode requires the 'terra' package. Please install it.", call. = FALSE)
    }

    pu_is_r <- inherits(pu, "SpatRaster") || .pa_is_raster_path(pu)
    ft_is_r <- inherits(features, "SpatRaster") || .pa_is_raster_path(features)

    if (pu_is_r && ft_is_r) {
      return(.pa_create_problem_raster_cells_impl(
        pu = pu,
        features = features,
        cost = cost,
        ...
      ))
    }

    fun_cost <- .pa_fun_from_name(cost_aggregation)

    pu_r <- NULL
    pu_v <- NULL

    if (inherits(pu, "SpatRaster") || .pa_is_raster_path(pu)) {
      pu_r <- .pa_read_rast(pu)
      pu_r <- terra::round(pu_r)
      pu_v <- terra::as.polygons(pu_r, dissolve = TRUE, values = TRUE, na.rm = TRUE)
      names(pu_v) <- pu_id_col
    } else {
      pu_v <- .pa_read_vect(pu)
      if (is.null(pu_v)) stop("Could not read `pu` as a vector layer.", call. = FALSE)

      if (!(pu_id_col %in% names(pu_v))) {
        if (identical(pu_id_col, "id")) {
          warning(
            "Planning unit layer has no 'id' column. Creating sequential ids (1..n). ",
            "If you want to use an existing field, set pu_id_col to its name.",
            call. = FALSE, immediate. = TRUE
          )
          pu_v$id <- seq_len(nrow(pu_v))
          pu_id_col <- "id"
        } else {
          stop(
            "Planning unit vector is missing the id column: '", pu_id_col, "'. ",
            "Either provide that column or set pu_id_col to an existing column name.",
            call. = FALSE
          )
        }
      }
    }

    pu_df_raw <- terra::as.data.frame(pu_v)
    pu_df_raw$row_id <- seq_len(nrow(pu_df_raw))
    names(pu_df_raw)[names(pu_df_raw) == pu_id_col] <- "id"
    pu_df_raw$id <- as.integer(round(pu_df_raw$id))

    pu_df <- pu_df_raw

    if (is.character(cost) && (cost %in% names(pu_df))) {
      pu_df$cost <- pu_df[[cost]]
    } else {
      cost_r <- .pa_read_rast(cost)
      if (is.null(cost_r)) {
        stop("You must provide `cost` either as a PU column name (vector) or as a raster/file path.", call. = FALSE)
      }
      cost_ex <- terra::extract(cost_r, pu_v, fun = fun_cost, na.rm = TRUE)
      idx <- match(pu_df$row_id, cost_ex[[1]])
      pu_df$cost <- cost_ex[[2]][idx]
    }

    ctr <- terra::centroids(pu_v)
    xy  <- terra::crds(ctr)
    pu_coords <- data.frame(
      id = pu_df$id,
      x  = as.numeric(xy[, 1]),
      y  = as.numeric(xy[, 2]),
      stringsAsFactors = FALSE
    )

    feat_r <- .pa_read_rast(features)
    if (is.null(feat_r)) {
      stop(
        "Spatial mode: `features` must be a terra::SpatRaster (or raster file path) with one layer per feature.\n",
        "If you intended tabular mode, provide `dist_features` as a data.frame.",
        call. = FALSE
      )
    }

    feat_names <- names(feat_r)
    if (is.null(feat_names) || any(feat_names == "")) {
      feat_names <- paste0("feature.", seq_len(terra::nlyr(feat_r)))
      names(feat_r) <- feat_names
    }

    features_df <- data.frame(
      id = seq_len(terra::nlyr(feat_r)),
      name = feat_names,
      stringsAsFactors = FALSE
    )

    feat_mat <- .pa_fast_extract(feat_r, pu_v, fun = "sum")

    ord <- order(pu_df$id)
    pu_df     <- pu_df[ord, , drop = FALSE]
    pu_df_raw <- pu_df_raw[ord, , drop = FALSE]
    feat_mat  <- feat_mat[ord, , drop = FALSE]
    pu_coords <- pu_coords[ord, , drop = FALSE]

    pu_df_out <- pu_df[, c("id", "cost"), drop = FALSE]
    pu_df_out$locked_in <- FALSE
    pu_df_out$locked_out <- FALSE

    dist_features_df <- data.frame(
      pu      = rep(pu_df_out$id, times = terra::nlyr(feat_r)),
      feature = rep(features_df$id, each = nrow(pu_df_out)),
      amount  = as.vector(feat_mat),
      stringsAsFactors = FALSE
    )
    dist_features_df <- dist_features_df[
      is.finite(dist_features_df$amount) & dist_features_df$amount > 0,
      , drop = FALSE
    ]

    x <- .pa_create_problem_tabular_impl(
      pu = pu_df_out,
      features = features_df,
      dist_features = dist_features_df,
      boundary = NULL,
      ...
    )

    x$data$pu_coords <- pu_coords
    x$data$pu_data_raw <- pu_df_raw

    if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
      x$data$spatial_relations <- list()
    }
    if (!is.null(pu_r)) x$data$pu_raster_id <- pu_r

    if (requireNamespace("sf", quietly = TRUE)) {
      pu_sf_store <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)
      if (!is.null(pu_sf_store)) {
        if (pu_id_col %in% names(pu_sf_store)) names(pu_sf_store)[names(pu_sf_store) == pu_id_col] <- "id"
        if (!("id" %in% names(pu_sf_store))) pu_sf_store$id <- seq_len(nrow(pu_sf_store))

        pu_sf_store <- pu_sf_store[, "id", drop = FALSE]
        ord2 <- match(x$data$pu$id, pu_sf_store$id)

        if (any(is.na(ord2))) {
          warning(
            "Could not safely match PU geometry to PU ids; 'pu_sf' will not be stored in the problem object.",
            call. = FALSE, immediate. = TRUE
          )
        } else {
          x$data$pu_sf <- pu_sf_store[ord2, , drop = FALSE]
        }
      }
    }

    x
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features explicitly NULL)
# =========================================================
#' @export
#' @rdname create_problem
methods::setMethod(
  "create_problem",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "NULL"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    methods::selectMethod("create_problem", signature = c("ANY", "ANY", "missing"))(
      pu = pu,
      features = features,
      cost = cost,
      pu_id_col = pu_id_col,
      cost_aggregation = cost_aggregation,
      ...
    )
  }
)


# =========================================================
# Method: HYBRID sf + tabular features/dist_features
# =========================================================
#' @export
#' @rdname create_problem
methods::setMethod(
  "create_problem",
  methods::signature(pu = "ANY", features = "data.frame", dist_features = "data.frame"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    cost_aggregation = c("mean", "sum"),
    ...
  ) {

    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("This method requires the 'sf' package.", call. = FALSE)
    }

    pu_sf <- pu

    # ---- ensure id column exists
    if (!(pu_id_col %in% names(pu_sf))) {
      if (identical(pu_id_col, "id")) {
        warning(
          "Planning unit sf object has no 'id' column. Creating sequential ids (1..n). ",
          "If you want to use an existing field, set pu_id_col to its name.",
          call. = FALSE,
          immediate. = TRUE
        )
        pu_sf$id <- seq_len(nrow(pu_sf))
        pu_id_col <- "id"
      } else {
        stop(
          "Planning unit sf object is missing the id column: '", pu_id_col, "'. ",
          "Either provide that column or set pu_id_col to an existing column name.",
          call. = FALSE
        )
      }
    }

    # ---- raw PU attributes (without geometry)
    pu_df_raw <- sf::st_drop_geometry(pu_sf)
    names(pu_df_raw)[names(pu_df_raw) == pu_id_col] <- "id"
    pu_df_raw$id <- as.integer(round(pu_df_raw$id))

    # ---- cost must come from sf attribute table
    if (is.null(cost)) {
      if (!("cost" %in% names(pu_df_raw))) {
        stop(
          "For pu='sf' + features='data.frame' + dist_features='data.frame', ",
          "you must provide `cost` as a column name in the sf attribute table, ",
          "or include a column literally named 'cost'.",
          call. = FALSE
        )
      }
      pu_df_raw$cost <- as.numeric(pu_df_raw$cost)
    } else if (is.character(cost) && length(cost) == 1L && (cost %in% names(pu_df_raw))) {
      pu_df_raw$cost <- as.numeric(pu_df_raw[[cost]])
    } else {
      stop(
        "For pu='sf' + features='data.frame' + dist_features='data.frame', ",
        "`cost` must be the name of a column in the sf attribute table.",
        call. = FALSE
      )
    }

    if (any(!is.finite(pu_df_raw$cost))) {
      stop("PU cost column contains non-finite values.", call. = FALSE)
    }

    # ---- features checks
    if (!inherits(features, "data.frame")) {
      stop("`features` must be a data.frame.", call. = FALSE)
    }
    if (!("id" %in% names(features))) {
      stop("`features` must contain an 'id' column.", call. = FALSE)
    }
    features$id <- as.integer(features$id)

    if ("name" %in% names(features)) {
      if (anyNA(features$name) || any(!nzchar(as.character(features$name)))) {
        stop("`features$name` must not contain NA or empty strings.", call. = FALSE)
      }
      if (anyDuplicated(as.character(features$name)) != 0L) {
        dups <- unique(as.character(features$name)[duplicated(as.character(features$name))])
        stop(
          "`features$name` must be unique when feature names are used in `dist_features$feature`. ",
          "Duplicated names: ", paste(dups, collapse = ", "),
          call. = FALSE
        )
      }
    }

    # ---- dist_features checks
    if (!inherits(dist_features, "data.frame")) {
      stop("`dist_features` must be a data.frame.", call. = FALSE)
    }

    req_df_cols <- c("pu", "feature", "amount")
    miss_df_cols <- setdiff(req_df_cols, names(dist_features))
    if (length(miss_df_cols) > 0) {
      stop(
        "`dist_features` must contain columns: pu, feature, amount. Missing: ",
        paste(miss_df_cols, collapse = ", "),
        call. = FALSE
      )
    }

    dist_features$pu <- as.integer(dist_features$pu)
    dist_features$amount <- as.numeric(dist_features$amount)

    # ---- resolve dist_features$feature against features$id or features$name
    feature_raw <- dist_features$feature

    # case 1: already numeric ids
    feature_as_int <- suppressWarnings(as.integer(feature_raw))
    can_use_ids <- !anyNA(feature_as_int) &&
      all(feature_as_int %in% features$id)

    if (can_use_ids) {
      dist_features$feature <- feature_as_int

    } else {
      # case 2: try matching against features$name
      if (!("name" %in% names(features))) {
        stop(
          "`dist_features$feature` could not be matched to `features$id`, ",
          "and `features` has no `name` column to resolve feature names.",
          call. = FALSE
        )
      }

      feature_chr <- as.character(feature_raw)
      idx <- match(feature_chr, as.character(features$name))

      if (anyNA(idx)) {
        bad <- unique(feature_chr[is.na(idx)])
        stop(
          "`dist_features$feature` must contain either valid feature ids ",
          "from `features$id` or valid feature names from `features$name`.\n",
          "Unmatched values: ",
          paste(utils::head(bad, 20), collapse = ", "),
          if (length(bad) > 20) " ..." else "",
          call. = FALSE
        )
      }

      dist_features$feature <- as.integer(features$id[idx])
    }

    # ---- build PU table for tabular backend
    pu_df_out <- pu_df_raw[, c("id", "cost"), drop = FALSE]
    pu_df_out$locked_in <- FALSE
    pu_df_out$locked_out <- FALSE

    # ---- consistency checks
    bad_pu <- setdiff(unique(dist_features$pu), pu_df_out$id)
    if (length(bad_pu) > 0) {
      stop(
        "Some `dist_features$pu` ids are not present in `pu`: ",
        paste(utils::head(bad_pu, 20), collapse = ", "),
        if (length(bad_pu) > 20) " ..." else "",
        call. = FALSE
      )
    }

    bad_feat <- setdiff(unique(dist_features$feature), features$id)
    if (length(bad_feat) > 0) {
      stop(
        "Some `dist_features$feature` ids are not present in `features`: ",
        paste(utils::head(bad_feat, 20), collapse = ", "),
        if (length(bad_feat) > 20) " ..." else "",
        call. = FALSE
      )
    }

    # ---- build tabular problem
    x <- .pa_create_problem_tabular_impl(
      pu = pu_df_out,
      features = features,
      dist_features = dist_features,
      boundary = NULL,
      ...
    )

    # ---- preserve spatial information
    ctr <- suppressWarnings(sf::st_centroid(sf::st_geometry(pu_sf)))
    xy <- sf::st_coordinates(ctr)

    pu_coords <- data.frame(
      id = pu_df_out$id,
      x = as.numeric(xy[, 1]),
      y = as.numeric(xy[, 2]),
      stringsAsFactors = FALSE
    )

    pu_sf_store <- pu_sf
    if (pu_id_col %in% names(pu_sf_store)) {
      names(pu_sf_store)[names(pu_sf_store) == pu_id_col] <- "id"
    }
    pu_sf_store$id <- as.integer(round(pu_sf_store$id))

    ord <- match(x$data$pu$id, pu_sf_store$id)
    if (any(is.na(ord))) {
      warning(
        "Could not safely match PU geometry to PU ids; 'pu_sf' will not be stored in the problem object.",
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      x$data$pu_sf <- pu_sf_store[ord, , drop = FALSE]
    }

    x$data$pu_coords <- pu_coords
    x$data$pu_data_raw <- pu_df_raw

    if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
      x$data$spatial_relations <- list()
    }

    x
  }
)
