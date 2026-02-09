#' @include internal.R
NULL

#' Create a planning problem input object
#'
#' @description
#' Builds a `Data` object from either tabular inputs or spatial inputs.
#' This is the entry point for the **mosap** workflow.
#'
#' The function supports three input styles:
#'
#' 1) **Tabular mode**: If `pu`, `features`, and `dist_features` are `data.frame`s,
#'    a purely tabular workflow is used (no spatial packages required).
#'
#' 2) **Vector-PU spatial mode**: If `dist_features` is missing (or `NULL`) and `pu` is
#'    a spatial vector (e.g. `terra::SpatVector`, `sf`, a vector file path), while
#'    `features` is a raster stack (one layer per feature), then feature amounts are
#'    aggregated by polygon (default `sum`) and stored as `dist_features`.
#'
#' 3) **Raster-cell fast mode**: If `dist_features` is missing (or `NULL`) and `pu` and
#'    `features` are rasters (e.g. `terra::SpatRaster` or raster file paths), then each
#'    valid raster cell becomes a planning unit. In this mode:
#'    - cells are valid if `cost` is finite and `cost > 0`
#'    - `pu` is used as a mask/template (cells with `NA` in `pu` are excluded)
#'    This avoids raster-to-polygon conversion and is substantially faster for large grids.
#'
#' @details
#' \strong{Spatial relations are not created automatically.}
#' This version of `inputData()` does \emph{not} take a `boundary` argument.
#' Spatial relations (boundary/rook/queen/kNN/distance) must be registered explicitly
#' after `inputData()` using `add_spatial_boundary()`, `add_spatial_rook()`,
#' `add_spatial_queen()`, `add_spatial_knn()`, `add_spatial_distance()`, or
#' `add_spatial_relations()`.
#'
#' In raster-cell mode you typically build relations from coordinates with
#' `add_spatial_knn()` or `add_spatial_distance()`.
#'
#' @param pu Planning units input. Either:
#' \itemize{
#' \item a `data.frame` with an `id` column (tabular mode),
#' \item a spatial vector (`terra::SpatVector`, `sf`, or a vector file path) (vector-PU mode), or
#' \item a raster (`terra::SpatRaster` or raster file path) used as a mask/template (raster-cell mode).
#' }
#' @param features Features input. Either:
#' \itemize{
#' \item a `data.frame` with `id` (and optionally `name`) (tabular mode), or
#' \item a raster stack with one layer per feature (`terra::SpatRaster` or file path)
#'   (vector-PU mode or raster-cell mode).
#' }
#' @param dist_features Distribution of features. A `data.frame` with columns `pu`, `feature`,
#'   and `amount`. If missing or `NULL`, spatial workflows derive it automatically.
#' @param cost In spatial modes, required. Either a column name in the PU vector attribute table
#'   (vector-PU mode) or a raster/file path (vector-PU or raster-cell mode). In raster-cell mode,
#'   cells with `cost <= 0` or `NA` are excluded (no PU is created).
#' @param pu_id_col For vector-PU mode, the name of the id column in the PU layer.
#' @param locked_in_col For vector-PU mode, the name of a logical column indicating locked-in PUs.
#' @param locked_out_col For vector-PU mode, the name of a logical column indicating locked-out PUs.
#' @param pu_status Optional (vector-PU or raster-cell) status input. Either a column name or a raster/file path.
#'   Values `2` mark locked-in and `3` mark locked-out (Marxan-style).
#' @param cost_aggregation In vector-PU mode, how to aggregate raster cell costs to polygons when `cost` is a raster.
#'   Either `"mean"` or `"sum"`.
#' @param ... Additional arguments forwarded to internal builders.
#'
#' @return A `Data` object used by downstream functions (`add_*()`, `set_solver()`, `solve()`, etc.).
#'
#' @examples
#' \dontrun{
#' # ------------------------------------------------------
#' # 1) Tabular mode
#' # ------------------------------------------------------
#' pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
#' features <- data.frame(id = 1:2, name = c("sp1", "sp2"))
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3),
#'   feature = c(1, 2, 2, 1),
#'   amount = c(1, 5, 2, 1)
#' )
#'
#' x <- inputData(pu = pu, features = features, dist_features = dist_features)
#'
#' # Optional: register a spatial relation later (if you need spatial objectives)
#' rel <- data.frame(pu1 = 1, pu2 = 2, weight = 1)
#' x <- add_spatial_relations(x, rel, name = "rook_like")
#'
#'
#' # ------------------------------------------------------
#' # 2) Raster-cell fast mode (1 PU per valid cell)
#' # ------------------------------------------------------
#' library(terra)
#' pu_mask <- rast("pu_mask.tif")           # NA outside study area
#' cost_r  <- rast("cost.tif")              # cost per cell
#' feat_r  <- rast(c("sp1.tif", "sp2.tif")) # layers = features
#'
#' x <- inputData(pu = pu_mask, features = feat_r, cost = cost_r)
#'
#' # Optional: add spatial relations after inputData (e.g., kNN)
#' x <- add_spatial_knn(x, k = 8, name = "knn8", weight_fn = "inverse")
#'
#'
#' # ------------------------------------------------------
#' # 3) Vector-PU spatial mode (polygons + raster features)
#' # ------------------------------------------------------
#' pu_v   <- vect("pus.gpkg")               # polygon PUs
#' feat_r <- rast(c("sp1.tif", "sp2.tif"))  # features as raster layers
#' cost_r <- rast("cost.tif")               # raster cost (aggregated to polygons)
#'
#' x <- inputData(
#'   pu = pu_v,
#'   features = feat_r,
#'   cost = cost_r,
#'   pu_id_col = "id",
#'   cost_aggregation = "mean"
#' )
#'
#' # Add boundary-length relation explicitly (requires sf)
#' x <- add_spatial_boundary(x, pu_sf = x$data$pu_sf, name = "boundary")
#' }
#'
#' @name inputData
NULL

#' @export
#' @rdname inputData
methods::setGeneric(
  "inputData",
  signature = methods::signature("pu", "features", "dist_features"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    standardGeneric("inputData")
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
.pa_inputData_raster_cells_impl <- function(
    pu,
    features,
    cost,
    pu_status = NULL,
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

  if (!is.null(pu_status)) {
    st_r <- .pa_read_rast(pu_status)
    if (is.null(st_r) || terra::nlyr(st_r) != 1) {
      stop("In raster-cell mode, `pu_status` must be a single-layer SpatRaster or raster file path.", call. = FALSE)
    }
    if (!terra::compareGeom(pu_r, st_r, stopOnError = FALSE)) {
      stop("`pu_status` raster must match `pu` geometry.", call. = FALSE)
    }
    st <- terra::values(st_r, mat = FALSE)[idx_cells]
    pu_df$locked_in  <- st == 2
    pu_df$locked_out <- st == 3
    if (any(pu_df$locked_in & pu_df$locked_out, na.rm = TRUE)) {
      stop("Some cells are both locked_in and locked_out (status raster inconsistent).", call. = FALSE)
    }
  }

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

  x <- .pa_inputData_tabular_impl(
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
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "data.frame", features = "data.frame", dist_features = "data.frame"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    x <- .pa_inputData_tabular_impl(
      pu = pu,
      features = features,
      dist_features = dist_features,
      boundary = NULL,
      ...
    )
    x
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features missing)
# =========================================================
#' @export
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "missing"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
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
      return(.pa_inputData_raster_cells_impl(
        pu = pu,
        features = features,
        cost = cost,
        pu_status = pu_status,
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

    pu_df <- terra::as.data.frame(pu_v)
    pu_df$row_id <- seq_len(nrow(pu_df))
    names(pu_df)[names(pu_df) == pu_id_col] <- "id"
    pu_df$id <- as.integer(round(pu_df$id))

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

    pu_df$locked_in  <- if (locked_in_col %in% names(pu_df))  as.logical(pu_df[[locked_in_col]])  else FALSE
    pu_df$locked_out <- if (locked_out_col %in% names(pu_df)) as.logical(pu_df[[locked_out_col]]) else FALSE

    if (!is.null(pu_status) &&
        !(locked_in_col %in% names(pu_df)) &&
        !(locked_out_col %in% names(pu_df))) {

      if (is.character(pu_status) && (pu_status %in% names(pu_df))) {
        st <- pu_df[[pu_status]]
      } else {
        st_r <- .pa_read_rast(pu_status)
        if (is.null(st_r)) stop("pu_status must be a column name or a raster/file path.", call. = FALSE)
        st_ex <- terra::extract(st_r, pu_v, fun = terra::modal, na.rm = TRUE)
        st <- st_ex[[2]]
      }
      pu_df$locked_in  <- st == 2
      pu_df$locked_out <- st == 3
    }

    if (any(pu_df$locked_in & pu_df$locked_out, na.rm = TRUE)) {
      stop("Some planning units are both locked_in and locked_out. Please fix your inputs.", call. = FALSE)
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
    feat_mat  <- feat_mat[ord, , drop = FALSE]
    pu_coords <- pu_coords[ord, , drop = FALSE]

    pu_df_out <- pu_df[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]

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

    x <- .pa_inputData_tabular_impl(
      pu = pu_df_out,
      features = features_df,
      dist_features = dist_features_df,
      boundary = NULL,
      ...
    )

    x$data$pu_coords <- pu_coords

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
#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(pu = "ANY", features = "ANY", dist_features = "NULL"),
  function(
    pu,
    features,
    dist_features,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    methods::selectMethod("inputData", signature = c("ANY", "ANY", "missing"))(
      pu = pu,
      features = features,
      cost = cost,
      pu_id_col = pu_id_col,
      locked_in_col = locked_in_col,
      locked_out_col = locked_out_col,
      pu_status = pu_status,
      cost_aggregation = cost_aggregation,
      ...
    )
  }
)
