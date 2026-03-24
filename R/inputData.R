#' @include internal.R
NULL

#' Create a planning problem input object
#'
#' @description
#' Builds a `Problem` object from either tabular inputs or spatial inputs.
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
#'#' 4) **Hybrid sf-tabular mode**: If `pu` is an `sf` object, while `features` and
#'    `dist_features` are `data.frame`s, then the problem is built from the supplied
#'    tabular feature distribution, while preserving PU geometry and raw spatial
#'    attributes for downstream spatial operations.
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
#' \strong{Locks are not read automatically.}
#' This function does not interpret locked planning units, even if the input data
#' contain columns such as `locked_in`, `locked_out`, or Marxan-style status codes.
#' Those should be handled later using a dedicated function such as
#' `add_locked_pu()`.
#'
#' In spatial vector mode, the original planning-unit attributes are preserved in
#' `x$data$pu_data_raw`, aligned by `id`, so that downstream functions can read
#' lock columns or other metadata from the original source.
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
#' @param cost_aggregation In vector-PU mode, how to aggregate raster cell costs to polygons when `cost` is a raster.
#'   Either `"mean"` or `"sum"`.
#' @param ... Additional arguments forwarded to internal builders.
#'
#' @return A `Problem` object used by downstream functions (`add_*()`, `set_solver()`, `solve()`, etc.).
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
#' # ------------------------------------------------------
#' # 2) Raster-cell fast mode (1 PU per valid cell)
#' # ------------------------------------------------------
#' library(terra)
#' pu_mask <- rast("pu_mask.tif")
#' cost_r  <- rast("cost.tif")
#' feat_r  <- rast(c("sp1.tif", "sp2.tif"))
#'
#' x <- inputData(pu = pu_mask, features = feat_r, cost = cost_r)
#'
#' # ------------------------------------------------------
#' # 3) Vector-PU spatial mode (polygons + raster features)
#' # ------------------------------------------------------
#' pu_v   <- vect("pus.gpkg")
#' feat_r <- rast(c("sp1.tif", "sp2.tif"))
#' cost_r <- rast("cost.tif")
#'
#' x <- inputData(
#'   pu = pu_v,
#'   features = feat_r,
#'   cost = cost_r,
#'   pu_id_col = "id",
#'   cost_aggregation = "mean"
#' )
#'
#' # Original PU attributes are preserved for later use
#' # x$data$pu_data_raw
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

    # preserve raw PU attributes for downstream helpers such as add_locked_pu()
    x$data$pu_data_raw <- pu

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

    x <- .pa_inputData_tabular_impl(
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
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    methods::selectMethod("inputData", signature = c("ANY", "ANY", "missing"))(
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
#' @rdname inputData
methods::setMethod(
  "inputData",
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
          paste(head(bad, 20), collapse = ", "),
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
        paste(head(bad_pu, 20), collapse = ", "),
        if (length(bad_pu) > 20) " ..." else "",
        call. = FALSE
      )
    }

    bad_feat <- setdiff(unique(dist_features$feature), features$id)
    if (length(bad_feat) > 0) {
      stop(
        "Some `dist_features$feature` ids are not present in `features`: ",
        paste(head(bad_feat, 20), collapse = ", "),
        if (length(bad_feat) > 20) " ..." else "",
        call. = FALSE
      )
    }

    # ---- build tabular problem
    x <- .pa_inputData_tabular_impl(
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
