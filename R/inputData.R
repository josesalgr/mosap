#' @include internal.R
#'
#' Create a prioritization planning problem input object
#'
#' @description
#' Builds a \code{Data} object (class \code{"Data"}) from either tabular inputs or spatial inputs.
#' This is the entry point for the \pkg{prioriactions} workflow.
#'
#' The function supports three input styles.
#'
#' If \code{pu}, \code{features}, and \code{dist_features} are \code{data.frame}s, a purely tabular
#' workflow is used (no spatial packages required).
#'
#' If \code{dist_features} is missing (or \code{NULL}) and \code{pu}/\code{features} are spatial,
#' a spatial workflow is used. In the vector-PU workflow, planning units are polygons and feature
#' amounts are aggregated by polygon.
#'
#' If \code{dist_features} is missing (or \code{NULL}) and \code{pu} and \code{features} are rasters,
#' a fast raster workflow is used where each valid raster cell becomes a planning unit. In this mode,
#' cells are considered valid if \code{cost} is finite and \code{cost > 0}, and \code{pu} is used
#' as a mask/template (cells with \code{NA} in \code{pu} are excluded). This avoids raster-to-polygon
#' conversion and is substantially faster for large grids.
#'
#' @param pu Planning units input. Either a \code{data.frame} with an \code{id} column,
#'   a spatial vector (e.g. \code{terra::SpatVector} or a vector file path), or a raster
#'   (e.g. \code{terra::SpatRaster} or a raster file path).
#' @param features Features input. Either a \code{data.frame} with \code{id} (and optionally \code{name}),
#'   or a raster stack/brick with one layer per feature (as \code{terra::SpatRaster} or file path).
#' @param dist_features Distribution of features. A \code{data.frame} with columns \code{pu}, \code{feature},
#'   and \code{amount}. If missing or \code{NULL}, spatial workflows derive it automatically.
#' @param boundary Optional boundary information. In tabular mode, a \code{data.frame} with columns
#'   \code{id1}, \code{id2}, \code{boundary}. In spatial vector mode, use \code{"auto"} to derive adjacency
#'   from PU polygons (requires \pkg{sf}). In raster-cell mode, \code{"auto"} derives rook adjacency
#'   on the grid; you can also set \code{"none"}, \code{"rook"}, or \code{"queen"}.
#' @param cost In spatial modes, required. Either a column name in the PU vector attribute table
#'   (vector-PU mode) or a raster/file path (vector-PU or raster-cell mode). In raster-cell mode,
#'   cells with \code{cost <= 0} or \code{NA} are excluded (no PU is created).
#' @param pu_id_col For vector-PU mode, the name of the id column in the PU layer.
#' @param locked_in_col For vector-PU mode, the name of a logical column indicating locked-in PUs.
#' @param locked_out_col For vector-PU mode, the name of a logical column indicating locked-out PUs.
#' @param pu_status Optional (vector-PU or raster-cell) status input. Either a column name or a raster/file path.
#'   Values \code{2} mark locked-in and \code{3} mark locked-out (Marxan-style).
#' @param cost_aggregation In vector-PU mode, how to aggregate cell costs to polygons when \code{cost} is a raster.
#'   Either \code{"mean"} or \code{"sum"}.
#' @param ... Additional arguments forwarded to internal builders (including legacy mode args in your tabular impl).
#'
#' @return A \code{Data} object used by downstream functions (\code{problem()}, \code{add_*()}, \code{solve()}, etc.).
#'
#' @examples
#' \dontrun{
#' # Tabular
#' pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
#' features <- data.frame(id = 1:2, name = c("sp1", "sp2"))
#' dist_features <- data.frame(pu = c(1,1,2,3), feature = c(1,2,2,1), amount = c(1,5,2,1))
#' x <- inputData(pu, features, dist_features)
#'
#' # Raster-cell fast mode
#' library(terra)
#' pu_mask <- rast("pu_mask.tif")     # NA outside study area
#' cost_r  <- rast("cost.tif")        # cost per cell
#' feat_r  <- rast(c("sp1.tif","sp2.tif"))  # layers = features
#' x <- inputData(pu = pu_mask, features = feat_r, cost = cost_r, boundary = "auto")
#'
#' # Vector-PU spatial mode
#' pu_v <- vect("pus.gpkg")
#' x <- inputData(pu = pu_v, features = feat_r, cost = cost_r, boundary = "auto")
#' }
#'
#' @export
#' @rdname inputData
methods::setGeneric(
  "inputData",
  signature = methods::signature("pu", "features", "dist_features"),
  function(
    pu,
    features,
    dist_features,
    boundary = NULL,
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
    out <- tryCatch(terra::rast(x), error = function(e) NULL)
    return(out)
  }
  NULL
}

.pa_read_vect <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  if (inherits(x, "SpatVector")) return(x)
  if (inherits(x, "sf")) return(tryCatch(terra::vect(x), error = function(e) NULL))
  if (.pa_is_vector_path(x)) {
    out <- tryCatch(terra::vect(x), error = function(e) NULL)
    return(out)
  }
  NULL
}

.pa_fun_from_name <- function(x) {
  x <- match.arg(x, c("mean", "sum"))
  if (identical(x, "mean")) return(function(v) mean(v, na.rm = TRUE))
  function(v) sum(v, na.rm = TRUE)
}

# Grid adjacency builder for raster-cell mode
.pa_boundary_from_grid <- function(template_r, idx_cells, mode = c("rook", "queen")) {
  mode <- match.arg(mode)

  ncell <- terra::ncell(template_r)
  ncol  <- terra::ncol(template_r)
  nrow  <- terra::nrow(template_r)

  cell_to_pu <- integer(ncell)
  cell_to_pu[idx_cells] <- seq_along(idx_cells)

  rc <- terra::rowColFromCell(template_r, idx_cells)
  r <- rc[, 1]; c <- rc[, 2]
  base_cell <- idx_cells

  offsets <- c(-1L, 1L, -ncol, ncol)
  if (mode == "queen") offsets <- c(offsets, -ncol-1L, -ncol+1L, ncol-1L, ncol+1L)

  id1_all <- integer(0)
  id2_all <- integer(0)

  for (off in offsets) {
    nb_cell <- base_cell + off

    inb <- nb_cell >= 1L & nb_cell <= ncell

    if (off %in% c(-1L, -ncol-1L, ncol-1L)) inb <- inb & (c > 1L)
    if (off %in% c(1L, -ncol+1L, ncol+1L))   inb <- inb & (c < ncol)
    if (off %in% c(-ncol, -ncol-1L, -ncol+1L)) inb <- inb & (r > 1L)
    if (off %in% c(ncol, ncol-1L, ncol+1L))     inb <- inb & (r < nrow)

    nb_cell <- nb_cell[inb]
    id1 <- cell_to_pu[base_cell[inb]]
    id2 <- cell_to_pu[nb_cell]

    keep <- id2 != 0L
    id1 <- id1[keep]; id2 <- id2[keep]

    swap <- id1 > id2
    if (any(swap)) {
      tmp <- id1[swap]
      id1[swap] <- id2[swap]
      id2[swap] <- tmp
    }

    keep2 <- id1 < id2
    if (any(keep2)) {
      id1_all <- c(id1_all, id1[keep2])
      id2_all <- c(id2_all, id2[keep2])
    }
  }

  if (!length(id1_all)) return(NULL)

  boundary_df <- data.frame(id1 = id1_all, id2 = id2_all, boundary = 1, stringsAsFactors = FALSE)

  key <- paste(boundary_df$id1, boundary_df$id2, sep = "||")
  if (anyDuplicated(key)) boundary_df <- stats::aggregate(boundary ~ id1 + id2, boundary_df, sum)

  boundary_df
}

.pa_register_boundary_relation <- function(x, boundary_df, name = "boundary",
                                           weight_col = "boundary") {
  if (is.null(boundary_df) || !inherits(boundary_df, "data.frame") || nrow(boundary_df) == 0) {
    return(x)
  }

  # Normaliza nombres esperados id1/id2/boundary
  if (!all(c("id1", "id2") %in% names(boundary_df))) {
    stop("boundary data.frame must have columns 'id1' and 'id2'.", call. = FALSE)
  }
  if (!is.null(weight_col) && !(weight_col %in% names(boundary_df))) {
    stop("boundary data.frame must have weight column '", weight_col, "'.", call. = FALSE)
  }

  if (exists("add_spatial_boundary", mode = "function")) {
    x <- add_spatial_boundary(
      x,
      boundary   = boundary_df,
      name       = name,
      weight_col = weight_col
    )
  } else {
    # fallback: deja la tabla cruda (tu build_model exigirá internal_* si el objetivo la usa)
    x$data$spatial_relations <- x$data$spatial_relations %||% list()
    x$data$spatial_relations[[name]] <- boundary_df
    warning(
      "add_spatial_boundary() is not available; stored raw boundary table in x$data$spatial_relations[['",
      name, "']]. Objectives requiring this relation may fail later.",
      call. = FALSE, immediate. = TRUE
    )
  }

  x
}

# =========================================================
# Internal: fast raster-cell implementation (1 PU per valid cell)
# =========================================================
.pa_inputData_raster_cells_impl <- function(
    pu,
    features,
    cost,
    boundary = c("none", "auto", "rook", "queen"),
    pu_status = NULL,
    ...
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Raster-cell mode requires the 'terra' package.", call. = FALSE)
  }

  boundary <- match.arg(boundary)

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

  # valid = inside mask AND finite cost AND cost > 0
  cvals <- terra::values(cost_r, mat = FALSE)
  ok <- !is.na(terra::values(pu_r, mat = FALSE)) & !is.na(cvals) & is.finite(cvals) & (cvals > 0)

  idx_cells <- which(ok)
  if (!length(idx_cells)) {
    stop("No valid cells found after applying mask and cost rules (cost must be finite and > 0).", call. = FALSE)
  }

  # pu table
  pu_df <- data.frame(
    id = seq_along(idx_cells),
    cost = cvals[idx_cells],
    locked_in = FALSE,
    locked_out = FALSE,
    stringsAsFactors = FALSE
  )

  # optional status raster: 2 locked_in, 3 locked_out
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

  # coordinates (cell centers)
  xy <- terra::xyFromCell(cost_r, idx_cells)
  pu_coords <- data.frame(id = pu_df$id, x = xy[, 1], y = xy[, 2], stringsAsFactors = FALSE)

  # features table + dist_features (read once, subset valid cells)
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

  # boundary
  boundary_df <- NULL
  if (is.character(boundary)) {
    if (boundary == "auto") boundary <- "rook"
    if (boundary %in% c("rook", "queen")) {
      boundary_df <- .pa_boundary_from_grid(cost_r, idx_cells, mode = boundary)
    }
  }

  # build via stable tabular impl (your existing function)
  x <- .pa_inputData_tabular_impl(
    pu = pu_df,
    features = features_df,
    dist_features = dist_features_df,
    boundary = boundary_df,
    ...
  )

  # store raster artifacts for later spatial ops (optional but useful)
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
    boundary = NULL,
    cost = NULL,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    ...
  ) {
    .pa_inputData_tabular_impl(
      pu = pu,
      features = features,
      dist_features = dist_features,
      boundary = NULL,
      ...
    )

    # 2) si el usuario dio boundary (legacy), regístralo como spatial relation
    if (is.data.frame(boundary)) {
      x <- .pa_register_boundary_relation(x, boundary_df = boundary, name = "boundary", weight_col = "boundary")
    } else if (!is.null(boundary)) {
      stop("In tabular mode, boundary must be NULL or a data.frame (id1,id2,boundary).", call. = FALSE)
    }

    x
  }
)

# =========================================================
# Method: SPATIAL inputs (dist_features missing)
# This method routes to raster-cell mode when pu+features are rasters,
# otherwise uses your existing vector-PU spatial workflow.
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
    boundary = NULL,
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

    # -----------------------------------------------------
    # Route 1: Raster-cell fast mode (unchanged)
    # -----------------------------------------------------
    pu_is_r <- inherits(pu, "SpatRaster") || .pa_is_raster_path(pu)
    ft_is_r <- inherits(features, "SpatRaster") || .pa_is_raster_path(features)

    if (pu_is_r && ft_is_r) {
      b <- boundary
      if (is.null(b)) b <- "none"
      if (is.character(b) && !(b %in% c("auto", "none", "rook", "queen"))) {
        stop("Raster-cell mode: boundary must be one of 'auto', 'none', 'rook', 'queen' (or NULL).", call. = FALSE)
      }
      return(
        .pa_inputData_raster_cells_impl(
          pu = pu,
          features = features,
          cost = cost,
          boundary = if (is.null(b)) "none" else b,
          pu_status = pu_status,
          ...
        )
      )
    }

    # -----------------------------------------------------
    # Route 2: Vector-PU spatial mode (MODIFIED)
    # -----------------------------------------------------
    fun_cost <- .pa_fun_from_name(cost_aggregation)

    if (!(is.null(boundary) || is.character(boundary) || is.data.frame(boundary))) {
      stop("boundary must be NULL, 'auto', or a data.frame.", call. = FALSE)
    }

    # ---- read pu as raster->polygons OR as vector
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

    # base pu table (DO NOT reorder yet; keep row order aligned with pu_v)
    pu_df <- terra::as.data.frame(pu_v)
    pu_df$row_id <- seq_len(nrow(pu_df))     # row index in pu_v
    names(pu_df)[names(pu_df) == pu_id_col] <- "id"
    pu_df$id <- as.integer(round(pu_df$id))

    # ---- cost
    if (is.character(cost) && (cost %in% names(pu_df))) {
      pu_df$cost <- pu_df[[cost]]
    } else {
      cost_r <- .pa_read_rast(cost)
      if (is.null(cost_r)) {
        stop("You must provide 'cost' either as a column name in pu (vector) or as a raster/file path.", call. = FALSE)
      }

      # IMPORTANT: terra::extract returns ID = row index of pu_v (1..n), NOT pu_df$id
      cost_ex <- terra::extract(cost_r, pu_v, fun = fun_cost, na.rm = TRUE)
      idx <- match(pu_df$row_id, cost_ex[[1]])
      pu_df$cost <- cost_ex[[2]][idx]
    }

    # ---- locks (preserve)
    pu_df$locked_in  <- if (locked_in_col %in% names(pu_df))  as.logical(pu_df[[locked_in_col]])  else FALSE
    pu_df$locked_out <- if (locked_out_col %in% names(pu_df)) as.logical(pu_df[[locked_out_col]]) else FALSE

    # optional pu_status (0/2/3) only fills missing locks
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

    # ---- centroid coordinates (still aligned to pu_v row order)
    ctr <- terra::centroids(pu_v)
    xy  <- terra::crds(ctr)
    pu_coords <- data.frame(
      id = pu_df$id,
      x  = as.numeric(xy[, 1]),
      y  = as.numeric(xy[, 2]),
      stringsAsFactors = FALSE
    )


    # ---- features + dist_features (prioritizr-like: "sum" with exactextractr for polygons)
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

    # IMPORTANT: extract in the SAME row-order as pu_v/pu_df (before sorting by id)
    # prioritizr does "sum" (area-fraction weighted) for polygons
    feat_mat <- .pa_fast_extract(feat_r, pu_v, fun = "sum")  # [n_pu x n_features]

    # ---- NOW sort by pu id (and reorder feat_mat + coords consistently)
    ord <- order(pu_df$id)
    pu_df     <- pu_df[ord, , drop = FALSE]
    feat_mat  <- feat_mat[ord, , drop = FALSE]
    pu_coords <- pu_coords[ord, , drop = FALSE]

    pu_df_out <- pu_df[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]

    # NOTE: as.vector(feat_mat) is column-major => [feat1 all pu], [feat2 all pu], ...
    dist_features_df <- data.frame(
      pu      = rep(pu_df_out$id, times = terra::nlyr(feat_r)),
      feature = rep(features_df$id, each = nrow(pu_df_out)),
      amount  = as.vector(feat_mat),
      stringsAsFactors = FALSE
    )
    dist_features_df <- dist_features_df[
      is.finite(dist_features_df$amount) & dist_features_df$amount > 0,
      ,
      drop = FALSE
    ]

    # --- boundary relation to register (do NOT feed tabular impl)
    boundary_df <- NULL
    register_boundary_relation <- FALSE
    boundary_relation_name <- "boundary"

    # ---- boundary (relation only; does NOT affect the model unless an objective uses it)
    if (is.data.frame(boundary)) {

      boundary_df <- boundary
      register_boundary_relation <- TRUE

    } else if (is.character(boundary)) {

      if (identical(boundary, "auto")) {

        warning(
          "boundary='auto' will derive a *queen* adjacency relation using sf::st_touches(). ",
          "This does NOT activate any boundary penalty by itself; it only registers a spatial relation. ",
          "For explicit control, provide a boundary table (id1,id2,boundary) or use add_spatial_rook()/add_spatial_queen().",
          call. = FALSE, immediate. = TRUE
        )
        boundary <- "rock"

        if (!requireNamespace("sf", quietly = TRUE)) {
          stop(
            "Automatic boundary derivation requires the 'sf' package. Please install it or provide boundary as a data.frame.",
            call. = FALSE
          )
        }

        pu_sf <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)
        if (is.null(pu_sf)) {
          stop("Could not convert planning units to sf for boundary derivation. Provide boundary as a data.frame.", call. = FALSE)
        }

        nb <- sf::st_touches(pu_sf)

        # ids in pu_v row order (pre-sorting)
        pu_df0 <- terra::as.data.frame(pu_v)
        names(pu_df0)[names(pu_df0) == pu_id_col] <- "id"
        id_vec0 <- as.integer(round(pu_df0$id))

        id1 <- integer(0)
        id2 <- integer(0)

        for (i in seq_along(nb)) {
          j <- nb[[i]]
          if (!length(j)) next
          keep <- j > i
          if (any(keep)) {
            id1 <- c(id1, rep(id_vec0[i], sum(keep)))
            id2 <- c(id2, id_vec0[j[keep]])
          }
        }

        boundary_df <- data.frame(
          id1 = id1,
          id2 = id2,
          boundary = 1,
          stringsAsFactors = FALSE
        )

        register_boundary_relation <- TRUE
      }

      if (identical(boundary, "none")) {
        # explicitly do nothing
        boundary_df <- NULL
        register_boundary_relation <- FALSE
      }
    }

    # ---- build via stable tabular impl
    x <- .pa_inputData_tabular_impl(
      pu = pu_df_out,
      features = features_df,
      dist_features = dist_features_df,
      boundary = NULL,
      ...
    )

    # --- register spatial relation (if requested/provided)
    if (isTRUE(register_boundary_relation) && !is.null(boundary_df) && nrow(boundary_df) > 0) {

      # ensure infrastructure exists
      if (exists("add_spatial_boundary", mode = "function")) {

        # add_spatial_boundary expects id1/id2/boundary OR pu1/pu2/weight
        x <- add_spatial_boundary(
          x,
          boundary = boundary_df,
          name = boundary_relation_name,
          weight_col = if ("boundary" %in% names(boundary_df)) "boundary" else NULL
        )

      } else {
        # fallback: store raw table; build_model will error if objective needs internal_* fields
        x$data$spatial_relations <- x$data$spatial_relations %||% list()
        x$data$spatial_relations[[boundary_relation_name]] <- boundary_df
        warning(
          "add_spatial_boundary() not available at inputData() time; stored boundary table raw in x$data$spatial_relations[['",
          boundary_relation_name, "']].",
          call. = FALSE, immediate. = TRUE
        )
      }
    }


    # store coords (sorted by id)
    x$data$pu_coords <- pu_coords

    if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
      x$data$spatial_relations <- list()
    }

    if (!is.null(pu_r)) x$data$pu_raster_id <- pu_r

    # store pu_sf (sorted to match x$data$pu$id)
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
    boundary = NULL,
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
      boundary = boundary,
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
