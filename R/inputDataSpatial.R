#' @include internal.R
NULL

#' @title Create a planning problem from spatial inputs
#'
#' @description
#' Convert common spatial inputs (rasters and vector layers such as Shapefiles or
#' GeoPackages) into the tabular inputs required by [inputData()].
#'
#' This function only builds the *base* planning problem: planning units (pu),
#' features, dist_features, and optionally boundary. Actions and action-benefits
#' are added later via [add_actions()] and [add_action_benefit()].
#'
#' Spatial backends are **optional** (the package does not import them by default).
#' To use this function you need:
#' \itemize{
#' \item \code{terra} for reading rasters/vectors and extracting raster values.
#' \item \code{sf} only if you ask to automatically derive the \code{boundary} table.
#' }
#'
#' @param pu Planning units as either:
#' \itemize{
#' \item a file path to a raster (e.g. GeoTIFF) where cell values are PU ids,
#' \item a \code{terra::SpatRaster} with PU ids,
#' \item a file path to a vector layer (e.g. \code{.shp}, \code{.gpkg}),
#' \item a \code{terra::SpatVector} or \code{sf} object with a column of PU ids.
#' }
#'
#' @param features A raster (or file path) with one layer per feature. Values are
#' interpreted as *amount* per cell. Layer names are used as feature names.
#'
#' @param cost Either:
#' \itemize{
#' \item a column name in \code{pu} (when \code{pu} is a vector), or
#' \item a raster (or file path) providing cost per cell (when \code{pu} is a raster
#' or when \code{pu} lacks a cost column).
#' }
#'
#' @param pu_id_col When \code{pu} is a vector layer, name of the column containing
#' planning unit ids.
#'
#' @param locked_in_col,locked_out_col Column names in \code{pu} (vector) with
#' logical lock flags. If missing, defaults to FALSE.
#'
#' @param pu_status Optional. Either a column name (when \code{pu} is a vector) or a
#' raster (or file path) with values {0,2,3} meaning available, locked-in, locked-out.
#' This is converted to \code{locked_in}/\code{locked_out}. If both column locks and
#' \code{pu_status} are provided, column locks take precedence.
#'
#' @param cost_aggregation Aggregation used to compute PU-level cost from a cost raster
#' (default: mean).
#'
#' @param boundary Either \code{NULL}, a \code{data.frame} with columns \code{id1}, \code{id2},
#' \code{boundary}, or the string \code{"auto"} to derive adjacency-based boundaries from
#' polygon planning units (boundary weight is set to 1 for touching PU pairs).
#'
#' @param ... Passed to [inputData()].
#'
#' @return An object created by [inputData()].
#'
#' @keywords internal
inputDataSpatial <- function(
    pu,
    features,
    cost,
    pu_id_col = "id",
    locked_in_col = "locked_in",
    locked_out_col = "locked_out",
    pu_status = NULL,
    cost_aggregation = c("mean", "sum"),
    boundary = "auto",
    ...
) {

  cost_aggregation <- match.arg(cost_aggregation)

  # dependencies
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("inputDataSpatial() requires the 'terra' package. Please install it.", call. = FALSE)
  }

  if (!(is.null(boundary) || is.character(boundary) || is.data.frame(boundary))) {
    stop("boundary must be NULL, 'auto', or a data.frame.", call. = FALSE)
  }


  # helpers
  .read_rast <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "SpatRaster")) return(x)
    if (is.character(x)) return(terra::rast(x))
    stop("Unsupported raster input.", call. = FALSE)
  }

  .read_vect <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "SpatVector")) return(x)
    if (inherits(x, "sf")) return(terra::vect(x))
    if (is.character(x)) return(terra::vect(x))
    stop("Unsupported vector input.", call. = FALSE)
  }

  .is_raster_path <- function(x) {
    is.character(x) && grepl("\\.(tif|tiff|grd|asc|nc)$", tolower(x))
  }

  .fun_from_name <- function(name) {
    switch(
      name,
      mean = function(v) mean(v, na.rm = TRUE),
      sum  = function(v) sum(v, na.rm = TRUE)
    )
  }

  fun_cost <- .fun_from_name(cost_aggregation)

  # ---- read pu
  pu_r <- NULL
  pu_v <- NULL

  if (inherits(pu, "SpatRaster") || .is_raster_path(pu)) {
    pu_r <- .read_rast(pu)
    pu_r <- terra::round(pu_r)
    pu_v <- terra::as.polygons(pu_r, dissolve = TRUE, values = TRUE, na.rm = TRUE)
    names(pu_v) <- pu_id_col
  } else {
    pu_v <- .read_vect(pu)

    if (!(pu_id_col %in% names(pu_v))) {
      # si el usuario no cambió pu_id_col (sigue siendo "id"), autogeneramos
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

  # base pu table
  pu_df <- terra::as.data.frame(pu_v)
  names(pu_df)[names(pu_df) == pu_id_col] <- "id"

  # ensure stable integer ids and sort by id
  pu_df$id <- as.integer(round(pu_df$id))
  pu_df <- pu_df[order(pu_df$id), , drop = FALSE]

  if (!("id" %in% names(pu_df))) stop("Could not find planning unit ids.", call. = FALSE)

  # ---- cost
  if (is.character(cost) && (cost %in% names(pu_df))) {
    pu_df$cost <- pu_df[[cost]]
  } else {
    cost_r <- .read_rast(cost)
    if (is.null(cost_r)) {
      stop("You must provide 'cost' either as a column name in pu (vector) or as a raster/file path.", call. = FALSE)
    }
    cost_ex <- terra::extract(cost_r, pu_v, fun = fun_cost, na.rm = TRUE)
    cost_ex <- cost_ex[match(pu_df$id, cost_ex[[1]]), , drop = FALSE]
    pu_df$cost <- cost_ex[[2]]
  }

  # ---- locks (prefer columns if present)
  pu_df$locked_in  <- FALSE
  pu_df$locked_out <- FALSE

  if (locked_in_col %in% names(pu_df)) {
    pu_df$locked_in <- as.logical(pu_df[[locked_in_col]])
  }
  if (locked_out_col %in% names(pu_df)) {
    pu_df$locked_out <- as.logical(pu_df[[locked_out_col]])
  }

  # optional pu_status (0/2/3) only fills missing locks
  if (!is.null(pu_status) && !(locked_in_col %in% names(pu_df)) && !(locked_out_col %in% names(pu_df))) {
    if (is.character(pu_status) && (pu_status %in% names(pu_df))) {
      st <- pu_df[[pu_status]]
    } else {
      st_r <- .read_rast(pu_status)
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

  # keep only the columns your core expects (plus allow extra cols if you want)
  pu_df <- pu_df[, c("id", "cost", "locked_in", "locked_out"), drop = FALSE]

  # ---- features + dist_features
  feat_r <- .read_rast(features)
  if (is.null(feat_r)) stop("features must be a raster (SpatRaster) or a raster file path.", call. = FALSE)

  feat_names <- names(feat_r)
  if (is.null(feat_names) || any(feat_names == "")) {
    feat_names <- paste0("feature.", seq_len(terra::nlyr(feat_r)))
  }

  features_df <- data.frame(
    id = seq_len(terra::nlyr(feat_r)),
    name = feat_names,
    stringsAsFactors = FALSE
  )

  feat_ex <- terra::extract(feat_r, pu_v, fun = sum, na.rm = TRUE)

  # reorder extract output to match pu_df$id
  feat_ex <- feat_ex[match(pu_df$id, feat_ex[[1]]), , drop = FALSE]

  feat_mat <- as.matrix(feat_ex[, -1, drop = FALSE])

  dist_features <- data.frame(
    pu      = rep(pu_df$id, times = terra::nlyr(feat_r)),
    feature = rep(features_df$id, each = nrow(pu_df)),
    amount  = as.vector(t(feat_mat)),
    stringsAsFactors = FALSE
  )

  # conservative filtering: remove NAs and zeros
  dist_features <- dist_features[!is.na(dist_features$amount) & dist_features$amount > 0, , drop = FALSE]

  # ---- boundary (optional)
  boundary_df <- NULL
  if (is.data.frame(boundary)) {
    boundary_df <- boundary
  } else if (is.character(boundary) && identical(boundary, "auto")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Automatic boundary derivation requires the 'sf' package. Please install it or provide boundary as a data.frame.", call. = FALSE)
    }

    pu_sf <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)
    if (is.null(pu_sf)) {
      stop("Could not convert planning units to sf for boundary derivation. Provide boundary as a data.frame.", call. = FALSE)
    }

    nb <- sf::st_touches(pu_sf)
    id_vec <- pu_df$id

    id1 <- integer(0)
    id2 <- integer(0)

    for (i in seq_along(nb)) {
      if (length(nb[[i]]) == 0) next
      j <- nb[[i]]
      keep <- j > i
      if (any(keep)) {
        id1 <- c(id1, rep(id_vec[i], sum(keep)))
        id2 <- c(id2, id_vec[j[keep]])
      }
    }

    boundary_df <- data.frame(
      id1 = id1,
      id2 = id2,
      boundary = 1,
      stringsAsFactors = FALSE
    )
  }

  # ---- (optional) store planning unit geometry for downstream spatial operations
  # This enables add_actions(where = list(action = sf_layer)) without requiring the user
  # to pass pu geometry again.
  # ---- call base constructor (problem())
  x <- inputData(
    pu = pu_df,
    features = features_df,
    dist_features = dist_features,
    boundary = boundary_df,
    ...
  )

  if (!is.null(pu_r)) {
    x$data$pu_raster_id <- pu_r  # el raster con IDs (ya redondeado)
  }

  # ---- (optional) store planning unit geometry for downstream spatial operations
  if (requireNamespace("sf", quietly = TRUE)) {

    pu_sf_store <- tryCatch(sf::st_as_sf(pu_v), error = function(e) NULL)

    if (!is.null(pu_sf_store)) {

      # rename id column to 'id' if needed
      if (pu_id_col %in% names(pu_sf_store)) {
        names(pu_sf_store)[names(pu_sf_store) == pu_id_col] <- "id"
      }

      # if still no id, create one (should be rare now)
      if (!("id" %in% names(pu_sf_store))) {
        pu_sf_store$id <- seq_len(nrow(pu_sf_store))
      }

      # keep only id + geometry
      pu_sf_store <- pu_sf_store[, "id", drop = FALSE]

      # reorder to match x$data$pu$id
      ord <- match(x$data$pu$id, pu_sf_store$id)

      if (any(is.na(ord))) {
        warning(
          "Could not safely match PU geometry to PU ids; 'pu_sf' will not be stored in the problem object.",
          call. = FALSE, immediate. = TRUE
        )
      } else {
        x$data$pu_sf <- pu_sf_store[ord, , drop = FALSE]
      }
    }
  }

  x
}
