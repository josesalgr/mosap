#' @include internal.R
#'
#' Spatial relations infrastructure
#'
#' This file defines a single internal representation for spatial relations
#' between planning units, plus multiple constructors:
#' - boundary table (Marxan-style),
#' - rook/queen adjacency from sf geometry,
#' - kNN graph from coordinates,
#' - distance-threshold graph from coordinates.
#'
#' Internally, relations are stored in:
#'   x$data$spatial_relations[[name]]
#'
#' Each relation is a data.frame with at least:
#'   - internal_pu1 (int, 1..n_pu)
#'   - internal_pu2 (int, 1..n_pu)
#'   - weight       (numeric >= 0)
#'
#' Additional columns can exist (e.g., pu1, pu2, distance, source, etc.).
NULL

# ---- internal helpers --------------------------------------------------------

.pa_has_sf <- function() requireNamespace("sf", quietly = TRUE)

.pa_ensure_pu_index <- function(x) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$pu) || !inherits(x$data$pu, "data.frame")) {
    stop("x$data$pu is missing. Create the problem with inputData()/inputDataSpatial().", call. = FALSE)
  }
  if (is.null(x$data$pu$internal_id)) x$data$pu$internal_id <- seq_len(nrow(x$data$pu))
  if (is.null(x$data$pu$id)) {
    stop("x$data$pu must contain column 'id' (planning unit id).", call. = FALSE)
  }
  x$data$pu$id <- as.integer(x$data$pu$id)
  x$data$pu$internal_id <- as.integer(x$data$pu$internal_id)
  if (anyNA(x$data$pu$id) || anyNA(x$data$pu$internal_id)) {
    stop("x$data$pu$id/internal_id contain NA after coercion.", call. = FALSE)
  }
  if (anyDuplicated(x$data$pu$internal_id) != 0) stop("x$data$pu$internal_id must be unique.", call. = FALSE)
  if (anyDuplicated(x$data$pu$id) != 0) stop("x$data$pu$id must be unique.", call. = FALSE)
  if (is.null(x$data$index) || !is.list(x$data$index)) x$data$index <- list()
  x$data$index$pu <- stats::setNames(x$data$pu$internal_id, as.character(x$data$pu$id))
  x
}

.pa_store_relation <- function(x, rel, name) {
  stopifnot(inherits(x, "Data"))
  if (is.null(x$data$spatial_relations) || !is.list(x$data$spatial_relations)) {
    x$data$spatial_relations <- list()
  }
  x$data$spatial_relations[[name]] <- rel
  x
}


.pa_coords_from_input <- function(x, coords = NULL) {
  stopifnot(inherits(x, "Data"))

  if (!is.null(coords)) {
    if (inherits(coords, "data.frame")) {
      if (!all(c("id", "x", "y") %in% names(coords))) {
        stop("coords data.frame must contain columns id, x, y.", call. = FALSE)
      }
      out <- coords[, c("id", "x", "y")]
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
    if (is.matrix(coords)) {
      if (ncol(coords) < 2) stop("coords matrix must have at least 2 columns (x,y).", call. = FALSE)
      out <- data.frame(id = x$data$pu$id, x = coords[, 1], y = coords[, 2])
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
    stop("Unsupported coords type. Use data.frame(id,x,y) or a matrix with 2 columns.", call. = FALSE)
  }

  # NEW fallback: x$data$pu_coords
  if (!is.null(x$data$pu_coords) && inherits(x$data$pu_coords, "data.frame")) {
    pc <- x$data$pu_coords
    if (all(c("id", "x", "y") %in% names(pc))) {
      out <- pc[, c("id", "x", "y")]
      out$id <- as.integer(out$id)
      out$x  <- as.numeric(out$x)
      out$y  <- as.numeric(out$y)
      return(out)
    }
  }

  # fallback: try x$data$pu columns
  if (all(c("x", "y") %in% names(x$data$pu))) {
    out <- data.frame(id = x$data$pu$id, x = x$data$pu$x, y = x$data$pu$y)
    out$id <- as.integer(out$id)
    out$x  <- as.numeric(out$x)
    out$y  <- as.numeric(out$y)
    return(out)
  }

  stop(
    "No coordinates available. Provide coords=data.frame(id,x,y) or store x$data$pu_coords (id,x,y), or add x$data$pu$x/y.",
    call. = FALSE
  )
}


# ---- public API --------------------------------------------------------------

#' Add spatial relations (core)
#'
#' @description
#' Store a spatial relation graph between planning units using a single internal representation.
#' Most users should call one of the convenience wrappers:
#' [add_spatial_boundary()], [add_spatial_rook()], [add_spatial_queen()],
#' [add_spatial_knn()], or [add_spatial_distance()].
#'
#' This function is useful when you already computed the relation externally and want to
#' register it inside the problem object.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param relations A `data.frame` with columns `pu1`, `pu2`, and `weight`, or already with
#' `internal_pu1`, `internal_pu2`, `weight`.
#' @param name Character. Name/key under which the relation will be stored.
#' @param directed Logical. If `FALSE` (default), edges are treated as undirected and duplicates
#' are collapsed.
#' @param allow_self Logical. Allow self-edges (almost always `FALSE`).
#'
#' @return Updated [data-class] object with `x$data$spatial_relations[[name]]`.
#' @export
add_spatial_relations <- function(x,
                                  relations,
                                  name = "default",
                                  directed = FALSE,
                                  allow_self = FALSE,
                                  duplicate_agg = c("sum", "max", "min", "mean")) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  n_pu <- nrow(x$data$pu)

  duplicate_agg <- match.arg(duplicate_agg)

  stopifnot(inherits(relations, "data.frame"), nrow(relations) > 0)

  rel <- relations

  if (all(c("pu1", "pu2", "weight") %in% names(rel)) &&
      !all(c("internal_pu1", "internal_pu2") %in% names(rel))) {

    rel$pu1 <- as.integer(rel$pu1)
    rel$pu2 <- as.integer(rel$pu2)
    rel$weight <- as.numeric(rel$weight)

    idx <- x$data$index$pu
    rel$internal_pu1 <- unname(idx[as.character(rel$pu1)])
    rel$internal_pu2 <- unname(idx[as.character(rel$pu2)])

    if (anyNA(rel$internal_pu1) || anyNA(rel$internal_pu2)) {
      stop("Some pu1/pu2 ids were not found in x$data$pu$id.", call. = FALSE)
    }
  }

  rel <- rel[, intersect(names(rel), c("internal_pu1","internal_pu2","weight","pu1","pu2","distance","source")),
             drop = FALSE]

  if (!directed) {
    rel <- .pa_validate_relation(rel, n_pu = n_pu, allow_self = allow_self, dup_agg = duplicate_agg)
  } else {
    # (tu bloque directed tal cual)
    rel$internal_pu1 <- as.integer(rel$internal_pu1)
    rel$internal_pu2 <- as.integer(rel$internal_pu2)
    rel$weight <- as.numeric(rel$weight)
    if (any(rel$internal_pu1 < 1L | rel$internal_pu1 > n_pu)) stop("internal_pu1 out of range.", call. = FALSE)
    if (any(rel$internal_pu2 < 1L | rel$internal_pu2 > n_pu)) stop("internal_pu2 out of range.", call. = FALSE)
    if (!allow_self && any(rel$internal_pu1 == rel$internal_pu2)) stop("Self-edges are not allowed.", call. = FALSE)
    if (any(!is.finite(rel$weight)) || any(rel$weight < 0)) stop("weight must be finite and >= 0.", call. = FALSE)
  }

  rel$relation_name <- name
  x <- .pa_store_relation(x, rel, name)
  x
}


#' Add spatial relations from a boundary table (Marxan-style)
#'
#' @description
#' Register a boundary-length relation between planning units using a table with columns
#' `id1`, `id2`, and `boundary` (or `pu1`, `pu2`, `weight`).
#'
#' This does not require `sf`.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param boundary Optional. If `NULL`, reuses an existing relation `x$data$spatial_relations[[name]]` if present. Otherwise, a `data.frame`
#' with `id1`, `id2`, `boundary` (or `pu1`, `pu2`, `weight`).
#' @param name Name/key under which the relation will be stored.
#' @param weight_col Column in `boundary` to use as weight. Defaults to `"boundary"` when present.
#'
#' @return Updated [data-class] object.
#' @export
add_spatial_boundary <- function(x,
                                 boundary = NULL,
                                 name = "boundary",
                                 weight_col = NULL) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)

  # If boundary not provided, try to reuse an already-registered relation
  if (is.null(boundary)) {
    if (!is.null(x$data$spatial_relations) &&
        is.list(x$data$spatial_relations) &&
        !is.null(x$data$spatial_relations[[name]])) {
      return(x)
    }
    stop(
      "boundary is NULL and no spatial relation named '", name, "' is registered. ",
      "Provide a boundary table or use add_spatial_rook()/queen() to build one.",
      call. = FALSE
    )
  }

  stopifnot(inherits(boundary, "data.frame"), nrow(boundary) > 0)
  b <- boundary

  # Normalize column names
  if (all(c("id1", "id2") %in% names(b)) && !all(c("pu1", "pu2") %in% names(b))) {
    b$pu1 <- b$id1
    b$pu2 <- b$id2
  }

  if (is.null(weight_col)) {
    if ("boundary" %in% names(b)) weight_col <- "boundary"
    else if ("weight" %in% names(b)) weight_col <- "weight"
    else stop("Could not find a weight column. Provide weight_col.", call. = FALSE)
  }
  if (!(weight_col %in% names(b))) stop("weight_col not found in boundary table.", call. = FALSE)

  rel <- data.frame(
    pu1 = as.integer(b$pu1),
    pu2 = as.integer(b$pu2),
    weight = as.numeric(b[[weight_col]]),
    source = "boundary_table",
    stringsAsFactors = FALSE
  )

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE, duplicate_agg = "sum")
}


#' Add rook adjacency from sf polygons
#'
#' @description
#' Build a rook adjacency relation (shared edge) from planning unit polygons.
#' Requires `sf`.
#'
#' @param x A [data-class] object created with [inputDataSpatial()].
#' @param pu_sf Optional sf object with planning units geometry and an `id` column. If `NULL`,
#' tries `x$data$pu_sf`.
#' @param name Name/key under which the relation will be stored.
#' @param weight Weight assigned to each adjacency edge (default 1).
#'
#' @return Updated [data-class] object.
#' @export
add_spatial_rook <- function(x,
                             pu_sf = NULL,
                             name = "default",
                             weight = 1) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  if (!.pa_has_sf()) stop("add_spatial_rook requires the 'sf' package.", call. = FALSE)

  if (is.null(pu_sf)) {
    pu_sf <- x$data$pu_sf
    if (is.null(pu_sf)) stop("pu_sf is NULL and x$data$pu_sf is missing.", call. = FALSE)
  }
  if (!inherits(pu_sf, "sf")) stop("pu_sf must be an sf object.", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop("pu_sf must contain an 'id' column.", call. = FALSE)

  pu_sf$id <- as.integer(pu_sf$id)
  idx <- x$data$index$pu

  # Rook = intersection dimension 1 (shared line). Use st_relate + DE-9IM.
  # This is heavier than st_touches but correct for rook vs queen.
  mat <- sf::st_relate(pu_sf, pu_sf, pattern = "F***1****", sparse = TRUE)

  edges <- vector("list", length(mat))
  for (i in seq_along(mat)) {
    js <- mat[[i]]
    js <- js[js != i]
    if (length(js) == 0) next
    edges[[i]] <- data.frame(
      pu1 = pu_sf$id[i],
      pu2 = pu_sf$id[js],
      weight = as.numeric(weight),
      source = "rook_sf",
      stringsAsFactors = FALSE
    )
  }
  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No rook adjacencies found.", call. = FALSE)

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE, duplicate_agg = "max")
}

#' Add queen adjacency from sf polygons
#'
#' @description
#' Build a queen adjacency relation (shared edge or vertex) from planning unit polygons.
#' Requires `sf`.
#'
#' @param x A [data-class] object created with [inputDataSpatial()].
#' @param pu_sf Optional sf object with planning units geometry and an `id` column. If `NULL`,
#' tries `x$data$pu_sf`.
#' @param name Name/key under which the relation will be stored.
#' @param weight Weight assigned to each adjacency edge (default 1).
#'
#' @return Updated [data-class] object.
#' @export
add_spatial_queen <- function(x,
                              pu_sf = NULL,
                              name = "default",
                              weight = 1) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  if (!.pa_has_sf()) stop("add_spatial_queen requires the 'sf' package.", call. = FALSE)

  if (is.null(pu_sf)) {
    pu_sf <- x$data$pu_sf
    if (is.null(pu_sf)) stop("pu_sf is NULL and x$data$pu_sf is missing.", call. = FALSE)
  }
  if (!inherits(pu_sf, "sf")) stop("pu_sf must be an sf object.", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop("pu_sf must contain an 'id' column.", call. = FALSE)

  pu_sf$id <- as.integer(pu_sf$id)

  # Queen adjacency: st_touches
  mat <- sf::st_touches(pu_sf, pu_sf, sparse = TRUE)

  edges <- vector("list", length(mat))
  for (i in seq_along(mat)) {
    js <- mat[[i]]
    js <- js[js != i]
    if (length(js) == 0) next
    edges[[i]] <- data.frame(
      pu1 = pu_sf$id[i],
      pu2 = pu_sf$id[js],
      weight = as.numeric(weight),
      source = "queen_sf",
      stringsAsFactors = FALSE
    )
  }
  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No queen adjacencies found.", call. = FALSE)

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE, duplicate_agg = "max")
}

#' Add kNN spatial relations from coordinates
#'
#' @description
#' Build a k-nearest-neighbours graph between planning units using coordinates.
#' Does not require `sf`. For speed on large instances, it uses `RANN` if available.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param coords Optional coordinates: `data.frame(id,x,y)` or a matrix with 2 columns (x,y).
#' If `NULL`, tries `x$data$pu$x` and `x$data$pu$y`.
#' @param k Integer. Number of neighbours per PU.
#' @param name Name/key under which the relation will be stored.
#' @param weight_fn How to convert distance to edge weight: `"constant"`, `"inverse"`, or `"inverse_sq"`.
#' @param eps Small constant to avoid division by zero (default 1e-9).
#'
#' @return Updated [data-class] object.
#' @export
add_spatial_knn <- function(x,
                            coords = NULL,
                            k = 8,
                            name = "default",
                            weight_fn = c("constant", "inverse", "inverse_sq"),
                            eps = 1e-9) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  weight_fn <- match.arg(weight_fn)
  k <- as.integer(k)
  if (k < 1) stop("k must be >= 1.", call. = FALSE)

  C <- .pa_coords_from_input(x, coords = coords)
  C <- C[match(x$data$pu$id, C$id), , drop = FALSE]
  if (anyNA(C$id)) stop("coords does not cover all pu ids.", call. = FALSE)

  X <- as.matrix(C[, c("x", "y")])
  n <- nrow(X)
  if (k >= n) stop("k must be < number of PUs.", call. = FALSE)

  # Find neighbours
  if (requireNamespace("RANN", quietly = TRUE)) {
    nn <- RANN::nn2(X, k = k + 1) # includes self
    idx_mat <- nn$nn.idx[, -1, drop = FALSE]
    dist_mat <- nn$nn.dists[, -1, drop = FALSE]
  } else {
    # fallback: full distance (OK for small n)
    D <- as.matrix(stats::dist(X))
    diag(D) <- Inf
    idx_mat <- t(apply(D, 1, function(d) order(d)[seq_len(k)]))
    dist_mat <- matrix(D[cbind(rep(seq_len(n), each = k), as.vector(t(idx_mat)))], nrow = n, byrow = TRUE)
  }

  # Convert to edges
  pu1 <- rep(C$id, each = k)
  pu2 <- C$id[as.vector(t(idx_mat))]
  dist <- as.vector(t(dist_mat))

  w <- switch(
    weight_fn,
    constant = rep(1, length(dist)),
    inverse = 1 / pmax(dist, eps),
    inverse_sq = 1 / pmax(dist, eps)^2
  )

  rel <- data.frame(
    pu1 = as.integer(pu1),
    pu2 = as.integer(pu2),
    weight = as.numeric(w),
    distance = as.numeric(dist),
    source = paste0("knn_", weight_fn),
    stringsAsFactors = FALSE
  )

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE, duplicate_agg = "max")
}

#' Add distance-threshold spatial relations from coordinates
#'
#' @description
#' Build edges between PUs whose Euclidean distance is <= `dmax`. Does not require `sf`.
#' For very large instances, consider kNN instead.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param coords Optional coordinates: `data.frame(id,x,y)` or matrix (x,y). If `NULL`, uses `x$data$pu$x/y`.
#' @param dmax Numeric. Maximum distance for an edge.
#' @param name Name/key under which the relation will be stored.
#' @param weight_fn `"constant"`, `"inverse"`, or `"inverse_sq"`.
#' @param eps Small constant to avoid division by zero.
#'
#' @return Updated [data-class] object.
#' @export
add_spatial_distance <- function(x,
                                 coords = NULL,
                                 dmax,
                                 name = "default",
                                 weight_fn = c("constant", "inverse", "inverse_sq"),
                                 eps = 1e-9) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  weight_fn <- match.arg(weight_fn)
  dmax <- as.numeric(dmax)
  if (!is.finite(dmax) || dmax <= 0) stop("dmax must be a positive finite number.", call. = FALSE)

  C <- .pa_coords_from_input(x, coords = coords)
  C <- C[match(x$data$pu$id, C$id), , drop = FALSE]
  if (anyNA(C$id)) stop("coords does not cover all pu ids.", call. = FALSE)

  X <- as.matrix(C[, c("x", "y")])
  n <- nrow(X)

  # O(n^2) fallback: OK for small/moderate n
  D <- as.matrix(stats::dist(X))
  diag(D) <- Inf
  which_edges <- which(D <= dmax, arr.ind = TRUE)

  if (nrow(which_edges) == 0) stop("No edges found under dmax. Try a larger dmax.", call. = FALSE)

  pu1 <- C$id[which_edges[, 1]]
  pu2 <- C$id[which_edges[, 2]]
  dist <- D[which_edges]

  w <- switch(
    weight_fn,
    constant = rep(1, length(dist)),
    inverse = 1 / pmax(dist, eps),
    inverse_sq = 1 / pmax(dist, eps)^2
  )

  rel <- data.frame(
    pu1 = as.integer(pu1),
    pu2 = as.integer(pu2),
    weight = as.numeric(w),
    distance = as.numeric(dist),
    source = paste0("distance_", weight_fn),
    stringsAsFactors = FALSE
  )

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE, duplicate_agg = "max")
}
