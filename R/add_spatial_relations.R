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


.pa_get_pu_sf_aligned <- function(x, pu_sf = NULL, arg_name = "pu_sf") {
  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)

  if (is.null(pu_sf)) pu_sf <- x$data$pu_sf

  if (is.null(pu_sf)) {
    stop(
      arg_name, " is NULL and x$data$pu_sf is missing.\n",
      "Provide ", arg_name, " (sf polygons with an 'id' column) or make sure inputData() stored x$data$pu_sf.",
      call. = FALSE
    )
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("This function requires the 'sf' package.", call. = FALSE)
  }

  if (!inherits(pu_sf, "sf")) stop(arg_name, " must be an sf object.", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop(arg_name, " must contain an 'id' column.", call. = FALSE)

  pu_sf$id <- as.integer(pu_sf$id)

  # align to x$data$pu$id (critical)
  ord <- match(x$data$pu$id, pu_sf$id)
  if (anyNA(ord)) {
    missing_ids <- x$data$pu$id[is.na(ord)]
    stop(
      arg_name, "$id does not match x$data$pu$id (some ids missing). Missing: ",
      paste(utils::head(missing_ids, 20), collapse = ", "),
      if (length(missing_ids) > 20) " ..." else "",
      call. = FALSE
    )
  }

  pu_sf[ord, , drop = FALSE]
}

.pa_rbind_fill <- function(a, b) {
  stopifnot(inherits(a, "data.frame"), inherits(b, "data.frame"))
  cols <- union(names(a), names(b))
  for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
  for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
  a <- a[, cols, drop = FALSE]
  b <- b[, cols, drop = FALSE]
  rbind(a, b)
}

.pa_swap_edges <- function(rel) {
  out <- rel
  out$internal_pu1 <- rel$internal_pu2
  out$internal_pu2 <- rel$internal_pu1
  if ("pu1" %in% names(rel) && "pu2" %in% names(rel)) {
    out$pu1 <- rel$pu2
    out$pu2 <- rel$pu1
  }
  out
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
                                  duplicate_agg = c("sum", "max", "min", "mean"),
                                  symmetric = FALSE) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)
  n_pu <- nrow(x$data$pu)

  duplicate_agg <- match.arg(duplicate_agg)

  stopifnot(inherits(relations, "data.frame"), nrow(relations) > 0)
  rel <- relations

  # map pu ids -> internal ids if needed
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

  keep_cols <- c("internal_pu1","internal_pu2","weight","pu1","pu2","distance","source")
  rel <- rel[, intersect(keep_cols, names(rel)), drop = FALSE]

  .pa_swap_edges_noself <- function(df) {
    df2 <- df
    has_pu <- all(c("pu1","pu2") %in% names(df2))
    tmp <- df2$internal_pu1
    df2$internal_pu1 <- df2$internal_pu2
    df2$internal_pu2 <- tmp
    if (has_pu) {
      tmp <- df2$pu1
      df2$pu1 <- df2$pu2
      df2$pu2 <- tmp
    }
    df2
  }

  .pa_rbind_samecols <- function(a, b) {
    cols <- union(names(a), names(b))
    a2 <- a; b2 <- b
    for (cc in setdiff(cols, names(a2))) a2[[cc]] <- NA
    for (cc in setdiff(cols, names(b2))) b2[[cc]] <- NA
    a2 <- a2[, cols, drop = FALSE]
    b2 <- b2[, cols, drop = FALSE]
    base::rbind(a2, b2)
  }

  if (!directed) {

    self_edges <- NULL
    if (isTRUE(allow_self)) {
      self <- rel$internal_pu1 == rel$internal_pu2
      if (any(self, na.rm = TRUE)) {
        self_edges <- rel[self, , drop = FALSE]
        rel <- rel[!self, , drop = FALSE]
      }
    }

    rel_u <- .pa_validate_relation(rel, n_pu = n_pu, allow_self = FALSE, dup_agg = duplicate_agg)

    if (!is.null(self_edges)) {
      key <- as.character(self_edges$internal_pu1)
      agg_fun <- switch(duplicate_agg, sum=sum, max=max, min=min, mean=mean)
      wself <- tapply(self_edges$weight, key, agg_fun)

      self_fix <- data.frame(
        internal_pu1 = as.integer(names(wself)),
        internal_pu2 = as.integer(names(wself)),
        weight = as.numeric(wself),
        stringsAsFactors = FALSE
      )

      # si hay columnas extra, recupera la primera ocurrencia
      extra_cols <- intersect(names(self_edges), c("pu1","pu2","distance","source"))
      if (length(extra_cols) > 0) {
        rep_idx <- match(names(wself), as.character(self_edges$internal_pu1))
        extras <- self_edges[rep_idx, extra_cols, drop = FALSE]
        self_fix <- cbind(self_fix, extras)
      }

      rel_u <- .pa_rbind_samecols(rel_u, self_fix)
    }

    if (isTRUE(symmetric)) {
      off <- rel_u$internal_pu1 != rel_u$internal_pu2
      rel_sw <- .pa_swap_edges_noself(rel_u[off, , drop = FALSE])
      rel <- .pa_rbind_samecols(rel_u, rel_sw)
      directed <- TRUE
    } else {
      rel <- rel_u
    }

  } else {
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

.sum_incident_by_pu <- function(i, j, w, n_pu) {
  i <- as.integer(i); j <- as.integer(j); w <- as.numeric(w)

  s1 <- rowsum(w, i, reorder = FALSE)
  s2 <- rowsum(w, j, reorder = FALSE)

  out <- numeric(n_pu)
  out[as.integer(rownames(s1))] <- out[as.integer(rownames(s1))] + s1[, 1]
  out[as.integer(rownames(s2))] <- out[as.integer(rownames(s2))] + s2[, 1]
  out
}


#' Add spatial boundary-length relations from sf polygons OR a boundary table
#'
#' @description
#' Registers a boundary-length relation between planning units.
#' - If `boundary` is provided: uses it (expects id1/id2/boundary or pu1/pu2/weight).
#' - If `boundary` is NULL: derives boundary lengths from `pu_sf` (or x$data$pu_sf).
#'
#' This function defines *boundary* (shared-edge length), not queen touches.
#'
#' @param x Data object.
#' @param boundary Optional data.frame with columns (id1,id2,boundary) or (pu1,pu2,weight).
#' @param pu_sf Optional sf with PU polygons and an `id` column. If NULL, uses x$data$pu_sf.
#' @param name Name to store relation under (default "boundary").
#' @param weight_col For boundary tables, which column to use as weight. Defaults to "boundary" or "weight".
#' @param weight_multiplier Multiplier applied to computed/loaded weights.
#' @param progress Show basic progress for large N (logical).
#'
#' @export
add_spatial_boundary <- function(x,
                                 boundary = NULL,
                                 pu_sf = NULL,
                                 name = "boundary",
                                 weight_col = NULL,
                                 weight_multiplier = 1,
                                 progress = FALSE,
                                 include_self = TRUE,
                                 edge_factor = 1) {

  stopifnot(inherits(x, "Data"))
  x <- .pa_ensure_pu_index(x)

  edge_factor <- as.numeric(edge_factor)[1]
  if (!is.finite(edge_factor) || edge_factor < 0) {
    stop("edge_factor must be a finite number >= 0.", call. = FALSE)
  }

  weight_multiplier <- as.numeric(weight_multiplier)[1]
  if (!is.finite(weight_multiplier) || weight_multiplier <= 0) {
    stop("weight_multiplier must be a positive finite number.", call. = FALSE)
  }

  # ------------------------------------------------------------
  # Case A) boundary table provided
  # ------------------------------------------------------------
  if (!is.null(boundary)) {

    stopifnot(inherits(boundary, "data.frame"), nrow(boundary) > 0)
    b <- boundary

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

    rel0 <- data.frame(
      pu1 = as.integer(b$pu1),
      pu2 = as.integer(b$pu2),
      weight = as.numeric(b[[weight_col]]) * weight_multiplier,
      source = "boundary_table",
      stringsAsFactors = FALSE
    )

    x <- .pa_ensure_pu_index(x)
    idx <- x$data$index$pu
    rel0$internal_pu1 <- unname(idx[as.character(rel0$pu1)])
    rel0$internal_pu2 <- unname(idx[as.character(rel0$pu2)])
    if (anyNA(rel0$internal_pu1) || anyNA(rel0$internal_pu2)) {
      stop("Some pu1/pu2 ids were not found in x$data$pu$id.", call. = FALSE)
    }

    n_pu <- nrow(x$data$pu)

    diag_rows <- rel0$internal_pu1 == rel0$internal_pu2
    total <- numeric(n_pu); names(total) <- as.character(seq_len(n_pu))
    if (any(diag_rows)) {
      tt <- tapply(rel0$weight[diag_rows], rel0$internal_pu1[diag_rows], sum)
      total[names(tt)] <- tt
    } else if (isTRUE(include_self)) {
      stop("boundary table has no diagonal (total perimeter) but include_self=TRUE.", call. = FALSE)
    }

    off <- rel0[!diag_rows, , drop = FALSE]
    if (nrow(off) > 0) {
      a <- pmin(off$internal_pu1, off$internal_pu2)
      b2 <- pmax(off$internal_pu1, off$internal_pu2)
      off$internal_pu1 <- a
      off$internal_pu2 <- b2

      key <- paste(off$internal_pu1, off$internal_pu2, sep = "_")
      wmax <- tapply(off$weight, key, max)

      parts <- strsplit(names(wmax), "_", fixed = TRUE)
      i1 <- as.integer(vapply(parts, `[`, "", 1))
      j1 <- as.integer(vapply(parts, `[`, "", 2))

      off_u <- data.frame(
        internal_pu1 = i1,
        internal_pu2 = j1,
        weight = as.numeric(wmax),
        source = "boundary_table_shared",
        stringsAsFactors = FALSE
      )
    } else {
      off_u <- off
    }

    if (!isTRUE(include_self)) {
      if (nrow(off_u) == 0) stop("Boundary table has no off-diagonal edges and include_self=FALSE.", call. = FALSE)
      rel <- off_u
      rel$pu1 <- x$data$pu$id[rel$internal_pu1]
      rel$pu2 <- x$data$pu$id[rel$internal_pu2]
      return(add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE,
                                   duplicate_agg = "max", symmetric = FALSE))
    }

    # incident por PU (OJO: suma, no sobrescribir)
    incident <- numeric(n_pu)
    if (nrow(off_u) > 0) {
      incident <- .sum_incident_by_pu(
        i = off_u$internal_pu1,
        j = off_u$internal_pu2,
        w = off_u$weight,
        n_pu = n_pu
      )
    }

    exposed <- pmax(total - incident, 0)

    # ✅ CLAVE: diagonal efectiva equivalente a prioritizr
    diag_eff <-  edge_factor * exposed

    rel_self <- data.frame(
      internal_pu1 = seq_len(n_pu),
      internal_pu2 = seq_len(n_pu),
      weight = as.numeric(diag_eff),
      source = "boundary_table_diag_effective",
      stringsAsFactors = FALSE
    )

    rel <- if (nrow(off_u) == 0) rel_self else rbind(off_u, rel_self)
    rel$pu1 <- x$data$pu$id[rel$internal_pu1]
    rel$pu2 <- x$data$pu$id[rel$internal_pu2]

    return(add_spatial_relations(
      x, rel, name = name,
      directed = FALSE,
      allow_self = TRUE,
      duplicate_agg = "max",
      symmetric = FALSE
    ))
  }

  # ------------------------------------------------------------
  # Case B) derive from sf polygons
  # ------------------------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("add_spatial_boundary(boundary=NULL) requires the 'sf' package.", call. = FALSE)
  }

  if (is.null(pu_sf)) pu_sf <- x$data$pu_sf
  if (is.null(pu_sf)) stop("boundary is NULL and pu_sf is missing.", call. = FALSE)
  if (!inherits(pu_sf, "sf")) stop("pu_sf must be an sf object.", call. = FALSE)
  if (!("id" %in% names(pu_sf))) stop("pu_sf must contain an 'id' column.", call. = FALSE)

  pu_sf$id <- as.integer(pu_sf$id)
  ord <- match(x$data$pu$id, pu_sf$id)
  if (anyNA(ord)) stop("pu_sf$id does not match x$data$pu$id.", call. = FALSE)
  pu_sf <- pu_sf[ord, , drop = FALSE]

  geom <- sf::st_make_valid(sf::st_geometry(pu_sf))
  pu_sf <- sf::st_set_geometry(pu_sf, geom)
  geom <- sf::st_geometry(pu_sf)

  nb <- sf::st_intersects(pu_sf, pu_sf, sparse = TRUE)

  pu1 <- integer(0); pu2 <- integer(0); w <- numeric(0)
  n <- length(nb)

  if (isTRUE(progress)) message("Computing shared boundary lengths for ", n, " planning units...")

  for (i in seq_len(n)) {
    js <- nb[[i]]
    js <- js[js > i]
    if (!length(js)) next

    for (j in js) {
      inter <- sf::st_intersection(geom[i], geom[j])
      if (length(inter) == 0) next
      inter_line <- suppressWarnings(sf::st_cast(inter, "MULTILINESTRING"))
      len <- suppressWarnings(sf::st_length(inter_line))
      len <- sum(as.numeric(len), na.rm = TRUE)

      if (is.finite(len) && len > 0) {
        pu1 <- c(pu1, pu_sf$id[i])
        pu2 <- c(pu2, pu_sf$id[j])
        w   <- c(w, len)
      }
    }
  }

  w <- round(w, 6)

  if (!length(pu1) && !isTRUE(include_self)) {
    stop("No shared-boundary adjacencies found.", call. = FALSE)
  }

  rel_off <- data.frame(
    pu1 = as.integer(pu1),
    pu2 = as.integer(pu2),
    weight = as.numeric(w) * weight_multiplier,
    source = "boundary_sf_shared_length",
    stringsAsFactors = FALSE
  )

  if (!isTRUE(include_self)) {
    return(add_spatial_relations(x, rel_off, name = name,
                                 directed = FALSE, allow_self = FALSE,
                                 duplicate_agg = "max", symmetric = FALSE))
  }

  total <- as.numeric(sf::st_length(sf::st_boundary(sf::st_geometry(pu_sf))))
  total <- round(total, 6) * weight_multiplier

  incident <- numeric(nrow(pu_sf))
  if (nrow(rel_off) > 0) {
    idx_map <- seq_len(nrow(pu_sf))
    names(idx_map) <- as.character(pu_sf$id)

    i_int <- idx_map[as.character(rel_off$pu1)]
    j_int <- idx_map[as.character(rel_off$pu2)]

    incident <- .sum_incident_by_pu(
      i = i_int,
      j = j_int,
      w = rel_off$weight,
      n_pu = nrow(pu_sf)
    )
  }

  exposed <- pmax(total - incident, 0)

  # ✅ CLAVE: diagonal efectiva equivalente a prioritizr
  diag_eff <-  edge_factor * exposed

  rel_self <- data.frame(
    pu1 = pu_sf$id,
    pu2 = pu_sf$id,
    weight = as.numeric(diag_eff),
    source = "boundary_sf_diag_effective",
    stringsAsFactors = FALSE
  )

  rel <- if (nrow(rel_off) == 0) rel_self else rbind(rel_off, rel_self)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE,
    symmetric = FALSE,
    allow_self = TRUE,
    duplicate_agg = "max"
  )
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

  pu_sf <- .pa_get_pu_sf_aligned(x, pu_sf = pu_sf, arg_name = "pu_sf")
  weight <- as.numeric(weight)[1]
  if (!is.finite(weight) || weight < 0) stop("weight must be finite and >= 0.", call. = FALSE)

  # Rook = shared edge (DE-9IM)
  nb <- sf::st_relate(pu_sf, pu_sf, pattern = "F***1****", sparse = TRUE)

  edges <- vector("list", length(nb))
  for (i in seq_along(nb)) {
    js <- nb[[i]]
    #js <- js[js > i]  # upper triangle to avoid duplicates
    if (!length(js)) next
    edges[[i]] <- data.frame(
      pu1 = pu_sf$id[i],
      pu2 = pu_sf$id[js],
      weight = weight,
      source = "rook_sf",
      stringsAsFactors = FALSE
    )
  }

  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No rook adjacencies found.", call. = FALSE)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE, allow_self = FALSE,
    duplicate_agg = "max"
  )
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

  pu_sf <- .pa_get_pu_sf_aligned(x, pu_sf = pu_sf, arg_name = "pu_sf")
  weight <- as.numeric(weight)[1]
  if (!is.finite(weight) || weight < 0) stop("weight must be finite and >= 0.", call. = FALSE)

  nb <- sf::st_touches(pu_sf, pu_sf, sparse = TRUE)

  edges <- vector("list", length(nb))
  for (i in seq_along(nb)) {
    js <- nb[[i]]
    js <- js[js > i]  # upper triangle
    if (!length(js)) next
    edges[[i]] <- data.frame(
      pu1 = pu_sf$id[i],
      pu2 = pu_sf$id[js],
      weight = weight,
      source = "queen_sf",
      stringsAsFactors = FALSE
    )
  }

  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No queen adjacencies found.", call. = FALSE)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE, allow_self = FALSE,
    duplicate_agg = "max"
  )
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
