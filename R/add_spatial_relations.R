#' @include internal.R
#'
#' Spatial relations infrastructure
#'
#' @description
#' Defines a unified internal representation for spatial relations (graphs) between planning units
#' and provides constructors to populate these relations from common sources:
#' \itemize{
#'   \item boundary tables (Marxan-style),
#'   \item rook/queen adjacency derived from \code{sf} polygons,
#'   \item k-nearest-neighbours (kNN) graphs from coordinates,
#'   \item distance-threshold graphs from coordinates.
#' }
#'
#' Relations are stored inside the problem object as:
#' \code{x$data$spatial_relations[[name]]}.
#'
#' @details
#' \strong{Internal representation.}
#' Each relation is stored as a \code{data.frame} with (at minimum) the columns:
#' \describe{
#'   \item{\code{internal_pu1}}{Integer index of the first PU (1..n\_pu).}
#'   \item{\code{internal_pu2}}{Integer index of the second PU (1..n\_pu).}
#'   \item{\code{weight}}{Non-negative numeric edge weight. Its interpretation depends on the relation type.}
#' }
#' Additional columns may be present for provenance or diagnostics (e.g., \code{pu1}, \code{pu2},
#' \code{distance}, \code{source}).
#'
#' \strong{Directed vs undirected.}
#' Some constructors create undirected relations (default); these are stored once per unordered
#' pair \code{(i,j)} with \code{i < j}. If \code{symmetric=TRUE} is used in
#' \code{\link{add_spatial_relations}}, undirected relations can be expanded to a directed
#' representation by duplicating edges in both directions.
#'
#' \strong{Boundary relations and diagonal weights.}
#' \code{\link{add_spatial_boundary}} supports two common inputs:
#' \itemize{
#'   \item a boundary table (e.g., Marxan \code{bound.dat}), and
#'   \item \code{sf} polygon planning units.
#' }
#' When \code{include_self=TRUE}, the function also creates diagonal entries
#' \code{(i,i)} representing the effective boundary length exposed to the outside of the solution.
#' This is computed as:
#' \deqn{\mathrm{diag\_eff}_i = \mathrm{edge\_factor} \cdot \max\{ \mathrm{total}_i - \mathrm{incident}_i,\, 0 \}}
#' where \code{total} is the PU perimeter and \code{incident} is the sum of shared boundary lengths
#' with neighbouring PUs.
#'
#' \strong{Geometry safety.}
#' All \code{sf}-based constructors operate only through spatial predicates and length calculations;
#' they never cut polygons or create new planning unit geometries. Planning units are aligned to
#' the \code{x$data$pu$id} order before any computation.
#'
#' @name spatial_relations
#' @keywords internal
NULL
#
#' @title Add spatial relations (core)
#'
#' @description
#' Register an externally computed spatial relation inside a \code{Data} object using the unified
#' internal representation. Most users should prefer the convenience wrappers:
#' \code{\link{add_spatial_boundary}}, \code{\link{add_spatial_rook}}, \code{\link{add_spatial_queen}},
#' \code{\link{add_spatial_knn}}, or \code{\link{add_spatial_distance}}.
#'
#' @details
#' The input can be given in external PU ids (\code{pu1}, \code{pu2}) or in internal PU indices
#' (\code{internal_pu1}, \code{internal_pu2}). If external ids are provided, they are mapped using
#' \code{x$data$pu$id} and \code{x$data$pu$internal_id}.
#'
#' If \code{directed = FALSE}, edges are treated as undirected and duplicates are collapsed using
#' \code{duplicate_agg}. If \code{symmetric = TRUE}, the relation is expanded to a directed edge list
#' by adding swapped copies for off-diagonal edges.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param relations A \code{data.frame} describing edges. Must contain either:
#' \itemize{
#'   \item \code{pu1}, \code{pu2}, \code{weight} (external PU ids), or
#'   \item \code{internal_pu1}, \code{internal_pu2}, \code{weight} (internal indices).
#' }
#' Extra columns (e.g., \code{distance}, \code{source}) are allowed and preserved when possible.
#' @param name Character. Name/key under which to store the relation (default \code{"default"}).
#' @param directed Logical. If \code{FALSE} (default), treats edges as undirected and collapses duplicates.
#' If \code{TRUE}, keeps directed edges as provided.
#' @param allow_self Logical. Whether to allow self-edges \code{(i,i)}. Default \code{FALSE}.
#' @param duplicate_agg Aggregation for duplicate undirected edges when \code{directed=FALSE}.
#' One of \code{"sum"}, \code{"max"}, \code{"min"}, \code{"mean"}.
#' @param symmetric Logical. If \code{TRUE}, expands an undirected relation into a directed one by duplicating
#' off-diagonal edges in both directions. Default \code{FALSE}.
#'
#' @return Updated [data-class] object with \code{x$data$spatial_relations[[name]]}.
#' @examples
#' \dontrun{
#' # Register an externally computed adjacency list:
#' rel <- data.frame(pu1 = c(1, 1, 2), pu2 = c(2, 3, 3), weight = 1)
#' x <- x |> add_spatial_relations(relations = rel, name = "my_adj")
#' }
#'
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


#' @title Add spatial boundary-length relations from sf polygons or a boundary table
#'
#' @description
#' Register a boundary-length relation between planning units. Boundary relations represent
#' shared edge length between adjacent polygons (not queen touches).
#'
#' @details
#' Two input modes are supported:
#' \enumerate{
#'   \item \strong{Boundary table mode.} If \code{boundary} is provided, it is interpreted as a table
#'   containing PU pairs and a boundary-length weight (e.g., Marxan-style \code{bound.dat}).
#'   \item \strong{Geometry mode.} If \code{boundary} is \code{NULL}, boundary lengths are derived from
#'   planning-unit polygons (\code{pu_sf} or \code{x$data$pu_sf}).
#' }
#'
#' If \code{include_self=TRUE}, the function also adds diagonal entries \code{(i,i)} with weights equal to
#' an "effective exposed boundary" (scaled by \code{edge_factor}). This is useful for objectives such as
#' boundary-length modifier / fragmentation penalties where perimeter exposed to the outside should be counted.
#'
#' @param x A [data-class] object.
#' @param boundary Optional \code{data.frame} describing boundaries. Accepted formats:
#' \itemize{
#'   \item \code{(id1, id2, boundary)} (Marxan-style), or
#'   \item \code{(pu1, pu2, weight)}.
#' }
#' @param pu_sf Optional \code{sf} object with PU polygons and an \code{id} column. If \code{NULL},
#' uses \code{x$data$pu_sf}.
#' @param name Character name/key under which to store the relation (default \code{"boundary"}).
#' @param weight_col Character. Column in \code{boundary} to use as weights. If \code{NULL}, attempts to
#' guess \code{"boundary"} or \code{"weight"}.
#' @param weight_multiplier Numeric multiplier applied to boundary weights (e.g., BLM scaling).
#' Must be finite and positive.
#' @param progress Logical. If \code{TRUE}, prints basic progress messages for large instances.
#' @param include_self Logical. If \code{TRUE} (default), include diagonal \code{(i,i)} entries representing
#' effective exposed boundary length.
#' @param edge_factor Numeric \eqn{\ge 0}. Multiplier applied to exposed boundary when computing diagonal weights.
#'
#' @return Updated [data-class] object with a stored relation \code{x$data$spatial_relations[[name]]}.
#' @examples
#' \dontrun{
#' # From a Marxan-style boundary table:
#' x <- x |> add_spatial_boundary(boundary = bound_df, name = "boundary")
#'
#' # From sf polygons:
#' x <- x |> add_spatial_boundary(pu_sf = pu_sf, include_self = TRUE, edge_factor = 1)
#' }
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





#' @title Add rook adjacency from sf polygons
#'
#' @description
#' Build and register a rook adjacency relation (shared edge) from planning-unit polygons.
#' Rook adjacency detects pairs of polygons that share a non-zero-length boundary segment.
#'
#' @param x A [data-class] object created with [inputDataSpatial()].
#' @param pu_sf Optional \code{sf} object with PU polygons and an \code{id} column. If \code{NULL},
#' uses \code{x$data$pu_sf}.
#' @param name Character name/key under which to store the relation.
#' @param weight Numeric edge weight assigned to each rook adjacency (default 1).
#'
#' @return Updated [data-class] object.
#' @examples
#' \dontrun{
#' x <- x |> add_spatial_rook(name = "rook", weight = 1)
#' }
#'
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

#' @title Add queen adjacency from sf polygons
#'
#' @description
#' Build and register a queen adjacency relation (shared edge or shared vertex) from planning-unit polygons.
#' Queen adjacency includes all rook neighbours plus corner-touching neighbours.
#'
#' @param x A [data-class] object created with [inputDataSpatial()].
#' @param pu_sf Optional \code{sf} object with PU polygons and an \code{id} column. If \code{NULL},
#' uses \code{x$data$pu_sf}.
#' @param name Character name/key under which to store the relation.
#' @param weight Numeric edge weight assigned to each queen adjacency (default 1).
#'
#' @return Updated [data-class] object.
#' @examples
#' \dontrun{
#' x <- x |> add_spatial_queen(name = "queen", weight = 1)
#' }
#'
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


#' @title Add k-nearest-neighbours spatial relations from coordinates
#'
#' @description
#' Build and register a k-nearest-neighbours (kNN) graph between planning units based on coordinates.
#' This constructor does not require \code{sf}. If the \code{RANN} package is available, it is used for speed.
#'
#' @details
#' Coordinates can be supplied explicitly via \code{coords}, stored in \code{x$data$pu_coords},
#' or stored as columns \code{x$data$pu$x} and \code{x$data$pu$y}.
#'
#' Edge weights can be constant or derived from distance using \code{weight_fn}.
#' The stored relation is undirected by default (duplicates are collapsed).
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param coords Optional coordinates specification:
#' \itemize{
#'   \item a \code{data.frame(id, x, y)}, or
#'   \item a matrix with two columns \code{(x,y)} aligned to the PU order.
#' }
#' If \code{NULL}, uses \code{x$data$pu_coords} or \code{x$data$pu$x/y}.
#' @param k Integer. Number of neighbours per planning unit (must be \code{>= 1} and \code{< n_pu}).
#' @param name Character name/key under which to store the relation.
#' @param weight_fn Character. How to convert distance to weight:
#' \code{"constant"}, \code{"inverse"}, or \code{"inverse_sq"}.
#' @param eps Small numeric constant to avoid division by zero when using inverse weights.
#'
#' @return Updated [data-class] object.
#' @examples
#' \dontrun{
#' x <- x |> add_spatial_knn(k = 8, name = "knn8", weight_fn = "inverse")
#' }
#'
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

#'
#' @title Add distance-threshold spatial relations from coordinates
#'
#' @description
#' Build and register edges between planning units whose Euclidean distance is less than or equal to
#' \code{dmax}, based on coordinates. This constructor does not require \code{sf}.
#'
#' @details
#' This constructor uses an \eqn{O(n^2)} distance computation and is therefore best suited to small or
#' moderate numbers of planning units. For large instances, consider \code{\link{add_spatial_knn}} instead.
#'
#' @param x A [data-class] object created with [inputData()] or [inputDataSpatial()].
#' @param coords Optional coordinates specification (see \code{\link{add_spatial_knn}}).
#' @param dmax Numeric. Maximum distance for an edge (must be finite and positive).
#' @param name Character name/key under which to store the relation.
#' @param weight_fn Character. How to convert distance to weight:
#' \code{"constant"}, \code{"inverse"}, or \code{"inverse_sq"}.
#' @param eps Small numeric constant to avoid division by zero when using inverse weights.
#'
#' @return Updated [data-class] object.
#' @examples
#' \dontrun{
#' x <- x |> add_spatial_distance(dmax = 1000, name = "within_1km", weight_fn = "constant")
#' }
#'
#' @export
add_spatial_distance <- function(x,
                                 coords = NULL,
                                 dmax,
                                 name = "distance",
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
