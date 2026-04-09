#' @include internal.R
#'
#' @name spatial_relations
#' @title Spatial relations infrastructure
#'
#' @description
#' Define a unified internal representation for spatial relations between
#' planning units and provide constructors for building such relations from
#' common spatial inputs.
#'
#' Spatial relations in \code{multiscape} are graph-like structures defined over
#' the set of planning units. They are used to represent adjacency, shared
#' boundary length, proximity, neighbourhood graphs, or other pairwise spatial
#' relationships that may later be used by objectives, constraints, or
#' diagnostics.
#'
#' Relations are stored inside the \code{Problem} object as
#' \code{x$data$spatial_relations[[name]]}.
#'
#' @details
#' \strong{Internal representation}
#'
#' Each spatial relation is stored as a \code{data.frame} containing, at a
#' minimum, the columns:
#' \describe{
#'   \item{\code{internal_pu1}}{Integer index of the first planning unit.}
#'   \item{\code{internal_pu2}}{Integer index of the second planning unit.}
#'   \item{\code{weight}}{Non-negative edge weight. Its interpretation depends
#'   on the relation type.}
#' }
#'
#' Additional columns may also be present, such as \code{pu1}, \code{pu2},
#' \code{distance}, \code{source}, or other diagnostic fields. These extra
#' columns are preserved when possible.
#'
#' Thus, a spatial relation can be interpreted as a weighted graph
#' \eqn{G = (\mathcal{I}, E, \omega)}, where:
#' \itemize{
#'   \item \eqn{\mathcal{I}} is the set of planning units,
#'   \item \eqn{E} is the set of stored edges,
#'   \item \eqn{\omega_{ij} \ge 0} is the weight associated with edge
#'   \eqn{(i,j)}.
#' }
#'
#' \strong{Directed and undirected relations}
#'
#' Some relation constructors create undirected relations by default. In that
#' case, each unordered pair is typically stored once, usually with
#' \eqn{i < j}, unless self-edges are explicitly allowed.
#'
#' \strong{Boundary relations and diagonal terms}
#'
#' The function \code{\link{add_spatial_boundary}} supports boundary-length
#' relations derived either from a boundary table or from polygon geometry.
#'
#' If \code{include_self = TRUE}, diagonal entries \eqn{(i,i)} may be added to
#' represent exposed boundary. These diagonal weights are intended for
#' boundary-based compactness or fragmentation formulations, where perimeter
#' exposed to the outside of the selected solution should contribute to the
#' objective.
#'
#' \strong{Geometry safety}
#'
#' All \code{sf}-based constructors operate only through spatial predicates,
#' topological relations, or boundary-length calculations. They never subdivide,
#' cut, or alter planning-unit geometries. Planning units are aligned to the
#' internal planning-unit order before spatial relations are computed.
#'
#' \strong{Available constructors}
#'
#' The following functions build and register spatial relations:
#' \itemize{
#'   \item \code{\link{add_spatial_relations}}: register a precomputed relation,
#'   \item \code{\link{add_spatial_boundary}}: shared boundary length relation,
#'   \item \code{\link{add_spatial_rook}}: rook adjacency from polygons,
#'   \item \code{\link{add_spatial_queen}}: queen adjacency from polygons,
#'   \item \code{\link{add_spatial_knn}}: k-nearest-neighbours graph from
#'   coordinates,
#'   \item \code{\link{add_spatial_distance}}: distance-threshold graph from
#'   coordinates.
#' }
#'
#' @keywords internal
NULL

#' @title Add spatial relations
#'
#' @description
#' Register an externally computed spatial relation inside a \code{Problem}
#' object using the unified internal representation adopted by \code{multiscape}.
#'
#' Most users will typically prefer one of the convenience constructors such as
#' \code{\link{add_spatial_boundary}}, \code{\link{add_spatial_rook}},
#' \code{\link{add_spatial_queen}}, \code{\link{add_spatial_knn}}, or
#' \code{\link{add_spatial_distance}}. This function is the advanced low-level
#' entry point for adding an already computed relation.
#'
#' @details
#' Use this function when the spatial relation has already been computed
#' externally and should be registered directly in the \code{Problem} object.
#'
#' The input relation may be provided either in terms of external planning-unit
#' identifiers or in terms of internal planning-unit indices.
#'
#' Specifically, the input \code{relations} table must contain either:
#' \itemize{
#'   \item \code{pu1}, \code{pu2}, and \code{weight}, or
#'   \item \code{internal_pu1}, \code{internal_pu2}, and \code{weight}.
#' }
#'
#' If external ids are supplied, they are mapped to internal indices using the
#' planning-unit identifiers stored in the problem.
#'
#' Let \eqn{G = (\mathcal{I}, E, \omega)} denote the supplied relation, where
#' \eqn{E} corresponds to the rows of \code{relations}. If
#' \code{directed = FALSE}, each edge is treated as undirected, so pairs
#' \eqn{(i,j)} and \eqn{(j,i)} are interpreted as the same edge. In that case,
#' duplicated undirected edges are collapsed automatically using the maximum
#' weight observed for each unordered pair.
#'
#' If \code{directed = TRUE}, edges are preserved as ordered pairs, so
#' \eqn{(i,j)} and \eqn{(j,i)} are distinct unless the user provides both.
#'
#' Self-edges \eqn{(i,i)} are permitted only if \code{allow_self = TRUE}.
#'
#' The final relation is stored in \code{x$data$spatial_relations[[name]]}.
#'
#' If a relation with the same \code{name} already exists, it is replaced.
#'
#' @param x A \code{Problem} object created with \code{\link{create_problem}}.
#' @param relations A \code{data.frame} describing relation edges. It must
#'   contain either:
#'   \itemize{
#'     \item \code{pu1}, \code{pu2}, and \code{weight}, using external
#'     planning-unit ids, or
#'     \item \code{internal_pu1}, \code{internal_pu2}, and \code{weight}, using
#'     internal planning-unit indices.
#'   }
#'   Extra columns such as \code{distance} or \code{source} are allowed and are
#'   preserved when possible.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param directed Logical. If \code{FALSE}, treat edges as undirected and
#'   collapse duplicate unordered pairs. If \code{TRUE}, keep edges as directed
#'   ordered pairs.
#' @param allow_self Logical. If \code{TRUE}, allow self-edges
#'   \eqn{(i,i)}. Default is \code{FALSE}.
#'
#' @return An updated \code{Problem} object with the relation stored in
#'   \code{x$data$spatial_relations[[name]]}.
#'
#' @examples
#' pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
#'
#' features <- data.frame(
#'   id = 1,
#'   name = "sp1"
#' )
#'
#' dist_features <- data.frame(
#'   pu = 1:3,
#'   feature = 1,
#'   amount = c(1, 1, 1)
#' )
#'
#' p <- create_problem(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' rel <- data.frame(
#'   pu1 = c(1, 1, 2),
#'   pu2 = c(2, 3, 3),
#'   weight = c(1, 1, 2)
#' )
#'
#' p <- add_spatial_relations(
#'   x = p,
#'   relations = rel,
#'   name = "my_relation"
#' )
#'
#' p$data$spatial_relations$my_relation
#'
#' @seealso
#' \code{\link{add_spatial_boundary}},
#' \code{\link{add_spatial_rook}},
#' \code{\link{add_spatial_queen}},
#' \code{\link{add_spatial_knn}},
#' \code{\link{add_spatial_distance}}
#'
#' @export
add_spatial_relations <- function(x,
                                  relations,
                                  name,
                                  directed = FALSE,
                                  allow_self = FALSE) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
  x <- .pa_ensure_pu_index(x)
  n_pu <- nrow(x$data$pu)

  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("name must be a non-empty character string.", call. = FALSE)
  }

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

  if (!all(c("internal_pu1","internal_pu2","weight") %in% names(rel))) {
    stop(
      "relations must contain either (pu1, pu2, weight) or (internal_pu1, internal_pu2, weight).",
      call. = FALSE
    )
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

  rel$internal_pu1 <- as.integer(rel$internal_pu1)
  rel$internal_pu2 <- as.integer(rel$internal_pu2)
  rel$weight <- as.numeric(rel$weight)

  if (any(rel$internal_pu1 < 1L | rel$internal_pu1 > n_pu)) stop("internal_pu1 out of range.", call. = FALSE)
  if (any(rel$internal_pu2 < 1L | rel$internal_pu2 > n_pu)) stop("internal_pu2 out of range.", call. = FALSE)
  if (!allow_self && any(rel$internal_pu1 == rel$internal_pu2)) stop("Self-edges are not allowed.", call. = FALSE)
  #if (any(!is.finite(rel$weight)) || any(rel$weight < 0)) stop("weight must be finite and >= 0.", call. = FALSE)

  if (!directed) {
    self_edges <- rel[rel$internal_pu1 == rel$internal_pu2, , drop = FALSE]
    off_edges  <- rel[rel$internal_pu1 != rel$internal_pu2, , drop = FALSE]

    if (nrow(off_edges) > 0) {
      a <- pmin(off_edges$internal_pu1, off_edges$internal_pu2)
      b <- pmax(off_edges$internal_pu1, off_edges$internal_pu2)
      off_edges$internal_pu1 <- a
      off_edges$internal_pu2 <- b

      key <- paste(off_edges$internal_pu1, off_edges$internal_pu2, sep = "_")
      wmax <- tapply(off_edges$weight, key, max)

      parts <- strsplit(names(wmax), "_", fixed = TRUE)
      i1 <- as.integer(vapply(parts, `[`, "", 1))
      j1 <- as.integer(vapply(parts, `[`, "", 2))

      rel_u <- data.frame(
        internal_pu1 = i1,
        internal_pu2 = j1,
        weight = as.numeric(wmax),
        stringsAsFactors = FALSE
      )

      extra_cols <- intersect(names(off_edges), c("pu1","pu2","distance","source"))
      if (length(extra_cols) > 0) {
        rep_idx <- match(names(wmax), key)
        extras <- off_edges[rep_idx, extra_cols, drop = FALSE]
        rel_u <- cbind(rel_u, extras)
      }
    } else {
      rel_u <- off_edges
    }

    if (nrow(self_edges) > 0) {
      key <- as.character(self_edges$internal_pu1)
      wself <- tapply(self_edges$weight, key, max)

      self_fix <- data.frame(
        internal_pu1 = as.integer(names(wself)),
        internal_pu2 = as.integer(names(wself)),
        weight = as.numeric(wself),
        stringsAsFactors = FALSE
      )

      extra_cols <- intersect(names(self_edges), c("pu1","pu2","distance","source"))
      if (length(extra_cols) > 0) {
        rep_idx <- match(names(wself), as.character(self_edges$internal_pu1))
        extras <- self_edges[rep_idx, extra_cols, drop = FALSE]
        self_fix <- cbind(self_fix, extras)
      }

      rel <- if (nrow(rel_u) == 0) self_fix else .pa_rbind_samecols(rel_u, self_fix)
    } else {
      rel <- rel_u
    }
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

#' @title Add spatial boundary-length relations
#'
#' @description
#' Build and register a boundary-length spatial relation between planning units.
#'
#' Boundary relations represent shared edge length between adjacent polygons. In
#' contrast to queen adjacency, they only account for boundary segments of
#' positive length and ignore point-only contacts.
#'
#' @details
#' Use this function when spatial structure should be represented through shared
#' boundary length rather than binary contiguity or coordinate-based proximity.
#'
#' Two input modes are supported:
#' \enumerate{
#'   \item \strong{Boundary-table mode.} If \code{boundary} is supplied, it is
#'   interpreted as a boundary table, for example a Marxan-style
#'   \code{bound.dat}.
#'
#'   \item \strong{Geometry mode.} If \code{boundary = NULL}, boundary lengths
#'   are derived from polygon geometry using \code{geometry} or
#'   \code{x$data$pu_sf}.
#' }
#'
#' Let \eqn{\omega_{ij} \ge 0} denote the shared boundary length between
#' planning units \eqn{i} and \eqn{j}, multiplied by
#' \code{weight_multiplier}.
#'
#' For off-diagonal entries \eqn{i \neq j}, the stored weight is:
#' \deqn{
#' \omega_{ij} = \mathrm{\gamma} \times b_{ij},
#' }
#' where \eqn{b_{ij}} is the shared boundary length and \eqn{\gamma} is
#' the user-supplied \code{weight_multiplier}.
#'
#' If \code{include_self = TRUE}, diagonal entries are also created. These are
#' not geometric self-neighbours in the graph sense; instead, they represent the
#' effective boundary exposed to the outside of the solution.
#'
#' Let \eqn{p_i} be the total perimeter of planning unit \eqn{i}, and let
#' \eqn{\sum_{j \neq i} \omega_{ij}} be the total incident shared boundary
#' recorded for that planning unit. Then the exposed boundary is represented by
#' a diagonal term derived from the difference between total perimeter and shared
#' boundary, scaled by \code{edge_factor}.
#'
#' These diagonal terms are useful in boundary-based compactness or
#' fragmentation objectives, because they encode the portion of each planning
#' unit's perimeter that would remain exposed if the unit were selected.
#'
#' \strong{Boundary-table mode}
#'
#' If \code{boundary} is provided, accepted formats are:
#' \itemize{
#'   \item \code{(id1, id2, boundary)}, or
#'   \item \code{(pu1, pu2, weight)}.
#' }
#'
#' If the table contains diagonal rows \eqn{(i,i)}, these are interpreted as
#' total perimeter values in boundary-table mode.
#'
#' \strong{Geometry mode}
#'
#' If \code{boundary = NULL}, shared boundary lengths are derived directly from
#' polygon geometry. Only positive-length intersections are retained. Point
#' touches are ignored.
#'
#' \strong{Storage}
#'
#' The final relation is stored through \code{\link{add_spatial_relations}},
#' typically as an undirected relation with optional diagonal entries.
#'
#' @param x A \code{Problem} object.
#' @param boundary Optional \code{data.frame} describing boundary lengths.
#'   Accepted formats are:
#'   \itemize{
#'     \item \code{(id1, id2, boundary)}, or
#'     \item \code{(pu1, pu2, weight)}.
#'   }
#' @param geometry Optional \code{sf} object with planning-unit polygons and an
#'   \code{id} column. If \code{NULL}, \code{x$data$pu_sf} is used.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param weight_col Optional character string giving the name of the weight
#'   column in \code{boundary}. If \code{NULL}, the function tries to infer it
#'   from \code{"boundary"} or \code{"weight"}.
#' @param weight_multiplier Positive numeric scalar applied to all boundary
#'   weights.
#' @param include_self Logical. If \code{TRUE}, include diagonal entries
#'   representing exposed boundary.
#' @param edge_factor Numeric scalar greater than or equal to zero. Multiplier
#'   applied to exposed boundary when constructing diagonal entries.
#'
#' @return An updated \code{Problem} object with the stored relation in
#'   \code{x$data$spatial_relations[[name]]}.
#'
#' @examples
#' \dontrun{
#' # From a boundary table
#' p <- add_spatial_boundary(
#'   x = p,
#'   boundary = bound_df,
#'   name = "boundary"
#' )
#'
#' # From sf polygons
#' p <- add_spatial_boundary(
#'   x = p,
#'   geometry = pu_sf,
#'   include_self = TRUE,
#'   edge_factor = 1
#' )
#' }
#'
#' @seealso
#' \code{\link{add_spatial_relations}},
#' \code{\link{add_objective_min_fragmentation_pu}},
#' \code{\link{add_objective_min_fragmentation_action}}
#'
#' @export
add_spatial_boundary <- function(x,
                                 boundary = NULL,
                                 geometry = NULL,
                                 name = "boundary",
                                 weight_col = NULL,
                                 weight_multiplier = 1,
                                 include_self = TRUE,
                                 edge_factor = 1) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
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
    if (!(weight_col %in% names(b))) {
      stop("weight_col not found in boundary table.", call. = FALSE)
    }

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

    total <- numeric(n_pu)
    names(total) <- as.character(seq_len(n_pu))
    if (any(diag_rows)) {
      tt <- tapply(rel0$weight[diag_rows], rel0$internal_pu1[diag_rows], sum)
      total[names(tt)] <- tt
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

    incident <- numeric(n_pu)
    if (nrow(off_u) > 0) {
      incident <- .sum_incident_by_pu(
        i = off_u$internal_pu1,
        j = off_u$internal_pu2,
        w = off_u$weight,
        n_pu = n_pu
      )
    }

    if (!isTRUE(include_self)) {
      if (nrow(off_u) == 0) {
        stop("Boundary table has no off-diagonal edges and include_self=FALSE.", call. = FALSE)
      }
      rel <- off_u
      rel$pu1 <- x$data$pu$id[rel$internal_pu1]
      rel$pu2 <- x$data$pu$id[rel$internal_pu2]
      return(add_spatial_relations(
        x, rel, name = name,
        directed = FALSE,
        allow_self = FALSE
      ))
    }

    has_useful_diagonal <- any(total > 0)

    if (has_useful_diagonal) {
      diag_eff <- edge_factor * (total - incident)
      diag_source <- "boundary_table_diag_effective"
    } else {
      diag_eff <- -incident
      diag_source <- "boundary_table_diag_algebraic"
    }

    rel_self <- data.frame(
      internal_pu1 = seq_len(n_pu),
      internal_pu2 = seq_len(n_pu),
      weight = as.numeric(diag_eff),
      source = diag_source,
      stringsAsFactors = FALSE
    )

    rel <- if (nrow(off_u) == 0) rel_self else rbind(off_u, rel_self)
    rel$pu1 <- x$data$pu$id[rel$internal_pu1]
    rel$pu2 <- x$data$pu$id[rel$internal_pu2]

    return(add_spatial_relations(
      x, rel, name = name,
      directed = FALSE,
      allow_self = TRUE
    ))
  }

  # ------------------------------------------------------------
  # Case B) derive from sf polygons
  # ------------------------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("add_spatial_boundary(boundary=NULL) requires the 'sf' package.", call. = FALSE)
  }

  if (is.null(geometry)) geometry <- x$data$pu_sf
  if (is.null(geometry)) stop("boundary is NULL and geometry is missing.", call. = FALSE)
  if (!inherits(geometry, "sf")) stop("geometry must be an sf object.", call. = FALSE)
  if (!("id" %in% names(geometry))) stop("geometry must contain an 'id' column.", call. = FALSE)

  geometry$id <- as.integer(geometry$id)
  ord <- match(x$data$pu$id, geometry$id)
  if (anyNA(ord)) stop("geometry$id does not match x$data$pu$id.", call. = FALSE)
  geometry <- geometry[ord, , drop = FALSE]

  geom <- sf::st_make_valid(sf::st_geometry(geometry))
  geometry <- sf::st_set_geometry(geometry, geom)
  geom <- sf::st_geometry(geometry)

  nb <- sf::st_intersects(geometry, geometry, sparse = TRUE)

  pu1 <- integer(0)
  pu2 <- integer(0)
  w <- numeric(0)
  n <- length(nb)

  for (i in seq_len(n)) {
    js <- nb[[i]]
    js <- js[js > i]
    if (!length(js)) next

    for (j in js) {
      inter <- suppressWarnings(sf::st_intersection(geom[i], geom[j]))
      if (length(inter) == 0) next

      gtype <- as.character(sf::st_geometry_type(inter, by_geometry = TRUE))

      if (all(gtype %in% c("LINESTRING", "MULTILINESTRING"))) {
        inter_line <- inter
      } else if (any(gtype == "GEOMETRYCOLLECTION")) {
        inter_line <- suppressWarnings(
          sf::st_collection_extract(inter, "LINESTRING")
        )
        if (length(inter_line) == 0) next
      } else {
        next
      }

      inter_line <- suppressWarnings(sf::st_cast(inter_line, "MULTILINESTRING"))
      len <- suppressWarnings(sf::st_length(inter_line))
      len <- sum(as.numeric(len), na.rm = TRUE)

      if (is.finite(len) && len > 0) {
        pu1 <- c(pu1, geometry$id[i])
        pu2 <- c(pu2, geometry$id[j])
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
    return(add_spatial_relations(
      x, rel_off, name = name,
      directed = FALSE,
      allow_self = FALSE
    ))
  }

  total <- as.numeric(sf::st_length(sf::st_boundary(sf::st_geometry(geometry))))
  total <- round(total, 6) * weight_multiplier

  incident <- numeric(nrow(geometry))
  if (nrow(rel_off) > 0) {
    idx_map <- seq_len(nrow(geometry))
    names(idx_map) <- as.character(geometry$id)

    i_int <- idx_map[as.character(rel_off$pu1)]
    j_int <- idx_map[as.character(rel_off$pu2)]

    incident <- .sum_incident_by_pu(
      i = i_int,
      j = j_int,
      w = rel_off$weight,
      n_pu = nrow(geometry)
    )
  }

  has_useful_diagonal <- any(total > 0)

  if (has_useful_diagonal) {
    diag_eff <- edge_factor * (total - incident)
    diag_source <- "boundary_sf_diag_effective"
  } else {
    diag_eff <- -incident
    diag_source <- "boundary_sf_diag_algebraic"
  }

  rel_self <- data.frame(
    pu1 = geometry$id,
    pu2 = geometry$id,
    weight = as.numeric(diag_eff),
    source = diag_source,
    stringsAsFactors = FALSE
  )

  rel <- if (nrow(rel_off) == 0) rel_self else rbind(rel_off, rel_self)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE,
    allow_self = TRUE
  )
}

#' @title Add rook adjacency from polygons
#'
#' @description
#' Build and register a rook adjacency relation from planning-unit polygons.
#'
#' Two planning units are rook-adjacent if they share a boundary segment of
#' positive length. Corner-only contact does not count as rook adjacency.
#'
#' @details
#' Use this function when neighbourhood should be defined by shared polygon
#' edges rather than by point-touching or coordinate-based proximity.
#'
#' This constructor derives an adjacency graph from polygon geometry using a
#' rook criterion. If planning units \eqn{i} and \eqn{j} share a common edge of
#' non-zero length, then an edge \eqn{(i,j)} is added to the relation.
#'
#' Let \eqn{G = (\mathcal{I}, E)} denote the resulting graph. Then:
#' \deqn{
#' (i,j) \in E \quad \Longleftrightarrow \quad
#' \mathrm{length}(\partial i \cap \partial j) > 0.
#' }
#'
#' All edges receive the same user-supplied weight.
#'
#' The resulting relation is stored as an undirected spatial relation.
#'
#' @param x A \code{Problem} object created with
#'   \code{\link{create_problem}} or another object containing aligned
#'   planning-unit polygons.
#' @param geometry Optional \code{sf} object with planning-unit polygons and an
#'   \code{id} column. If \code{NULL}, \code{x$data$pu_sf} is used.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param weight Numeric scalar giving the edge weight assigned to each rook
#'   adjacency.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_spatial_rook(
#'   x = p,
#'   name = "rook",
#'   weight = 1
#' )
#' }
#'
#' @seealso
#' \code{\link{add_spatial_queen}},
#' \code{\link{add_spatial_boundary}}
#'
#' @export
add_spatial_rook <- function(x,
                             geometry = NULL,
                             name = "rook",
                             weight = 1) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
  x <- .pa_ensure_pu_index(x)
  if (!.pa_has_sf()) stop("add_spatial_rook requires the 'sf' package.", call. = FALSE)

  geometry <- .pa_get_pu_sf_aligned(x, pu_sf = geometry, arg_name = "geometry")
  weight <- as.numeric(weight)[1]
  if (!is.finite(weight) || weight < 0) stop("weight must be finite and >= 0.", call. = FALSE)

  nb <- sf::st_relate(geometry, geometry, pattern = "F***1****", sparse = TRUE)

  edges <- vector("list", length(nb))
  for (i in seq_along(nb)) {
    js <- nb[[i]]
    if (!length(js)) next
    edges[[i]] <- data.frame(
      pu1 = geometry$id[i],
      pu2 = geometry$id[js],
      weight = weight,
      source = "rook_sf",
      stringsAsFactors = FALSE
    )
  }

  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No rook adjacencies found.", call. = FALSE)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE, allow_self = FALSE
  )
}

#' @title Add queen adjacency from polygons
#'
#' @description
#' Build and register a queen adjacency relation from planning-unit polygons.
#'
#' Two planning units are queen-adjacent if their boundaries touch, either along
#' a shared edge or at a shared vertex.
#'
#' @details
#' Use this function when neighbourhood should include both shared edges and
#' corner-touching polygon contacts.
#'
#' This constructor derives an adjacency graph from polygon geometry using a
#' queen criterion. If planning units \eqn{i} and \eqn{j} touch at any boundary
#' point, then an edge \eqn{(i,j)} is added to the relation.
#'
#' Let \eqn{G = (\mathcal{I}, E)} denote the resulting graph. Then:
#' \deqn{
#' (i,j) \in E \quad \Longleftrightarrow \quad
#' \partial i \cap \partial j \neq \varnothing.
#' }
#'
#' Thus, queen adjacency includes all rook neighbours plus corner-touching
#' neighbours.
#'
#' All edges receive the same user-supplied weight.
#'
#' The resulting relation is stored as an undirected spatial relation.
#'
#' @param x A \code{Problem} object created with
#'   \code{\link{create_problem}} or another object containing aligned
#'   planning-unit polygons.
#' @param geometry Optional \code{sf} object with planning-unit polygons and an
#'   \code{id} column. If \code{NULL}, \code{x$data$pu_sf} is used.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param weight Numeric scalar giving the edge weight assigned to each queen
#'   adjacency.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_spatial_queen(
#'   x = p,
#'   name = "queen",
#'   weight = 1
#' )
#' }
#'
#' @seealso
#' \code{\link{add_spatial_rook}},
#' \code{\link{add_spatial_boundary}}
#'
#' @export
add_spatial_queen <- function(x,
                              geometry = NULL,
                              name = "queen",
                              weight = 1) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
  x <- .pa_ensure_pu_index(x)
  if (!.pa_has_sf()) stop("add_spatial_queen requires the 'sf' package.", call. = FALSE)

  geometry <- .pa_get_pu_sf_aligned(x, pu_sf = geometry, arg_name = "geometry")
  weight <- as.numeric(weight)[1]
  if (!is.finite(weight) || weight < 0) stop("weight must be finite and >= 0.", call. = FALSE)

  nb <- sf::st_touches(geometry, geometry, sparse = TRUE)

  edges <- vector("list", length(nb))
  for (i in seq_along(nb)) {
    js <- nb[[i]]
    js <- js[js > i]
    if (!length(js)) next
    edges[[i]] <- data.frame(
      pu1 = geometry$id[i],
      pu2 = geometry$id[js],
      weight = weight,
      source = "queen_sf",
      stringsAsFactors = FALSE
    )
  }

  rel <- do.call(rbind, edges)
  if (is.null(rel) || nrow(rel) == 0) stop("No queen adjacencies found.", call. = FALSE)

  add_spatial_relations(
    x, rel, name = name,
    directed = FALSE, allow_self = FALSE
  )
}

#' @title Add k-nearest-neighbours spatial relations
#'
#' @description
#' Build and register a k-nearest-neighbours graph between planning units using
#' coordinates.
#'
#' This constructor does not require polygon geometry. It uses planning-unit
#' coordinates supplied explicitly or stored in the \code{Problem} object.
#'
#' @details
#' Use this function when neighbourhood should be defined by a fixed number of
#' nearby planning units rather than by polygon topology or a fixed distance
#' threshold.
#'
#' Let \eqn{s_i = (x_i, y_i)} denote the coordinates of planning unit
#' \eqn{i}. For each planning unit, this function identifies the \code{k}
#' nearest distinct planning units under Euclidean distance.
#'
#' If \eqn{d_{ij}} denotes the Euclidean distance between units \eqn{i} and
#' \eqn{j}, then the k-nearest-neighbours relation is constructed by adding an
#' edge from \eqn{i} to each of its \code{k} nearest neighbours.
#'
#' Edge weights are then assigned according to \code{weight_mode}:
#' \itemize{
#'   \item \code{"constant"}:
#'   \deqn{\omega_{ij} = 1,}
#'   \item \code{"inverse"}:
#'   \deqn{\omega_{ij} = \frac{1}{\max(d_{ij}, \varepsilon)},}
#'   \item \code{"inverse_sq"}:
#'   \deqn{\omega_{ij} = \frac{1}{\max(d_{ij}, \varepsilon)^2},}
#' }
#' where \eqn{\varepsilon} = \code{distance_eps} is a small constant to avoid
#' division by zero.
#'
#' The raw k-nearest-neighbours structure is directional by construction, but
#' the stored relation is registered as undirected by default through
#' \code{\link{add_spatial_relations}}, which collapses duplicate unordered
#' pairs.
#'
#' If the \pkg{RANN} package is available, it is used for efficient nearest
#' neighbour search. Otherwise, a full distance matrix is computed.
#'
#' @param x A \code{Problem} object created with \code{\link{create_problem}}.
#' @param coords Optional coordinates specification. This may be:
#'   \itemize{
#'     \item a \code{data.frame(id, x, y)}, or
#'     \item a numeric matrix with two columns \code{(x, y)} aligned to the
#'     order of planning units.
#'   }
#'   If \code{NULL}, coordinates are taken from \code{x$data$pu_coords} or from
#'   columns \code{x$data$pu$x} and \code{x$data$pu$y}.
#' @param k Integer giving the number of neighbours per planning unit. Must be
#'   at least 1 and strictly less than the number of planning units.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param weight_mode Character string indicating how distance is converted to
#'   weight. Must be one of \code{"constant"}, \code{"inverse"}, or
#'   \code{"inverse_sq"}.
#' @param distance_eps Small positive numeric constant used to avoid division by
#'   zero in inverse-distance weighting.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_spatial_knn(
#'   x = p,
#'   k = 8,
#'   name = "knn8",
#'   weight_mode = "inverse"
#' )
#' }
#'
#' @seealso
#' \code{\link{add_spatial_distance}},
#' \code{\link{add_spatial_relations}}
#'
#' @export
add_spatial_knn <- function(x,
                            coords = NULL,
                            k = 8,
                            name = "knn",
                            weight_mode = c("constant", "inverse", "inverse_sq"),
                            distance_eps = 1e-9) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
  x <- .pa_ensure_pu_index(x)
  weight_mode <- match.arg(weight_mode)
  k <- as.integer(k)
  if (k < 1) stop("k must be >= 1.", call. = FALSE)

  C <- .pa_coords_from_input(x, coords = coords)
  C <- C[match(x$data$pu$id, C$id), , drop = FALSE]
  if (anyNA(C$id)) stop("coords does not cover all pu ids.", call. = FALSE)

  X <- as.matrix(C[, c("x", "y")])
  n <- nrow(X)
  if (k >= n) stop("k must be < number of PUs.", call. = FALSE)

  if (requireNamespace("RANN", quietly = TRUE)) {
    nn <- RANN::nn2(X, k = k + 1)
    idx_mat <- nn$nn.idx[, -1, drop = FALSE]
    dist_mat <- nn$nn.dists[, -1, drop = FALSE]
  } else {
    D <- as.matrix(stats::dist(X))
    diag(D) <- Inf
    idx_mat <- t(apply(D, 1, function(d) order(d)[seq_len(k)]))
    dist_mat <- matrix(D[cbind(rep(seq_len(n), each = k), as.vector(t(idx_mat)))], nrow = n, byrow = TRUE)
  }

  pu1 <- rep(C$id, each = k)
  pu2 <- C$id[as.vector(t(idx_mat))]
  dist <- as.vector(t(dist_mat))

  w <- switch(
    weight_mode,
    constant = rep(1, length(dist)),
    inverse = 1 / pmax(dist, distance_eps),
    inverse_sq = 1 / pmax(dist, distance_eps)^2
  )

  rel <- data.frame(
    pu1 = as.integer(pu1),
    pu2 = as.integer(pu2),
    weight = as.numeric(w),
    distance = as.numeric(dist),
    source = paste0("knn_", weight_mode),
    stringsAsFactors = FALSE
  )

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE)
}

#' @title Add distance-threshold spatial relations
#'
#' @description
#' Build and register a spatial relation connecting planning units whose
#' Euclidean distance is less than or equal to a user-defined threshold.
#'
#' This constructor does not require polygon geometry and instead uses
#' planning-unit coordinates.
#'
#' @details
#' Use this function when neighbourhood should be defined by a fixed distance
#' radius rather than by polygon topology or a fixed number of neighbours.
#'
#' Let \eqn{s_i = (x_i, y_i)} denote the coordinates of planning unit
#' \eqn{i}. Let \eqn{d_{ij}} be the Euclidean distance between planning units
#' \eqn{i} and \eqn{j}.
#'
#' For a user-supplied threshold \eqn{d_{\max}}, this constructor creates an
#' edge between \eqn{i} and \eqn{j} whenever:
#' \deqn{
#' d_{ij} \le d_{\max}.
#' }
#'
#' Edge weights are assigned according to \code{weight_mode}:
#' \itemize{
#'   \item \code{"constant"}:
#'   \deqn{\omega_{ij} = 1,}
#'   \item \code{"inverse"}:
#'   \deqn{\omega_{ij} = \frac{1}{\max(d_{ij}, \varepsilon)},}
#'   \item \code{"inverse_sq"}:
#'   \deqn{\omega_{ij} = \frac{1}{\max(d_{ij}, \varepsilon)^2},}
#' }
#' where \eqn{\varepsilon} = \code{distance_eps} is a small constant.
#'
#' The implementation computes an \eqn{O(n^2)} distance matrix and is therefore
#' best suited to small or moderate numbers of planning units. For large
#' problems, \code{\link{add_spatial_knn}} is often more scalable.
#'
#' The resulting relation is registered as undirected.
#'
#' @param x A \code{Problem} object created with \code{\link{create_problem}}.
#' @param coords Optional coordinates specification, following the same rules as
#'   in \code{\link{add_spatial_knn}}.
#' @param max_distance Positive numeric scalar giving the maximum distance for
#'   an edge.
#' @param name Character string giving the key under which the relation is
#'   stored.
#' @param weight_mode Character string indicating how distance is converted to
#'   weight. Must be one of \code{"constant"}, \code{"inverse"}, or
#'   \code{"inverse_sq"}.
#' @param distance_eps Small positive numeric constant used to avoid division by
#'   zero in inverse-distance weighting.
#'
#' @return An updated \code{Problem} object.
#'
#' @examples
#' \dontrun{
#' p <- add_spatial_distance(
#'   x = p,
#'   max_distance = 1000,
#'   name = "within_1km",
#'   weight_mode = "constant"
#' )
#' }
#'
#' @seealso
#' \code{\link{add_spatial_knn}},
#' \code{\link{add_spatial_relations}}
#'
#' @export
add_spatial_distance <- function(x,
                                 coords = NULL,
                                 max_distance,
                                 name = "distance",
                                 weight_mode = c("constant", "inverse", "inverse_sq"),
                                 distance_eps = 1e-9) {

  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)
  x <- .pa_ensure_pu_index(x)
  weight_mode <- match.arg(weight_mode)
  max_distance <- as.numeric(max_distance)
  if (!is.finite(max_distance) || max_distance <= 0) {
    stop("max_distance must be a positive finite number.", call. = FALSE)
  }

  C <- .pa_coords_from_input(x, coords = coords)
  C <- C[match(x$data$pu$id, C$id), , drop = FALSE]
  if (anyNA(C$id)) stop("coords does not cover all pu ids.", call. = FALSE)

  X <- as.matrix(C[, c("x", "y")])
  n <- nrow(X)

  D <- as.matrix(stats::dist(X))
  diag(D) <- Inf
  which_edges <- which(D <= max_distance, arr.ind = TRUE)

  if (nrow(which_edges) == 0) stop("No edges found under max_distance. Try a larger threshold.", call. = FALSE)

  pu1 <- C$id[which_edges[, 1]]
  pu2 <- C$id[which_edges[, 2]]
  dist <- D[which_edges]

  w <- switch(
    weight_mode,
    constant = rep(1, length(dist)),
    inverse = 1 / pmax(dist, distance_eps),
    inverse_sq = 1 / pmax(dist, distance_eps)^2
  )

  rel <- data.frame(
    pu1 = as.integer(pu1),
    pu2 = as.integer(pu2),
    weight = as.numeric(w),
    distance = as.numeric(dist),
    source = paste0("distance_", weight_mode),
    stringsAsFactors = FALSE
  )

  add_spatial_relations(x, rel, name = name, directed = FALSE, allow_self = FALSE)
}
