#' Configure epsilon-constraint multi-objective method
#'
#' @param x A Data or MOProblem.
#' @param primary character(1). Alias of the primary objective to optimize.
#' @param eps Either:
#'   - named numeric vector: eps for constrained objectives (single run), or
#'   - named list of numeric vectors: grid per objective (multiple runs).
#'   Names must be objective aliases (usually all except `primary`).
#' @param aliases Optional character vector of objective aliases to consider.
#'   Default: all registered objectives in x (MOProblem) / in x$data$model_registry (Data-side).
#' @param normalize Logical. Reserved (future). Stored in method config.
#' @param keep_primary_eps Logical. If TRUE and eps contains primary, keep it (not typical).
#' @return MOProblem with method configured.
#' @export
#' @export
set_method_epsilon_constraint <- function(
    x,
    primary,
    eps,
    aliases = NULL
) {
  x <- .pamo_as_mo(x)
  stopifnot(inherits(x, "MOProblem"))

  primary <- as.character(primary)[1]
  if (is.na(primary) || !nzchar(primary)) stop("primary must be a non-empty string.", call. = FALSE)

  .pamo_validate_objectives(x)

  specs_all <- .pamo_get_specs(x)
  obj_alias <- names(specs_all)
  if (!primary %in% obj_alias) stop("primary alias not found: '", primary, "'.", call. = FALSE)

  if (is.null(aliases)) aliases <- obj_alias
  aliases <- as.character(aliases)
  if (any(!aliases %in% obj_alias)) {
    bad <- aliases[!aliases %in% obj_alias]
    stop("Unknown aliases: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (!primary %in% aliases) stop("primary must be included in aliases.", call. = FALSE)

  # ---- normalize eps input -> named list(alias -> numeric vector)
  if (is.numeric(eps) && !is.null(names(eps))) {
    eps_list <- as.list(eps)
    eps_list <- lapply(eps_list, function(v) as.numeric(v)[1])
    eps_list <- lapply(eps_list, function(v) c(v))
  } else if (is.list(eps) && length(eps) > 0 && !is.null(names(eps))) {
    eps_list <- lapply(eps, function(v) as.numeric(v))
  } else {
    stop("eps must be a *named* numeric vector or a *named* list of numeric vectors.", call. = FALSE)
  }

  # Constrain all except primary
  constrained <- setdiff(aliases, primary)

  # eps must cover constrained
  miss <- setdiff(constrained, names(eps_list))
  if (length(miss) > 0) {
    stop("eps must include all constrained objectives. Missing: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  browser()

  # build grid
  grid <- expand.grid(eps_list[constrained], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (nrow(grid) == 0) stop("Empty epsilon grid.", call. = FALSE)
  grid$run_id <- seq_len(nrow(grid))

  x$method <- list(
    name = "epsilon_constraint",
    primary = primary,
    aliases = aliases,
    constrained = constrained,
    eps = eps_list,   # original list
    runs = grid
  )

  x
}
