set_method_epsilon_constraint <- function(
    x,
    primary,
    eps = NULL,
    aliases = NULL,
    mode = c("manual", "auto"),
    n_points = 10,
    include_extremes = TRUE,
    lexicographic = TRUE,
    lexicographic_tol = 1e-8
) {
  x <- .pamo_as_mo(x)
  stopifnot(inherits(x, "MOProblem"))

  mode <- match.arg(mode)

  primary <- as.character(primary)[1]
  if (is.na(primary) || !nzchar(primary)) {
    stop("primary must be a non-empty string.", call. = FALSE)
  }

  .pamo_validate_objectives(x)

  specs_all <- .pamo_get_specs(x)
  obj_alias <- names(specs_all)
  if (!primary %in% obj_alias) {
    stop("primary alias not found: '", primary, "'.", call. = FALSE)
  }

  if (is.null(aliases)) aliases <- obj_alias
  aliases <- as.character(aliases)

  if (any(!aliases %in% obj_alias)) {
    bad <- aliases[!aliases %in% obj_alias]
    stop("Unknown aliases: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (!primary %in% aliases) {
    stop("primary must be included in aliases.", call. = FALSE)
  }

  constrained <- setdiff(aliases, primary)

  if (length(constrained) == 0L) {
    stop("At least one constrained objective is required.", call. = FALSE)
  }

  if (!is.logical(lexicographic) || length(lexicographic) != 1L || is.na(lexicographic)) {
    stop("lexicographic must be TRUE or FALSE.", call. = FALSE)
  }

  lexicographic_tol <- as.numeric(lexicographic_tol)[1]
  if (!is.finite(lexicographic_tol) || lexicographic_tol < 0) {
    stop("lexicographic_tol must be a finite non-negative number.", call. = FALSE)
  }

  if (mode == "manual") {
    if (is.null(eps)) {
      stop("In mode='manual', eps must be provided.", call. = FALSE)
    }

    if (is.numeric(eps) && !is.null(names(eps))) {
      eps_list <- as.list(eps)
      eps_list <- lapply(eps_list, function(v) c(as.numeric(v)[1]))
    } else if (is.list(eps) && length(eps) > 0 && !is.null(names(eps))) {
      eps_list <- lapply(eps, function(v) as.numeric(v))
    } else {
      stop("eps must be a named numeric vector or a named list of numeric vectors.", call. = FALSE)
    }

    miss <- setdiff(constrained, names(eps_list))
    if (length(miss) > 0) {
      stop("eps must include all constrained objectives. Missing: ", paste(miss, collapse = ", "), call. = FALSE)
    }

    extra <- setdiff(names(eps_list), constrained)
    if (length(extra) > 0) {
      stop(
        "eps contains aliases that are not constrained objectives: ",
        paste(extra, collapse = ", "),
        call. = FALSE
      )
    }

    bad_empty <- names(eps_list)[vapply(eps_list, length, integer(1)) == 0L]
    if (length(bad_empty) > 0) {
      stop("eps contains empty vectors for: ", paste(bad_empty, collapse = ", "), call. = FALSE)
    }

    bad_nonfinite <- names(eps_list)[vapply(eps_list, function(v) any(!is.finite(v)), logical(1))]
    if (length(bad_nonfinite) > 0) {
      stop("eps contains non-finite values for: ", paste(bad_nonfinite, collapse = ", "), call. = FALSE)
    }

    grid <- expand.grid(
      eps_list[constrained],
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    if (nrow(grid) == 0) {
      stop("Empty epsilon grid.", call. = FALSE)
    }

    names(grid) <- paste0("eps_", names(grid))
    grid$run_id <- seq_len(nrow(grid))

    x$method <- list(
      name = "epsilon_constraint",
      mode = "manual",
      primary = primary,
      aliases = aliases,
      constrained = constrained,
      eps = eps_list,
      runs = grid,
      lexicographic = isTRUE(lexicographic),
      lexicographic_tol = lexicographic_tol
    )

    return(x)
  }

  # mode == "auto"
  if (length(aliases) != 2L) {
    stop(
      "set_method_epsilon_constraint(mode='auto') currently supports exactly 2 objectives.\n",
      "Use mode='manual' for 3+ objectives.",
      call. = FALSE
    )
  }

  n_points <- as.integer(n_points)[1]
  if (!is.finite(n_points) || is.na(n_points) || n_points < 2L) {
    stop("n_points must be an integer >= 2.", call. = FALSE)
  }

  x$method <- list(
    name = "epsilon_constraint",
    mode = "auto",
    primary = primary,
    aliases = aliases,
    constrained = constrained,
    n_points = n_points,
    include_extremes = isTRUE(include_extremes),
    lexicographic = isTRUE(lexicographic),
    lexicographic_tol = lexicographic_tol,
    runs = NULL
  )

  x
}
