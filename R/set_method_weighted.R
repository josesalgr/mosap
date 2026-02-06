#' Set multi-objective method: weighted sum
#'
#' Promotes a `prioriactions::Data` object to a multi-objective problem (class
#' `"MOProblem"`) and stores the configuration for the weighted-sum method.
#'
#' This function assumes atomic objectives have already been registered using
#' `{prioriactions}` objective setters (e.g., `add_objective_*()` with `alias =`),
#' and that each objective alias is unique.
#'
#' @param x A `prioriactions::Data` object (or an `"MOProblem"`).
#' @param aliases `character`. Objective aliases to combine (e.g. `c("cost","frag")`).
#' @param weights `numeric`. Non-negative weights, same length as `aliases`.
#' @param normalize `logical`. If `TRUE`, weights are rescaled to sum to 1.
#'
#' @return The same object `x`, promoted to class `"MOProblem"`, with the method stored in
#'   `x$method`.
#'
#' @export
set_method_weighted <- function(x,
                                aliases,
                                weights,
                                normalize = FALSE) {

  # ---- promote (handles both Data and MOProblem)
  x <- .pamo_as_mo(x)

  # ---- validate aliases
  if (!is.character(aliases) || length(aliases) == 0L || anyNA(aliases)) {
    stop("`aliases` must be a non-empty character vector without NA.", call. = FALSE)
  }
  aliases <- as.character(aliases)

  if (any(!nzchar(aliases))) {
    stop("`aliases` must not contain empty strings.", call. = FALSE)
  }
  if (anyDuplicated(aliases) != 0L) {
    dups <- unique(aliases[duplicated(aliases)])
    stop("`aliases` must not contain duplicates: ", paste(dups, collapse = ", "), call. = FALSE)
  }

  # ---- validate weights
  if (!is.numeric(weights) || length(weights) != length(aliases) || anyNA(weights)) {
    stop("`weights` must be a numeric vector, same length as `aliases`, without NA.", call. = FALSE)
  }
  weights <- as.numeric(weights)

  if (any(!is.finite(weights))) stop("`weights` must be finite.", call. = FALSE)
  if (any(weights < 0)) stop("`weights` must be non-negative.", call. = FALSE)

  # ---- normalize flag
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("`normalize` must be TRUE or FALSE.", call. = FALSE)
  }
  normalize <- isTRUE(normalize)

  if (normalize) {
    s <- sum(weights)
    if (!is.finite(s) || s <= 0) {
      stop("`normalize = TRUE` requires sum(weights) > 0.", call. = FALSE)
    }
    weights <- weights / s
  }

  # ---- validate that objective aliases exist (reads x$base$data$objectives)
  # This gives you nicer errors and also future-proofs (e.g., checks objective_id/sense)
  .pamo_get_objective_specs(x, aliases)

  # ---- store method configuration
  x$method <- list(
    name = "weighted",
    params = list(
      aliases = aliases,
      weights = weights,
      normalize = normalize
    )
  )

  x
}
