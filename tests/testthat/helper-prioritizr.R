skip_if_no_prioritizr <- function() {
  testthat::skip_if_not_installed("prioritizr")
}

skip_if_no_ompr <- function() {
  testthat::skip_if_not_installed("ompr")
}

build_prioritizr_basic <- function(toy, target = 0.5, boundary_matrix = NULL) {
  skip_if_no_prioritizr()

  pu <- toy$pu
  feat_wide <- reshape(
    toy$dist_features,
    idvar = "pu",
    timevar = "feature",
    direction = "wide"
  )

  dat <- merge(pu, feat_wide, by.x = "id", by.y = "pu", all.x = TRUE, sort = FALSE)
  dat[is.na(dat)] <- 0

  names(dat) <- sub("^amount\\.", "feature_", names(dat))

  p <- prioritizr::problem(
    dat,
    features = paste0("feature_", toy$features$id),
    cost_column = "cost"
  ) |>
    prioritizr::add_min_set_objective() |>
    prioritizr::add_relative_targets(target)

  if (!is.null(boundary_matrix)) {
    p <- p |>
      prioritizr::add_boundary_penalties(
        penalty = 1,
        data = boundary_matrix
      )
  }

  p
}

solve_prioritizr <- function(x, ...) {
  get("solve", envir = asNamespace("prioritizr"))(x, ...)
}
