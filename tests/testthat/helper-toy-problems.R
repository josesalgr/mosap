toy_equivalent_basic <- function() {
  pu <- data.frame(
    id = 1:4,
    cost = c(10, 12, 30, 40),
    locked_in = c(0, 0, 0, 0),
    locked_out = c(0, 0, 0, 0)
  )

  features <- data.frame(
    id = 1:2,
    name = c("f1", "f2")
  )

  dist_features <- data.frame(
    pu = c(1, 2, 3, 4, 1, 2, 3, 4),
    feature = c(1, 1, 1, 1, 2, 2, 2, 2),
    amount = c(8, 6, 3, 1, 1, 2, 8, 7)
  )

  actions <- data.frame(
    id = "conservation",
    name = "conservation"
  )

  effects <- expand.grid(
    action = "conservation",
    feature = features$id,
    stringsAsFactors = FALSE
  )
  effects$multiplier <- 1

  boundary <- data.frame(
    pu1 = c(1, 1, 2, 3),
    pu2 = c(2, 3, 4, 4),
    boundary = c(1, 1, 1, 1)
  )

  list(
    pu = pu,
    features = features,
    dist_features = dist_features,
    actions = actions,
    effects = effects,
    boundary = boundary
  )
}

toy_multiaction_semantics <- function() {
  pu <- data.frame(
    id = 1:5,
    cost = c(5, 6, 8, 9, 20),
    locked_in = c(0, 0, 0, 0, 0),
    locked_out = c(0, 0, 0, 0, 0)
  )

  features <- data.frame(
    id = 1:2,
    name = c("common", "restoration_only")
  )

  dist_features <- data.frame(
    pu = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    feature = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    amount = c(5, 5, 5, 5, 5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )

  actions <- data.frame(
    id = c("conservation", "restoration"),
    name = c("conservation", "restoration")
  )

  # conservation mejora solo feature 1; restoration mejora mucho feature 2
  effects <- data.frame(
    action = c(
      "conservation", "conservation",
      "restoration",  "restoration"
    ),
    feature = c(1, 2, 1, 2),
    multiplier = c(0.2, 0, 0.1, 2)
  )

  boundary <- data.frame(
    pu1 = c(1, 2, 3, 4),
    pu2 = c(2, 3, 4, 5),
    boundary = c(1, 1, 1, 1)
  )

  list(
    pu = pu,
    features = features,
    dist_features = dist_features,
    actions = actions,
    effects = effects,
    boundary = boundary
  )
}


toy_boundary_matrix <- function(toy) {
  ids <- sort(unique(c(toy$boundary$pu1, toy$boundary$pu2)))
  n <- max(ids)

  mat <- Matrix::Matrix(0, nrow = n, ncol = n, sparse = TRUE)

  for (i in seq_len(nrow(toy$boundary))) {
    a <- toy$boundary$pu1[i]
    b <- toy$boundary$pu2[i]
    w <- toy$boundary$boundary[i]

    mat[a, b] <- w
    mat[b, a] <- w
  }

  mat
}
