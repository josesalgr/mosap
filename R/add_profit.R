#' @include internal.R
#'
#' @title Add profit to a planning problem
#'
#' @description
#' Define (economic) profit values for feasible \code{(pu, action)} pairs and store them in
#' \code{x$data$dist_profit}. Profit is intentionally stored separately from ecological
#' \code{benefit}/\code{loss} (see \code{\link{add_effects}} / \code{\link{add_benefits}}).
#'
#' @details
#' This function creates a profit table aligned to the current feasibility table
#' \code{x$data$dist_actions}. The resulting \code{x$data$dist_profit} contains:
#' \itemize{
#'   \item \code{pu}: external planning unit id,
#'   \item \code{action}: action id,
#'   \item \code{profit}: numeric profit value,
#'   \item \code{internal_pu}: internal PU index,
#'   \item \code{internal_action}: internal action index.
#' }
#'
#' Profit values can later be used to build objectives (e.g., maximize profit or maximize
#' net profit), budget constraints (e.g., net cost = cost - profit), or reporting summaries.
#'
#' The function is \strong{data-only}: it does not build or modify the optimization model.
#' It also does not change feasibility; it simply assigns profits to rows currently present
#' in \code{dist_actions}. Any additional filtering (e.g., dropping locked-out pairs) should
#' be applied when preparing model-ready tables (typically inside the model builder invoked by
#' \code{solve()}).
#'
#' @param x A \code{Data} object created with \code{\link{inputData}} or \code{\link{inputDataSpatial}}.
#'   Must contain \code{x$data$dist_actions} and \code{x$data$actions} (run \code{\link{add_actions}} first).
#' @param profit Profit specification. One of:
#' \itemize{
#'   \item \code{NULL}: profit is set to 0 for all feasible \code{(pu, action)} pairs.
#'   \item A numeric scalar: recycled to all feasible \code{(pu, action)} pairs.
#'   \item A named numeric vector: names are action ids; assigns a global profit per action.
#'   \item A \code{data.frame(action, profit)}: assigns a global profit per action.
#'   \item A \code{data.frame(pu, action, profit)}: assigns explicit profit values by pair.
#' }
#' @param keep_zero Logical. If \code{TRUE}, keep rows with \code{profit == 0} in the stored table.
#'   Default \code{FALSE} (zero-profit rows are dropped).
#' @param na_to_zero Logical. If \code{TRUE}, treat missing profit values as 0 after joins/matching.
#'   Default \code{TRUE}.
#'
#' @return The updated \code{Data} object with \code{x$data$dist_profit} created/updated.
#'
#' @examples
#' \dontrun{
#' # 1) Default: profit = 0 everywhere (table may be empty if keep_zero = FALSE)
#' x <- x |> add_profit()
#'
#' # 2) Constant profit for every feasible (pu, action)
#' x <- x |> add_profit(profit = 10)
#'
#' # 3) Profit per action (named vector)
#' pr <- c(harvest = 50, sustainable = 20, restoration = -5)
#' x <- x |> add_profit(profit = pr)
#'
#' # 4) Profit per action (data.frame)
#' pr_df <- data.frame(action = c("harvest", "sustainable"), profit = c(50, 20))
#' x <- x |> add_profit(profit = pr_df)
#'
#' # 5) Profit per (pu, action) pair
#' pr_sp <- data.frame(
#'   pu = c(1, 2, 2),
#'   action = c("harvest", "harvest", "sustainable"),
#'   profit = c(100, 80, 30)
#' )
#' x <- x |> add_profit(profit = pr_sp, keep_zero = TRUE)
#' }
#'
#' @export
add_profit <- function(
    x,
    profit = NULL,
    keep_zero = FALSE,
    na_to_zero = TRUE
) {

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a prioriactions Data object")
  assertthat::assert_that(!is.null(x$data$pu), msg = "x$data$pu is missing. Run inputData()/inputDataSpatial() first.")
  assertthat::assert_that(!is.null(x$data$dist_actions), msg = "No actions found. Run add_actions() first.")
  assertthat::assert_that(!is.null(x$data$actions), msg = "No action catalog found. Run add_actions() first.")

  pu   <- x$data$pu
  da   <- x$data$dist_actions
  acts <- x$data$actions

  # pu must have id; internal_id can be created if missing (defensive)
  assertthat::assert_that("id" %in% names(pu), msg = "x$data$pu must contain column 'id'.")
  if (!("internal_id" %in% names(pu))) {
    pu$internal_id <- seq_len(nrow(pu))
    x$data$pu <- pu
  }

  assertthat::assert_that(all(c("pu", "action") %in% names(da)), msg = "x$data$dist_actions must contain columns 'pu' and 'action'.")
  assertthat::assert_that("id" %in% names(acts), msg = "x$data$actions must contain column 'id'.")

  # enforce internal_id for actions (defensive)
  if (!("internal_id" %in% names(acts))) {
    acts$internal_id <- seq_len(nrow(acts))
    x$data$actions <- acts
  }

  pu_ids     <- pu$id
  action_ids <- as.character(acts$id)

  # base skeleton: all (pu, action) pairs currently in dist_actions (no feasibility filtering here)
  base <- da[, c("pu", "action"), drop = FALSE]
  base$action <- as.character(base$action)

  # default profit
  base$profit <- 0

  # ---- fill profit from spec
  if (is.null(profit)) {

    # keep default 0

  } else if (is.numeric(profit) && length(profit) == 1) {

    base$profit <- as.numeric(profit)

  } else if (is.numeric(profit) && !is.null(names(profit))) {

    # named vector by action
    if (!all(names(profit) %in% action_ids)) {
      bad <- setdiff(names(profit), action_ids)
      stop("profit contains unknown action ids: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    base$profit <- as.numeric(profit[base$action])
    if (na_to_zero) base$profit[is.na(base$profit)] <- 0

  } else if (inherits(profit, "data.frame")) {

    p <- profit

    # normalize legacy column naming
    if ("id" %in% names(p) && !("action" %in% names(p))) names(p)[names(p) == "id"] <- "action"

    # Case A: (action, profit)
    if (all(c("action", "profit") %in% names(p)) && !("pu" %in% names(p))) {

      p$action <- as.character(p$action)
      p$profit <- as.numeric(p$profit)

      if (!all(p$action %in% action_ids)) {
        bad <- unique(p$action[!p$action %in% action_ids])
        stop("profit contains unknown action ids: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      # en vez de distinct(...)
      if (anyDuplicated(p$action)) {
        stop("profit (action,profit) must have unique action rows.", call. = FALSE)
      }


      base <- dplyr::left_join(base, p, by = "action", suffix = c("", ".new"))
      if ("profit.new" %in% names(base)) {
        if (na_to_zero) base$profit.new[is.na(base$profit.new)] <- 0
        base$profit <- base$profit.new
        base$profit.new <- NULL
      } else {
        base$profit <- as.numeric(base$profit)
      }

      # Case B: (pu, action, profit)
    } else if (all(c("pu", "action", "profit") %in% names(p))) {

      p$pu     <- as.numeric(p$pu)
      p$action <- as.character(p$action)
      p$profit <- as.numeric(p$profit)

      if (!all(p$pu %in% pu_ids)) {
        bad <- unique(p$pu[!p$pu %in% pu_ids])
        stop("profit contains unknown pu ids: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      if (!all(p$action %in% action_ids)) {
        bad <- unique(p$action[!p$action %in% action_ids])
        stop("profit contains unknown action ids: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      tmp <- p[, c("pu", "action")]
      if (nrow(dplyr::distinct(tmp)) != nrow(tmp)) {
        stop("profit has duplicate (pu, action) rows.", call. = FALSE)
      }

      base <- dplyr::left_join(base, p, by = c("pu", "action"), suffix = c("", ".new"))
      if ("profit.new" %in% names(base)) {
        if (na_to_zero) base$profit.new[is.na(base$profit.new)] <- 0
        base$profit <- base$profit.new
        base$profit.new <- NULL
      } else {
        base$profit <- as.numeric(base$profit)
      }

    } else {
      stop("Unsupported profit data.frame format. Use (action,profit) or (pu,action,profit).", call. = FALSE)
    }

  } else {
    stop("Unsupported type for 'profit'. Use NULL, numeric scalar, named numeric vector, or a data.frame.", call. = FALSE)
  }

  # ---- cleanup / validation
  base$profit <- as.numeric(base$profit)
  if (na_to_zero) base$profit[is.na(base$profit)] <- 0

  if (!all(is.finite(base$profit))) stop("profit values must be finite.", call. = FALSE)

  if (!keep_zero) base <- base[base$profit != 0, , drop = FALSE]

  # ---- add internal ids
  pu_map   <- x$data$pu[, c("id", "internal_id")]
  acts_map <- x$data$actions[, c("id", "internal_id")]

  base$internal_pu     <- pu_map$internal_id[match(base$pu, pu_map$id)]
  base$internal_action <- acts_map$internal_id[match(base$action, acts_map$id)]

  dist_profit <- base[, c("pu", "action", "profit", "internal_pu", "internal_action"), drop = FALSE]

  # store
  x$data$dist_profit <- dist_profit

  x
}
