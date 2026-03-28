#' @include internal.R
#'
#' @title Add profit to a planning problem
#'
#' @description
#' Define economic profit values for feasible planning unit--action pairs and
#' store them in \code{x$data$dist_profit}.
#'
#' Profit is stored separately from ecological effects. In particular,
#' \code{profit} is not the same as ecological \code{benefit} or
#' \code{loss} as represented in \code{\link{add_effects}}. This separation
#' allows the package to distinguish economic returns from ecological
#' consequences when building objectives, constraints, and reporting summaries.
#'
#' @details
#' Let \eqn{\mathcal{F} \subseteq \mathcal{P} \times \mathcal{A}} denote the set
#' of feasible planning unit--action pairs currently stored in
#' \code{x$data$dist_actions}, where \eqn{\mathcal{P}} is the set of planning
#' units and \eqn{\mathcal{A}} is the set of actions.
#'
#' This function assigns to each feasible pair \eqn{(i,a) \in \mathcal{F}} a
#' numeric profit value \eqn{\pi_{ia} \in \mathbb{R}} and stores the result in
#' \code{x$data$dist_profit}.
#'
#' Thus, the stored table can be interpreted as a mapping
#' \deqn{
#' \pi : \mathcal{F} \to \mathbb{R},
#' }
#' where \eqn{\pi_{ia}} represents the economic return associated with selecting
#' action \eqn{a} in planning unit \eqn{i}.
#'
#' Profit values may be positive, zero, or negative. Positive values represent
#' gains or revenues, zero represents no net profit contribution, and negative
#' values can be used to encode penalties or net economic losses.
#'
#' The resulting table \code{x$data$dist_profit} contains:
#' \itemize{
#'   \item \code{pu}: external planning-unit id,
#'   \item \code{action}: action id,
#'   \item \code{profit}: numeric profit value,
#'   \item \code{internal_pu}: internal planning-unit index,
#'   \item \code{internal_action}: internal action index.
#' }
#'
#' \strong{Supported input formats}
#'
#' The \code{profit} argument may be specified in several ways:
#' \itemize{
#'   \item \code{NULL}: assign profit 0 to all feasible \code{(pu, action)}
#'   pairs,
#'   \item a numeric scalar: assign the same profit value to all feasible pairs,
#'   \item a named numeric vector: names are action ids, assigning one global
#'   profit value per action,
#'   \item a \code{data.frame(action, profit)}: assign one global profit value
#'   per action,
#'   \item a \code{data.frame(pu, action, profit)}: assign pair-specific profit
#'   values.
#' }
#'
#' When action-level profit is supplied, the same profit value is assigned to
#' all feasible planning units for that action. When pair-specific profit is
#' supplied, only the listed \code{(pu, action)} pairs receive explicit values;
#' unmatched feasible pairs are interpreted as zero-profit pairs.
#'
#' \strong{Storage behaviour}
#'
#' This function stores only rows with non-zero profit values. Feasible pairs
#' whose final profit is zero are omitted from \code{x$data$dist_profit}.
#' Missing values produced during matching or joins are treated as zero before
#' this filtering step.
#'
#' \strong{Data-only behaviour}
#'
#' This function is purely data-oriented. It does not build or modify the
#' optimization model, and it does not change feasibility. It simply assigns
#' profit values to rows already present in \code{x$data$dist_actions}.
#'
#' In particular:
#' \itemize{
#'   \item it does not add new feasible \code{(pu, action)} pairs,
#'   \item it does not remove infeasible pairs,
#'   \item it does not apply solver-side filtering such as dropping locked-out
#'   decisions.
#' }
#'
#' Any such filtering is expected to occur later when model-ready tables are
#' prepared, typically during the build stage invoked by \code{solve()}.
#'
#' \strong{Use in optimization}
#'
#' Profit values stored by this function can later be used in objectives such as
#' \code{\link{add_objective_max_profit}} or
#' \code{\link{add_objective_max_net_profit}}, in derived budget expressions, or
#' in reporting and summary functions.
#'
#' For example, if \eqn{x_{ia} \in \{0,1\}} denotes whether action \eqn{a} is
#' selected in planning unit \eqn{i}, then a profit-maximization objective
#' typically takes the form
#' \deqn{
#' \max \sum_{(i,a) \in \mathcal{F}} \pi_{ia} x_{ia}.
#' }
#'
#' @param x A \code{Problem} object created with \code{\link{input_data}}. It
#'   must already contain \code{x$data$dist_actions} and \code{x$data$actions};
#'   run \code{\link{add_actions}} first.
#'
#' @param profit Profit specification. One of:
#' \itemize{
#'   \item \code{NULL}: profit is set to 0 for all feasible
#'   \code{(pu, action)} pairs,
#'   \item a numeric scalar: recycled to all feasible pairs,
#'   \item a named numeric vector: names are action ids and values define
#'   action-level profit,
#'   \item a \code{data.frame(action, profit)} defining action-level profit,
#'   \item a \code{data.frame(pu, action, profit)} defining pair-specific
#'   profit.
#' }
#'
#' @return An updated \code{Problem} object with \code{x$data$dist_profit}
#'   created or replaced. The stored table contains columns \code{pu},
#'   \code{action}, \code{profit}, \code{internal_pu}, and
#'   \code{internal_action}, and includes only rows with non-zero profit.
#'
#' @examples
#' # Minimal problem
#' pu <- data.frame(
#'   id = 1:3,
#'   cost = c(2, 3, 1)
#' )
#'
#' features <- data.frame(
#'   id = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3),
#'   feature = c("sp1", "sp2", "sp1", "sp2"),
#'   amount = c(1, 2, 1, 3)
#' )
#'
#' p <- input_data(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_actions(
#'   x = p,
#'   actions = data.frame(id = c("harvest", "restoration"))
#' )
#'
#' # 1) Constant profit for every feasible (pu, action)
#' p1 <- add_profit(p, profit = 10)
#' p1$data$dist_profit
#'
#' # 2) Profit per action using a named vector
#' pr <- c(harvest = 50, restoration = -5)
#' p2 <- add_profit(p, profit = pr)
#' p2$data$dist_profit
#'
#' # 3) Profit per action using a data frame
#' pr_df <- data.frame(
#'   action = c("harvest", "restoration"),
#'   profit = c(40, 15)
#' )
#' p3 <- add_profit(p, profit = pr_df)
#' p3$data$dist_profit
#'
#' # 4) Profit per (pu, action) pair
#' pr_pair <- data.frame(
#'   pu = c(1, 2, 3),
#'   action = c("harvest", "harvest", "restoration"),
#'   profit = c(100, 80, 30)
#' )
#' p4 <- add_profit(p, profit = pr_pair)
#' p4$data$dist_profit
#'
#' @seealso
#' \code{\link{add_actions}},
#' \code{\link{add_objective_max_profit}},
#' \code{\link{add_objective_max_net_profit}},
#' \code{\link{add_effects}}
#'
#' @export
add_profit <- function(
    x,
    profit = NULL
) {

  # ---- checks: x
  assertthat::assert_that(!is.null(x), msg = "x is NULL")
  assertthat::assert_that(!is.null(x$data), msg = "x does not look like a mosap Problem object")
  assertthat::assert_that(!is.null(x$data$pu), msg = "x$data$pu is missing. Run input_data() first.")
  assertthat::assert_that(!is.null(x$data$dist_actions), msg = "No actions found. Run add_actions() first.")
  assertthat::assert_that(!is.null(x$data$actions), msg = "No action catalog found. Run add_actions() first.")

  x <- .pa_clone_data(x)
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

  # base skeleton: all (pu, action) pairs currently in dist_actions
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
    base$profit[is.na(base$profit)] <- 0

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
      if (anyDuplicated(p$action)) {
        stop("profit (action,profit) must have unique action rows.", call. = FALSE)
      }

      base <- dplyr::left_join(base, p, by = "action", suffix = c("", ".new"))
      if ("profit.new" %in% names(base)) {
        base$profit.new[is.na(base$profit.new)] <- 0
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
        base$profit.new[is.na(base$profit.new)] <- 0
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
  base$profit[is.na(base$profit)] <- 0

  if (!all(is.finite(base$profit))) stop("profit values must be finite.", call. = FALSE)

  # store only non-zero rows
  base <- base[base$profit != 0, , drop = FALSE]

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
