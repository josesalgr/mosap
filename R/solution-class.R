#' @include internal.R
#'
#' @export
if (!methods::isClass("Solution")) methods::setOldClass("Solution")
NULL

#' Solution class
#'
#' @description
#' The `Solution` class stores the output of solving a `Problem` object in
#' `mosap`. It contains the optimization status, objective value, solver
#' metadata, decoded decision vectors, and human-readable result tables.
#'
#' Objects of this class are typically created with [solve()].
#'
#' @section Fields:
#' \describe{
#'   \item{data}{A named `list` containing optimization outputs, such as the
#'   objective value, raw solution vector, optimality gap, runtime, solver
#'   arguments, decoded decision vectors, and result tables.}
#'   \item{Problem}{The `Problem` object used to generate the solution.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{print()}{Print a concise summary of the solution, including solver
#'   status, objective value, runtime, selection summary, and target fulfillment.}
#'   \item{show()}{Alias of `print()`.}
#'   \item{repr()}{Returns a short string representation.}
#' }
#'
#' @return No return value.
#'
#' @name solution-class
#' @aliases Solution
NULL

# internal helper
.pa_solution_status_class <- function(status_txt) {
  status_txt <- as.character(status_txt %||% "unknown")[1]

  if (status_txt %in% c("optimal")) {
    return("ok")
  }

  if (status_txt %in% c("time_limit_feasible", "solution_limit")) {
    return("warn")
  }

  if (status_txt %in% c("infeasible_or_unbounded", "time_limit_no_solution", "unknown")) {
    return("bad")
  }

  "muted"
}

# internal helper
.pa_solution_status_inline <- function(status_txt) {
  cls <- .pa_solution_status_class(status_txt)
  paste0("{.", cls, " ", status_txt, "}")
}

# internal helper
.pa_pct_text <- function(x, digits = 3) {
  if (!is.numeric(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  paste0(round(100 * x, digits), "%")
}

# internal helper
.pa_num_text <- function(x, digits = 6) {
  if (!is.numeric(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  format(round(x, digits), trim = TRUE, scientific = FALSE)
}

# internal helper
.pa_n_of_total_text <- function(n, total) {
  if (is.null(n) || length(n) == 0 || is.na(n)) n <- 0L
  if (is.null(total) || length(total) == 0 || is.na(total)) {
    return(as.character(n))
  }
  paste0(n, " of ", total)
}

# internal helper
.pa_solution_alias_values_summary <- function(self, max_show = 6L) {
  av <- self$data$alias_values %||% NULL

  if (is.null(av) || length(av) == 0) {
    return(NULL)
  }

  av <- unlist(av, use.names = TRUE)
  nm <- names(av) %||% rep("objective", length(av))

  if (length(av) > max_show) {
    keep <- seq_len(max_show)
    txt <- paste0(
      nm[keep], ": ", vapply(av[keep], .pa_num_text, character(1)),
      collapse = ", "
    )
    txt <- paste0(txt, ", ...")
  } else {
    txt <- paste0(
      nm, ": ", vapply(av, .pa_num_text, character(1)),
      collapse = ", "
    )
  }

  txt
}

#' @export
Solution <- pproto(
  "Solution",
  data = list(),
  Problem = NULL,
  name = "sol",

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    cli::cli_text("A mosap solution ({.cls Solution})")

    obj <- self$data$objval %||% NA_real_
    gap <- self$data$gap %||% NA_real_
    rt  <- self$data$runtime %||% NA_real_
    st  <- getStatus(self)

    tb <- self$data$tables %||% list()
    sa <- self$data$args %||% list()
    pr <- self$Problem %||% NULL

    # ---- totals from problem
    n_pu_total <- if (!is.null(pr) && !is.null(pr$data$pu) && inherits(pr$data$pu, "data.frame")) {
      nrow(pr$data$pu)
    } else {
      NA_integer_
    }

    n_act_total <- if (!is.null(pr) && !is.null(pr$data$dist_actions) && inherits(pr$data$dist_actions, "data.frame")) {
      nrow(pr$data$dist_actions)
    } else {
      NA_integer_
    }

    n_tgt_total <- if (!is.null(tb$targets) && inherits(tb$targets, "data.frame")) {
      nrow(tb$targets)
    } else {
      NA_integer_
    }

    # ---- selected counts
    n_pu_sel <- if (!is.null(tb$pu) && inherits(tb$pu, "data.frame") && "selected" %in% names(tb$pu)) {
      sum(tb$pu$selected %in% 1L, na.rm = TRUE)
    } else {
      NA_integer_
    }

    n_act_sel <- if (!is.null(tb$actions) && inherits(tb$actions, "data.frame") && "selected" %in% names(tb$actions)) {
      sum(tb$actions$selected %in% 1L, na.rm = TRUE)
    } else {
      NA_integer_
    }

    n_tgt_met <- if (!is.null(tb$targets) && inherits(tb$targets, "data.frame") && "gap" %in% names(tb$targets)) {
      sum(tb$targets$gap >= 0, na.rm = TRUE)
    } else {
      NA_integer_
    }

    alias_txt <- .pa_solution_alias_values_summary(self)

    # ---- RESULT
    cli::cli_text("{ch$j}{ch$b}{.h result}", .envir = environment())
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}status:          {.eval .pa_solution_status_inline(st)}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}objective value: {.strong {.val { .pa_num_text(obj) }}}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}gap:             { .pa_pct_text(gap) }",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$l}{ch$b}runtime:         { .pa_num_text(rt, digits = 3) } sec",
      .envir = environment()
    )

    # ---- SELECTION
    cli::cli_text("{ch$l}{ch$b}{.h selection}", .envir = environment())

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}planning units:  { .pa_n_of_total_text(n_pu_sel, n_pu_total) } selected",
      .envir = environment()
    )

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}actions:         { .pa_n_of_total_text(n_act_sel, n_act_total) } selected",
      .envir = environment()
    )

    if (!is.na(n_tgt_total)) {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}targets met:     { .pa_n_of_total_text(n_tgt_met, n_tgt_total) }",
        .envir = environment()
      )
    } else {
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}targets met:     {.muted none}",
        .envir = environment()
      )
    }

    # ---- OBJECTIVE VALUES
    if (!is.null(alias_txt)) {
      cli::cli_text("{ch$l}{ch$b}{.h objective values}", .envir = environment())
      cli::cli_text(
        " {ch$v}{ch$l}{ch$b}{alias_txt}",
        .envir = environment()
      )
    }

    # ---- SOLVER
    cli::cli_text("{ch$l}{ch$b}{.h solver}", .envir = environment())

    solver_txt <- as.character(sa$solver %||% "unknown")[1]
    cores_txt  <- as.character(sa$cores %||% NA)[1]
    tl_txt     <- as.character(sa$timelimit %||% NA)[1]

    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}name:            {.code {solver_txt}}",
      .envir = environment()
    )
    cli::cli_text(
      " {ch$v}{ch$j}{ch$b}cores:           {cores_txt}",
      .envir = environment()
    )
    cli::cli_text(
      " {ch$v}{ch$l}{ch$b}time limit:      {tl_txt}",
      .envir = environment()
    )

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(
      cli::col_grey(
        paste0("# ", info_sym, " Use {.code x$data$tables} to inspect decoded solution tables.")
      )
    )

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    st <- tryCatch(getStatus(self), error = function(e) "unknown")
    obj <- self$data$objval %||% NA_real_
    paste0("<Solution> status=", st, ", objective=", .pa_num_text(obj))
  }
)
