#' @include internal.R
NULL

#' @title Print
#'
#' @description Displays information about an object.
#'
#' @param x Any object.
#' @param ... Not used.
#'
#' @name print
#'
#' @return None.
#'
#' @seealso [base::print()].
#'
#' @aliases print
#' @keywords internal

NULL

#' @rdname print
#' @method print Problem
#' @export
print.Problem <- function(x, ...) x$print()


#' @rdname print
#' @method print Solution
#' @export
print.Solution <- function(x, ...) x$print()

#' @rdname print
#' @method print SolutionSet
#' @export
print.SolutionSet <- function(x, ...) x$print()
