#' @include internal.R
NULL

#' Show
#'
#' Display information about an object.
#'
#' @param x Any object.
#'
#' @seealso [methods::show()].
#'
#' @name show
#'
#' @aliases show,Data-method show,OptimizationProblem-method show,Solution-method show,Portfolio-method show
NULL

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Problem}(x)
#'
methods::setMethod("show", "Problem",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{SolutionSet}(x)
#'
methods::setMethod("show", "SolutionSet",
                   function(object) object$show())

