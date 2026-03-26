#' @include internal.R
#' @include problem-class.R
#' @include solution-class.R
#' @include solutionset-class.R
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
#' @aliases show,Problem-method show,Solution-method show,SolutionSet-method show
#' @keywords internal
#'
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

