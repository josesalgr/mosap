#' @importFrom Rcpp evalCpp
#' @useDynLib mosap, .registration = TRUE
NULL

#' @title Create and solve multi-actions planning problems
#'
#' @description
#' Create and solve a multi-actions planning problem in a single call.
#' This is a convenience wrapper around:
#' \preformatted{
#' inputData() -> (optional: add_spatial_* relations) -> solve()
#' }
#'
#' @details
#' \strong{No \code{problem()}:}
#' This wrapper does not call \code{problem()}. The optimization model is expected to be
#' built and solved by \code{solve()} from the information stored in the \code{Data} object
#' (objectives, targets, relations, etc.).
#'
#' \strong{Spatial relations / boundary:}
#' \itemize{
#' \item This function does not pass \code{boundary} into \code{inputData()}.
#' \item If you provide \code{boundary}, it is interpreted as a request to register a
#'   spatial relation after \code{inputData()} and before \code{solve()}.
#' \item Supported \code{boundary} values:
#'   \itemize{
#'   \item a \code{data.frame} boundary table (e.g., columns \code{id1,id2,boundary} or \code{pu1,pu2,weight}),
#'         registered via \code{add_spatial_boundary()}.
#'   \item \code{"auto"} to derive boundary-length relations from \code{x$data$pu_sf}
#'         (requires \pkg{sf} and that \code{inputData()} stored \code{pu_sf}).
#'   }
#' }
#'
#' @param ... Arguments inherited from \code{inputData()} and \code{solve()}.
#'   In addition, this wrapper supports \code{boundary} as described above.
#'
#' @name mosap
#'
#' @return An object of class [solution-class].
#'
#' @examples
#' \donttest{
#' set.seed(14)
#'
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#'      sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#'      sim_boundary_data)
#'
#' ## One-shot solve (tabular), registering a boundary relation from a table
#' s <- mosap(
#'   pu = sim_pu_data,
#'   features = sim_features_data,
#'   dist_features = sim_dist_features_data,
#'   threats = sim_threats_data,
#'   dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data,
#'   boundary = sim_boundary_data,
#'   model_type = "minimizeCosts",
#'   time_limit = 50,
#'   output_file = FALSE,
#'   cores = 2
#' )
#' print(s)
#'
#' ## Spatial example (conceptual): boundary derived from PU polygons stored by inputData()
#' ## s2 <- mosap(
#' ##   pu = pu_sf, features = feat_r, cost = cost_r,
#' ##   boundary = "auto",
#' ##   model_type = "minimizeFragmentation",
#' ##   time_limit = 60
#' ## )
#' }
#'
#' @rdname mosap
#' @export
mosap <- function(...) {

  params = list(...)

  params_data <- c(names(formals(inputData)), "sensitivity", "boundary")
  params_model <- names(formals(problem))
  params_solve <- names(formals(solve))

  #verifying input parameters
  if(!all(names(params) %in% c(params_data, params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }

  #Creating and solving mathematical model--------------------------------------------------

  conservation_model <- do.call(inputData, args = params[names(params) %in% params_data])
  #conservation_model <- inputData(params[names(params) %in% params_model])

  optimization_model <- do.call(problem, args = append(x = conservation_model,
                                                          params[names(params) %in% params_model]))

  solution <- do.call(solve, args = append(x = optimization_model,
                                           params[names(params) %in% params_solve]))

  solution
}
