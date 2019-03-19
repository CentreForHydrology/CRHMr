#' Calculates Hayashi-van der Kamp parameters for a Prairie depressions
#'
#' @description Computing pond area and depth for Prairie depressions from the Hayashi-van der Kamp method
#' requires values of the parameters h and s. Unfortunately, these values can't be solved explicitly
#' from the pond max area and volume. This function finds the values of the parameters by iterative solution.
#' @param max_volume Required. Maximum volume of a pond in m\eqn{^3}{^3}.
#' @param max_area Required. Maximum area of a pond in m\eqn{^2}{^2}.
#' @param p Required. Scaling exponent for pond depth-area relationshp. See reference.
#' @param max_iterations Optional. Maximum number of iterations for solution. Default is 100.
#' @param max_error Optional. Maximum error for estimation of parameters, as a fraction of \code{max_area}.
#' @param return_stats If \code{TRUE}, then the error of solution and the number of iterations
#' will also be returned. Default is \code{FALSE}
#' Default is 0.001.
#'
#' @return If successful, returns a vector of the parameters \code{h} (the maximum pond depth) in m and
#' \code{s} (the pond area for a depth of 1 m) in m\eqn{^2}{^2}). Optionally returns the error and number of iterations
#' required for solution. If unsuccessful, returns \code{FALSE}.
#' @export
#' @author Kevin Shook
#' @family wetland_functions
#' @references \cite{Hayashi, M., and G. van der Kamp (2000), Simple equations to represent the volume–area–depth relations
#' of shallow wetlands in small topographic depressions, J. Hydrol., 237(1–2), 74–85, doi:10.1016/S0022-1694(00)00300-0.}
#' @examples
#' wetland_h_s(5000, 500, 1.72)
#'
wetland_h_s <- function(max_volume, max_area, p, max_iterations = 100, max_error = 0.0001, return_stats = FALSE) {

  done <- FALSE
  iterations <- 0

  # check for missing parameters
  if (is.null(max_volume)) {
    cat('Error: max_volume is missing\n')
    return(FALSE)
  }

  if (is.null(max_area)) {
    cat('Error: max_area is missing\n')
    return(FALSE)
  }

  if (is.null(p)) {
    cat('Error: p is missing\n')
    return(FALSE)
  }

  h <- max_volume / max_area        # initial guess
  while (!done) {
    s <- max_area / (h ^ (2.0 / p))
    h <-  ((max_volume * (1.0 + 2.0 / p)) / s) ^ (1.0 / (1.0 + 2.0/p))

    est_area <-  s  * (h ^ (2.0 / p))
    est_volume <- s / ( 1+ (2/p)) * h ^ (1 + (2/p))

    area_error = abs(est_area - max_area) / max_area
    volume_error = abs(est_volume - max_volume) / max_volume
    if (((area_error < max_error)  & (volume_error < max_error))| (iterations > max_iterations))
      done <- TRUE
    else
      iterations <- iterations + 1
  }


  if (return_stats)
    return_value <- c(h, s, area_error, iterations)
  else
    return_value <- c(h, s)

  return(return_value)
}
