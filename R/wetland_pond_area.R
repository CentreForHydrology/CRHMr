#' Calculates Hayashi-van der Kamp area of water in a Prairie pond
#'
#' @param pond_depth Required. Depth of water in pond (in m). May be a vector of values. Note that
#' in this context the depth refers to the deepest point in the pond.
#' @param p Required. Value of \code{p} exponent for ponds. May be a vector of values.
#' @param s Required. Value of \code{s} parameter (pond area for a depth of 1 m) in m\eqn{^2}{^2}) for ponds.
#' May be a vector of values.
#'
#' @return If successful, returns a vector of the pond areas, for the given depths.
#' If unsuccessful, returns \code{FALSE}.
#' @export
#' @author Kevin Shook
#' @family wetland_functions
#' @references \cite{Hayashi, M., and G. van der Kamp (2000), Simple equations to represent the volume–area–depth relations
#' of shallow wetlands in small topographic depressions, J. Hydrol., 237(1–2), 74–85, doi:10.1016/S0022-1694(00)00300-0.}
#'
#' @examples  wetland_pond_area(1, 1.72, 25)
wetland_pond_area <- function(pond_depth = NULL, p = NULL, s = NULL){
  if(is.null(pond_depth)) {
    cat('Error: pond_depth is missing\n')
    return(FALSE)
  }

  if(is.null(p)) {
    cat('Error: p is missing\n')
    return(FALSE)
  }

  if(is.null(s)) {
    cat('Error: s is missing\n')
    return(FALSE)
  }

  area <- s * (pond_depth) ^ (2.0/p)
  return(area)
}
