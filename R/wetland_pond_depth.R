#' Calculates Hayashi-van der Kamp maximum depth of water in a Prairie pond
#'
#' @param pond_volume Required. Volume of water in ponds (in m\eqn{^3}{^3}). May be a vector of values.
#' @param p Required. Value of \code{p} exponent for ponds. May be a vector of values.
#' @param s Required. Value of \code{s} parameter (pond area for a depth of 1 m) in m\eqn{^2}{^2}) for pond.
#' May be a vector of values.
#'
#' @return If successful, returns a vector of the pond maximum depths, for the given volumes.
#' If unsuccessful, returns \code{FALSE}.
#' @export
#' @author Kevin Shook
#' @family wetland_functions
#' @references \cite{Hayashi, M., and G. van der Kamp (2000), Simple equations to represent the volume–area–depth relations
#' of shallow wetlands in small topographic depressions, J. Hydrol., 237(1–2), 74–85, doi:10.1016/S0022-1694(00)00300-0.}
#'
#' @examples wetland_pond_depth(100, 1.72, 100)
wetland_pond_depth <- function(pond_volume = NULL, p = NULL, s = NULL){
  if(is.null(pond_volume)) {
    cat('Error: pond_volume is missing\n')
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

  depth <- (pond_volume*(1.0+2.0/p)/s) ^ (1.0/(1.0+2.0/p))
  return(depth)
}
