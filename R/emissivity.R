#' Calculates atmospheric emissivity
#' @description Calculates atmospheric emissivity from specified longwave radiation and air temperatures. This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking.
#' @param longwave Required. Longwave radiation in W/m\eqn{^2}{^2}.
#' @param tK Required. Air temperatures in K.
#'
#' @return Returns the atmospheric emissivity.
#' @export
#' @author Kevin Shook
#' @seealso \code{\link{distributeQli}} \code{\link{longwave}}
#'
#' @examples
#' emissivity(100, 293)
emissivity <- function(longwave, tK){
  stefanBoltzmann <- 5.670367e-8 #W m-2 K-4
  e <- longwave / (stefanBoltzmann * tK ^ 4)
  return(e)
}