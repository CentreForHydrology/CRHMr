#' Converts air temperature and vapour pressure to relative humidity
#'
#' @description This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking. Use the function \code{changeEatoRH} instead.
#' @param airtemp Required. Air temperature in \eqn{^\circ}{ }C.
#' @param vapourPressure Required. Vapour pressure in kPa.
#'
#' @return Returns RH in percent.
#' @author Kevin Shook
#' @seealso  \code{\link{changeEatoRH}}
#' @export
#'
#' @examples
#' vp2rh(-5, 0.2007355)
vp2rh <- function(airtemp, vapourPressure){
  # converts air temp (C) and vapour pressure (kPa) to rh (percent) 
  estar <- rh2vp(airtemp, 100)
  rh <- 100 * (vapourPressure/estar)
  
  # set limits
  rh <- pmax(rh, 0)
  rh <- pmin(rh, 100)
  return(rh)
}
