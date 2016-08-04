#' Converts air temperature and relative humidity to vapour pressure
#'
#' @description This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly, as it does \emph{NO} data checking. Use the function \code{changeRHtoEa} instead.
#' @param airtemp Required. Air temperature in \eqn{^\circ}{ }C.
#' @param rh Required. Relative humidity in percent.
#'
#' @return Returns vapour pressure in kPa. If the air temperatures are scalar, returns a scalar value. If the air temperatures are a vector, returns a vector.
#' @author Kevin Shook
#' @seealso  \code{\link{changeRHtoEa}}
#' @export
#'
#' @examples
#' rh2vp(-5, 50)
rh2vp <- function(airtemp, rh){
  if (length(airtemp) <= 1){
    # do scalar calculation
    estar <- saturatedVP(airtemp)
    vp <- (estar * rh) / 100
    vp <- vp[1]
  }
  else{
    # do values for t > 0
    estar <- saturatedVP(airtemp)
    vp <- (estar * rh) / 100
  }
  # set limits
  vp <- pmax(vp, 0)
  return(vp)
}
