#' Converts dew point temperature and ambient temperature to RH
#' 
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. Note that it does \emph{NO} data checking.
#' @param t Required. Air temperature in \eqn{^\circ}{ }C.
#' @param td Required. Dew point temperature in \eqn{^\circ}{ }C.
#'
#' @return Returns the RH as a percentage.
#' @author Kevin Shook
#' @references Lawrence, M. 2005. The relationship between relative humidity and the dewpoint temperature in moist air. American Meteorological Society: 225-233. \url{http://andrew.rsmas.miami.edu/bmcnoldy/Humidity.htm}
#' @export
#'
#' @examples convertDPtoRH(10, 9)
convertDPtoRH <- function(t, td){
  
  A <- 17.625
  B <- 243.04 #C
  
  RH <- 100*(exp((17.625*td)/(243.04+td))/exp((17.625*t)/(243.04+t))) 
  
  return(RH)
}