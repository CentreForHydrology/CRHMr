#' Converts specific humidity to vapour pressure.
#'
#' @param qair Required. Specific humidity (dimensionless).
#' @param press Optional. Air pressure in Pa. Default is 101325, the standard atmospheric air pressure.
#' @return If successful, returns the vapour pressure in Pa. If unsuccessful, returns the value \code{FALSE}.
#' @export
#' @author Kevin Shook
#' @seealso  \code{\link{qair2rh}}
#' @references R code for conversion of air pressure and absolute humidity was taken
#' from project PEcAn The Predictive Ecosystem Analyzer \url{http://pecanproject.github.io}.
#' The source code is available at \url{https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/R/metutils.R.}
#' @examples ea <- qair2ea(0.0001, 101325)

qair2ea <- function(qair, press=101325) {

  # check parameter values
  if (length(qair)  == 0) {
    cat('Error: missing humidities')
    return(FALSE)
  }

  if (length(press) == 0) {
    cat('Error: missing surface pressures')
    return(FALSE)
  }

  press <- press * 0.01                           # Pa -> mb
  e <- qair * press / (0.378 * qair + 0.622)      # mb
  ea <- e * 0.1                                   # convert mb back to kPa
  return(ea)
}
