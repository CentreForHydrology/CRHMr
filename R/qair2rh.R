#' Converts specific humidity to relative humidity
#' @description From Bolton 1980 The computation of Equivalent Potential Temperature
#' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}.
#' @title qair2rh
#' @param qair Required. Specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass.
#' @param temp Required. Air temp in degrees C.
#' @param press Optional. Air pressure in Pa. Default is 101325, the standard atmospheric air pressure.
#' @param allowStupid Optional. If \code{TRUE} then impossible values (smaller than zero or
#' greater than 1) will be allowed. The default value, \code{FALSE}, restricts the
#' returned value to be between 0 and 1.
#' @return If successful, returns rh (relative humidity), the ratio of actual water
#' mixing ratio to saturation mixing ratio. Note that this value is a fraction (i.e.
#' between 0 and 1) rather than a percentage. If unsuccessful, returns the value \code{FALSE}.
#' @export
#' @author David LeBauer. Modified by Kevin Shook for air temps < 0 \eqn{^\circ}{ }C and to work for vectors or scalars.
#' @seealso  \code{\link{qair2ea}}
#' @references \url{https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/R/metutils.R}.
#' @examples qair2rh(0.0001, 10)
qair2rh <- function(qair, temp, press = 101325, allowStupid = FALSE) {

  # check parameter values
  if (length(qair)  == 0) {
    cat('Error: missing humidities')
    return(FALSE)
  }

  if (length(temp) == 0) {
    cat('Error: missing air temperatures')
    return(FALSE)
  }
  press <- press * 0.01                           # Pa -> mb
  es <- saturatedVP(temp)                         # kPa
  es <- es * 1000                                 # kPa -> Pa
  e <- qair * press / (0.378 * qair + 0.622)      # mb
  ea <- e * 100                                   # mb -> Pa
  rh <- ea / es
  if (!allowStupid) {
    rh[rh > 1] <- 1
    rh[rh < 0] <- 0
  }

  return(rh)
}
