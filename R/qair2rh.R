##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer \url{https://github.com/PecanProject/pecan/blob/master/modules/data.atmosphere/R/metutils.R}. Modified by Kevin Shook for air temps < 0 \eqn{^\circ}{ }C.
qair2rh <- function(qair, temp, press = 1013.25) {
  if (temp <= 0)
    es <- 0.611 * exp((21.88 * temp) / (temp + 265.5))
  else
    es <- 0.611 * exp((17.27 * temp) / (temp + 237.3))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
} # qair2rh
