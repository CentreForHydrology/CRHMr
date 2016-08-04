#' Estimates extra-terrestrial shortwave radiation
#'
#' @description Simple estimation of extra-terrestrial radiation on a horizontal plane. Daily and sub-daily values in MJ/m\eqn{^2}{^2} and W/m\eqn{^2}{^2} are provided,
#' @param datetime Required. Can either be a vector of \pkg{CRHMr} datetimes or any dataframe with a \code{datetime} in column 1.
#' @param latitude Required. The latitude for which values are to calculated.
#' @param hoursOffset Optional. The offset (in hours) is added to the solar time to convert it to local time. The default value \option{2} shifts the daily peak to occur at 2pm.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns a dataframe containing these variables:
#' \enumerate{
#'    \item \code{datetime}
#'    \item \code{So_MJ}, the daily extra-terrestrial radiation in MJ/m\eqn{^2}{^2}
#'    \item \code{So_W}, the daily extra-terrestrial radiation in W/m\eqn{^2}{^2}
#'    \item \code{R_h_d}, the conversion from daily to hourly fluxes
#'    \item \code{So_h_W}, the sub-daily extra-terrestrial radiation in W/m\eqn{^2}{^2}
#'    \item \code{So_h_MJ}, the sub-daily extra-terrestrial radiation in MJ/m\eqn{^2}{^2}}
#'If unsuccessful, the value \code{FALSE} will be returned.
#' @author Kevin Shook
#' @references This code is based on Shook, K., and J. Pomeroy (2011), \dQuote{Synthesis of incoming shortwave radiation for hydrological simulation}, Hydrol. Res., 42(6), 433, doi:10.2166/nh.2011.074. \cr Please cite this paper if you use this function in a publication.
#' @note This version is valid for latitudes between 49 \eqn{^\circ}{ }N and 55\eqn{^\circ}{ }N. It is not known how well it will perform outside this range.
#' @export
#'
#' @examples 
#' maxSolar <- simpleMaxSolar(BadLake7376, 51.366)
simpleMaxSolar <- function(datetime, latitude, hoursOffset=2, quiet=TRUE, logfile=''){
    if (latitude == 0){
    cat('Error: missing latitude\n')
    return(FALSE)    
  }
  
  datetimeName <- deparse(substitute(datetime))
  
  # check type of data
  if (!is.null(ncol(datetime)))
    datetime <- datetime[,1]
  
  # find interval length
  dt <- timestep.hours(datetime[2], datetime[1])
  
  
  if (length(datetime) == 0){
    cat('Error: no datetime values\n')
    return(FALSE)    
  }
    
  Io <- 117.500 # MJ/m2/d
  
  datetimeShifted <- datetime - hoursOffset * 3600
  daynum <- as.numeric(format(datetimeShifted, format='%j'))
  hournum <- as.numeric(format(datetimeShifted, format='%H')) + 
    as.numeric(format(datetimeShifted, format='%M') )/ 60 

  delta <- 0.4093 * sin((daynum - 81) * 2 * pi / 365)
  #delta <- 0.4102 * sin(2*pi/365*(daynum-80))
  
  phi <- latitude *  pi / 180

  # get extraterrestrial radiation
  term1 <- acos(-tan(phi)*tan(delta))*sin(phi)*sin(delta)
  term2 <- cos(phi)*cos(delta)*sin(acos(-tan(delta)*tan(phi)))
  So <- (term1 + term2) * (Io / pi)
  
  # get hourly radiation factors
  # first, get coefficients
  c3 <- (8.840749e-5 * daynum^2) - (0.030195 * daynum) + 5.33384
  
  # find value for day #304
  c3_304 <- (c3[daynum==304])
  c3_304 <- c3_304[1]
  
  # apply to days between 305 & 366
  c3[(daynum >=305) & (daynum <= 366)] <- c3_304
  
  c2 <- -0.963832 * c3 + 7.86346
  c1 <- -0.658542 * c2 + 5.18605

  R_h_d <- sin(c1 * pi *(hournum / 24) + c2) * c3
  R_h_d <- pmax(R_h_d, 0)
  
  # set erroneous values to zero
  R_h_d[(hournum <= 3) | (hournum >=22)] <- 0
  
  # set missing values to zero
  R_h_d[is.na(R_h_d)] <- 0
  
  # convert to W/m2
  So_W <- So * (1e6/(24*3600))
  
  # now calculate hourly values
  So_h_W <- So_W * R_h_d
  So_h_MJ <- So_h_W * dt * 3600 / 1e6 
  
  # constuct output dataframe
  outvals <- data.frame(datetime, So, So_W, R_h_d, So_h_W, So_h_MJ)
  names(outvals)[2] <- 'So_MJ'
  
  # output log files
  outvals.info <- CRHM_summary(outvals)
  if (!quiet)
    print(outvals.info)
  
  comment <- paste('simpleMaxSolar datetime:', datetimeName, 
                   ' latitude:', latitude, sep='')  
  
  result <- logAction(comment, logfile)
  
   if (result)
    return (outvals)
  else
    return(result)

}