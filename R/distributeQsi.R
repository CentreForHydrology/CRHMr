#' Distributes incoming shortwave radiation to shorter time interval
#' @name distributeQsi
#' @description Downscales incoming shortwave radiation to shorter time intervals, based on
#' the extra-terrestrial radiation. The mean atmospheric transmittance is computed from the
#' ratio of the incoming shortwave to the extra-terrestrial radiation over the original time
#' intervals. The extra-terrestrial radiation over the shorter time intervals is then multiplied
#' by the mean transmittance values to calculate the shorter interval incoming shortwave radiation.
#' @param QsiObs Required. A \pkg{CRHMr} obs data frame containing incoming shortwave radiation
#' in in W/m\eqn{^2}{^2}.
#' @param QsiColnum Optional. The number of the columns (not including the datetime) containing
#' the incoming shortwave values. Default is \code{1}.
#' @param latitude Required. The latitude for which values are to be calculated.
#' @param sunTimeOffset Optional. The offset (in hours) is added to the solar time to convert it
#' to local time. The default value \option{2} shifts the daily peak to occur at 2pm.
#' @param timeStep Optional. The number of hours to be used as the downscaled time step. Default
#' is \code{1}.
#' @param solarMethod The method to be used for calculating the extra-terrestrial radiation. The
#' default method is \option{simpleMaxSolar}. Note that this method is only valid for latitudes
#' between 49 and 55\eqn{^\circ}{ }N. The other supported method is \option{PotSolarInst}, which
#' requires the package \pkg{EcoHydRology} to be installed.
#' @param details Optional. If \code{TRUE} then the calculated atmospheric transmittance and
#' extra-terrestrial radiation (\code{So_h_W}) will be returned along with the downscaled Qsi.
#' If \code{FALSE} (the default) then only the Qsi values and the datetime are returned.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling
#' this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used
#' @return If successful, returns an obs data frame containing the downscaled incoming shortwave
#' radiation (Qsi) in W/m\eqn{^2}{^2} and, optionally, the transmittance and extra-terrestrial
#' solar radiation (\code{So_h_W}). If unsuccessful, returns an error.
#' @author Kevin Shook
#' @seealso \code{\link{simpleMaxSolar}} \code{\link{distributeQli}}
#' @importFrom stringr str_to_lower
#' @importFrom EcoHydRology PotSolarInst
#' @export
#' @examples \dontrun{
#' hourlyShortwave <- distributeQsi(QsiObs=SWobs, QsiColnum=1, latitude=51.1, timeStep=1)}
#'
distributeQsi <- function(QsiObs, QsiColnum=1, latitude='', sunTimeOffset=2, timeStep=1,
                          solarMethod='simpleMaxSolar', details=FALSE, quiet=TRUE, logfile=''){
  # check parameters
  if (timeStep < 1) {
    stop('Time step must be specified and >= 1 hour')
  }
  if (nrow(QsiObs) == 0){
    stop('Missing Qsi values')
  }
  if (latitude == ''){
    stop('Must specify latitude')
  }

  QsiObsName <- deparse(substitute(QsiObs))
  if (QsiObsName == ''){
    cat('Error: must specify Qsi dataframe')
  }

  QsiVarcol <- QsiColnum + 1
  QsiObs <- QsiObs[,c(1, QsiVarcol)]
  names(QsiObs) <- c('datetime', 'Qsi')

  datetime <- QsiObs[,1]

  # figure out interval from first 2 values
  first.datetime <- datetime[1]
  second.datetime <- datetime[2]
  last.datetime <- datetime[length(datetime)]
  QsiInterval <- timestep.hours(second.datetime, first.datetime)

  # set back by 1 interval
  downscaled.first.datetime <- first.datetime - (QsiInterval - timeStep) * 3600

  # repeat Qsi values
  repeatRatio <- QsiInterval / timeStep


  # now get corresponding datetimes
  downscaledDatetime <- seq(from=downscaled.first.datetime,
                            to=last.datetime, by=(timeStep*3600))


  # get hourly max (extra-terrestrial) radiation
  if(str_to_lower(solarMethod) == 'simplemaxsolar'){
    downscaleMax <-  simpleMaxSolar(downscaledDatetime, latitude, sunTimeOffset)
    downscaleMax <- downscaleMax[,c('datetime', 'So_h_W')]
  }
  else if (str_to_lower(solarMethod) == 'potsolarinst'){
    # use EcoHydRology
    if (!requireNamespace("EcoHydRology", quietly = TRUE)) {
      stop("EcoHydRology is needed for this function to work. Please install it.",
           call. = FALSE)
    return(FALSE)
    }

    # use PotSolarInst function
    Jday <- as.numeric(format(downscaledDatetime, format='%j'))
    hour <- as.numeric(format(downscaledDatetime, format='%H'))
    So_h_W <- PotSolarInst(Jday, hour, lat=latitude, latUnits='degrees',
                                 SolarNoon=(12+sunTimeOffset), units='Wm2')
    downscaleMax <- data.frame(downscaledDatetime, So_h_W)
    names(downscaleMax)[1] <- 'datetime'
  }
  else{
    stop('Missing or incorrect solarMethod')
  }


  # now calculate the mean solar max and transmittance

  meanDatetime <- QsiObs$datetime
  # repeat the values
  meanDatetime <- rep(meanDatetime, each=repeatRatio)
  # add to downscaleMax
  downscaleMax$meanDatetime <- meanDatetime

  # aggregate
  meanMax <- aggregate(downscaleMax$So_h_W, by=list(downscaleMax$meanDatetime),
                       FUN='mean')
  names(meanMax) <- c('datetime', 'meanMax')

  # merge and calculate transmittance
  merged <- merge(meanMax, QsiObs, by='datetime')
  merged$transmittance <- 0
  merged$transmittance[merged$meanMax > 0] <- merged$Qsi[merged$meanMax > 0] /
                         merged$meanMax[merged$meanMax > 0]

  merged$transmittance <- pmin(pmax(merged$transmittance, 0), 1)
  # repeat transmittance
  meanTransmittance <- rep(merged$transmittance, each=repeatRatio)

  trans <- data.frame(downscaledDatetime, meanTransmittance)
  names(trans) <- c('datetime', 'transmittance')

  # repeat original
  originalQsi <- rep(merged$Qsi, each=repeatRatio)
  original <- data.frame(downscaledDatetime, originalQsi)
  names(original) <- c('datetime', 'originalQsi')

  # add to max values
  downscaled <- merge(downscaleMax, trans, by='datetime')

  # calculate Qsi
  downscaled$Qsi <- downscaled$So_h_W * downscaled$transmittance
  if (!details)
    downscaled <- downscaled[,c('datetime', 'Qsi')]
  else{
    downscaled <- downscaled[,c('datetime', 'Qsi', 'So_h_W', 'transmittance')]
    downscaled <- merge(downscaled, original, by='datetime')
    }


  # output log files
  obs.info <- CRHM_summary(downscaled)
  if (!quiet)
    print(obs.info)

  comment <- paste('distributeQsi Qsi_dataframe:', QsiObsName,
                   ' solarMethod:', solarMethod,
                   ' latitude:', latitude,
                   ' sunTimeOffset:', sunTimeOffset,
                   sep='')

  result <- logAction(comment, logfile)

  if (result)
    return (downscaled)
  else
    return(result)
}
