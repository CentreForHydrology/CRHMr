#' Distributes incoming longwave radiation to shorter time interval 
#' @name distributeQli
#' @description Downscales incoming longwave radiation to shorter time intervals, based on the air temperatures, using the Stefan-Boltzmann law \eqn{Q_{li}=\epsilon \sigma T^{4}}{}. The atmospheric emissivity \eqn{(\epsilon)}{} is calculated from the longwave radiation and the temperature at the original time intervals. The air temperatures are interpolated to the shorter intervals (if required), and are then used to calculate the incoming longwave, using the longer-interval emissivities. Finally, the downscaled longwave radiation is adjusted so that its mean values over the original time intervals are the same as the original values.
#' @param QliObs Required. A \pkg{CRHMr} obs data frame containing incoming longwave radiation in in W/m\eqn{^2}{^2}.
#' @param QliColnum Optional. The number of the columns (not including the datetime) containing the incoming longwave values. Default is \code{1}.
#' @param tObs Required. A \pkg{CRHMr} obs data frame containing air temperatures. The values may be in K or \eqn{^\circ}{}C (the function will detect and adjust).
#' @param tColnum Optional. The number of the columns (not including the datetime) containing the air temperature values. Default is \code{1}.
#' @param timeStep Optional. The number of hours to be used as the downscaled time step. Default is \code{1}.
#' @param interpolationMethod Optional. The method to be used for interpolating the air temperatures (if required). Currently supported methods are \option{linear} and \option{spline}. The default is to use linear interpolation.
#' @param maxlength Optional. The maximum gap length to be interpolated. If not specified (the default), then the maximum gap length permitted is twice the time interval of the air temperatures.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used 
#' @return If successful, returns an obs data frame containing the downscaled incoming longwave radiation (Qli) in W/m\eqn{^2}{^2}. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @export
#' @seealso \code{\link{emissivity}} \code{\link{longwave}} \code{\link{interpolate}}
#' @examples \dontrun{
#' hourlyLongwave <- distributeQli(LWobs, 1, tobs, 1)}
distributeQli <- function(QliObs, QliColnum=1, tObs, tColnum=1, timeStep=1, 
                          interpolationMethod='linear', 
                          maxlength=0, quiet=TRUE, logfile=''){
  # check parameters
  if (timeStep <= 0){
    cat('Error: time step must be specified and > 0\n')
    return(FALSE)
  }
  
  if (nrow(QliObs) == 0){
    cat('Error: missing Qli values\n')
    return(FALSE)
  }
  QliObsName <- deparse(substitute(QliObs))
  if (QliObsName == ''){
    cat('Error: must specify Qli dataframe\n')
    return(FALSE)
  }
  QliVarcol <- QliColnum + 1
  
  if (nrow(tObs) == 0){
    cat('Error: missing t values\n')
    return(FALSE)
  }
  tObsName <- deparse(substitute(tObs))
  if (tObsName == ''){
    cat('Error: must specify air temperature dataframe\n')
    return(FALSE)
  }
  tVarcol <- tColnum + 1
  
  QliObs <- QliObs[,c(1, QliVarcol)]
  names(QliObs) <- c('datetime', 'Qli')
  tObs <- tObs[,c(1, tVarcol)]
  names(tObs) <- c('datetime', 't')
  
  #convert air temps to K if required
  if(max(tObs$t) < 200)
    tObs$t <- tObs$t + 273.15
  
  # get intervals
  QliInterval <- timestep.hours(QliObs[2,1], QliObs[1,1])
  tInterval <- timestep.hours(tObs[2,1], tObs[1,1])
  
  # merge datasets to find common time interval
  merged <- merge(QliObs, tObs, by='datetime', all=TRUE)
  clean <- na.omit(merged)
  first.clean.datetime <- clean[1,1]
  last.clean.datetime <- clean[nrow(clean),1]
  clean$emissivity <- emissivity(clean$Qli, clean$t)
  
  if(tInterval > timeStep){
    if (!quiet)
      cat('Air temperatures will have to be interpolated\n')
    
    # generate time synthetic series from beginning to end
    cleanDatetime <- seq(from=first.clean.datetime, 
                     to=last.clean.datetime, by=timeStep*3600)
    

    # create dataframe and merge temps into it
    cleanDatetime <- data.frame(cleanDatetime)
    names(cleanDatetime) <- 'datetime'
    temps <- merge(cleanDatetime, tObs, by='datetime', all.x=TRUE)
    
    # interpolate air temps
    if (maxlength <= 0)
      maxInterval <- tInterval * 2
    else
      maxInterval <- maxlength
    
    interpolatedTemps <- interpolate(temps, methods=interpolationMethod, 
                                     maxlength=maxInterval, quiet=quiet,
                                     logfile=logfile)
    
    
    tObs <- interpolatedTemps
  }
  
  # repeat emissivities, so that they match the air temperatures
  repeatRatio <- QliInterval / timeStep
  repeatedEmissivities <- rep(clean$emissivity, each=repeatRatio)
  
  # calculate the appropriate datetimes
  datetimeOffset <-  (QliInterval - timeStep) / timeStep
  firstDatetime <- first.clean.datetime - (datetimeOffset * 3600)
  lastDatetime <- last.clean.datetime
  downscaledDatetime <- seq(from=firstDatetime, 
                  to=lastDatetime, by=timeStep*3600)
  
  # create the emissivity dataframe
  emissivities <- data.frame(downscaledDatetime, repeatedEmissivities)
  names(emissivities) <- c('datetime', 'emissivity')
  
  # merge the emissivities with the downscaled air temperatures
  downscaled <- merge(emissivities, tObs, by='datetime')
  
  # calculate longwave radiation
  downscaled$lw <- longwave(downscaled$emissivity, downscaled$t)
  
  # now adjust Qli values so that their means are the same as the original values
  # add the original time steps
  originalDatetime <- data.frame(downscaledDatetime, 
                                 rep(clean$datetime, each=repeatRatio))
  names(originalDatetime) <- c('datetime', 'originalDatetime')
  downscaled <- merge(downscaled, originalDatetime, by='datetime')
  meanHourlyLongwave <- aggregate(downscaled$lw, 
                                  by=list(downscaled$originalDatetime),
                                  FUN='mean')
  
  names(meanHourlyLongwave) <- c('originalDatetime', 'meanLW')
  correctionFactor <- clean$Qli / meanHourlyLongwave$meanLW
  cf <- data.frame(downscaledDatetime,
                                 rep(correctionFactor, each=repeatRatio))
  names(cf) <- c('datetime', 'correctionFactor')
  
  if (!quiet)
    cat('longwave correction factor mean  = ', mean(cf$correctionFactor),
        ' sd = ', sd(cf$correctionFactor), '\n', sep='')
  
  downscaled <- merge(downscaled, cf)
  downscaled$Qli <- downscaled$lw * downscaled$correctionFactor
  downscaled <- downscaled[,c('datetime', 'Qli')]

  # output log files
  obs.info <- CRHM_summary(downscaled)
  if (!quiet)
    print(obs.info)
  
  comment <- paste('distributeQli Qli_dataframe:', QliObsName,
                   ' t_dataframe:', tObsName,
                   ' methods:', interpolationMethod,
                   ' maxlength:', maxlength,
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  if (result)
    return (downscaled)
  else
    return(result)

}