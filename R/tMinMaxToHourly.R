#' Converts daily to hourly temperatures
#'
#' @description Converts daily values of tmin, tmax and (if they exist) tmean to hourly values, by spline interpolation. Only single columns of the daily temperatures can be used.
#' @param obs Required. A \pkg{CRHMr} obs dataframe.
#' @param tmin.col Optional. The number of the tmin column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmin' will be used. There must be a tmin column in the dataset
#' @param tmax.col Optional. The number of the tmax column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmax' will be used. There must be a tmax column in the dataset.
#' @param tmean.col Optional. The number of the tmax column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmean' will be searched for. This column need not be present in the dataset. If the value is set to \code{-1}, then the mean temperature will not be used in the interpolation.
#' @param tmin.time Optional. The time of day at which the minimum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{07:00}.
#' @param tmax.time Optional. The time of day at which the maximum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{15:00}.
#' @param tmean.time Optional. The time of day at which the maximum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{12:00}.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns an obs dataframe with the hourly air temperatures. If not successful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note Interpolation over intervals longer than 1 day is not allowed.
#' @seealso \code{\link{interpolate}}
#' @examples
#' \dontrun{
#' MSC.hourly <- tMinMaxToHourly(MSC.daily, timezone='etc/GMT+7')}
#' @export


tMinMaxToHourly <-
function(obs, tmin.col=0, tmax.col=0, tmean.col=0, tmin.time='07:00',
         tmax.time='15:00', tmean.time='12:00', timezone="", quiet=TRUE, logfile=""){
  # converts min, max and (optionally) mean air temps to hourly values
  # it is assumed that the data are daily.
  # sets missing precipitation values to zero
  if (nrow(obs) == 0){
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }
  obsName <- deparse(substitute(obs))

  if (tmin.time == ''){
    cat('Error: missing time for tmin\n')
    return(FALSE)
  }

  if (tmax.time == ''){
    cat('Error: missing time for tmin\n')
    return(FALSE)
  }

  # get time intervals
  tmin.datetime <- paste('2000-01-01 ', tmin.time, sep='')
  tmax.datetime <- paste('2000-01-01 ', tmax.time, sep='')
  tmin.next.datetime <- paste('2000-01-02 ', tmin.time, sep='')
  day1.interval <- timestep.hours(tmin.datetime, tmax.datetime)
  day2.interval <- timestep.hours(tmax.datetime, tmin.next.datetime)
  max.interpolation.length <- max(day1.interval, day2.interval)


  obs.names <- names(obs)
  if (tmin.col == 0){
    tmin.col <- grep("tmin", tolower(obs.names), fixed=TRUE)
    tmin.col <- tmin.col[1]
  }
  else{
    tmin.col <- tmin.col + 1
  }

  if (tmax.col == 0){
    tmax.col <- grep("tmax", tolower(obs.names), fixed=TRUE)
    tmax.col <- tmax.col[1]
  }
  else{
    tmax.col <- tmax.col + 1
  }


  if((tmin.col == 0) | (tmax.col == 0)){
    cat('Error: missing tmin/tmax values\n')
    return(FALSE)
  }

  # assign times of day to min, max, and mean values

  tmin <- obs[, c(1, tmin.col)]
  tmin$datetime <- as.POSIXct(paste(format(tmin$datetime, '%Y-%m-%d'), ' ',
                                    tmin.time, sep=''), format='%Y-%m-%d %H:%M', tz=timezone)
  tmin <- na.omit(tmin)
  names(tmin) <- c('datetime', 't')

  tmax <- obs[, c(1, tmax.col)]
  tmax$datetime <- as.POSIXct(paste(format(tmax$datetime, '%Y-%m-%d'), ' ',
                                    tmax.time, sep=''), format='%Y-%m-%d %H:%M', tz=timezone)
  names(tmax) <- c('datetime', 't')
  tmax <- na.omit(tmax)


  # create synthetic dataframe of hourly values
  first.datetime <- tmin$datetime[1]
  last.row <- nrow(tmax)
  last.datetime <- tmax$datetime[last.row]


  # create synthetic hourly time series for merging
  datetime <- seq(from=first.datetime, to=last.datetime, by=3600)
  synthetic <- as.data.frame(datetime)


  # merge obs into synthetic
  merged1 <- merge(synthetic, tmin, all.x=TRUE)
  merged2 <- merge(synthetic, tmax, all.x=TRUE)
  merged <- merged1
  merged.maxtimes <- !is.na(merged2$t)
  merged[merged.maxtimes,2] <- merged2[merged.maxtimes,2]

  if (tmean.col == 0){
    tmean.col <- grep("tmean", tolower(obs.names), fixed=TRUE)
  }
  else{
    tmean.col <- tmean.col + 1
  }

  # check to see if mean values are to be used
  if (tmean.col > 0){
    if (length(tmean.col) != 0){
      tmean.col <- tmean.col[1]
      tmean <- obs[, c(1, tmean.col)]
      tmean$datetime <- as.POSIXct(paste(format(tmean$datetime, '%Y-%m-%d'), ' ',
                                         tmean.time, sep=''), format='%Y-%m-%d %H:%M', tz=timezone)
      names(tmean) <- c('datetime', 't')
      merged3 <- merge(synthetic, tmean, all=TRUE)
      merged.meantimes <- !is.na(merged3$t)
      merged[merged.meantimes,2] <- merged3[merged.meantimes,2]

    }
  }

  # interpolate
  interpolated <- interpolate(merged, varcols=1, methods='spline',
                              maxlength=max.interpolation.length,
                              quiet=TRUE, logfile=logfile)
  # output
  if (!quiet){
    obs.info <- CRHM_summary(interpolated)
    print(obs.info)
  }

  comment <- paste('tMinMaxToHourly dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if(result)
    return(interpolated)
  else
    return(result)
}
