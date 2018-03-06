#' Distributes daily precipitation according to hourly values
#'
#' @description Used to distrbute daily percipitation totals, according to hourly values. Typically, this is used when the hourly values are of lower quality than the daily values, such as when they are imputed from another location.
#'
#' @param dailyObs obs Required. A \pkg{CRHMr} data frame containing daily precipitation values.
#' @param dailyPcol Optional. The number of the column containing the daily precipitation. If not specified, the column will be guessed from the variable name. Note that the variable name MUST include the letter 'p', even if the number is specified.
#' @param hourlyObs obs Required. A \pkg{CRHMr} data frame containing hourly precipitation values.
#' @param hourlyPcol Optional. The number of the column containing the hourly precipitation. If not specified, the column will be guessed from the variable name. Note that the variable name MUST include the letter 'p', even if the number is specified.
#' @param missingRatio Optional. The value of the ratio used to multiply the hourly preciptiation when daily precipitation data are not available. The default is 1. If you want to omit these values, use \code{NA_real_}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If sucessful, returns a modified version of the obs data frame, consiting of the original hourly precipitation data, and the \code{adjusted_precip}.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{ hourly <- dailyByHourlyP(dailyObs = daily, hourlyObs = hourly)
#' }
dailyByHourlyP <- function(dailyObs, dailyPcol=0, hourlyObs, hourlyPcol=0,
                             missingRatio = 1, quiet=TRUE, logfile="") {

  # check variables
  if (nrow(dailyObs) == 0) {
    cat('Error: missing any daily values\n')
    return(FALSE)
  }

  dailyObsName <- deparse(substitute(dailyObs))

  variable.names <- names(dailyObs)

  p.length <- length(grep("p", tolower(variable.names), fixed = TRUE))
  if (p.length == 0) {
    cat("Error: no precip data\n")
    return(FALSE)
  }
  daily_p_loc <- grep("p", tolower(variable.names), fixed = TRUE)
  if (length(dailyPcol) > 1) {
    # locations are specified
    cat('Error: only 1 daily precip column can be specified\n')
    return(FALSE)
  }
  else if (dailyPcol != 0) {
    # locations are specified
    dailyPcol <- dailyPcol + 1
  }
  else{
    dailyPcol <- daily_p_loc
  }

  # get selected variables
  daily_pcols_with_time <- c(1, dailyPcol)
  dailyObs <- dailyObs[,daily_pcols_with_time]

  if (nrow(hourlyObs) == 0) {
    cat('Error: missing any hourly values\n')
    return(FALSE)
  }

  hourlyObsName <- deparse(substitute(hourlyObs))

  variable.names <- names(hourlyObs)

  p.length <- length(grep("p", tolower(variable.names), fixed = TRUE))
  if (p.length == 0) {
    cat("Error: no precip data\n")
    return(FALSE)
  }
  hourly_p_loc <- grep("p", tolower(variable.names), fixed = TRUE)
  if (length(hourlyPcol) > 1) {
    cat('Error: only 1 hourly precip column can be specified\n')
    return(FALSE)
  }
  else if (hourlyPcol != 0) {
    # locations are specified
    hourlyPcol <- hourlyPcol + 1
  }
  else{
    hourlyPcol <- hourly_p_loc
  }

  # get selected variables
  hourly_pcols_with_time <- c(1, hourlyPcol)
  hourlyObs <- hourlyObs[,hourly_pcols_with_time]


  # aggregate daily to hourly
  hourly_to_daily <- aggDataframe(hourlyObs, 1, period = "daily",
                                  funs = "sum")

  # merge daily and hourly_to_daily
  merged <- merge(dailyObs, hourly_to_daily, by = "date")
  names(merged) <- c("date", "daily_P", "hourly_to_daily_P")
  merged$ratio <- missingRatio
  merged$ratio[merged$hourly_to_daily <= 0 ] <- 0
  merged$ratio[merged$hourly_to_daily > 0 ] <- merged$daily[merged$hourly_to_daily > 0 ]  /
                                    merged$hourly_to_daily[merged$hourly_to_daily > 0 ]
  merged <- merged[,c("date", "ratio")]
  # add the date to the hourlies
  timezone <- lubridate::tz(hourlyObs$datetime)
  hourlyObs$date <- as.Date(hourlyObs$datetime, tz = timezone)

  # merge in the ratios
  merged_hourly <- merge(hourlyObs, merged, by = "date")
  merged_hourly$ratio[is.na(merged_hourly$ratio) == missingRatio]
  hourlyObs$adjusted_precip <- hourlyObs[,2] * merged_hourly$ratio

  output.data <- hourlyObs

  # output log files
  obs.info <- CRHM_summary(output.data)
  if (!quiet)
    print(obs.info)

  comment <- paste('daylyByHourlyP dailyObs:', dailyObsName,
                   ' hourlyObs:', hourlyObsName, sep = '')

  result <- logAction(comment, logfile)

  if (result)
    return(output.data)
  else
    return(result)
}
