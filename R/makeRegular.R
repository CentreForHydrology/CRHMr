#' Makes the datetimes of an obs data frame fit the time step exactly
#'
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param timezone Required. Timezone to be assigned to data.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling
#' this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default).
#' If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns a modified version of the \code{obs} data frame containing
#' the adjusted datetime and all of the variables. If unsuccessful, returns an error.
#' @author Kevin Shook
#'
#' @export
#'
#' @examples
#' regular <- makeRegular(BadLake7376, timezone = "CST")
makeRegular <- function(obs, timezone = "", quiet = TRUE, logfile = "") {
  # check parameter values
  if (nrow(obs) == 0) {
    stop("Missing obs values")
  }

  obsName <- deparse(substitute(obs))


  if (timezone == "") {
    stop("Timezone must be specified")
  }

  datetime <- obs[, 1]

  # figure out interval from first 2 values, assuming 1 of them will be correct
  interval.hours <- timestep.hours(datetime[1], datetime[2])

  # get CRHM time components
  year <- as.numeric(format(datetime, format = "%Y"))
  month <- as.numeric(format(datetime, format = "%m"))
  day <- as.numeric(format(datetime, format = "%d"))
  hour <- as.numeric(format(datetime, format = "%H"))
  minute <- as.numeric(format(datetime, format = "%M"))



  # reassemble data frame
  # check for sub-hourly data
  if (interval.hours < 1) {
    # round time to nearest 5 minute interval
    hour <- round(hour, digits = 0)
    minute <- round(minute / 5, digits = 0) * 5
    hour[minute >= 60] <- hour[minute >= 60] + 1
    minute[minute >= 60] <- 0

    datetime.string <- paste(year, "-", month, "-", day, " ", hour, ":", minute, sep = "")
  }
  else {
    # get hour as a fraction
    hour <- hour + minute / 60
    # round off
    hour <- round(hour, digits = 0)
    datetime.string <- paste(year, "-", month, "-", day, " ", hour, ":00", sep = "")
  }


  datetime.corrected <- as.POSIXct(datetime.string, format = "%Y-%m-%d %H:%M ", tz = timezone)
  obs$datetime <- datetime.corrected

  obs.info <- CRHM_summary(obs)
  if (!quiet) {
    print(obs.info)
  }

  # output to logfile
  outputMessage <- ""
  comment <- paste("makeRegular obs:", obsName, outputMessage, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  } else {
    return(obs)
  }


  return(obs)
}
