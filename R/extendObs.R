#' Extends an obs data frame
#'
#' @description This function extends a \pkg{CRHMr} obs data frame to include the specified dates.
#' All added values are set to be \code{NA_real_}. The purpose of this function is to
#' create locations for data that can be infilled or imputed from another data set.
#' @param obs Required. The \pkg{CRHMr} obs data frame to be extended.
#' @param startDate Optional. The starting date for the obs data frame. Can either be a
#' year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e.
#' \option{1995-06-01}. Either the startDate or the endDate, or both, must be specified.
#' @param endDate Optional. The ending date for the obs data frame.  Can either be a
#' year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e.
#' \option{1995-07-01}.
#' @param timezone Required. The name of the timezone of the data as a character
#' string. This should be the timezone of your data, but omitting daylight savings
#' time. Note that the timezone code is specific to your OS. To avoid problems,
#' you should use a timezone without daylight savings time. Under Linux, you can
#'  use \option{CST} and \option{MST} for Central Standard or Mountain Standard
#'  time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or
#'   \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT
#'   use \option{America/Regina} as the time zone, as it includes historical changes
#'   between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are
#' calling this function in an \R script, you will usually leave \code{quiet=TRUE}
#' (i.e. the default). If you are working interactively, you will probably want to set
#' \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action.
#' Normally not used.
#' @return If successful, returns a modified version of the obs data frame. The
#' missing values in the specified data frame are replaced by \code{NA_real_} values.
#' If unsuccessful, returns an error.
#' @author Kevin Shook
#' @examples
#' BadLake7378 <- extendObs(BadLake7376, endDate = "1978-01-01", timezone = "CST")
#' @importFrom stringr str_length
#' @export


extendObs <- function(obs, startDate = "", endDate = "", timezone = "", quiet = TRUE, logfile = "") {
  # extends an obs data frame by adding missing values

  # check parameters
  if (nrow(obs) == 0) {
    stop("Missing data values")
  }

  obsName <- deparse(substitute(obs))

  if ((startDate == "") & (endDate == "")) {
    stop("Start date and/or end date must be specified")
  }
  if (timezone == "") {
    stop("Error: must specify a timezone")
  }

  if (startDate == "") {
    startDate <- endDate
  }

  if (endDate == "") {
    endDate <- startDate
  }

  # find time interval
  dt <- timestep.hours(obs[2, 1], obs[1, 1])

  # get start and end dates
  if (startDate != "") {
    if (str_length(startDate) == 4) {
      # year
      startDate <- paste(startDate, "-01-01 00:00", sep = "")
    }
    else {
      startDate <- paste(startDate, " 00:00", sep = "")
    }
    startDate <- as.POSIXct(startDate, format = "%Y-%m-%d %H:%M", tz = timezone)
  }

  if (endDate != "") {
    if (str_length(endDate) == 4) {
      endDate <- paste(endDate, "-12-31 23:00", sep = "")
    }
    else {
      endDate <- paste(endDate, " 00:00", sep = "")
    }
    endDate <- as.POSIXct(endDate, format = "%Y-%m-%d %H:%M", tz = timezone)
  }

  first.datetime <- obs$datetime[1]
  lastrow <- nrow(obs)
  last.datetime <- obs$datetime[lastrow]

  startDate <- min(startDate, endDate)
  endDate <- max(startDate, endDate)

  obs.colnum <- ncol(obs) - 1
  obs.rownum <- lastrow
  obs.colnames <- names(obs)

  if (dt == 24) {
    # daily
    startDate <- as.Date(startDate)
    endDate <- as.Date(endDate)

    if (startDate < first.datetime) {
      datetime <- seq(from = startDate, to = (first.datetime - 1), by = 1)
      missing.rownum <- length(datetime)
      empty.vals <- matrix(data = NA_real_, nrow = missing.rownum, ncol = obs.colnum)
      start.empty <- as.data.frame(datetime, empty.vals)
      names(start.empty) <- obs.colnames
      obs <- rbind(start.empty, obs)
    }
    if (endDate > last.datetime) {
      datetime <- seq(from = last.datetime + 1, to = endDate, by = 1)
      missing.rownum <- length(datetime)
      empty.vals <- matrix(data = NA_real_, nrow = missing.rownum, ncol = obs.colnum)
      end.empty <- as.data.frame(datetime, empty.vals)
      names(end.empty) <- obs.colnames
      obs <- rbind(obs, end.empty)
    }
  }
  else {
    # sub-daily
    if (startDate < first.datetime) {
      datetime <- seq(from = startDate, to = (first.datetime - 1), by = dt * 3600)
      missing.rownum <- length(datetime)
      empty.vals <- matrix(data = NA_real_, nrow = missing.rownum, ncol = obs.colnum)
      start.empty <- cbind(as.data.frame(datetime), empty.vals)
      names(start.empty) <- obs.colnames
      obs <- rbind(start.empty, obs)
    }
    if (endDate > last.datetime) {
      datetime <- seq(from = last.datetime + 1, to = endDate, by = dt * 3600)
      missing.rownum <- length(datetime)
      empty.vals <- matrix(data = NA_real_, nrow = missing.rownum, ncol = obs.colnum)
      end.empty <- cbind(as.data.frame(datetime), empty.vals)
      names(end.empty) <- obs.colnames
      obs <- rbind(obs, end.empty)
    }
  }

  # make sure interval is exact
  obs <- makeRegular(obs, timezone)

  if (!quiet) {
    obs.info <- CRHM_summary(obs)
    print(obs.info)
  }

  comment <- paste("extendObs dataframe:", obsName, sep = "")
  result <- logAction(comment, logfile)
  if (result) {
    return(obs)
  } else {
    return(result)
  }
}
