#' Distributes total precipitation over intervals
#'
#' @description Distributes total precipitation (often daily) evenly over all of the applicable
#' intervals. The function has 2 uses. The primary use is to distribute total precipitation
#' when the precipitation time step is greater than the time step of the obs dataframe.
#' For example, there may be 3-hour total precipitation within an obs dataframe holding
#' hourly values for all other variables. The secondary use is to disaggregate precipitation
#' to a shorter time step. Normally this can be done inside CRHM by putting the data in a
#' separate obs file. However, if you want to do it in this function, then you must specify
#' the new time step.
#' @param obs Required. The \pkg{CRHMr} data frame of obs values.
#' @param p.cols Optional. The number of the column(s) to be used. If not specified,
#' the column will be guessed from the variable name. Note that the variable name MUST
#' include the letter 'p', even if the number is specified.
#' @param timestep Optional. The new time step in hours. If zero (the default) the time
#' step is not used. The time step is only used when disaggregating precipitation to a
#' shorter time step. Note that the specified time step must be shorter than that used
#' in the original data frame.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are
#' calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If sucessful, returns a modified version of the obs data frame. If no time step
#' is specified, the missing precipitation values in the specified data frame are replaced
#' by average precipitation values. If a time step is specified, a new data frame containing
#' ONLY the datetime and the distributed precipitation will be returned. If unsuccessful,
#' returns an error.
#' @author Kevin Shook
#' @note This function is potentially destructive. If you have a missing precipitation total, then the next precipitation total will be distributed over a longer time period. For example, you may have 3-hour precipitation data, normally at 03:00, 06:00, 09:00 etc., to be distributed hourly. If the 06:00 data are missing, then the 09:00 precipitation will be distributed over the interval from 04:00 to 09:00.
#' @seealso \code{\link{impute}} \code{\link{interpolate}} \code{distributeInst} \code{distributeQli} \code{distributeQsi}
#' @examples
#' \dontrun{
#' distributed <- distributeP(obs, 3)
#' }
#' @importFrom stringr str_c
#' @export
distributeP <- function(obs, p.cols = 0, timestep = 0, quiet = TRUE, logfile = "") {
  obs.length <- nrow(obs)

  if (nrow(obs) == 0) {
    stop("No values specified")
  }
  obsName <- deparse(substitute(obs))

  variable.names <- names(obs)

  p.length <- length(grep("p", tolower(variable.names), fixed = TRUE))
  if (p.length == 0) {
    stop("No precip data")
  }
  p.loc <- grep("p", tolower(variable.names), fixed = TRUE)
  if (length(p.cols) > 1) {
    # locations are specified
    p.cols <- p.cols + 1
  }
  else if (p.cols != 0) {
    # locations are specified
    p.cols <- p.cols + 1
  }
  else {
    p.cols <- p.loc
  }

  # get selected variables
  pcols.with.time <- c(1, p.cols)

  first.datetime <- obs$datetime[1]
  last.datetime <- obs$datetime[nrow(obs)]
  dt <- as.numeric(difftime(obs$datetime[2], obs$datetime[1], units = "hours"))

  # check for daily values - set time to 23:00 if req'd
  hour <- as.numeric(format(obs$datetime[1], format = "%H"))
  if ((dt == 24) & (hour == 0)) {
    obs$datetime <- obs$datetime + (23 * 3600)
  }

  # check to see if we are infilling or downscaling
  # infilling = filling in missing precip values
  # downscaling = moving precip to different time interval
  # see if worth doing
  if (timestep <= 0) {
    action <- "infilling"
    filled.good <- na.omit(obs)
    if (obs.length == nrow(filled.good)) {
      stop("NO missing values - nothing to do")
    }
    # generate time synthetic series from beginning to end
  }
  else {
    action <- "downscaling"
    if (dt < timestep) {
      stop("Specified time step > data time step")
    }
    # generate time synthetic series from beginning to end
    dt <- timestep
  }

  datetime <- seq(from = first.datetime, to = last.datetime, by = dt * 3600)
  # merge values into data frame and
  # create data frame of times and merge variables into it
  filled <- as.data.frame(datetime)
  filled <- merge(filled, obs, by = "datetime", all = TRUE)

  # deaccumulate each precip column
  output.data <- obs
  for (colnum in p.cols) {
    # get vals as a time series
    selected <- obs[, c(1, colnum)]
    # get goodvals
    good <- na.omit(selected)

    # get intervals
    t2 <- good$datetime[-1]
    t1 <- good$datetime[1:(length(good$datetime) - 1)]
    timeIntervals <- difftime(t2, t1, units = "hours")
    hourIntervals <- as.numeric(timeIntervals / as.numeric(dt))
    initialInterval <- as.numeric(hourIntervals[1])

    # add a presumed time interval for first value
    hourIntervals <- c(hourIntervals[1], hourIntervals)
    # divide precips by intervals
    intervalPrecip <- good[, 2] / hourIntervals

    # now create series of values
    allVals <- rep.int(intervalPrecip, hourIntervals)

    # now add back to output data frame
    if (action == "infilling") {
      # add back into data frame
      first.datetime.number <- which(obs$datetime == good$datetime[1])
      last.datetime.number <- which(obs$datetime == good$datetime[nrow(good)])

      # find locations of values to be replaced
      if (first.datetime.number == initialInterval) {
        original.start.row <- 1
        replacement.start.row <- 1
      }
      else if (first.datetime.number < initialInterval) {
        original.start.row <- first.datetime.number
        replacement.start.row <- initialInterval - first.datetime.number + 1
      }
      else {
        original.start.row <- first.datetime.number - (initialInterval - 1)
        replacement.start.row <- 1
      }
      original.end.row <- last.datetime.number
      replacement.end.row <- min(last.datetime.number, length(allVals))

      output.data[original.start.row:original.end.row, colnum] <-
        allVals[replacement.start.row:replacement.end.row]
    }
    else {
      # create new data frame
      if (colnum == p.cols[1]) {
        # first column - get datetimes
        first.datetime <- good$datetime[1]
        # now back up
        first.datetime <- first.datetime - (initialInterval - 1) * 3600
        numvals <- length(allVals)
        # create time series
        datetimesSynthetic <- seq(from = first.datetime, length.out = numvals, by = dt * 3600)

        df <- data.frame(datetimesSynthetic)
        names(df) <- "datetime"
      }
      names(allVals) <- names(obs)[colnum]
      df <- cbind(df, allVals)
      names(df)[ncol(df)] <- names(obs)[colnum]
      output.data <- df
    }
  }

  # output log files
  obs.info <- CRHM_summary(output.data)
  if (!quiet) {
    print(obs.info)
  }

  comment <- paste("distributedP precip_dataframe:", obsName,
    " Variables:", str_c(names(output.data)[-1], collapse = ","),
    sep = "")

  result <- logAction(comment, logfile)

  if (result) {
    return(output.data)
  } else {
    return(result)
  }
}
