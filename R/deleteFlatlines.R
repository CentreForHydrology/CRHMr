#' Deletes values in a obs dataframe column that have a flatline
#'
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param colnum Optional. The number of the column to test for spikes, not
#'   including the datetime.
#' @param window_size Optional. The min number of values within the vector
#'   required to be the same before is flagged as a flat line.
#' @param logfile Optional. Name of the file to be used for logging the action.
#'   Normally not used.
#'
#' @return obs dataframe with col flat lines replaced with NANs
#' @export
#'
#' @author Alex Cebulski
#'
#' @examples
#' deleteFlatLines(BadLake7376, colnum = 3)
#'
deleteFlatLines <- function(obs, colnum = 1, window_size = 5, logfile = "") {

  if (nrow(obs) == 0) {
    stop("Missing obs values")
  }
  obsName <- deparse(substitute(obs))

  if (any(is.na(obs[, colnum + 1]))) {
    stop("Missing values. Remove before searching for spikes")
  }

  if (window_size == 0) {
    stop("Missing threshold. Set before searching for spikes")
  }

  flatline_datetimes <- findFlatLines(obs,
                                      colnum = colnum,
                                      window_size = window_size,
                                      logfile = logfile)

  if (length(flatline_datetimes) == 1) {
    if (flatline_datetimes == 0) {
      cat("No flatlines found\n")
      return(FALSE)
    }
    if (flatline_datetimes == FALSE) {
      stop("Error in finding flatlines")
    }
  }

  flatLocs <- match(flatline_datetimes, obs$datetime)
  obs[flatLocs, colnum + 1] <- NA_real_

  # output to logfile
  outputMessage <- paste(length(flatline_datetimes), " values set to NA_real_")
  comment <- paste("deleteFlatlines obs:", obsName, outputMessage, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  } else {
    return(obs)
  }
}
