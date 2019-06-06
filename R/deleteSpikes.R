#' Deletes spikes in an obs data frame
#'
#' @description Finds spikes, and sets their values to be NA_real_.
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param colnum Optional. The number of the column to test for spikes, not including the datetime.
#' @param threshold Required. The threshold for the \emph{change} in the observed values. The threshold
#' is actually a rate, i.e. the change per unit time. So if you are looking at air temperature, and the
#' threshold is set to \code{5}, then any change in temperature of +/- 5 degrees in one time interval
#' will be considered to be a spike.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If successful, returns a data frame consiting of the datetime and the original obs values, where
#' all of the spike values have been set to be \code{NA_real_}. If no spikes are found a message is
#' printed and the funtion returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{findSpikes}}
#' @export
#'
#' @examples
#' BadLake <- BadLake7376
#' # finds all windspeeds which change by more than 10 m/s per interval,
#' # and sets their values to NA_real_
#' BadLake$u.nospikes <- deleteSpikes(BadLake7376, 3, 10)
deleteSpikes <- function(obs, colnum=1, threshold=0, logfile="") {
  # removes spikes by filling them with NA_real_values
  if (nrow(obs) == 0) {
    stop("Missing obs values")
  }
  obsName <- deparse(substitute(obs))

  if (any(is.na(obs[, colnum + 1]))) {
    stop("Missing values. Remove before searching for spikes")
  }

  if (threshold == 0) {
    stop("Missing threshold. Set before searching for spikes")
  }

  spikeDatetimes <- findSpikes(obs, colnum = colnum, threshold = threshold, logfile = logfile)

  if (length(spikeDatetimes) == 1) {
    if (spikeDatetimes == 0) {
      cat("No spikes found\n")
      return(FALSE)
    }
    if (spikeDatetimes == FALSE) {
      stop("Error in finding spikes")
    }
  }

  spikeLocs <- match(spikeDatetimes, obs$datetime)
  obs[spikeLocs, colnum + 1] <- NA_real_

  # output to logfile
  outputMessage <- paste(length(spikeDatetimes), " values set to NA_real_")
  comment <- paste("deleteSpikes obs:", obsName, outputMessage, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  } else {
    return(obs)
  }
}
