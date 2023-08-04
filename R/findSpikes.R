#' Finds spikes in an obs data frame
#'
#' @description Finds spikes (short bursts of very large or small values) in the
#'   specified time variable. This uses the R base function \emph{diff} to
#'   calculate the difference between the next (lead) and current value.
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param colnum Optional. The number of the column to test for spikes, not
#'   including the \code{datetime}.
#' @param threshold Required. The threshold for the \emph{change} in the
#'   observed values. The threshold is actually a rate, i.e. the change per unit
#'   time. So if you are looking at air temperature, and the threshold is set to
#'   \code{5}, then any change in temperature of +/- 5 degrees in one time
#'   interval will be considered to be a spike.
#' @param spike_direction string 'both' OR 'hi' OR 'low' to look at both high
#'   and low or hi or low spikes.
#' @param logfile Optional. Name of the file to be used for logging the action.
#'   Normally not used.
#'
#' @return If successful and there are no spikes, returns \code{0}. If there are
#'   spikes, returns their \code{datetime} values. If unsuccessful returns
#'   \code{FALSE}.
#' @author Kevin Shook, Alex Cebulski
#' @note If quiet=\code{FALSE}, the function displays a list of the datetimes of
#'   the spikes.
#' @seealso \code{\link{deleteSpikes}} \code{\link{findGaps}}
#'   \code{\link{findDupes}}
#' @export
#'
#' @examples
#' # check for spikes in wind speed - look for changes greater than 10 m/s per interval
#' findSpikes(BadLake7376, 3, 10)
findSpikes <-
  function(obs,
           colnum = 1,
           threshold = 0,
           spike_direction = 'both',
           logfile = "") {
    if (nrow(obs) == 0) {
    stop("Error: missing obs values")
  }

  if (!spike_direction %in% c('both', 'hi', 'low')) {
    stop("Error: spike direction must be one of 'both', 'hi', OR 'low'")
  }
  obsName <- deparse(substitute(obs))

  if (any(is.na(obs[, colnum + 1]))) {
    stop("Missing values. Remove before searching for spikes")
  }

  if (threshold == 0) {
    stop("Threshold is <= 0. Set before searching for spikes")
  }


  if (spike_direction == 'both'){
  threshold <- abs(threshold)

  # do diff and get abs values
  diff_vals <- abs(diff(obs[, colnum + 1]))

  # check for spikes
  spikes <- (diff_vals > threshold)
  numSpikes <- sum(spikes)
  if (numSpikes == 0) {
    outputMessage <- " No spikes found"
    returnvalue <- 0
  }
  else {
    locs <- obs[spikes, 1][-1] # the -1 removes the first spike and shifts the date times to align with the actual spikes
    outputMessage <- paste(" ", numSpikes, " spikes found", sep = "")
    returnvalue <- locs
  }
  }

  if (spike_direction == 'hi'){

    # dont return abs values so we are just looking in one direction of spike
    diff_vals <- c(0, diff(obs[, colnum + 1]))

    # check for spikes
    spikes <- (diff_vals > threshold)
    numSpikes <- sum(spikes)
    if (numSpikes == 0) {
      outputMessage <- " No spikes found"
      returnvalue <- 0
    }
    else {
      locs <- obs[spikes, 1]
      outputMessage <- paste(" ", numSpikes, " spikes found", sep = "")
      returnvalue <- locs
    }
  }

  if (spike_direction == 'low'){

    # dont return abs values so we are just looking in one direction of spike
    diff_vals <- c(0, diff(obs[, colnum + 1]))

    # check for spikes
    spikes <- (diff_vals < threshold)
    numSpikes <- sum(spikes)
    if (numSpikes == 0) {
      outputMessage <- " No spikes found"
      returnvalue <- 0
    }
    else {
      locs <- obs[spikes, 1]
      outputMessage <- paste(" ", numSpikes, " spikes found", sep = "")
      returnvalue <- locs
    }
  }

  # output to logfile
  comment <- paste("findSpikes obs:", obsName, outputMessage, sep = "")
  result <- logAction(comment, logfile)
  if (!result) {
    return(result)
  } else {
    return(returnvalue)
  }
}
