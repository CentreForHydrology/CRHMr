#' Deletes spikes using rolling window stdev filtering
#'
#' @param obs Required. A \pkg{CRHMr} obs data frame.
#' @param colnum Optional. The number of the column to test for spikes, not
#'   including the \code{datetime}.
#' @param min_frac_records Optional. The fraction of records required in a
#'   window for a sucessful computation, otherwise the current value is flagged.
#'   Default is 0.8.
#' @param lead_window Optional. A list of values corresponding to the offset
#'   from the current value. Default is 1:10.
#' @param lag_window Optional. A list of values corresponding to the offset from
#'   the current value. Default is 1:10.
#' @param number_sd Optional. The number of standard deviations away from the
#'   mean required for the current value to be flagged.
#' @param logfile Optional. Name of the file to be used for logging the action.
#'   Normally not used.
#'
#' @return If successful, returns a data frame consisting of the datetime and the original obs values, where
#' all of the spike values have been set to be \code{NA_real_}. If no spikes are found a message is
#' printed and the function returns the value \code{FALSE}.
#' @export
#'
#' @examples
#' deleteSpikesStdevWindow(BadLake7376, 1, min_frac_records = 0.5)
deleteSpikesStdevWindow <-
  function(obs,
           colnum = 1,
           min_frac_records = .8,
           lead_window = list(1:10),
           lag_window = list(-1:-10),
           number_sd = 10,
           logfile = ""){

    if (nrow(obs) == 0) {
      stop("Missing obs values")
    }
    obsName <- deparse(substitute(obs))

    if (any(is.na(obs[, colnum + 1]))) {
      stop("Missing values. Remove before searching for spikes")
    }

    if (number_sd == 0) {
      stop("sd is <= 0. Set before searching for spikes")
    }

    spikeDatetimes <- findSpikesStdevWindow(obs,
                                            colnum,
                                            min_frac_records,
                                            lead_window,
                                            lag_window,
                                            number_sd)

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
    comment <- paste("findSpikesStdevWindow obs:", obsName, outputMessage, sep = "")
    result <- logAction(comment, logfile)

    if (!result) {
      return(result)
    } else {
      return(obs)
    }

  }
