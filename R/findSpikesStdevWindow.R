#' Finds spikes using rolling window stdev filtering
#'
#' @description Finds spikes using a rolling window with a width defined as a
#'   lead and lag offset from the current value. Currently is set up to define
#'   spikes within the window given a number of standard deviations away from
#'   the mean. NOTE: the first few and last few values will automatically be
#'   removed due to the stdev function requiring a minimum of 3 vals.
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
#' @return If successful and there are no spikes, returns \code{0}. If there are
#'   spikes, returns their \code{datetime} values. If unsuccessful returns
#'   \code{FALSE}.
#' @author Alex Cebulski
#' @seealso \code{\link{deleteSpikes}} \code{\link{findGaps}}
#'   \code{\link{findDupes}}
#' @export
#'
#' @examples
#' findSpikesStdevWindow(BadLake7376, 1, min_frac_records = 0.5)
findSpikesStdevWindow <-
  function(obs,
           colnum = 1,
           min_frac_records = .8,
           lead_window = list(1:10),
           lag_window = list(-1:-10),
           number_sd = 10,
           logfile = ""
  ){

    if (nrow(obs) == 0) {
      stop("Error: missing obs values")
    }

    obsName <- deparse(substitute(obs))

    if (any(is.na(obs[, colnum + 1]))) {
      stop("Missing values. Remove before searching for spikes")
    }

    if (number_sd == 0) {
      stop("sd is <= 0. Set before searching for spikes")
    }

    datetime_col <- 1
    var_col <- colnum + 1 # raw colnum does not include datetime
    select_cols <- c(datetime_col, var_col)

    min_records_window <- max(lead_window[[1]])*min_frac_records

    obs_fltr <- obs[,select_cols]

    # count the number of records in the leading window
    obs_fltr$lead_count <- zoo::rollapply(obs_fltr[,var_col],
                           width=lead_window,
                           FUN = function(x) sum(!is.na(x)),
                           fill=NA,
                           partial = T)

    obs_fltr$lag_count <- zoo::rollapply(obs_fltr[,var_col],
                          width=lag_window,
                          FUN = function(x) sum(!is.na(x)),
                          fill=NA,
                          partial = T)

    obs_fltr$lead_count_filter <-
      ifelse(obs_fltr$lead_count<min_records_window|is.na(obs_fltr$lead_count), 0, 1)
    obs_fltr$lag_count_filter <-
      ifelse(obs_fltr$lag_count<min_records_window|is.na(obs_fltr$lag_count), 0, 1)

    obs_fltr$lead_mean <- zoo::rollapply(obs_fltr[,var_col],
                                      width=lead_window,
                                      FUN = mean,
                                      partial = T,
                                      na.rm = T,
                                      fill=NA)

    obs_fltr$lag_mean <- zoo::rollapply(obs_fltr[,var_col],
                                   width=lag_window,
                                   FUN = mean,
                                   partial = T,
                                   na.rm = T,
                                   fill=NA)

    obs_fltr$lead_sd <- zoo::rollapply(obs_fltr[,var_col],
                                  width=lead_window,
                                  FUN = sd,
                                  partial = T,
                                  na.rm = T,
                                  fill=NA)

    obs_fltr$lag_sd <- zoo::rollapply(obs_fltr[,var_col],
                                 width=lag_window,
                                 FUN = sd,
                                 partial = T,
                                 na.rm = T,
                                 fill=NA)

    obs_fltr$lead_fltr_max <-
      obs_fltr$lead_mean + (number_sd * obs_fltr$lead_sd)
    obs_fltr$lead_fltr_min <-
      obs_fltr$lead_mean - (number_sd * obs_fltr$lead_sd)
    obs_fltr$lag_fltr_max <-
      obs_fltr$lag_mean + (number_sd * obs_fltr$lag_sd)
    obs_fltr$lag_fltr_min <-
      obs_fltr$lag_mean - (number_sd * obs_fltr$lag_sd)

    obs_fltr$sd_filter <- T

    # set sd_filter to FALSE for those that pass our test
    obs_fltr$sd_filter[(obs_fltr$lead_count_filter == 1 &
                          obs_fltr[,var_col] < obs_fltr$lead_fltr_max &
                          obs_fltr[,var_col] > obs_fltr$lead_fltr_min) &
                         (obs_fltr$lag_count_filter == 1 &
                            obs_fltr[,var_col] < obs_fltr$lag_fltr_max &
                            obs_fltr[,var_col] > obs_fltr$lag_fltr_min)] <- F

    locs <- obs_fltr[obs_fltr$sd_filter, 1]
    numSpikes <- sum(obs_fltr$sd_filter)

    if (numSpikes == 0) {
      outputMessage <- " No spikes found"
      returnvalue <- 0
    }
    else {
      outputMessage <- paste(" ", numSpikes, " spikes found with standard deviations ",
                             number_sd, " above/below the mean using the rolling window method ",
                             sep = "")
      returnvalue <- locs
    }

    # output to logfile
    comment <- paste("findStdevSpikeWindow obs:", obsName, outputMessage, sep = "")
    result <- logAction(comment, logfile)
    if (!result) {
      return(result)
    } else {
      return(returnvalue)
    }
  }
