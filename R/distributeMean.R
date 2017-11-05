#' Distribute mean values
#'
#' @description Distributes mean values to a shorter time interval. The missing datetimes are inserted and then the values are repeated. This function is typically used to downscale obs values such as t, ea, and u.
#' @param obs Required. The \pkg{CRHMr} data frame of obs values.
#' @param obsCols Optional. A vector containing the columns to be imputed in the obs data frame, not including the datetime. The default \option{all} specifies all columns.
#' @param timeStep Required. The time step (in hours) for the interpolated values. This value must be smaller than the time step in the original time series.
#' @param maxLength Optional. The maximum gap length to be interpolated. Defaults to 5 time steps.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not use
#'
#' @return If successful, returns a dataframe of the selected columns interpolated to the specified time step. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{distributeInst}}
#' @export
#'
#' @examples \dontrun{
#' hourly_vals <- distributeMean(vegreville, c(1,2,3), timeStep = 1)}
distributeMean <- function(obs,  obsCols='all', timeStep=0, maxLength=5,
                           quiet=TRUE, logfile=''){


  if (nrow(obs) == 0) {
    cat('Error: missing any values\n')
    return(FALSE)
  }
  obs_name <- deparse(substitute(obs))
  # subset dataframe
  if (length(obsCols) == 1) {
    if (obsCols == 'all') {
      var_cols <- seq(2, ncol(obs))
      obs <- obs
    } else {
      var_cols <-  obsCols + 1
      obs <- obs[,c(1, var_cols)]
    }
  }
  else{
    var_cols <- obsCols + 1
    obs <- obs[,c(1,var_cols)]
  }

  if (timeStep <= 0) {
    cat('Error: time step must be specified and > 0\n')
    return(FALSE)
  }

  dt <- as.numeric(difftime(obs$datetime[2], obs$datetime[1], units = 'hours'))

  if (timeStep >= dt) {
    cat('Error: time step must be smaller than in the original data\n')
    return(FALSE)
  }

  # create dataframe and merge values into it
  clean <- na.omit(obs)
  first.clean.datetime <- clean[1,1]
  last.clean.datetime <- clean[nrow(clean),1]
  cleanDatetime <- seq(from = first.clean.datetime,
                       to = last.clean.datetime, by = timeStep*3600)


  cleanDatetime <- data.frame(cleanDatetime)
  names(cleanDatetime) <- 'datetime'
  cleanVals <- merge(cleanDatetime, obs, by = 'datetime', all.x = TRUE)
  clean_rows <- nrow(cleanVals)
  clean_cols <- ncol(cleanVals)
  # now have a dataframe with all datetimes and NA for missing values
  # now interpolate values

  for (col in 2:clean_cols) {
    vals <- cleanVals[,col]
    vals_clean <- na.omit(as.vector(vals))
    num_original <- length(vals_clean)
    reps <- ceiling(clean_rows / num_original)

    # do reps
    reps <- rep(vals_clean, each = reps)
    cleanVals[,col] <- reps[1:clean_rows]

  }

  distrib <- cleanVals
  # output log files
  obs.info <- CRHM_summary(distrib)
  if (!quiet)
    print(obs.info)

  comment <- paste('distributeMean dataframe:', obs_name,
                   ' Variables:', stringr::str_c(names(distrib)[-1],
                                                 collapse = ','),
                   sep = '')

  result <- logAction(comment, logfile)

  if (result)
    return(distrib)
  else
    return(result)

}
