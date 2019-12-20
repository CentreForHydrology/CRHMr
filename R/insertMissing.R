#' Inserts missing datetimes
#'
#' @description Many time series have missing rows. This function finds missing rows,
#' and adds rows with the appropriate datetime values and \code{NA_real_} values for
#' all variables. Once the missing values have been added, you can use
#' \code{\link{interpolate}} or \code{\link{impute}} to infill them.
#' @param obs Required. A \pkg{CRHMr} data frame containing obs values.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you
#' are calling this function in an \R script, you will usually leave
#' quiet=\code{TRUE} (i.e. the default). If you are working interactively, you
#' will probably want to set quiet=\code{FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the
#' action. Normally not used.
#' @return If successful returns a modified version of the obs data frame with missing datetime values  inserted. All of the missing variables in the inserted rows are replaced by \code{NA_real_} values. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @note This function should be used before doing interpolation or imputation of variables. Note that this function is also called by the function \code{\link{findGaps}}.
#' @seealso \code{\link{findGaps}} \code{\link{interpolate}} \code{\link{impute}}
#' @examples
#' BadLake.withmissing <- insertMissing(BadLake7376, quiet=FALSE)
#' @export

insertMissing <- function(obs, quiet=TRUE, logfile=''){
  # inserts missing rows in the obs data frame

  if (nrow(obs) == 0) {
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))

  first.datetime <- obs$datetime[1]
  second.datetime <- obs$datetime[2]
  last.row <- nrow(obs)
  last.datetime <- obs$datetime[last.row]

  # find time step
  timestep.hours <- timestep.hours(first.datetime, second.datetime)
  timestep.seconds <- timestep.hours * 3600

  # create synthetic time series for merging
  datetime <- seq(from = first.datetime, to = last.datetime, by = timestep.seconds)
  synthetic <- as.data.frame(datetime)

  # merge obs into synthetic
  merged <- merge(synthetic, obs, all.x = TRUE)

  if (!quiet) {
    cat('original obs:\n')
    original.data.info <- CRHM_summary(obs)
    print(original.data.info)

    cat('\nnew obs with missing values inserted:\n')
    new.data.info <- CRHM_summary(merged)
    print(new.data.info)
  }
  comment <- paste('insertMissing Obs:', obsName, sep = '')

  result <- logAction(comment, logfile)
  if (result)
    return(merged)
  else
    return(result)
}
