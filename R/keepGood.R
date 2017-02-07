#' Keeps rows where there are some obs values
#'
#' @description Removes rows where all values are missing, keeps the remainder.
#' @param obs Required. A \pkg{CRHMr} obs dataframe.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns the trimmed dataframe. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{trimObs}}
#' @examples
#' BadLake.good <- keepGood(BadLake7376)
#' @export

keepGood <- function(obs,  quiet=TRUE, logfile=''){
  # trims off rows where all obs are missing
  minval <- -99999
  if (nrow(obs) == 0){
    cat('Error: missing data values\n')
    return(FALSE)
  }

  obsName <- deparse(substitute(obs))
  vals <- abs(data.frame(obs[,-1]))
  ncols <- ncol(obs) - 1

  # get smallest possible value
  mintotal <- ncols * minval

  rowSum <- rowSums(vals)

  # set missing values to minval

  vals[is.na(vals)] <- -9999
  totals <- rowSums(vals)
  kept <- obs[totals > mintotal,]

  # output info to screen and write to log file

  if (!quiet){
    obs.info <- CRHM_summary(kept)
    print(obs.info)
  }

  comment <- paste('keepGood dataframe:', obsName, sep='')
  result <- logAction(comment, logfile)
  if(result)
    return(kept)
  else
    return(result)
}
