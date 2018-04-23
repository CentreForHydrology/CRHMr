#' Simplified weighing gauge processing
#' @description This function removes gauge resets (large negative changes usually due to servicing) and uses a simple method (cumulative maxima) to remove jitter. It is a good idea to use \code{weighingGauge1} to infill missing values before calling this function and \code{weighingGaugePlot} to look at your data before and after calling this function.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighing gauge cumulative precipitation data, not including the \code{datetime}. Default is column 1. These values are cumulative preciptiation. This function can only work on 1 column of precipitation. The precipitation cannot contain missing values - infill them using \code{weighingGauge1} first.
#' @param resetThreshold Optional. The minimum (absolute value) of negative values considered to be gauge resets. Default is 50 mm.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing only the datetime and the modified cumulative precipitation values.
#' @author Kevin Shook, based on an algorithm developed by Thai Nguyen of Alberta Environment.
#' @note This version does not explicitly incorporate the effects of evaporation.
#' @seealso \code{\link{weighingGauge1}} \code{\link{weighingGaugePlot}} \code{\link{weighingGaugeInterval}}
#' @export
#'
#' @examples \dontrun{
#' wg1 <- weighingGauge1(wg,  maxGapLength=500)
#' wg5 <- weighingGauge5(wg1,  resetThreshold=70)}
#'
weighingGauge5 <- function(obs, precipCol=1, resetThreshold=50, quiet=TRUE, logfile=''){
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }

  # get name of data frame
  obsName <- deparse(substitute(obs))

  # select specified column
  obs <- obs[,c(1, precipCol+1)]

  # check for missing values
  missingCount <- sum(is.na(obs[,2]))
  if (missingCount > 0){
    cat('Error: missing precipitation values\n')
    return(FALSE)
  }


  # convert threshold to negative value
  resetThreshold <- abs(resetThreshold)
  resetThreshold <- -1 * resetThreshold


  # diff the values
  precipDiff <- diff(obs[,2], lag=1, differences = 1)
  precipDiff <- c(0, precipDiff)
  # remove resets by setting them to zero
  resetLocs <- (precipDiff < resetThreshold)
  resetCount <- sum(resetLocs)

  if(!quiet){
    if (resetCount == 0)
      cat('No resets found\n')
    else if(resetCount == 1)
      cat('1 reset found\n')
    else
      cat(resetCount, 'resets found\n')
  }

  precipDiff[resetLocs] <- 0

  # reaccumulate
  precipAccum <-  data.frame(obs$datetime, cumsum(precipDiff))
  names(precipAccum) <- c('datetime', names(obs)[2])

  # apply cumulative max filter
  precipAccum[,2] <- cummax(precipAccum[,2])


  # output to logfile
  outputMessage <- paste(' resetThreshold:', abs(resetThreshold), sep='')
  comment <- paste('weighingGauge5 obs:', obsName, outputMessage, sep='')
  result <- logAction(comment, logfile)
  if (!result)
    return(result)
  else
    return(precipAccum)
}
