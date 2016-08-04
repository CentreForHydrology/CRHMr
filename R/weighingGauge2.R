#' Removes spikes from weighing gauge cumulative precipitation
#' @description Removes positive and negative spikes from deaccumulated weighing gauge precipitation. The gaps left from spike removal are infilled by linear interpolation. This function is a wrapper for other functions and was suggested by Craig Smith. This function need not be called if your data does not contain any spikes. It is a good idea to use \code{weighingGaugePlot} to look at your data before calling this function. Note that this function will also remove gauge resets, if they are wihin the range of the \code{spikeThreshold}.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighting gauge cumulative precipitation data, not including the \code{datetime}. Default is column 1.  This function can only work on 1 column of precipitation.
#' @param spikeThreshold Optional. Threshold for single-interval precipitation (mm). Any spikes (changes in cumulative precipitation) whose absoloute value is greater than the threshold will be deleted. The default value is 1000 mm.
#' @param maxSpikeGap Optional. Maximum length of spikes (in time steps) to be filled. Default is 3.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing only the datetime and the adjusted precipitation values.
#' @author Kevin Shook
#' @note If you don't specify a large enough value for \code{maxSpikeGap}, then all of the values after the spike will be set to \code{NA}.
#' @seealso \code{\link{weighingGauge1}} \code{\link{weighingGauge3}} \code{\link{weighingGauge4}} \code{\link{weighingGaugePlot}} \code{\link{weighingGaugeInterval}}
#' @export
#'
#' @examples
#' \dontrun{
#' test2 <- weightingGauge2(wg, spikeThreshold = 300)}
#' 
weighingGauge2 <- function(obs, precipCol=1, spikeThreshold=1000, maxSpikeGap=3, quiet=TRUE, logfile=''){
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }
  
  obsName <- deparse(substitute(obs))
  
  if (any(is.na(obs[, precipCol+1]))){
    cat('Error: missing values. Remove before searching for spikes\n')
    return(FALSE)
  }
  
  if (spikeThreshold <= 0){
    cat('Error: spikeThreshold must be greater than zero\n')
    return(FALSE)
  }

  
  # select specified column
  datetimes <- obs[,1]
  obs <- obs[,c(1, precipCol+1)]
  obsName <- names(obs)[2]

  # Diff the values
  precipDiff <- c(0, diff(obs[,2]))
  precipDiff <- data.frame(obs$datetime, precipDiff)
  names(precipDiff)[1] <- 'datetime'
  
  #Remove spikes
  spikeCount <- findSpikes(precipDiff, colnum=1, threshold = spikeThreshold)
  if (class(spikeCount)[[1]] == 'numeric'){
    # no spikes
    if (!quiet)
      cat('Warning: no spikes present')
    return(FALSE)
  }
  
  despiked <- deleteSpikes(precipDiff, colnum = 1, threshold = spikeThreshold)
  despiked <- interpolate(despiked, varcols=1, methods='linear', maxlength=maxSpikeGap, quiet=quiet, logfile)
  # Accumulate
  final <- data.frame(despiked[,1], cumsum(despiked[,2]))
  names(final) <- c('datetime', obsName)
  
  obs.info <- CRHM_summary(final)
  if (!quiet)
    print(obs.info)
 
  # output to logfile
  outputMessage <- paste(' spikeThreshold', spikeThreshold)
  comment <- paste('weighingGauge2 obs:', obsName, outputMessage, sep='')  
  result <- logAction(comment, logfile)
  if (!result)
    return(result)
  else
    return(final)
}