#' Runs Alan Barr's filter on weighing gauge cumulative precipitation
#' @description This function is a wrapper for Alan Barr's function \code{PcpFiltPosTh} which removes jitter from precipitation data. The called function is iterative and may be slow to execute.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighing gauge cumulative percipitaion data, not including the \code{datetime}. Default is column 1.  This function can only work on 1 column of precipitation.
#' @param smallDropThreshold Optional. Minimum interval precipitation (mm). Default is 0.1.
#' @param serviceThreshold Optional. Threshold for removing change in storage due to servicing (mm). Note that this must be a NEGATIVE value. Default is -100.
#' @param serviceGapLength Optional. Maximum gap length (in time intervals) to be filled by linear interpolation after removing servicing (i.e. gauge reset) data. The default value is 3, but you may have to adjust it for very high-frequency data.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing the adjusted precipitation values. Note that the name of the precipitation variable is modified: \code{'_PcpFiltPosT'} is appended.
#' @author Kevin Shook
#' @note This function is potentially destructive, as it smooths the accumulated precipitation. You may need to set the filter length by trial-and error. You can check the effects using the function \code{weighingGaugePlot}. This version does not explicitly incorporate the effects of evaporation.
#' @seealso \code{\link{PcpFiltPosTh}} \code{\link{weighingGauge1}} \code{\link{weighingGauge2}} \code{\link{weighingGauge3}} \code{\link{weighingGaugePlot}} \code{\link{weighingGaugeInterval}}
#' @export
#'
#' @examples
#' \dontrun{
#' test4 <- weighingGauge4(wg, smallDropThreshold = 0.05, serviceThreshold = -50)}
#' 
weighingGauge4 <- function(obs, precipCol=1, smallDropThreshold=0.1, serviceThreshold=-100, serviceGapLength=3, quiet=TRUE, logfile=''){
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }
  
  obsName <- deparse(substitute(obs))
  
  if (any(is.na(obs[, precipCol+1]))){
    cat('Error: missing values. Remove before searching for spikes\n')
    return(FALSE)
  }
 
  if (smallDropThreshold <= 0){
    cat('Error: smallDropThreshold must be greater than zero\n')
    return(FALSE)
  }
  
  if (serviceThreshold >= 0){
    cat('Error: serviceThreshold must be less than zero\n')
    return(FALSE)
  }
  

  # select specified column
  obs <- obs[,c(1, precipCol+1)]
  precipName <- names(obs)[2]
  
  
  # Use Alan Barr's cleanup filter
  cleaned <- PcpFiltPosTh(obs[,2], dPcpTh=smallDropThreshold, dpServicingTh=serviceThreshold)
  final <- data.frame(obs$datetime, cleaned)
  names(final)[1] <- 'datetime'
  names(final)[2] <- paste(precipName, '_PcpFiltPosT', sep='')
  
  # fill remaining gaps
  if (any(is.na(final[,2])))
    final <- interpolate(final, varcols=1, methods='linear', maxlength=serviceGapLength, quiet=quiet, logfile)
  
  obs.info <- CRHM_summary(final)
  if (!quiet)
    print(obs.info)
  
  
  # output to logfile
  outputMessage <- paste(' smallDropThreshold:', smallDropThreshold,
                         ' serviceThreshold:', serviceThreshold)
  comment <- paste('weighingGauge4 obs:', obsName, outputMessage, sep='')  
  result <- logAction(comment, logfile)
  if (!result)
    return(result)
  else
    return(final)
}