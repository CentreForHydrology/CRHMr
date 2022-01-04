#' Deaccumulates weighing gauge precipitation
#' @description Converts weighing gauged cumulative precipitation to interval values. The interval precipitation is checked to make sure that it does not contain any negative values. Negative precipitation will result in the function displaying an error message and returning the value \code{FALSE}.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighting gauge cumulative precipitation data, not including the \code{datetime}. Default is column 1.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing the datetime, the cumulative and interval precipitation.
#' @author Kevin Shook
#' @seealso \code{\link{weighingGauge1}} \code{\link{weighingGauge2}} \code{\link{weighingGauge3}}  \code{\link{weighingGauge4}}  \code{\link{weighingGauge5}} \code{\link{weighingGaugePlot}} 
#' @export
#'
#' @examples
#' \dontrun{
#' test5 <- weightingGaugeStep5(wg)}
#' 
weighingGaugeInterval <- function(obs, precipCol=1, quiet=TRUE, logfile=''){
  autocorThreshold = 0.5
  
  if (nrow(obs) == 0){
    cat('Error: no precipitation values\n')
    return(FALSE)
  }
  
  obsName <- deparse(substitute(obs))
  
  if (any(is.na(obs[, precipCol+1]))){
    cat('Error: missing values. Remove before deaccumulating\n')
    return(FALSE)
  }
 
  
  # select specified column
  obs <- obs[,c(1, precipCol+1)]
  obsName <- names(obs)[2]

  if (nrow(na.omit(obs)) == 0){
    cat('Error: no precipitation values\n')
    return(FALSE)
  }

  # Diff the values
  precipDiff <- data.frame(obs$datetime, c(0, diff(obs[,2])))
  final <- cbind(obs, precipDiff[,2])
  names(final)[3] <- paste(obsName,'_interval', sep='')
  
  # check for negative values
  minval <- min(final[,3])
  if (minval < 0){
    cat('Error: Negative precipitation values\n')
    return(FALSE)
  }

  obs.info <- CRHM_summary(final)
  if (!quiet)
    print(obs.info)
 
  # output to logfile
  comment <- paste('weighingGaugeInterval obs:', obsName, sep='')  
  result <- logAction(comment, logfile)
  if (!result)
    return(result)
  else
    return(final)
}