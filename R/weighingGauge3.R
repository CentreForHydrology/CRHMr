#' Filters weighing gauge cumulative precipitation
#' @description Thes functions is a wrapper for the Savitzky-Golay polynomial filter function \code{sgolayfilt} from the package \pkg{signal}. It passes the specified cumulative precipitation data, the filter length, the filter order (set to 1) and the derivative order (set to 0) to the filter. It is a good idea to use \code{weighingGaugePlot} to look at your data before and after calling this function.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighing guage cumulative precipitation data, not including the \code{datetime}. Default is column 1.  This function can only work on 1 column of precipitation.
#' @param filterLength Required. The number of time intervals used by the Savitzky-Golay polynomial filter. Note that this must be an ODD number. The appropriate value will depend on the type of your data, and the noise present.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing the adjusted precipitation values. Note that the name of the precipitation variable is modified: \code{'_sg_filtered'} is appended.
#' @author Kevin Shook
#' @note This function is potentially destructive, as it smooths the accumulated precipitation. You may need to set the filter length by trial-and error. You can check the effects using the function \code{weighingGaugePlot}.
#' @seealso  \code{\link{weighingGauge1}} \code{\link{weighingGauge2}} \code{\link{weighingGauge4}} \code{\link{weighingGaugePlot}} \code{\link{weighingGaugeInterval}}
#' @export
#'
#' @examples
#' \dontrun{
#' test3 <- weighingGauge3(wg, filterLength=5)}
#' 
weighingGauge3 <- function(obs, precipCol=1, filterLength=0, quiet=TRUE, logfile=''){
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }
  
  obsName <- deparse(substitute(obs))
  
  if (any(is.na(obs[, precipCol+1]))){
    cat('Error: missing values. Remove before searching for spikes\n')
    return(FALSE)
  }
 
  if (filterLength  == 0){
    cat('Error: filterLength must be > 0\n')
    return(FALSE)
  }
  else if ((filterLength %% 2) == 0){
    cat('Error: filterLength must be an odd number\n')
    return(FALSE)
  }

  # select specified column
  obs <- obs[,c(1, precipCol+1)]
  precipName <- names(obs)[2]
  
  sg <- signal::sgolayfilt(obs[,2], p=1, n=filterLength, m=0)
  final <- data.frame(obs$datetime, sg)
  names(final)[1] <- 'datetime'
  names(final)[2] <- paste(precipName, '_sg_filtered', sep='')
  
  obs.info <- CRHM_summary(final)
  if (!quiet)
    print(obs.info)
  
  # output to logfile
  outputMessage <- paste(' filterLength', filterLength)
  comment <- paste('weighingGauge3 obs:', obsName, outputMessage, sep='')  
  result <- logAction(comment, logfile)
  
  if (!result)
    return(result)
  else
    return(final)
}