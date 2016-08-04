#' Fills gaps in weighing gauge precipitation data
#' @description This method fills missing values in accumulated precipitation data from a weighing gauge using linear interpoaltion. This function is a wrapper for functions and was suggested by Craig Smith. This function need not be called if your data does not contain any gaps. It is a good idea to use \code{weighingGaugePlot} to look at your data before calling this function.
#' @param obs Required. A standard \pkg{CRHMr} obs file.
#' @param precipCol Optional. The number of the column containing the weighing gauge cumulative precipitation data, not including the \code{datetime}. Default is column 1. These values are cumulative preciptiation.
#' @param maxGapLength Optional. The maximum gap length included in the analysis (in time intervals). Default is 5 time intervals. Used by \code{interpolate}.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If sucessful, returns a modified version of the \code{obs} dataframe containing only the datetime and the gap-filled total precipitation values.
#' @author Kevin Shook
#' @note This version does not explicitly incorporate the effects of evaporation.
#' @seealso \code{\link{weighingGauge2}} \code{\link{weighingGauge3}} \code{\link{weighingGauge4}} \code{\link{weighingGaugePlot}} \code{\link{weighingGaugeInterval}}
#' @export
#'
#' @examples \dontrun{
#' test1 <- weighingGauge1(wg,  maxGapLength=6)}
#' 
weighingGauge1 <- function(obs, precipCol=1, maxGapLength=5, quiet=TRUE, logfile=''){
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }
  
  obsName <- deparse(substitute(obs))
  
  if (!any(is.na(obs[, precipCol+1]))){
    cat('Error: no missing values - nothing to do\n')
    return(FALSE)
  }
  

  if (maxGapLength <= 0){
    cat('Error: maxGapLength must be greater than zero\n')
    return(FALSE)
  }
  
  # select specified column
  obs <- obs[,c(1, precipCol+1)]


  # fill missing values by linear interpolation
  obs <- interpolate(obs, varcols=1, methods='linear', maxlength=maxGapLength, 
                     quiet=quiet, logfile)
  
  
  obs.info <- CRHM_summary(obs)
  if (!quiet)
    print(obs.info)
  
  # output to logfile
  outputMessage <- paste(' maxGapLength:', maxGapLength, sep='') 
  comment <- paste('weighingGauge1 obs:', obsName, outputMessage, sep='')  
  result <- logAction(comment, logfile)
  if (!result)
    return(result)
  else
    return(obs)
}