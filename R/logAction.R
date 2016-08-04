#' Writes a comment to the \pkg{CRHMr} log file
#'
#' @description This is an internal \pkg{CRHMr} function and should normally not need to be called directly. This function is called by almost all \pkg{CRHMr} functions, and writes the comment, and the date and time to the logfile.
#' @param comment Required. Comment string to be written. This normally includes the function name, as well as the obs file being processed.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used. If not specified, then 'CRHMr.log' in the current working directory will be used.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @export 
#' @examples\dontrun{
#'  comment <- paste('tMinMaxToHourly dataframe:', obsName, sep='')  
#'  result <- logAction(comment)}
logAction <- function(comment='', logfile=''){
  eol.val <- win.eol()
  # writes comments to a log file
  date.time <- date()
  date.time.formatted <- format(Sys.time(), format='%Y-%m-%d_%H:%M:%S')
  
  if(logfile == '')
    logfile <- 'CRHMr.log'
  
  if (comment == ''){
    cat('Missing comments \n')
    return(FALSE)
  }
  
  cat(date.time.formatted, comment, eol.val, sep=' ', file=logfile, append=TRUE)   
  return(TRUE)
}
