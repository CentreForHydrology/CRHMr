#' Sets missing precipitation values to zero
#'
#' @description Removes missing precipitation (\code{NA_real_}values (p and/or ppt) in an obs dataframe, by setting them to zero. This function is potentially dangerous, as setting the missing values to zero prevents any further imputation/interpolation of values.
#' @param obs Required. A \pkg{CRHMr} obs dataframe.
#' @param precipCols Optional. A vector containing the numbers of the columns (excluding the datetime) to be processed. If omitted, all of the columns whose names contain \option{p} or \option{ppt} will be processed
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If the action is successful, returns a dataframe with all missing precipitation values set to zero. If the action is unsuccessful, then the value \code{FALSE} is returned. If \code{quiet=FALSE}, then a summary of the returned obs data will be returned.
#' @author Kevin Shook
#' @note As described, this function is potentially dangerous, and its action \emph{CANNOT} be undone. It should only be used when there is no further hope for imputing/interpolating precipitation data. Note that its use is really only justified in arid or semi-arid climates, where precipitation is rare. Because it is so destructive, the function should only be used immediately before exporting to an obs file.
#' @seealso \code{\link{writeObsFile}}
#' @examples
#' BadLake7376.clean <- zeroMissingPrecip(BadLake7376, quiet=FALSE) 
#' @export
zeroMissingPrecip <- function(obs, precipCols=0, quiet=TRUE, logfile=''){
  # sets missing precipitation values to zero
  if (nrow(obs) == 0){
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }
  obsName <- deparse(substitute(obs))
  
  obs.names <- names(obs)[-1]
  
  if (precipCols == 0)
    p.loc <- grep("p", tolower(obs.names), fixed=TRUE)
  else
    p.loc <- precipCols
    
  if (length(p.loc > 0))
    for (p in p.loc){
      missing.locs <- is.na(obs[,p+1])
      obs[missing.locs, p+1] <- 0 
    }    
  
  # output info to screen and write to log file

  if (!quiet){
    obs.info <- CRHM_summary(obs)
    print(obs.info)
  }

  comment <- paste('zeroMissingPrecip dataframe:', obsName, sep='')  
  result <- logAction(comment, logfile)
  if(result)
    return(obs)
  else
    return(result)

}