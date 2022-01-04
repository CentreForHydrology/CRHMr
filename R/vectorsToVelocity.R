#' Convert wind vectors to velocity
#' @name vectorsToVelocity
#' @description Converts wind u and v vectors to a single wind velocity. The vectors may be in any units, i.e. km/h or m/s. 
#' @param uObs Required. A \pkg{CRHMr} obs data frame containing the wind u vectors. 
#' @param uColnum Optional.  The column number containing the u values, not including the datetime. Default is column 1.
#' @param vObs Required. A \pkg{CRHMr} obs data frame containing the wind v vectors. Note that u and v must have the same units!
#' @param vColnum Optional.  The column number containing the u values, not including the datetime. Default is column 1.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used 
#' @return If successful, returns an obs data frame containing the wind speed (in the original vector units) and the wind direction in degrees from North. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' windspeed <- vectorsToVelocity(windObs, uColnum=1, uObs, vColnum=2)}
vectorsToVelocity <- function(uObs, uColnum=1, vObs, vColnum=1, quiet=TRUE, logfile=''){
  # check parameters
  
  uObsName <- deparse(substitute(uObs))
  if (uObsName == ''){
    cat('Error: must specify u vector data frame\n')
    return(FALSE)
  }
  
  if (nrow(uObs) == 0){
    cat('Error: missing u values\n')
    return(FALSE)
  }
  
  vObsName <- deparse(substitute(vObs))
  if (vObsName == ''){
    cat('Error: must specify v vector data frame\n')
    return(FALSE)
  }
  
  if (nrow(vObs) == 0){
    cat('Error: missing v values\n')
    return(FALSE)
  }
  
  # subset data frames
  u <- uObs[,c(1,uColnum)]
  names(u) <- c('datetime', 'u')
  
  v <- vObs[,c(1,vColnum)]
  names(v) <- c('datetime', 'v')  
  
  # merge dataframes
  allData <- merge(u, v, by='datetime', all=TRUE)

  
  allData$windspeed <- (allData$u ^ 2 + allData$v ^ 2) ^ 0.5
  allData$angle <- atan2(-allData$u, -allData$v)
  
  # convert to degrees and orient to north
  allData$direction <- (allData$angle * 180 / pi) + 180
  
  velocity <- allData[, c('datetime', 'windspeed', 'angle')]
  
  # output log files
  obs.info <- CRHM_summary(velocity)
  if (!quiet)
    print(obs.info)
  
  comment <- paste('vectorsToVelocity u_dataframe:', uObsName,
                   ' v_dataframe:', vObsName,
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  if (result)
    return (velocity)
  else
    return(result)
  
}