#' Trims a dataframe of missing values at the beginning and end
#'
#' @description Removes values before 01:00 on the first complete day of the data, and after 00:00 on the last complete rows. Used before exporting observations to a CRHM .obs file. For daily obs, removes values before the first complete row and after the last complete row.
#' @param obs Required. A \pkg{CRHMr} obs dataframe.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns the trimmed dataframe. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{writeObsFile}}
#' @examples
#' BadLake.trimmed <- trimObs(BadLake7376)
#' @export

trimObs <-
function(obs,  quiet=TRUE, logfile=''){
  # trims off rows at beginning and end of obs dataframe
  # leaves complete obs data, ready for writing
  
  if (nrow(obs) == 0){
    cat('Error: missing data values\n')
    return(FALSE)    
  }
  
  obsName <- deparse(substitute(obs))
  
  # find time interval
  dt <- timestep.hours(obs[2,1], obs[1,1])
  
  if (dt == 24){
    # daily 
    clean <- na.omit(obs)
    last.row <- nrow(clean)
    first.clean.datetime <- clean$datetime[1]
    last.clean.datetime <- clean$datetime[last.row]
    trimmed <- obs[(obs$datetime >= first.clean.datetime) & 
                     (obs$datetime <= last.clean.datetime)]
  }
  else{
    # find first 01:00 and last 00:00 in clean data
    clean <- na.omit(obs)
    last.row <- nrow(clean)
    first.clean.datetime <- clean$datetime[1]
    last.clean.datetime <- clean$datetime[last.row]
    trimmed <- obs[(obs$datetime >= first.clean.datetime) & 
                     (obs$datetime <= last.clean.datetime),]
    
    hour <- as.numeric(format(trimmed$datetime, format='%H'))
    hour1.loc <- which(hour == dt)[1]
    hour00 <- which(hour == 0)
    hour00.loc <- hour00[length(hour00)]   
    
    trimmed <- trimmed[hour1.loc:hour00.loc,]
  }
  
  # output info to screen and write to log file

  if (!quiet){
    obs.info <- CRHM_summary(trimmed)  
    print(obs.info)
  }
  
  comment <- paste('trimObs dataframe:', obsName, sep='')  
  result <- logAction(comment, logfile)
  if(result)
    return(trimmed)
  else
    return(result)
}
