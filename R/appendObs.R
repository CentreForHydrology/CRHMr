#' Appends two CRHM obs data frames
#'
#' @description This function joins two data frames of CRHM obs. The data frames must have the same number of columns, and the variables must be in the same order. The usual reason is for joining two data frames of differing periods. The primary and secondary data frames may be of any time periods. Where there are two values for a given date, the primary values are used. Rows of missing values at the beginning and end of the time series are deleted.
#' @param primaryObs Required. The primary \pkg{CRHMr} data frame of obs values.
#' @param secondaryObs Required. The secondary obs data frame. Note that both data frames must have the same time intervals.
#' @param trim Optional. If set to \code{TRUE} (the default) then rows missing all values at the beginnings and/or ends of the obs data frames will be omitted.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns a data frame of values combined from the \code{primaryObs} and \code{secondaryObs} data frames. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @note In addition to the usual notation in the logfile, this function also writes a separate file which summarises the new data frame. The summaries are also printed to the screen, if \code{quiet=FALSE}, the \code{logfile} also contains a complete listing of the source of each value in the infilled data frame. Each value is listed as being \option{primary} (from the \code{primaryObs} data frame), \option{secondary} (derived from the \code{secondaryObs} data frame) or \option{NA} (missing).
#' @examples
#' \dontrun{
#' broadview <- appendObs(broad2855, broad2856)}
#' @export
appendObs <-
function(primaryObs, secondaryObs, trim=TRUE, quiet=TRUE, logfile=''){

  if (nrow(primaryObs) == 0){
    cat('Error: missing primary obs values\n')
    return(FALSE)
  }
  
  if (nrow(secondaryObs) == 0){
    cat('Error: missing secondary obs values\n')
    return(FALSE)
  }
  
  primaryName <- deparse(substitute(primaryObs))
  secondaryName <- deparse(substitute(secondaryObs))
  
  primaryObs.names <- names(primaryObs)
  secondaryObs.names <- names(secondaryObs)
    
  
  # find timesteps
  ts1 <- timestep.hours(primaryObs$datetime[1], primaryObs$datetime[2])
  ts2 <- timestep.hours(secondaryObs$datetime[1], secondaryObs$datetime[2])
  
  if (ts1 != ts2){
    cat('Error: obs data frames must have the same time step\n')
    return(FALSE)
  }   
  
  # check for required parameters
  primaryObs.cols <- ncol(primaryObs)
  secondaryObs.cols <- ncol(secondaryObs)
  
  if (primaryObs.cols != secondaryObs.cols){
    cat('Error: different numbers of columns\n')
    return(FALSE)
  }

  
  # check for repeated datetimes
  if (anyDuplicated(primaryObs$datetime)){
    primary.dupes <- duplicated(primaryObs$datetime)
    primary <- primary[!primary.dupes,]
  }
  
  if (anyDuplicated(secondaryObs$datetime)){
    secondary.dupes <- duplicated(secondaryObs$datetime)
    secondary <- secondary[!secondary.dupes,]
  }
  
  # find first and last values
  
  if (trim){
    primary.first.datetime <- max(primaryObs$datetime)
    primary.last.datetime <- min(primaryObs$datetime)
    secondary.first.datetime <- max(secondaryObs$datetime)
    secondary.last.datetime <- min(secondaryObs$datetime)
    
    for(colnum in 2:primaryObs.cols){
      selected.primary <- primaryObs[,c(1, colnum)]
      selected.primary.clean <- na.omit(selected.primary)
      if(nrow(selected.primary.clean > 0)){
        primary.first.datetime <- min(primary.first.datetime, selected.primary.clean$datetime) 
        primary.last.datetime <- max(primary.last.datetime, selected.primary.clean$datetime)        
      }
     
      selected.secondary <- secondaryObs[,c(1, colnum)]
      selected.secondary.clean <- na.omit(selected.secondary)
      
      if(nrow(selected.secondary.clean) > 0){
        secondary.first.datetime <- min(secondary.first.datetime, selected.secondary.clean$datetime) 
        secondary.last.datetime <- max(secondary.last.datetime, selected.secondary.clean$datetime) 
      }
    }
    
    # now have max and min datetimes
    first.datetime <- min(primary.first.datetime, secondary.first.datetime)
    last.datetime <- max(primary.last.datetime, secondary.last.datetime) 
  }
  else{
    first.datetime <- min(primaryObs$datetime[1], secondaryObs$datetime[1])
    last.datetime <- max(primaryObs$datetime, secondaryObs$datetime) 
  }
  
  # now generate a time sequence from first to last
  time.seq <- seq(from=first.datetime, to=last.datetime, by=(ts1*3600))
  times <- as.data.frame(time.seq)
  names(times) <- 'datetime'
  
  # merge data frames together
    
  merged1 <- merge(times, primaryObs, by='datetime', all.x=TRUE)
  merged2 <- merge(times, secondaryObs, by='datetime', all.x=TRUE)
  output.data <- merged1
  output.type <- matrix(data='',nrow=nrow(merged1), ncol=ncol(merged1))
  output.type <- as.data.frame(output.type, stringsAsFactors = FALSE)
  output.type[,1] <- times[1]
  output.type[,-1] <- 'primary'
  
  # figure out data type
  for (i in 2:primaryObs.cols ){
    merged1.na <- is.na(merged1[,i])
    output.data[merged1.na, i] <- merged2[merged1.na, i]
    output.type[merged1.na, i] <- 'secondary'
  }
  
  # now output to logfiles
  original.data.info <- CRHM_summary(output.data)
  new.data.info <- CRHM_summary(output.data)
  if (!quiet)
    print(new.data.info)
  
  comment <- paste('append primaryObs:', primaryName,
                   ' primary_variables:', stringr::str_c(primaryObs.names, 
                                                collapse=','),
                   ' secondaryObs:', secondaryName,
                   ' secondary_variables:', stringr::str_c(secondaryObs.names, 
                                                  collapse=','),
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  # write to ChangeLogFile
  comment1 <- paste('primary data :', primaryName, sep='\t')
  comment2 <- paste('secondary data: ', secondaryName, sep='\t')
  
  action <- 'appendObs'
  result <- writeChangeLogFile(action, original.data.info, new.data.info, 
                               output.type, comment1, comment2, quiet=TRUE)
  if (result)
    return(output.data)
  else
    return(result)
}
