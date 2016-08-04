#' Distribute instantaneous values
#'
#' @description Distributes instantaneous values to a shorter time interval. The missing datetimes are inserted and then the values are interpolated. This function is typically used to downscale obs values such as t, ea, and u.
#' @param obs Required. The \pkg{CRHMr} data frame of obs values.
#' @param obsCols Optional. A vector containing the columns to be imputed in the obs data frame, not including the datetime. The default \option{all} specifies all columns.
#' @param timeStep Required. The time step (in hours) for the interpolated values. This value must be smaller than the time step in the original time series.
#' @param interpolationMethod Optional. A vector containing the methods to be used for interpolation for each of the variables. Currently supported methods are \option{linear} and \option{spline}. The default is to use linear interpolation. If fewer methods than columns are specified, the methods are recycled.
#' @param maxLength Optional. The maximum gap length to be interpolated. Defaults to 5 time steps.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not use
#'
#' @return If successful, returns a dataframe of the selected columns interpolated to the specified time step. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' hourly_vals <- distributeInst(vegreville, c(1,2,3), timeStep = 1)}
distributeInst <- function(obs,  obsCols='all', timeStep=0, interpolationMethod='linear', maxLength=5,
                           quiet=TRUE, logfile=''){


  if (nrow(obs) == 0){
    cat('Error: missing any values\n')
    return(FALSE)
  }
  obs_name <- deparse(substitute(obs))
  # subset dataframe
  if (length(obsCols) == 1){
    if (obsCols != 'all'){
      var_cols <- seq(2, ncol(obs))
      obs <- obs
    }
  }
  else{
    var_cols <- obsCols + 1
    obs <- obs[,c(1,var_cols)]
  }

  if (timeStep <= 0){
    cat('Error: time step must be specified and > 0\n')
    return(FALSE)
  }
  
  dt <- as.numeric(difftime(obs$datetime[2], obs$datetime[1], units='hours'))
  
  if (timeStep >= dt){
    cat('Error: time step must be smaller than in the original data\n')
    return(FALSE)
  }
  
  # create dataframe and merge values into it
  clean <- na.omit(obs)
  first.clean.datetime <- clean[1,1]
  last.clean.datetime <- clean[nrow(clean),1]
  cleanDatetime <- seq(from=first.clean.datetime, 
                       to=last.clean.datetime, by=timeStep*3600)
  
  
  cleanDatetime <- data.frame(cleanDatetime)
  names(cleanDatetime) <- 'datetime'
  cleanVals <- merge(cleanDatetime, obs, by='datetime', all.x=TRUE)
 
  # now have a dataframe with all datetimes and NA for missing values
  # now interpolate values
  varcols <- seq(1, (ncol(cleanVals)-1))
  distrib <- interpolate(cleanVals, varcols, methods=interpolationMethod, 
                         maxlength=maxLength, quiet=quiet, logfile=logfile)
  
  # output log files
  obs.info <- CRHM_summary(distrib)
  if (!quiet)
    print(obs.info)
  
  comment <- paste('distributeInst dataframe:', obs_name, 
                   ' Variables:', stringr::str_c(names(distrib)[-1], collapse=','),
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  if (result)
    return(distrib)
  else
    return(result)
  
}