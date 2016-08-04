#' Creates a \pkg{ggplot} object from an obs data frame
#'
#' @description This function creates a \pkg{ggplot2} object from an obs data frame. If the specified variables are all of single type (e.g. temperature), it returns a single-pane plot. If the specified variables are of more than one type (e.g. temperature, precipitation), then the plot is faceted. The function returns an object that you can modify, or save with the standard \pkg{ggplot2} commands.
#' @param obs Required. The \pkg{CRHMr} obs data frame to be plotted.
#' @param varcols Optional. A vector containing the numbers of the columns (not including the datatime) to be plotted. If not specified, all variables are plotted.
#' @param startDate Optional. The starting date for the plot. Can either be a year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e. \option{1995-06-01}. If not specified, the first \code{datetime} value in the obs data is used.
#' @param endDate Optional. The ending date for the plot. Can either be a year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e. \option{1995-12-31}. If not specified, the first \code{datetime} value in the obs data is used.
#' @param plotType Optional. Defines the type of the plot. Either \option{lines} or \option{points}. Default is \option{lines}. If you want to show the extent of data, use \option{points} as lines will NOT be drawn when points are separated by \code{NA_real_} values.  
#' @details Units are automatically specified for obs variables of the following types: \code{t}, \code{rh}, \code{u}, \code{p}, \code{ea}, and solar radiation.
#' @note Note that missing (\code{NA_real_}) values in the obs data frame will result in a warning message.
#' @return If successful returns a \pkg{ggplot2} object. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso  \code{\link{summariseObsFiles}}
#' @examples
#' p <- plotObs(BadLake7376)
#' print(p)
#' @export

plotObs <-
function(obs, varcols=c(0), startDate='', endDate='', plotType='lines'){
  # suppress checking of data frame variables used by ggplot2
  datetime <- NULL
  value <- NULL
  variable <- NULL
  
  # check parameters
  if (nrow(obs) == 0){
    cat('Error: missing obs values\n')
    return(FALSE)
  }

  # convert obs datetime timezone to user's
  obs$datetime <- lubridate::force_tz(obs$datetime, tzone='')
  
  # get start and end dates, and subset
  if (startDate != ''){
    if (stringr::str_length(startDate) == 4){
      # year
      startDate <- paste(startDate,'-01-01 00:00', sep='')
    }
    else
      startDate <- paste(startDate,' 00:00', sep='')
    
  startDate <- as.POSIXct(startDate, format='%Y-%m-%d %H:%M',tz='')
  obs <- obs[obs$datetime >= startDate,]
  }
  
  if (endDate != ''){
    if (stringr::str_length(endDate) == 4){
      endDate <- paste(endDate,'-12-31 23:00', sep='')
    }
    else
      endDate <- paste(endDate,' 00:00', sep='')
    
    endDate <- as.POSIXct(endDate, format='%Y-%m-%d %H:%M',tz='')
    obs <- obs[obs$datetime <= endDate,]
  }
  
    
  # select variables for plotting
  if (varcols[1] != 0){
    obs <- obs[, c(1,varcols+1)]
  }
  
  # melt data for plotting
  obs.melted <- reshape2::melt(obs, id='datetime')
  
  # add units
  obs.melted$units <- 'Other'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('ea',ignore_case=TRUE))] <- 
    'Vapour pressure (kPa)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('t',ignore_case=TRUE))] <- 
    'Temperature (C)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('rh',ignore_case=TRUE))] <- 
    'RH (%)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('u',ignore_case=TRUE))] <- 
    'Windspeed (m/s)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('p',ignore_case=TRUE))] <- 
    'Precipitation (mm)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('q',ignore_case=TRUE))] <- 
    'Radiation (W/m2)'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('SunAct',ignore_case=TRUE))] <- 
    'Hours'
  obs.melted$units[stringr::str_detect(obs.melted$variable, stringr::fixed('airp',ignore_case=TRUE))] <- 
    'Pa'

   # check for number of variable types
  unit_types <- (unique(obs.melted$units))

 # now do plot

 if (length(unit_types) > 1){
   # facet plots
   # sort variables in order of units
   var_types <- data.frame(unique(obs.melted$variable))
   names(var_types) <- 'variable'
   merged <- merge(var_types, obs.melted, by='variable')
   merged <- unique(merged[,c(1,4)])
   merged_ordered <- merged[order(merged$variable),]
   unit_levels <- merged_ordered$units
   
   obs.melted$unit_factor <- factor(obs.melted$units, levels = unique(unit_levels))
   
   p <- ggplot2::ggplot(obs.melted, ggplot2::aes(datetime, value, colour=variable)) 
   if (stringr::str_detect(plotType, stringr::fixed('lin', ignore_case=TRUE)))
     p <- p + ggplot2::geom_line() 
   else
     p <- p + ggplot2::geom_point()
     
   p <- p + ggplot2::xlab('') + ggplot2::ylab('') +
     ggplot2::facet_grid(unit_factor ~ ., scales='free_y')
 }
 else{
   # unfacetted plot
   p <- ggplot2::ggplot(obs.melted, ggplot2::aes(datetime, value, colour=variable)) + 
     ggplot2::xlab('')
   
   if (stringr::str_detect(plotType, stringr::fixed('lin', ignore_case=TRUE)))
     p <- p + ggplot2::geom_line() 
   else
     p <- p + ggplot2::geom_point()
   
   p <- p + ggplot2::ylab(unit_types)
 }
  
 
  return(p)
}
