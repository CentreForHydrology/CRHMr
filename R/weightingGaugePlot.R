#' Plots weighing gauge precipitation
#'
#' @description Plots culumative precipitation data from a weighting gauge. The plot contains facets with the cumulative and interval precipitations plotted against time.
#' @param obs Required. A standard \pkg{CRHMr} obs dataframe.
#' @param precipCol Optional. The column containing the cumulative precipitation data. Default is column \code{1}.
#' @param startDate Optional. The starting date for the plot. Can either be a year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e. \option{1995-06-01}. If not specified, the first \code{datetime} value in the obs data is used.
#' @param endDate Optional. The ending date for the plot. Can either be a year as a number (e.g. \code{1995}) or a date in \option{Y-m-d} format, i.e. \option{1995-12-31}. If not specified, the first \code{datetime} value in the obs data is used.
#' @param showMissing Optional. If set to \code{TRUE} (the default), then the position of missing values will be indicated by red points on the x-axis.
#' @return If successful returns a \code{ggplot2} object. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook.
#' @seealso  \code{\link{weighingGauge1}} \code{\link{weighingGauge2}} \code{\link{weighingGauge3}} \code{\link{weighingGauge4}}  \code{\link{weighingGauge5}}
#' @export
#'
#' @examples \dontrun{
#' p <- weighingGaugePlot(test1) }
weighingGaugePlot <- function(obs, precipCol=1, startDate='', endDate='', showMissing=TRUE){
  # suppress checking of dataframe variables used by ggplot2
  datetime <- NULL
  value <- NULL
  
  # check parameters
  if (nrow(obs) == 0){
    cat('Error: missing values\n')
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
  
  obs <- obs[, c(1,precipCol+1)]
  names(obs)[2] <- 'Cumulative'

  # check number of variables
  precipDiff <- c(0, diff(obs[,2]))
  obs$Interval <- precipDiff
  
  # melt data for plotting
  obs.melted <- reshape2::melt(obs, id='datetime')
  
  # now create plot
  p <- ggplot2::ggplot(obs.melted, ggplot2::aes(datetime, value)) + 
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~variable, ncol=1,  scales='free_y') +  
    ggplot2::xlab('') + 
    ggplot2::ylab('Precipitation (mm)')
  
  # find missing values
  
  if(showMissing){
    obs.missing <- obs.melted[is.na(obs.melted$value),]
    if (nrow(obs.missing) > 0){
      obs.missing$value <- 0
      p <- p + ggplot2::geom_point(data=obs.missing, 
                                   ggplot2::aes(datetime, value), col='red', size=2)
    }
  }

  
  # # find runs of missing values
  # nlength <- length(obs.missing$value)
  # starts <- c(as.POSIXct('1970-01-01 01:00', format='%Y-%m-%d %H:%M',tz=''))
  # ends <- c(as.POSIXct('1970-01-01 01:00', format='%Y-%m-%d %H:%M',tz=''))
  # variable <- c(0)
  # for (i in 2:nlength) {
  #   mlength <- obs.missing$datetime[i] - obs.missing$datetime[i-1]
  #   if (mlength > 1) {
  #     starts <- c(starts, obs.missing$datetime[i-1])
  #     ends <- c(ends, obs.missing$datetime[i])
  #     variable <- c(variable, as.character(obs.missing$variable[i]))
  #   }
  # }
  # 
  # # find max and min vals
  # maxval1 <- max(na.omit(obs.melted$value[as.character(obs.melted$variable) == 'Cumulative']))
  # maxval2 <- max(na.omit(obs.melted$value[as.character(obs.melted$variable) == 'Interval']))
  # 
  # minval1 <- min(na.omit(obs.melted$value[as.character(obs.melted$variable) == 'Cumulative']))
  # minval2 <- min(na.omit(obs.melted$value[as.character(obs.melted$variable) == 'Interval']))
  # 
  # # add to dataframe
  # maxval <- rep(maxval1, length(starts))
  # maxval[as.character(obs.missing$variable) == 'Interval'] <- maxval2
  # 
  # minval <- rep(minval1, length(starts))
  # minval[as.character(obs.missing$variable) == 'Interval'] <- minval2
  # 
  # 
  # miss <- data.frame(Start=starts[-1], End=ends[-1], variable=variable[-1], value=NA_real_)
  # p <- p + geom_rect(data=miss, aes(xmin=Start, xmax=End, ymin=-Inf, ymax=Inf, x=1, y=1), fill='pink', alpha=0.2)
  return(p)
  
}