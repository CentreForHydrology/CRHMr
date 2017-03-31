#' Plots hydrograph of CRHM output and/or WSC daily and/or peak flows
#'
#' @description Creates a \pkg{ggplot} hydrograph from any of CRHM flows, WSC daily flows and/or WSC peak flows.
#' @param CRHMflows Optional. A data frame of CRHM modelled flows. The flows must be in m\eqn{^3}{^3}/s.
#' @param CRHMflowsLabels Optional. Labels for the CRHM data. If not specified, and CRHM data are plotted, then the name(s) of the CRHM variables will be used.
#' @param CRHMcols Required. Column(s) containing the flowrates. As always, the numbers do not include the \code{datetime}.
#' @param CRHMdaily Optional. Should CRHM flows be plotted as daily values? Default is \code{FALSE}.
#' @param dailyFlows Optional. Dataframe containing WSC daily flows. The data frame is the same as is returned by the function \code{DailyHydrometricData} in the \pkg{HYDAT} package developed by David Hutchinson. The datframe has the columns \code{station_number}, \code{date}, \code{value} and \code{flag}. The \code{date} must be an \R date.
#' @param dailyFlowsLabel Optional. Labels for the daily flows. If not specified, then the value in \code{station_number} will be used, followed by \option{daily}.
#' @param peakFlows Optional. Dataframe containing WSC annual peak flows for a single station. The data frame is the same as is returned by the function \code{AnnualPeakData} in the \pkg{HYDAT} package developed by David Hutchinson. The data frame has the columns \code{station_number}, \code{data_type}, \code{year}, \code{peak_code}, \code{precision_code}, \code{month}, \code{day}, \code{hour}, \code{minute}, \code{time_zone}, \code{peak}, and \code{symbol}.
#' @param peakFlowsLabel Optional. Labels for the annual peak flows. If not specified, then the value in \code{station_number} will be used, followed by \option{annual peak}.
#' @param commonTime Optional. If set to \code{TRUE} then the hydrographs will only plotted over their common time range. Default is \code{FALSE}.
#' @param fakeDates Optional. If set to \code{TRUE} then all dates have their year replaced with \code{2000}, and the actual year is added as a variable in the plotted data. This allows the plot to be faceted by year, as shown in the examples.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @return If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note The CRHM flows are plotted as lines, the daily flows are plotted as steps, and the annual peaks are plotted as points.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- hydrograph(BadLakeModel,'CRHM Bad Lake model',1, dailyFlows, '', peakFlows, '')
#' # once the ggplot graph has been returned, it can easily be modified:
#' mintime <- as.POSIXct(as.Date('1975-04-01', format='%Y-%m-%d'), tz='etc/GMT+6')
#' maxtime <- as.POSIXct(as.Date('1975-04-30', format='%Y-%m-%d'), tz='etc/GMT+6')
#' p <- p + xlim(mintime, maxtime) + ylim(0, 4)
#'
#' # re-plot with fake dates
#' p2 <- hydrograph(BadLakeModel,'CRHM Bad Lake model',1, dailyFlows, '', peakFlows, '',
#' commonTime=TRUE, fakeDates=TRUE)
#' p2 <- p2 + facet_wrap(~year, scales='free_y', ncol=8)
#' # set axis limits to be the specified date range, and set labels to be the month names
#' mintime <- as.POSIXct(as.Date('2000-03-01', format='%Y-%m-%d'), tz='etc/GMT+6')
#' maxtime <- as.POSIXct(as.Date('2000-06-30', format='%Y-%m-%d'), tz='etc/GMT+6')
#' p2 <- p2 + scale_x_datetime(date_breaks = "1 month",
#' limits=c(mintime, maxtime), date_labels = "%b")
#' }
hydrograph <- function(CRHMflows=NULL, CRHMflowsLabels='', CRHMcols=NULL, CRHMdaily=FALSE,
                       dailyFlows=NULL, dailyFlowsLabel='', peakFlows=NULL,
                       peakFlowsLabel='', commonTime=FALSE, fakeDates=FALSE, quiet=TRUE){

  # suppress checking of data frame variables used by ggplot2
  datetime <- NULL
  value <- NULL
  variable <- NULL
  value <- NULL
  label <- NULL
  peak <- NULL


  # check parameters
  if(!is.null(CRHMflows)){
    CRHMselected <- TRUE
    if(CRHMflowsLabels == '')
      CRHMlabelSpecified <- FALSE
    else
      CRHMlabelSpecified <- TRUE
  }
  else{
    if (!quiet)
      cat('No CRHM data selected\n')
    CRHMselected <- FALSE
  }

  if(!is.null(dailyFlows)){
    dailyFlowsSelected <- TRUE

    if(dailyFlowsLabel == '')
      dailyFlowsLabelSpecified <- FALSE
    else
      dailyFlowsLabelSpecified <- TRUE
  }
  else{
    if (!quiet)
      cat('No daily flow data selected\n')
    dailyFlowsSelected <- FALSE
  }

  if(!is.null(peakFlows)){
    peakFlowsSelected <- TRUE
    if(peakFlowsLabel == '')
      peakFlowsLabelSpecified <- FALSE
    else
      peakFlowsLabelSpecified <- TRUE
  }
  else{
    if (!quiet)
      cat('No peak flow data selected\n')
    peakFlowsSelected <- FALSE
  }

  # make sure there is some data to plot
  if(!CRHMselected & !dailyFlowsSelected & !peakFlowsSelected){
    cat('Error: no data selected to plot\n')
    return(FALSE)
  }
  # assemble hydrograph

 p <- ggplot2::ggplot() +
   ggplot2::xlab('') +
   ggplot2::ylab(expression(paste('Discharge (m',''^{3}, '/s)', sep = "")))+
   ggplot2::theme(legend.title=ggplot2::element_blank())


 # find limits to be plotted
 if (commonTime){
   if (CRHMselected){
     CRHMminDate <- min(CRHMflows$datetime)
     CRHMmaxDate <- max(CRHMflows$datetime)
   }

   if (dailyFlowsSelected){
     # force timezone to be same as current
     dailyFlows$datetime <- paste(format(dailyFlows$date, format='%Y-%m-%d'), ' 00:00', sep='')
     dailyFlows$datetime <- as.POSIXct(dailyFlows$datetime, format='%Y-%m-%d %H:%M', tzone='')
     dailyFlows$year <- as.numeric(format(dailyFlows$datetime, format='%Y'))

     dailyMinDate <- min(na.omit(dailyFlows$datetime))
     dailyMaxDate <- max(na.omit(dailyFlows$datetime))
   }

   if (peakFlowsSelected){

     peakFlows <- peakFlows[peakFlows$peak_code == 'MAXIMUM', ]
     # convert date + time to datetime
     peakFlows$datetime <- paste(peakFlows$year,'-', peakFlows$month,'-', peakFlows$day,' ',
                                 peakFlows$hour, ':', peakFlows$minute, sep='')
     timezone <- peakFlows$time_zone[1]
     peakFlows$datetime <- as.POSIXct(peakFlows$datetime, format ='%Y-%m-%d %H:%M', tz=timezone)

     # force timezone to be same as current
     peakFlows$datetime <- lubridate::force_tz(peakFlows$datetime, tzone='')
     peakFlows$year <- as.numeric(format(peakFlows$datetime, format='%Y'))

     peakMinDate <- min(na.omit(peakFlows$datetime))
     peakMaxDate <- max(na.omit(peakFlows$datetime))

     # remove values with missing datetimes
     peakFlows <- peakFlows[!is.na(peakFlows$datetime),]

   }

   # check for min and max of all common dates
   if (CRHMselected & dailyFlowsSelected & peakFlowsSelected){
     commonMinTime <- pmax(CRHMminDate, dailyMinDate, peakMinDate)
     commonMaxTime <- pmin(CRHMmaxDate, dailyMaxDate, peakMaxDate)
   }
   else if(CRHMselected & dailyFlowsSelected & !peakFlowsSelected){
     commonMinTime <- pmax(CRHMminDate, dailyMinDate)
     commonMaxTime <- pmin(CRHMmaxDate, dailyMaxDate)
   }
   else if(CRHMselected & !dailyFlowsSelected & peakFlowsSelected){
     commonMinTime <- pmax(CRHMminDate, peakMinDate)
     commonMaxTime <- pmin(CRHMmaxDate, peakMaxDate)
   }
   else if(!CRHMselected & dailyFlowsSelected & peakFlowsSelected){
     commonMinTime <- pmax(dailyMinDate, peakMinDate)
     commonMaxTime <- pmin(dailyMaxDate, peakMaxDate)
   }
   else if(CRHMselected & !dailyFlowsSelected & !peakFlowsSelected){
     commonMinTime <- CRHMminDate
     commonMaxTime <- CRHMmaxDate
   }
   else if(!CRHMselected & dailyFlowsSelected & !peakFlowsSelected){
     commonMinTime <- dailyMinDate
     commonMaxTime <- dailyMaxDate
   }
   else{
     commonMinTime <- peakMinDate
     commonMaxTime <- peakMaxDate
   }
 }


  if (CRHMselected){
    CRHMcols <- CRHMcols + 1
    CRHMflows <- CRHMflows[,c(1, CRHMcols)]
    originalNames <- names(CRHMflows)[-1]

    # force timezone to be same as current
    CRHMflows$datetime <- lubridate::force_tz(CRHMflows$datetime, tzone='')


    if (CRHMdaily){
      # aggregate CRHM flows to daily
      CRHMcolNum <- ncol(CRHMflows)
      CRHMcols <- seq(1:(CRHMcolNum-1))
      CRHMflowsDaily <- aggDataframe(CRHMflows, columns=CRHMcols, period='daily', funs='mean')

      # convert date to datetime
      # get timezone of data
      tz <- format(CRHMflows[1,1], format='%Z')
      CRHMflows  <- dateToDatetime(CRHMflowsDaily, timezone=tz)
      if (CRHMlabelSpecified)
        names(CRHMflows)[-1] <- CRHMflowsLabels
      else
        names(CRHMflows)[-1] <- paste(originalNames, 'daily', sep='')

    }


      CRHMmelted <- reshape2::melt(CRHMflows, id.vars='datetime')


      # add year to use for faceting
      CRHMmelted$year <- as.numeric(format(CRHMmelted$datetime, format='%Y'))
      CRHMmelted <- na.omit(CRHMmelted)

      # use common time limits if selected
      if (commonTime)
        CRHMmelted <- CRHMmelted[(CRHMmelted$datetime >= commonMinTime) &
                                   (CRHMmelted$datetime <= commonMaxTime),]


      # check to see if fakedates to be used
      if(fakeDates){
        fakeyear <- 2000  # use leap year, just in case
        CRHMmelted$datetime <- fakeDatetime(CRHMmelted$datetime, fakeyear)

      }

      p <- p +  ggplot2::geom_step(direction='hv', data=CRHMmelted,
                                   ggplot2::aes(x=datetime,y=value, colour=variable))

    }


  if (dailyFlowsSelected){
    if(!dailyFlowsLabelSpecified)
      dailyFlowsLabel <- paste(dailyFlows$station_number[1], ' daily', sep='')



    # convert date to datetime

    if(CRHMselected)
      tz <- format(CRHMflows$datetime[1], format='%Z')
    else if(peakFlowsSelected)
      tz <- format(peakFlows$datetime[1], format='%Z')
    else
      tz <- Sys.timezone()

    dailyFlows <- dailyFlows[,c('date', 'value')]
    dailyFlows$label <- dailyFlowsLabel
    dailyFlows <- dateToDatetime(dailyFlows, timezone=tz)

    # use common time limits if selected
    if (commonTime)
      dailyFlows <- dailyFlows[(dailyFlows$datetime >= commonMinTime) &
                                 (dailyFlows$datetime <= commonMaxTime),]

    # check to see if fakedates to be used
    if(fakeDates){
       fakeyear=2000
       # add year to use for faceting
       dailyFlows$year <- as.numeric(format(dailyFlows$datetime, format='%Y'))
       dailyFlows$datetime <- fakeDatetime(dailyFlows$datetime, fakeyear)
    }

    p <- p + ggplot2::geom_step(direction='hv', data=dailyFlows,
                               ggplot2::aes(x=datetime, y=value, color=label))
  }

  if (peakFlowsSelected){
    if(!peakFlowsLabelSpecified)
      peakFlowsLabel <- paste(peakFlows$station_number, ' annual peak', sep='')
    peakFlows$label <- peakFlowsLabel

    # use common time limits if selected
    if (commonTime)
      peakFlows <- peakFlows[(peakFlows$datetime >= commonMinTime) &
                               (peakFlows$datetime <= commonMaxTime),]

    # check to see if fakedates to be used
    if(fakeDates){
      fakeyear <- 2000  # use leap year, just in case
      fakedatetimes <- paste(fakeyear,'-', format(peakFlows$datetime, format='%m-%d %H:%M'), sep='')
      peakFlows$datetime <- as.POSIXct(fakedatetimes, format='%Y-%m-%d %H:%M', tzone='')
    }
    peakFlows <- peakFlows[,c('label', 'datetime', 'peak', 'year')]
    peakFlows <- na.omit(peakFlows)

    p <- p + ggplot2::geom_point(data=peakFlows, ggplot2::aes(x=datetime, y=peak,
                        fill=label), color='black', shape=3, size=2)
  }

  return(p)
}
