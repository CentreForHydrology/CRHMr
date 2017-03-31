#' Plots hydrograph of CRHM output and/or WSC daily and/or peak flows
#'
#' @description Creates a \pkg{ggplot} hydrograph from any of CRHM flows, WSC daily flows and/or WSC peak flows.
#' @param CRHMflows Optional. A data frame of CRHM modelled flows. The flows must be in m\eqn{^3}{^3}/s.
#' @param CRHMflowsLabels Optional. Labels for the CRHM data. If not specified, and CRHM data are plotted, then the name(s) of the CRHM variables will be used.
#' @param CRHMcols Required. Column(s) containing the flowrates. As always, the numbers do not include the \code{datetime}.
#' @param CRHMdaily Optional. Should CRHM flows be plotted as daily values? Default is \code{FALSE}.
#' @param WSCdailyFlows Optional. Dataframe containing WSC daily flows. The data frame is the same as is returned by the function \code{DailyHydrometricData} in the \pkg{HYDAT} package developed by David Hutchinson. The datframe has the columns \code{station_number}, \code{date}, \code{value} and \code{flag}. The \code{date} must be an \R date.
#' @param WSCdailyFlowsLabel Optional. Labels for the daily flows. If not specified, then the value in \code{station_number} will be used, followed by \option{daily}.
#' @param WSCpeakFlows Optional. Dataframe containing WSC annual peak flows for a single station. The data frame is the same as is returned by the function \code{AnnualPeakData} in the \pkg{HYDAT} package developed by David Hutchinson. The data frame has the columns \code{station_number}, \code{data_type}, \code{year}, \code{peak_code}, \code{precision_code}, \code{month}, \code{day}, \code{hour}, \code{minute}, \code{time_zone}, \code{peak}, and \code{symbol}.
#' @param WSCpeakFlowsLabel Optional. Labels for the annual peak flows. If not specified, then the value in \code{station_number} will be used, followed by \option{annual peak}.
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
                       WSCdailyFlows=NULL, WSCdailyFlowsLabel='', WSCpeakFlows=NULL,
                       WSCpeakFlowsLabel='', commonTime=FALSE, fakeDates=FALSE, quiet=TRUE){

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
    # force timezone to be same as current
    CRHMflows$datetime <- lubridate::force_tz(CRHMflows$datetime, tzone=Sys.timezone())

  }
  else{
    if (!quiet)
      cat('No CRHM data selected\n')
    CRHMselected <- FALSE
  }

  if(!is.null(WSCdailyFlows)){
    WSCdailyFlowsSelected <- TRUE

    if(WSCdailyFlowsLabel == '')
      WSCdailyFlowsLabelSpecified <- FALSE
    else
      WSCdailyFlowsLabelSpecified <- TRUE

    if(!WSCdailyFlowsLabelSpecified)
      WSCdailyFlowsLabel <- paste(WSCdailyFlows$station_number[1], ' daily', sep='')

    WSCdailyFlows <- WSCdailyFlows[,c('date', 'value')]
    WSCdailyFlows$label <- WSCdailyFlowsLabel

    # force timezone to be same as current
    WSCdailyFlows <- dateToDatetime(WSCdailyFlows, timezone=Sys.timezone())
    WSCdailyFlows$year <- as.numeric(format(WSCdailyFlows$datetime, format='%Y'))

    WSCdailyMinDatetime <- min(na.omit(WSCdailyFlows$datetime))
    WSCdailyMaxDatetime <- max(na.omit(WSCdailyFlows$datetime))
  }
  else{
    if (!quiet)
      cat('No WSC daily flow data selected\n')
    dailyFlowsSelected <- FALSE
  }

  if(!is.null(WSCpeakFlows)){
    WSCpeakFlowsSelected <- TRUE
    if(WSCpeakFlowsLabel == '')
      WSCpeakFlowsLabelSpecified <- FALSE
    else
      WSCpeakFlowsLabelSpecified <- TRUE

    WSCpeakFlows <- WSCpeakFlows[WSCpeakFlows$peak_code == 'MAXIMUM', ]
    # convert date + time to datetime
    WSCpeakFlows$datetime <- paste(WSCpeakFlows$year,'-', WSCpeakFlows$month,'-', WSCpeakFlows$day,' ',
                                   WSCpeakFlows$hour, ':', WSCpeakFlows$minute, sep='')

    # this will have to be changed
    timezone <- Sys.timezone()


    WSCpeakFlows$datetime <- as.POSIXct(WSCpeakFlows$datetime, format ='%Y-%m-%d %H:%M', tz=timezone)

    # force timezone to be same as current
    WSCpeakFlows$datetime <- lubridate::force_tz(WSCpeakFlows$datetime, tzone=Sys.timezone())
    WSCpeakFlows$year <- as.numeric(format(WSCpeakFlows$datetime, format='%Y'))

    WSCpeakMinDatetime <- min(na.omit(WSCpeakFlows$datetime))
    WSCpeakMaxDatetime <- max(na.omit(WSCpeakFlows$datetime))

    # remove values with missing datetimes
    WSCpeakFlows <- WSCpeakFlows[!is.na(WSCpeakFlows$datetime),]

  }
  else{
    if (!quiet)
      cat('No WSC peak flow data selected\n')
    WSCpeakFlowsSelected <- FALSE
  }

  # make sure there is some data to plot
  if(!CRHMselected & !WSCdailyFlowsSelected & !WSCpeakFlowsSelected){
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
     CRHMminDatetime <- min(CRHMflows$datetime)
     CRHMmaxDatetime <- max(CRHMflows$datetime)
   }


   # check for min and max of all common dates
   if (CRHMselected & WSCdailyFlowsSelected & WSCpeakFlowsSelected){
     commonMinTime <- pmax(CRHMminDatetime, WSCdailyMinDatetime, WSCpeakMinDatetime)
     commonMaxTime <- pmin(CRHMmaxDatetime, WSCdailyMaxDatetime, WSCpeakMaxDatetime)
   }
   else if(CRHMselected & WSCdailyFlowsSelected & !WSCpeakFlowsSelected){
     commonMinTime <- pmax(CRHMminDatetime, WSCdailyMinDatetime)
     commonMaxTime <- pmin(CRHMmaxDatetime, WSCdailyMaxDatetime)
   }
   else if(CRHMselected & !WSCdailyFlowsSelected & WSCpeakFlowsSelected){
     commonMinTime <- pmax(CRHMminDatetime, WSCpeakMinDatetime)
     commonMaxTime <- pmin(CRHMmaxDatetime, WSCpeakMaxDatetime)
   }
   else if(!CRHMselected & WSCdailyFlowsSelected & WSCpeakFlowsSelected){
     commonMinTime <- pmax(WSCdailyMinDatetime, WSCpeakMinDatetime)
     commonMaxTime <- pmin(WSCdailyMaxDatetime, WSCpeakMaxDatetime)
   }
   else if(CRHMselected & !WSCdailyFlowsSelected & !WSCpeakFlowsSelected){
     commonMinTime <- CRHMminDatetime
     commonMaxTime <- CRHMmaxDatetime
   }
   else if(!CRHMselected & WSCdailyFlowsSelected & !WSCpeakFlowsSelected){
     commonMinTime <- WSCdailyMinDatetime
     commonMaxTime <- WSCdailyMaxDatetime
   }
   else{
     commonMinTime <- WSCpeakMinDatetime
     commonMaxTime <- WSCpeakMaxDatetime
   }
 }


  if (CRHMselected){
    CRHMcols <- CRHMcols + 1
    CRHMflows <- CRHMflows[,c(1, CRHMcols)]
    originalNames <- names(CRHMflows)[-1]



    if (CRHMdaily){
      # aggregate CRHM flows to daily
      CRHMcolNum <- ncol(CRHMflows)
      CRHMcols <- seq(1:(CRHMcolNum-1))
      CRHMflowsDaily <- aggDataframe(CRHMflows, columns=CRHMcols, period='daily', funs='mean')

      # convert date to datetime
      # get timezone of data
      tz <- Sys.timezone()
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

      p <- p + ggplot2::geom_step(direction='hv', data=CRHMmelted,
                                   ggplot2::aes(x=datetime,y=value, colour=variable))

    }


  if (WSCdailyFlowsSelected){
    # use common time limits if selected
    if (commonTime)
      WSCdailyFlows <- WSCdailyFlows[(WSCdailyFlows$datetime >= commonMinTime) &
                                 (WSCdailyFlows$datetime <= commonMaxTime),]

    # check to see if fakedates to be used
    if(fakeDates){
       fakeyear=2000
       # add year to use for faceting
       WSCdailyFlows$year <- as.numeric(format(WSCdailyFlows$datetime, format='%Y'))
       WSCdailyFlows$datetime <- fakeDatetime(WSCdailyFlows$datetime, fakeyear)
    }

    p <- p + ggplot2::geom_step(direction='hv', data=WSCdailyFlows,
                               ggplot2::aes(x=datetime, y=value, color=label))
  }

  if (WSCpeakFlowsSelected){
    if(!WSCpeakFlowsLabelSpecified)
      WSCpeakFlowsLabel <- paste(WSCpeakFlows$station_number, ' annual peak', sep='')
    WSCpeakFlows$label <- WSCpeakFlowsLabel

    # use common time limits if selected
    if (commonTime)
      WSCpeakFlows <- WSCpeakFlows[(WSCpeakFlows$datetime >= commonMinTime) &
                               (WSCpeakFlows$datetime <= commonMaxTime),]

    # check to see if fakedates to be used
    if(fakeDates){
      fakeyear <- 2000  # use leap year, just in case
      fakedatetimes <- paste(fakeyear,'-', format(peakFlows$datetime, format='%m-%d %H:%M'), sep='')
      peakFlows$datetime <- as.POSIXct(fakedatetimes, format='%Y-%m-%d %H:%M', tzone='')
    }
    WSCpeakFlows <- WSCpeakFlows[,c('label', 'datetime', 'peak', 'year')]
    WSCpeakFlows <- na.omit(WSCpeakFlows)

    p <- p + ggplot2::geom_point(data=WSCpeakFlows, ggplot2::aes(x=datetime, y=peak,
                        fill=label), color='black', shape=3, size=2)
  }

  return(p)
}
