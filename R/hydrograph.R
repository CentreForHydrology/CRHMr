#' Plots hydrograph of CRHM output and/or WSC daily and/or peak flows
#'
#' @description Creates a \pkg{ggplot} hydrograph from any of CRHM flows, WSC daily flows and/or WSC peak flows.
#' @param CRHMflows Optional. A data frame of CRHM modelled flows. The flows must be in m\eqn{^3}{^3}/s. Remember
#' to divide interval flows by their length in seconds, i.e. 3600 if hourly.
#' @param CRHMflowsLabels Optional. Labels for the CRHM data. If not specified, and CRHM data are plotted, then the name(s) of the CRHM variables will be used.
#' @param CRHMcols Required. Column(s) containing the flowrates. As always, the numbers do not include the \code{datetime}.
#' @param CRHMdaily Optional. Should CRHM flows be plotted as daily values? Default is \code{FALSE}.
#' @param WSCdailyFlowsID Optional. If \code{NULL} (the default) the WSC daily flows will not be plotted. If a WSC station ID
#' is specified , e.g. \code{WSCdailyFlowsID = "05CC001"}, then the daily flows will be obtained from \pkg{tidyhydat} and plotted.
#' @param WSCdailyFlowsLabel Optional. Labels for the daily flows. If not specified, then the WSC station number will be used, followed by \option{daily}.
#' @param WSCpeakFlowsID Optional. If \code{NULL} (the default) the WSC peak flows will not be plotted. If a WSC station number
#' is specified , e.g. \code{WSCpeakFlows = "05CC001"}, then the daily flows will be obtained from \pkg{tidyhydat} and plotted.
#' @param WSCpeakFlowsLabel Optional. Labels for the annual peak flows. If not specified, then the WSC station ID will be used,
#' followed by \option{annual peak}.
#' @param forceMissingPeakTimes Optional. Some peaks may be missing their time of day and/or time zone. If \code{TRUE},
#' the missing peak times will be set to noon on the date of record. The missing time zone will be set to that of the other
#' values or the user's time zone if there are none specified. If \code{FALSE} (the default value) peaks missing times and/or
#' timezones will be deleted from the plot.
#' @param commonTime Optional. If set to \code{TRUE} then the hydrographs will only plotted over their common time range.
#' Default is \code{FALSE}.
#' @param fakeDates Optional. If set to \code{TRUE} then all dates have their year replaced with \code{2000}, and the a
#' ctual year is added as a variable in the plotted data. This allows the plot to be faceted by year, as shown in the examples.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script,
#' you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to
#' set \code{quiet=FALSE}.
#' @param hydat_path Optional. Path to the HYDAT database. This can usually be left blank.
#' @return If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}.
#' @author Kevin Shook
#' @note The CRHM flows are plotted as lines, the daily flows are plotted as steps, and the annual peaks are plotted as points.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- hydrograph(CRHMflows = "BadLakeModelOutput", CRHMflowsLabels = "CRHM Bad Lake model",
#' CRHMcols = 1, WSCpeakFlowsID = "05HF014")
#' # once the ggplot graph has been returned, it can easily be modified:
#' mintime <- as.POSIXct(as.Date('1975-04-01', format='%Y-%m-%d'), tz='etc/GMT+6')
#' maxtime <- as.POSIXct(as.Date('1975-04-30', format='%Y-%m-%d'), tz='etc/GMT+6')
#' # load in all of ggplot2 to modify plots
#' # library(ggplot2)
#' p <- p + xlim(mintime, maxtime) + ylim(0, 4)
#'
#' # re-plot with fake dates
#' p2 <- hydrograph(CRHMflows = "BadLakeModelOutput", CRHMflowsLabels = "CRHM Bad Lake model",
#' CRHMcols = 1, WSCdailyFlowsID = "05HF014", WSCpeakFlowsID = "05HF014",
#' commonTime=TRUE, fakeDates=TRUE)
#' p2 <- p2 + facet_wrap(~year, scales='free_y', ncol=8)
#' # set axis limits to be the specified date range, and set labels to be the month names
#' mintime <- as.POSIXct(as.Date('2000-03-01', format='%Y-%m-%d'), tz='etc/GMT+6')
#' maxtime <- as.POSIXct(as.Date('2000-06-30', format='%Y-%m-%d'), tz='etc/GMT+6')
#' p2 <- p2 + scale_x_datetime(date_breaks = "1 month",
#' limits=c(mintime, maxtime), date_labels = "%b")
#' }
hydrograph <- function(CRHMflows=NULL, CRHMflowsLabels="", CRHMcols=NULL, CRHMdaily=FALSE,
                       WSCdailyFlowsID=NULL, WSCdailyFlowsLabel="", WSCpeakFlowsID=NULL,
                       WSCpeakFlowsLabel="", forceMissingPeakTimes=FALSE, commonTime=FALSE,
                       fakeDates=FALSE, quiet=TRUE, hydat_path = NULL) {

  # suppress checking of data frame variables used by ggplot2
  datetime <- NULL
  value <- NULL
  Value <- NULL
  variable <- NULL
  label <- NULL
  peak <- NULL


  # check parameters to see which data sets are to be plotted

  if (!is.null(CRHMflows)) {
    CRHMselected <- TRUE
    if (CRHMflowsLabels == "") {
      CRHMlabelSpecified <- FALSE
    } else {
      CRHMlabelSpecified <- TRUE
    }
    # force timezone to be same as current
    CRHMflows$datetime <- lubridate::force_tz(CRHMflows$datetime, tzone = Sys.timezone())
  }
  else {
    if (!quiet) {
      cat("No CRHM data selected\n")
    }
    CRHMselected <- FALSE
  }

  if (!is.null(WSCdailyFlowsID)) {
    WSCdailyFlowsSelected <- TRUE

    if (WSCdailyFlowsLabel == "") {
      WSCdailyFlowsLabelSpecified <- FALSE
    } else {
      WSCdailyFlowsLabelSpecified <- TRUE
    }

    if (!WSCdailyFlowsLabelSpecified) {
      WSCdailyFlowsLabel <- paste(WSCdailyFlowsID, " daily", sep = "")
    }

    # get WSC daily flows using tidyhydat
    WSCdailyFlows <- tidyhydat::hy_daily_flows(station_number = WSCdailyFlowsID, hydat_path = hydat_path)
    WSCdailyFlows <- WSCdailyFlows[, c("Date", "Value")]
    names(WSCdailyFlows) <- c("date", "value")
    WSCdailyFlows$label <- WSCdailyFlowsLabel

    # force timezone to be same as current
    WSCdailyFlows <- dateToDatetime(WSCdailyFlows, timezone = Sys.timezone())
    WSCdailyFlows$year <- as.numeric(format(WSCdailyFlows$datetime, format = "%Y"))

    WSCdailyMinDatetime <- min(na.omit(WSCdailyFlows$datetime))
    WSCdailyMaxDatetime <- max(na.omit(WSCdailyFlows$datetime))
  }
  else {
    if (!quiet) {
      cat("No WSC daily flow data selected\n")
    }
    dailyFlowsSelected <- FALSE
  }

  if (!is.null(WSCpeakFlowsID)) {
    WSCpeakFlowsSelected <- TRUE
    if (WSCpeakFlowsLabel == "") {
      WSCpeakFlowsLabelSpecified <- FALSE
    } else {
      WSCpeakFlowsLabelSpecified <- TRUE
    }

    # get WSC peak flows using tidyhydat

    WSCpeakFlows <- tidyhydat::hy_annual_instant_peaks(station_number = WSCpeakFlowsID, hydat_path = hydat_path)
    names(WSCpeakFlows)[2] <- "datetime"

    # select max flows
    WSCpeakFlows <- WSCpeakFlows[WSCpeakFlows$Parameter == "Flow" & WSCpeakFlows$PEAK_CODE == "MAX",]

    if (forceMissingPeakTimes) {
      WSCpeakFlows$hour[is.na(WSCpeakFlows$hour)] <- 12
      WSCpeakFlows$minute[is.na(WSCpeakFlows$minute)] <- 0

      # see if there are any non-missing time zones, and use them

      goodTimeZone <- WSCtimezone[!is.na(WSCtimezone)]
      goodTimeZone <- goodTimeZone[goodTimeZone != "*"]
      goodTimeZone <- goodTimeZone[goodTimeZone != "0"]

      if (length(goodTimeZone) > 0) {
        fakeTZ <- goodTimeZone[1]
      } else {
        fakeTZ <- WSCpeakFlows$time_zone
      }

      WSCtimezone[is.na(WSCtimezone)] <- fakeTZ
    }

      # force timezone to be same as current
    WSCpeakFlows$datetime <- lubridate::force_tz(WSCpeakFlows$datetime, tzone = Sys.timezone())
    WSCpeakFlows$year <- as.numeric(format(WSCpeakFlows$datetime, format = "%Y"))

    WSCpeakMinDatetime <- min(na.omit(WSCpeakFlows$datetime))
    WSCpeakMaxDatetime <- max(na.omit(WSCpeakFlows$datetime))

    # remove values with missing datetimes
    WSCpeakFlows <- WSCpeakFlows[!is.na(WSCpeakFlows$datetime), ]
  }
  else {
    if (!quiet) {
      cat("No WSC peak flow data selected\n")
    }
    WSCpeakFlowsSelected <- FALSE
  }

  # make sure there is some data to plot
  if (!CRHMselected & !WSCdailyFlowsSelected & !WSCpeakFlowsSelected) {
    cat("Error: no data selected to plot\n")
    return(FALSE)
  }
  # assemble hydrograph

  p <- ggplot2::ggplot() +
    ggplot2::xlab("") +
    ggplot2::ylab(expression(paste("Discharge (m", ""^{
      3
    }, "/s)", sep = ""))) +
    ggplot2::theme(legend.title = ggplot2::element_blank())


  # find limits to be plotted
  if (commonTime) {
    if (CRHMselected) {
      CRHMminDatetime <- min(CRHMflows$datetime)
      CRHMmaxDatetime <- max(CRHMflows$datetime)
    }


    # check for min and max of all common dates
    if (CRHMselected & WSCdailyFlowsSelected & WSCpeakFlowsSelected) {
      commonMinTime <- pmax(CRHMminDatetime, WSCdailyMinDatetime, WSCpeakMinDatetime)
      commonMaxTime <- pmin(CRHMmaxDatetime, WSCdailyMaxDatetime, WSCpeakMaxDatetime)
    }
    else if (CRHMselected & WSCdailyFlowsSelected & !WSCpeakFlowsSelected) {
      commonMinTime <- pmax(CRHMminDatetime, WSCdailyMinDatetime)
      commonMaxTime <- pmin(CRHMmaxDatetime, WSCdailyMaxDatetime)
    }
    else if (CRHMselected & !WSCdailyFlowsSelected & WSCpeakFlowsSelected) {
      commonMinTime <- pmax(CRHMminDatetime, WSCpeakMinDatetime)
      commonMaxTime <- pmin(CRHMmaxDatetime, WSCpeakMaxDatetime)
    }
    else if (!CRHMselected & WSCdailyFlowsSelected & WSCpeakFlowsSelected) {
      commonMinTime <- pmax(WSCdailyMinDatetime, WSCpeakMinDatetime)
      commonMaxTime <- pmin(WSCdailyMaxDatetime, WSCpeakMaxDatetime)
    }
    else if (CRHMselected & !WSCdailyFlowsSelected & !WSCpeakFlowsSelected) {
      commonMinTime <- CRHMminDatetime
      commonMaxTime <- CRHMmaxDatetime
    }
    else if (!CRHMselected & WSCdailyFlowsSelected & !WSCpeakFlowsSelected) {
      commonMinTime <- WSCdailyMinDatetime
      commonMaxTime <- WSCdailyMaxDatetime
    }
    else {
      commonMinTime <- WSCpeakMinDatetime
      commonMaxTime <- WSCpeakMaxDatetime
    }
  }


  if (CRHMselected) {
    CRHMcols <- CRHMcols + 1
    CRHMflows <- CRHMflows[, c(1, CRHMcols)]
    originalNames <- names(CRHMflows)[-1]



    if (CRHMdaily) {
      # aggregate CRHM flows to daily
      CRHMcolNum <- ncol(CRHMflows)
      CRHMcols <- seq(1:(CRHMcolNum - 1))
      CRHMflowsDaily <- aggDataframe(CRHMflows, columns = CRHMcols, period = "daily", funs = "mean")

      # convert date to datetime
      # get timezone of data
      tz <- Sys.timezone()
      CRHMflows <- dateToDatetime(CRHMflowsDaily, timezone = tz)
    }

    if (CRHMlabelSpecified) {
      names(CRHMflows)[2] <- CRHMflowsLabels
    } else {
      names(CRHMflows)[2] <- paste(originalNames, "daily", sep = "")
    }

    CRHMmelted <- reshape2::melt(CRHMflows, id.vars = "datetime")


    # add year to use for faceting
    CRHMmelted$year <- as.numeric(format(CRHMmelted$datetime, format = "%Y"))
    CRHMmelted <- na.omit(CRHMmelted)

    # use common time limits if selected
    if (commonTime) {
      CRHMmelted <- CRHMmelted[(CRHMmelted$datetime >= commonMinTime) &
        (CRHMmelted$datetime <= commonMaxTime), ]
    }


    # check to see if fakedates to be used
    if (fakeDates) {
      fakeyear <- 2000 # use leap year, just in case
      CRHMmelted$datetime <- fakeDatetime(CRHMmelted$datetime, fakeyear)
    }

    p <- p + ggplot2::geom_step(
      direction = "hv", data = CRHMmelted,
      ggplot2::aes(x = datetime, y = value, colour = variable)
    )
  }


  if (WSCdailyFlowsSelected) {
    # use common time limits if selected
    if (commonTime) {
      WSCdailyFlows <- WSCdailyFlows[(WSCdailyFlows$datetime >= commonMinTime) &
        (WSCdailyFlows$datetime <= commonMaxTime), ]
    }

    # check to see if fakedates to be used
    if (fakeDates) {
      fakeyear <- 2000
      # add year to use for faceting
      WSCdailyFlows$year <- as.numeric(format(WSCdailyFlows$datetime, format = "%Y"))
      WSCdailyFlows$datetime <- fakeDatetime(WSCdailyFlows$datetime, fakeyear)
    }

    p <- p + ggplot2::geom_step(
      direction = "hv", data = WSCdailyFlows,
      ggplot2::aes(x = datetime, y = value, color = label)
    )
  }

  if (WSCpeakFlowsSelected) {
    if (!WSCpeakFlowsLabelSpecified) {
      WSCpeakFlowsLabel <- paste(WSCpeakFlowsID, " annual peak", sep = "")
    }
    WSCpeakFlows$label <- WSCpeakFlowsLabel

    # use common time limits if selected
    if (commonTime) {
      WSCpeakFlows <- WSCpeakFlows[(WSCpeakFlows$datetime >= commonMinTime) &
        (WSCpeakFlows$datetime <= commonMaxTime), ]
    }

    # check to see if fakedates to be used
    if (fakeDates) {
      fakeyear <- 2000 # use leap year, just in case
      fakedatetimes <- paste(fakeyear, "-", format(WSCpeakFlows$datetime, format = "%m-%d %H:%M"), sep = "")
      WSCpeakFlows$datetime <- as.POSIXct(fakedatetimes, format = "%Y-%m-%d %H:%M", tzone = "")
    }
    WSCpeakFlows <- WSCpeakFlows[, c("label", "datetime", "Value", "year")]
    WSCpeakFlows <- na.omit(WSCpeakFlows)

    p <- p + ggplot2::geom_point(data = WSCpeakFlows, ggplot2::aes(
      x = datetime, y = Value,
      fill = label
    ), color = "black", shape = 3, size = 2)
  }

  return(p)
}
