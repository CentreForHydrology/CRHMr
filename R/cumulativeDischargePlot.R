#' Plots cumulative CRHM and/or WSC flows
#'
#' @description Creates a \pkg{ggplot} object of annual cumulative flows from a data frame of CRHM
#' output and/or WSC daily flows. The WSC flows are obtained from the \pkg{tidyhydat} package.
#' The WSC data is truncated so that it only includes the range of the CRHM data.
#' If more than a single year of data is specified, then the plot will be faceted by year.
#' @param CRHMflows Optional. Optional. A data frame of CRHM modelled flows. The flows must be in m\eqn{^3}{^3}/s.
#' @param CRHMflowsLabel Optional. Labels for the CRHM data. If not specified, and CRHM data are plotted, then the name(s) of the CRHM variable(s) will be used.
#' @param CRHMflowCol Optional. Column containing the flowrates, not including the datetime. Default is 1.
#' @param WSCdailyFlowsID Optional. If \code{NULL} (the default) the WSC daily flows will not be plotted. If a WSC station ID
#' is specified , e.g. \code{WSCdailyFlowsID = "05CC001"}, then the daily flows will be obtained from \pkg{tidyhydat} and plotted.
#' @param WSCdailyFlowsLabel Optional. Labels for the daily flows. If not specified, then the WSC station number will be used, followed by \option{daily}.
#' @param facetCols Optional. Number of columns to wrap the facets (if they are used). Setting a value less than 1 will cause an error.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param hydat_path Optional. Path to the HYDAT database. This can usually be left blank.
#' @return If successful, returns a \pkg{ggplot2} object containing the cumulative discharge plot. If unsuccessful, the value \code{FALSE} will be returned.
#' @author Kevin Shook
#' @seealso \code{\link{hydrograph}}
#' @export
#'
#' @examples
#' \dontrun{
#' p <- cumulativeDischargePlot(crhm, 'CRHM', 1, dailyflows, 'HYDAT', facetCols=4, quiet=FALSE)}
cumulativeDischargePlot <- function(CRHMflows=NULL, CRHMflowsLabel="", CRHMflowCol=1,
                                    WSCdailyFlowsID=NULL, WSCdailyFlowsLabel="", facetCols=3,
                                    quiet=TRUE, hydat_path=NULL) {

  # suppress checking of data frame variables used by ggplot2
  Q <- NULL
  cumulQ <- NULL
  variable <- NULL

  # check parameters
  if (!is.null(CRHMflows)) {
    CRHMselected <- TRUE
    if (CRHMflowsLabel == "") {
      CRHMflowsLabel <- names(CRHMflows)[CRHMflowCol + 1]
    }
  }
  else {
    if (!quiet) {
      cat("No CRHM data selected\n")
    }
    CRHMselected <- FALSE
  }

  if (!is.null(WSCdailyFlowsID)) {
    WSCflowsSelected <- TRUE

    if (WSCdailyFlowsLabel == "") {
      WSCdailyFlowsLabel <- WSCdailyFlowsID
    }
  }
  else {
    if (!quiet) {
      cat("No daily flow data selected\n")
    }
    WSCflowsSelected <- FALSE
  }

  if (!CRHMselected & !WSCflowsSelected) {
    cat("Error: no data selected\n")
    return(FALSE)
  }

  if (CRHMselected) {
    # get selected column and aggregate it to daily
    CRHMflows <- CRHMflows[, c(1, (CRHMflowCol + 1))]
    CRHMdaily <- aggDataframe(CRHMflows, columns = 1, period = "daily", funs = "mean")
    names(CRHMdaily) <- c("date", "Q")
    # the next line should not be necessary in CRHM version > 1.4.7
    CRHMdaily$date <- as.Date(CRHMdaily$date)
    CRHMfirstdate <- CRHMdaily$date[1]
    CRHMlastdate <- CRHMdaily$date[nrow(CRHMdaily)]

    # accumulate
    CRHMcumulative <- plyr::ddply(CRHMdaily, plyr::.(lubridate::year(date)), transform,
      cumulQ = cumsum(Q)
    )

    names(CRHMcumulative)[1] <- "year"
    CRHMcumulative <- CRHMcumulative[, c("date", "year", "cumulQ")]

    # convert from m3/s per day to volume (dam3)
    CRHMcumulative$cumulQ <- CRHMcumulative$cumulQ * 24 * 3600 / 1e6
    CRHMcumulative$variable <- CRHMflowsLabel
  }

  if (WSCflowsSelected) {
    # get WSC daily flows using tidyhydat


    if (CRHMselected) {
      WSCdailyFlows <- tidyhydat::hy_daily_flows(station_number = WSCdailyFlowsID,
                                                 hydat_path = hydat_path,
                                                 start_date = CRHMfirstdate,
                                                 end_date = CRHMlastdate)

    } else {
      WSCdailyFlows <- tidyhydat::hy_daily_flows(station_number = WSCdailyFlowsID, hydat_path = hydat_path)
    }

    WSCdailyFlows <- WSCdailyFlows[, c("Date", "Value")]
    names(WSCdailyFlows) <- c("date", "Q")
    # accumulate
    WSCcumulative <- plyr::ddply(WSCdailyFlows,
      plyr::.(lubridate::year(date)),
      transform,
      cumulQ = cumsum(Q)
    )

    names(WSCcumulative)[1] <- "year"
    WSCcumulative <- WSCcumulative[, c("date", "year", "cumulQ")]

    # convert from m3/s per day to volume (dam3)
    WSCcumulative$cumulQ <- WSCcumulative$cumulQ * 24 * 3600 / 1e6
    WSCcumulative$variable <- WSCdailyFlowsLabel
  }


  # select data to plot

  if (CRHMselected & WSCflowsSelected) {
    plotVals <- rbind(CRHMcumulative, WSCcumulative)
  } else if (CRHMselected) {
    plotVals <- CRHMcumulative
  } else {
    plotVals <- WSCcumulative
  }

  # now do plot
  numyears <- length(unique(plotVals$year))
  colors <- c("red", "blue")
  p <- ggplot2::ggplot(plotVals, ggplot2::aes(date, cumulQ, color = variable)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_colour_manual(values = colors) +
    ggplot2::xlab("") +
    ggplot2::ylab(expression(paste("Cumulative discharge (dam", ""^{
      3
    }, ")", sep = ""))) +
    ggplot2::scale_x_date(
      labels = scales::date_format("%b"),
      breaks = scales::date_breaks("3 months")
    )

  if (numyears > 1) {
    p <- p + ggplot2::facet_wrap(~ year, ncol = facetCols, scales = "free")
  }

  return(p)
}
