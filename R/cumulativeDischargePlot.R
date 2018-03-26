#' Plots cumulative CRHM and/or HYDAT flows
#'
#' @description Creates a \pkg{ggplot} object of annual cumulative flows from data frames of CRHM and/or HYDAT data. The HYDAT flows can be obtained using the \pkg{HYDAT} package created by David Hutchinson. The HYDAT data is truncated so that it only includes the range of the CRHM data. If more than a single year of data is specified, then the plot will be faceted by year.
#' @param CRHMflows Optional. Optional. A data frame of CRHM modelled flows. The flows must be in m\eqn{^3}{^3}/s.
#' @param CRHMflowsLabel Optional. Labels for the CRHM data. If not specified, and CRHM data are plotted, then the name(s) of the CRHM variable(s) will be used.
#' @param CRHMflowCol Optional. Column containing the flowrates, not including the datetime. Default is 1.
#' @param HYDATflows Optional. Data frame containing WSC daily flows. The data frame is the same as is returned by the function \code{DailyHydrometric} in the package \pkg{HYDAT} developed by David Hutchinson. The data frame has the columns \code{STATION_NUMBER}, \code{DATE}, \code{VALUE} and \code{FLAG}. The \code{DATE} must be an \R date.
#' @param HYDATflowsLabel Optional. Labels for the daily flows. If not specified, then the value in \code{STATION_NUMBER} will be used, followed by 'daily'.
#' @param facetCols Optional. Number of columns to wrap the facets (if they are used). Setting a value less than 1 will cause an error.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#'
#' @return If successful, returns a \pkg{ggplot2} object containing the cumulative discharge plot. If unsuccessful, the value \code{FALSE} will be returned.
#' @author Kevin Shook
#' @seealso \code{\link{hydrograph}}
#' @export
#'
#' @examples
#' \dontrun{
#' p <- cumulativeDischargePlot(crhm, 'CRHM', 1, dailyflows, 'HYDAT', facetCols=4, quiet=FALSE)}
cumulativeDischargePlot <- function(CRHMflows=NULL, CRHMflowsLabel="", CRHMflowCol=1,
                                    HYDATflows=NULL, HYDATflowsLabel="", facetCols=3, quiet=TRUE) {

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

  if (!is.null(HYDATflows)) {
    HYDATflowsSelected <- TRUE

    if (HYDATflowsLabel == "") {
      HYDATflowsLabel <- HYDATflows$STATION_NUMBER[1]
    }
  }
  else {
    if (!quiet) {
      cat("No daily flow data selected\n")
    }
    HYDATflowsSelected <- FALSE
  }

  if (!CRHMselected & !HYDATflowsSelected) {
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

  if (HYDATflowsSelected) {
    # get selected column and aggregate it to daily
    HYDATflows <- HYDATflows[, c("DATE", "VALUE")]
    names(HYDATflows) <- c("date", "Q")
    if (CRHMselected) {
      HYDATflows <- HYDATflows[(HYDATflows$date >= CRHMfirstdate) &
        (HYDATflows$date <= CRHMlastdate), ]
    }
    # accumulate
    HYDATcumulative <- plyr::ddply(HYDATflows,
      plyr::.(lubridate::year(date)),
      transform,
      cumulQ = cumsum(Q)
    )

    names(HYDATcumulative)[1] <- "year"
    HYDATcumulative <- HYDATcumulative[, c("date", "year", "cumulQ")]

    # convert from m3/s per day to volume (dam3)
    HYDATcumulative$cumulQ <- HYDATcumulative$cumulQ * 24 * 3600 / 1e6
    HYDATcumulative$variable <- HYDATflowsLabel
  }


  # select data to plot

  if (CRHMselected & HYDATflowsSelected) {
    plotVals <- rbind(CRHMcumulative, HYDATcumulative)
  } else if (CRHMselected) {
    plotVals <- CRHMcumulative
  } else {
    plotVals <- HYDATcumulative
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
