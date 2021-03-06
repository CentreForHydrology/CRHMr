% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wellHydrograph.R
\name{wellHydrograph}
\alias{wellHydrograph}
\title{Plots hydrograph of daily well depths}
\usage{
wellHydrograph(
  wellObs,
  depthCol = 1,
  fakeDates = FALSE,
  hydroYear = TRUE,
  startMonth = 10,
  quiet = TRUE
)
}
\arguments{
\item{wellObs}{Required. \pkg{CRHMr} obs data frame of daily depths.}

\item{depthCol}{Optional. Column containing depths. Default is 1.}

\item{fakeDates}{Optional. If set to \code{TRUE} then all dates have their year replaced with \code{2000}, and the actual year is added as a variable in the plotted data. This allows the plot to be faceted by year, as shown in the examples.}

\item{hydroYear}{Optional. If \code{TRUE}, the depths are plotted against the hydrological year.}

\item{startMonth}{Optional. Sets the first month of the hydrological year (if required).}

\item{quiet}{Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.}
}
\value{
If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}.
}
\description{
Creates a \pkg{ggplot2} object of hydrograph of well depths.
}
\examples{
\dontrun{
p <- wellHydrograph(well, fakeDates = FALSE)
#' # re-plot with fake dates
p <- wellHydrograph(well, fakeDates = TRUE, hydroYear = TRUE)
p <- p + ggplot2::facet_wrap(~year)
p <- p + ggplot2::scale_x_date(date_breaks = "3 months",
date_labels = "\%b")
}
}
\author{
Kevin Shook
}
