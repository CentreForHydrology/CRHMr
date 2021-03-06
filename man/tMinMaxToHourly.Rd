% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tMinMaxToHourly.R
\name{tMinMaxToHourly}
\alias{tMinMaxToHourly}
\title{Converts daily to hourly temperatures}
\usage{
tMinMaxToHourly(
  obs,
  tmin.col = 0,
  tmax.col = 0,
  tmean.col = 0,
  tmin.time = "07:00",
  tmax.time = "15:00",
  tmean.time = "12:00",
  timezone = "",
  quiet = TRUE,
  logfile = ""
)
}
\arguments{
\item{obs}{Required. A \pkg{CRHMr} obs dataframe.}

\item{tmin.col}{Optional. The number of the tmin column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmin' will be used. There must be a tmin column in the dataset}

\item{tmax.col}{Optional. The number of the tmax column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmax' will be used. There must be a tmax column in the dataset.}

\item{tmean.col}{Optional. The number of the tmax column (omitting the datetime) in the obs dataframe. If omitted, the column named 'tmean' will be searched for. This column need not be present in the dataset. If the value is set to \code{-1}, then the mean temperature will not be used in the interpolation.}

\item{tmin.time}{Optional. The time of day at which the minimum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{07:00}.}

\item{tmax.time}{Optional. The time of day at which the maximum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{15:00}.}

\item{tmean.time}{Optional. The time of day at which the maximum air temperature occurs, specified in \option{HH:MM} format. If not specified, defaults to \option{12:00}.}

\item{timezone}{Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.}

\item{quiet}{Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.}

\item{logfile}{Optional. Name of the file to be used for logging the action. Normally not used.}
}
\value{
If successful, returns an obs dataframe with the hourly air temperatures. If not successful, returns \code{FALSE}.
}
\description{
Converts daily values of tmin, tmax and (if they exist) tmean to hourly values, by spline interpolation. Only single columns of the daily temperatures can be used.
}
\note{
Interpolation over intervals longer than 1 day is not allowed.
}
\examples{
\dontrun{
MSC.hourly <- tMinMaxToHourly(MSC.daily, timezone='etc/GMT+7')}
}
\seealso{
\code{\link{interpolate}}
}
\author{
Kevin Shook
}
