% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeRegular.R
\name{makeRegular}
\alias{makeRegular}
\title{Makes the datetimes of an obs data frame fit the time step exactly}
\usage{
makeRegular(obs, timezone = "", quiet = TRUE, logfile = "")
}
\arguments{
\item{obs}{Required. A \pkg{CRHMr} obs data frame.}

\item{timezone}{Required. Timezone to be assigned to data.}

\item{quiet}{Optional. Suppresses display of messages, except for errors. If you are calling
this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default).
If you are working interactively, you will probably want to set \code{quiet=FALSE}.}

\item{logfile}{Optional. Name of the file to be used for logging the action. Normally not used.}
}
\value{
If successful, returns a modified version of the \code{obs} data frame containing
the adjusted datetime and all of the variables. If unsuccessful, returns an error.
}
\description{
Makes the datetimes of an obs data frame fit the time step exactly
}
\examples{
regular <- makeRegular(BadLake7376, timezone = "CST")
}
\author{
Kevin Shook
}
