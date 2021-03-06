% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fakeDatetimeHydroyear}
\alias{fakeDatetimeHydroyear}
\title{Calculates fake datetime for hydrological year}
\usage{
fakeDatetimeHydroyear(datetime, fakeYear = 2000, startMonth = 10)
}
\arguments{
\item{datetime}{Required. Datetimes to be converted.}

\item{fakeYear}{Required. Base year.}

\item{startMonth}{Required. Start month for hydrological year.}
}
\value{
Returns vector of fake datetimes
}
\description{
Calculates fake datetime for hydrological year
}
\keyword{internal}
