% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fakeDate}
\alias{fakeDate}
\title{Returns fake date using specified year}
\usage{
fakeDate(dates, fakeYear = 2000)
}
\arguments{
\item{dates}{Required. Dates to be converted}

\item{fakeYear}{Required. Year to be used. Default is 2000.}
}
\value{
Returns vector of fake dates.
}
\description{
Returns fake date using specified year
}
\examples{
fakeDate(date())
}
\keyword{internal}
