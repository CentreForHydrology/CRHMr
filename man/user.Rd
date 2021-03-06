% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user.R
\name{user}
\alias{user}
\title{Logs information about the user}
\usage{
user(logfile = "")
}
\arguments{
\item{logfile}{Optional. Name of the file to be used for logging the action. Normally not used.}
}
\value{
Returns nothing.
}
\description{
Writes information about the user and the user's computer to the logfile.
}
\note{
This function records information about your computer (the user, the operating system, the version of \R, and all of the packages in use). It will make it easier to debug your problems and may help you to figure out why \R code that used to work is no longer giving the correct answer.
}
\examples{
user()
}
\author{
Kevin Shook
}
