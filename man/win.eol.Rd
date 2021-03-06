% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{win.eol}
\alias{win.eol}
\title{Gets the Windows end of line characters}
\usage{
win.eol()
}
\value{
Returns the Windows end of line characters (cr and lf).
}
\description{
Finds the end of line (eol) characters required for writing Windows files, such as CRHM obs files. No parameters are required. This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly.
}
\note{
This function is used to make the creation of Windows-specific files work on all platforms. CRHM requires its obs and project files to use the Windows end of line characters, which are expressed differently on UNIX-based operating systems such as Linux and OSX.
}
\examples{
windowsEndOfLine <- win.eol()

}
\seealso{
\code{\link{automatePrj}} \code{\link{setPrjDates}} \code{\link{writeObsFile}} \code{\link{runCRHM}}
}
\author{
Kevin Shook
}
\keyword{internal}
