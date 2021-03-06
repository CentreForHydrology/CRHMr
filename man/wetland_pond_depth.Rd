% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wetland_pond_depth.R
\name{wetland_pond_depth}
\alias{wetland_pond_depth}
\title{Calculates Hayashi-van der Kamp maximum depth of water in a Prairie pond}
\usage{
wetland_pond_depth(pond_volume = NULL, p = NULL, s = NULL)
}
\arguments{
\item{pond_volume}{Required. Volume of water in ponds (in m\eqn{^3}{^3}). May be a vector of values.}

\item{p}{Required. Value of \code{p} exponent for ponds. May be a vector of values.}

\item{s}{Required. Value of \code{s} parameter (pond area for a depth of 1 m) in m\eqn{^2}{^2}) for pond.
May be a vector of values.}
}
\value{
If successful, returns a vector of the pond maximum depths, for the given volumes.
If unsuccessful, returns \code{FALSE}.
}
\description{
Calculates Hayashi-van der Kamp maximum depth of water in a Prairie pond
}
\examples{
wetland_pond_depth(100, 1.72, 100)
}
\references{
\cite{Hayashi, M., and G. van der Kamp (2000), Simple equations to represent the volume–area–depth relations
of shallow wetlands in small topographic depressions, J. Hydrol., 237(1–2), 74–85, doi:10.1016/S0022-1694(00)00300-0.}
}
\seealso{
Other wetland_functions: 
\code{\link{wetland_h_s}()},
\code{\link{wetland_pond_area}()},
\code{\link{wetland_pond_size}()}
}
\author{
Kevin Shook
}
\concept{wetland_functions}
