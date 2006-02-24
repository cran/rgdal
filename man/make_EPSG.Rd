\name{make_EPSG}
\alias{make_EPSG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a data frame of EPSG projection codes}
\description{
  Make a data frame of the now-defunct European Petroleum Survey Group (EPSG) geodetic parameter dataset as distributed with PROJ.4 software and included in this package. Because finding the correct projection specification is not easy, lists still known as EPSG lists are maintained, and more generally retrieved from Access databases. The data collated here are as distributed with PROJ.4.
}
\usage{
make_EPSG(file=system.file("proj/epsg", package="rgdal"))
}

\arguments{
  \item{file}{file name of the file matching EPSG codes and PROJ.4 arguments}
}
%- maybe also 'usage' for other objects documented here.

\value{
  returns a data frame with columns:
  \item{code}{integer column of EPSG code numbers}
  \item{note}{character column of notes as included in the file}
  \item{prj4}{character column of PROJ.4 arguments for the equivalent projection definitions}
  ...
}
\references{\url{http://www.epsg.org/}}

\author{Roger Bivand}
\note{See also Clifford J. Mugnier's Grids \& Datums columns in Photogrammetric Engineering & Remote Sensing, \url{http://www.asprs.org/resources/GRIDS/}}
\examples{
EPSG <- make_EPSG()
EPSG[grep("Oslo", EPSG$note), 1:2]
EPSG[1925:1927, 3]
EPSG[grep("Poland", EPSG$note), 1:2]
EPSG[grep("Amersfoort", EPSG$note), 1:2]
EPSG[grep("North Carolina", EPSG$note), 1:2]
EPSG[2202, 3]
}
\keyword{spatial}
