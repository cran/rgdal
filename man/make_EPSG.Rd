\name{make_EPSG}
\alias{make_EPSG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a data frame of EPSG projection codes}
\description{
  Make a data frame of the European Petroleum Survey Group (EPSG) geodetic parameter dataset as distributed with PROJ.4 software (prior to PROJ 6.0.0, March 2019, only the CSV file, from March 2019 with PROJ >= 6 from the SQLite database). Because finding the correct projection specification is not easy, lists still known as EPSG lists are maintained, and more generally retrieved from databases. The data collated here are as distributed with PROJ.4.
}
\usage{
make_EPSG(file)
}

\arguments{
  \item{file}{file name of the file matching EPSG codes and PROJ.4 arguments, should usually be autodetected; not used for PROJ >= 6}
}
%- maybe also 'usage' for other objects documented here.

\value{
  returns a data frame with columns:
  \item{code}{integer column of EPSG code numbers}
  \item{note}{character column of notes as included in the file}
  \item{prj4}{character column of PROJ.4 arguments for the equivalent projection definitions}
  \item{prj_method}{extra character column from PROJ 6 showing the projection method}
  ...
}
\references{(unlinked because of certificate issues: https://epsg.org/home.html.}

\author{Roger Bivand}
\note{See also Clifford J. Mugnier's Grids \& Datums columns in Photogrammetric Engineering & Remote Sensing, \url{https://www.asprs.org/a/resources/grids/}, see also \code{\link{GridsDatums}}.}
\examples{
EPSG <- try(make_EPSG())
# from PROJ 6.0.0, EPSG data is no longer stored in a flat file
if (!inherits(EPSG, "try-error")) attr(EPSG, "metadata") 
# PROJ.4 version 5 and later include the EPSG version as an attribute
if (!inherits(EPSG, "try-error")) EPSG[grep("Oslo", EPSG$note), 1:2]
if (!inherits(EPSG, "try-error")) EPSG[1925:1927, 3]
if (!inherits(EPSG, "try-error")) EPSG[grep("Poland", EPSG$note), 1:2]
if (!inherits(EPSG, "try-error")) EPSG[grep("Amersfoort", EPSG$note), 1:2]
if (!inherits(EPSG, "try-error")) EPSG[grep("North Carolina", EPSG$note), 1:2]
if (!inherits(EPSG, "try-error")) EPSG[2202, 3]
}
\keyword{spatial}
