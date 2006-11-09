\name{GDALDriver-class}
\docType{class}
\alias{GDALDriver-class}
\alias{getGDALDriverNames}
\alias{gdalDrivers}
\alias{getDriverName}
\alias{getDriverLongName}
\alias{initialize,GDALDriver-method}
\title{Class "GDALDriver": GDAL Driver Object}

\description{\code{GDALDriver} objects encapsulate GDAL file format
  drivers. \code{GDALDriver} inherits from \code{\link{GDALMajorObject-class}}.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GDALDriver", name, handle)}, where name: a string giving the name of a GDAL driver, handle: used internally; not for public consumption (default = NULL).
}

\section{Slots}{
  \describe{
    \item{\code{handle}:}{Object of class \code{"externalptr", from class "GDALMajorObject"}, used internally; not for public consumption}
  }
}
\section{Extends}{
Class \code{"GDALMajorObject"}, directly.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "GDALDriver")}: drivername: a string giving the name of a GDAL driver, handle: used internally; not for public consumption (default = NULL) }
  }
}
\usage{
getGDALDriverNames()
gdalDrivers()
getDriverName(driver)
getDriverLongName(driver)
}
\arguments{
  \item{driver}{An object inheriting from class 'GDALDriver'}
}

\details{
  \describe{
    \item{\code{getGDALDriverNames, gdalDrivers}:}{returns all driver names currently installed in GDAL, with their declared write status.}
  
    \item{\code{getDriverName}:}{returns the GDAL driver name associated
      with the driver object.}

    \item{\code{getDriverLongName}:}{returns a longer driver name.}
  }
}

\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{\code{\link{GDALMajorObject-class}}}

\examples{
gdalDrivers()
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
getDriver(x)
getDriverLongName(getDriver(x))
GDAL.close(x)
}
\keyword{classes}
