\name{GDALDriver-class}
\docType{class}
\alias{GDALDriver-class}
\alias{getGDALDriverNames}
\alias{gdalDrivers}
\alias{getDriverName}
\alias{getDriverLongName}
\alias{initialize,GDALDriver-method}
\alias{getGDALVersionInfo}
\alias{getGDAL_DATA_Path}
\alias{getGDALCheckVersion}
\alias{getGDALwithGEOS}
\alias{getCPLConfigOption}
\alias{setCPLConfigOption}
\alias{rgdal_extSoftVersion}
\alias{get_cached_orig_PROJ_LIB}
\alias{get_cached_set_PROJ_LIB}
\alias{get_cached_orig_GDAL_DATA}
\alias{get_cached_set_GDAL_DATA}
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
getGDALVersionInfo(str = "--version")
getGDALCheckVersion()
getGDALwithGEOS()
rgdal_extSoftVersion()
getCPLConfigOption(ConfigOption)
setCPLConfigOption(ConfigOption, value)
get_cached_orig_PROJ_LIB()
get_cached_orig_GDAL_DATA()
get_cached_set_PROJ_LIB()
get_cached_set_GDAL_DATA()
}
\arguments{
  \item{driver}{An object inheriting from class 'GDALDriver'}
  \item{str}{A string, may be one of \code{"--version"}, \code{"VERSION_NUM"}, \code{"RELEASE_DATE"}, \code{"RELEASE_NAME"}}
  \item{ConfigOption}{CPL configure option documented in \url{https://trac.osgeo.org/gdal/wiki/ConfigOptions} and elsewhere in GDAL source code}
  \item{value}{a string value to set a CPL option; NULL is used to unset the CPL option}
}

\details{
  \describe{
    \item{\code{getGDALDriverNames, gdalDrivers}:}{returns all driver names currently installed in GDAL, with their declared create and copy status (some drivers can create datasets, others can only copy from a prototype with a different driver.}
  
    \item{\code{getDriverName}:}{returns the GDAL driver name associated
      with the driver object.}

    \item{\code{getDriverLongName}:}{returns a longer driver name.}
    \item{\code{getGDALVersionInfo}:}{returns the version of the GDAL runtime shared object.}
    \item{\code{getGDALCheckVersion}:}{checks the version of the GDAL headers used when building the package (GDAL_VERSION_MAJOR, GDAL_VERSION_MINOR) - if the two versions differ, problems may arise (the C++ API/ABI may have changed), and rgdal should be re-installed}
    \item{\code{getGDALwithGEOS}:}{because drivers may behave differently if GDAL itself was built with GEOS support, the function uses a heuristic to check whether GDAL has access to the GEOS Union function or not}
    \item{\code{get_cached_orig_PROJ_LIB}, \code{get_cached_orig_GDAL_DATA}}{The values of environment variables PROJ_LIB and GDAL_DATA as read when this package was loaded}
    \item{\code{get_cached_set_PROJ_LIB}, \code{get_cached_set_GDAL_DATA}}{If not \code{""}, the values set when loading this package to point to metadata files included in CRAN binary packages}
}
}

\note{Loading the rgdal package changes the \code{GDAL\_DATA} environmental variable to the GDAL support files bundled with the package.}

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
