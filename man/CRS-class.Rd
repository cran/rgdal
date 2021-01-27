\name{CRS-class}
\docType{class}
\alias{CRS-class}
\alias{CRS}
\alias{CRSargs}
\alias{checkCRSArgs}
\alias{checkCRSArgs_ng}
\alias{print.CRS}
\alias{show,CRS-method}
\alias{RGDAL_checkCRSArgs}
\alias{compare_CRS}
\title{Class "CRS" of coordinate reference system arguments}
\description{Interface class to the PROJ.4 projection system. The class is defined as an empty stub accepting value NA in the sp package. If the rgdal package is available, then the class will permit spatial data to be associated with coordinate reference systems}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{CRS("projargs")}, where "projargs" is a valid string of PROJ.4 arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. The initiation function calls the PROJ.4 library to verify the argument set against those known in the library, returning error messages where necessary. The complete argument set may be retrieved by examining the second list element returned by \code{validObject("CRS object")} to see which additional arguments the library will use (which assumptions it is making over and above submitted arguments). The function \code{CRSargs()} can be used to show the expanded argument list used by the PROJ.4 library.
}
\section{Slots}{
  \describe{
    \item{\code{projargs}:}{Object of class \code{"character"}: projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CRS")}: print projection arguments in object }
  }
}

\usage{
checkCRSArgs(uprojargs)
checkCRSArgs_ng(uprojargs=NA_character_, SRS_string=NULL,
  get_source_if_boundcrs=TRUE)
compare_CRS(CRS1, CRS2)
}
\arguments{
  \item{uprojargs}{character string PROJ.4 projection arguments}
  \item{SRS_string}{default NULL, experimental in connection with adaptation to GDAL>=3/PROJ>=6; a valid WKT string or SRS definition such as \code{"OGC:CRS84"}}
  \item{get_source_if_boundcrs}{The presence of the \code{+towgs84=} key in a Proj4 string \code{projargs=} argument value may promote the output WKT2 CRS to BOUNDCRS for PROJ >= 6 and GDAL >= 3, which is a coordinate operation from the input datum to WGS84. This is often unfortunate, so a PROJ function is called through \pkg{rgdal} to retrieve the underlying source definition.}
  \item{CRS1, CRS2}{objects of class \code{"CRS"}}
}
\references{\url{https://proj.org/}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{
Lists of projections may be seen by using the programs installed with the PROJ.4 library, in particular proj and cs2cs; with the latter, -lp lists projections, -le ellipsoids, -lu units, and -ld datum(s) known to the installed software (available in \pkg{rgdal} using \code{projInfo}). These are added to in successive releases, so tracking the website or compiling and installing the most recent revisions will give the greatest choice. Finding the very important datum transformation parameters to be given with the +towgs84 tag is a further challenge, and is essential when the datums used in data to be used together differ. Tracing projection arguments is easier now than before the mass ownership of GPS receivers raised the issue of matching coordinates from different argument sets (GPS output and paper map, for example). See \code{\link{GridsDatums}} and \code{\link{showEPSG}} for help in finding CRS definitions.

The 4.9.1 release of PROJ.4 omitted a small file of defaults, leading to reports of \dQuote{major axis or radius = 0 or not given} errors. From 0.9-3, rgdal checks for the presence of this file (proj_def.dat), and if not found, and under similar conditions to those used by PROJ.4, adds \dQuote{+ellps=WGS84} to the input string being checked by \code{checkCRSArgs} The \dQuote{+no_defs} tag ignores the file of defaults, and the default work-around implemented to get around this problem; strings including \dQuote{init} and \dQuote{datum} tags also trigger the avoidance of the work-around. Now messages are issued when a candidate CRS is checked; they may be suppressed using \code{suppressMessages}.}


\examples{
set_thin_PROJ6_warnings(TRUE)
CRSargs(CRS("+proj=longlat"))
try(CRS("+proj=longlat"))
CRSargs(CRS("+proj=longlat +datum=NAD27"))
CRSargs(CRS("+init=epsg:4267"))
CRSargs(CRS("+init=epsg:26978"))
CRSargs(CRS(paste("+proj=stere +lat_0=52.15616055555555", 
"+lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel", 
"+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812", 
"+units=m")))
# see http://trac.osgeo.org/gdal/ticket/1987
CRSargs(CRS("+init=epsg:28992"))
crs <- CRS("+init=epsg:28992")
CRSargs(CRS(CRSargs(crs)))
library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
CRSargs(CRS(proj4string(meuse)))
run <- new_proj_and_gdal()
if (run) {
c1 <- CRS(SRS_string="OGC:CRS84")
c2 <- CRS("+proj=longlat")
compare_CRS(c1, c2)
}
if (run) {
comment(c2) <- NULL
compare_CRS(c1, c2)
}
}
\keyword{classes}
\keyword{spatial}
