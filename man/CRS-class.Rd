\name{CRS-class}
\docType{class}
\alias{CRS-class}
\alias{CRS}
\alias{CRSargs}
\alias{print.CRS}
\alias{show,CRS-method}
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
\references{\url{http://www.remotesensing.org/proj/}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{
Lists of projections may be seen by using the programs installed with the PROJ.4 library, in particular proj and cs2cs; with the latter, -lp lists projections, -le ellipsoids, -lu units, and -ld datum(s) known to the installed software. These are added to in successive releases, so tracking the website or compiling and installing the most recent revisions will give the greatest choice. On occasion, ellipsoid parameters may be passed through directly. Tracing projection arguments is easier now than before the mass ownership of GPS receivers raised the issue of matching coordinates from different argument sets (GPS output and paper map, for example).}


\examples{
CRSargs(CRS("+proj=longlat +datum=NAD27"))
CRSargs(CRS("+init=epsg:4267"))
CRSargs(CRS("+init=epsg:26978"))
CRSargs(CRS("+proj=stere +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m"))
CRSargs(CRS("+init=epsg:28992"))
}
\keyword{classes}
\keyword{spatial}
