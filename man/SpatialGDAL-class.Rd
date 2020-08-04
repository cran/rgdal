\name{SpatialGDAL-class}
\docType{class}
\alias{SpatialGDAL-class}
\alias{SpatialGDALWrite-class}
\alias{[,SpatialGDAL-method}
\alias{summary,SpatialGDAL-method}
\alias{open.SpatialGDAL}
\alias{copy.SpatialGDAL}
\alias{close.SpatialGDAL}
\alias{print.summary.SpatialGDAL}
\alias{coerce,SpatialGDAL,SpatialGridDataFrame-method}
\alias{coerce,SpatialGDAL,SpatialPixelsDataFrame-method}
\alias{$,SpatialGDAL-method}
\alias{[<-,SpatialGDALWrite-method}
\alias{[[,SpatialGDAL,ANY,missing-method}
\alias{$<-,SpatialGDAL-method}
\alias{[[<-,SpatialGDAL,ANY,missing-method}

\title{Class "SpatialGDAL" }
\description{  Class for spatial attributes that have spatial locations
on a (full) regular grid on file, not (yet) actually read. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{open.
SpatialGDAL(name), }, where \code{name} is the name of
the GDAL file.
}

\section{Slots}{
  \describe{
    \item{\code{points}:}{see \link[sp]{SpatialPoints}; points slot which is
	not actually filled with all coordinates (only with min/max) }
    \item{\code{grid}:}{see \link[sp]{GridTopology-class}; grid parameters }
    \item{\code{grid.index}:}{see \link[sp]{SpatialPixels-class}; this slot
	is of zero length for this class, as the grid is full }
    \item{\code{bbox}:}{Object of class \code{"matrix"}; bounding box }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; projection }
    \item{\code{data}:}{Object of class data.frame, containing attribute data }
  }
}
\section{Extends}{ Class \link[sp]{Spatial-class}, directly.  }
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialGDAL", i, j, ...)}: 
		selects rows (i), columns (j), and bands (third argument); returns an object of 
		class \link[sp]{SpatialGridDataFrame-class}. Only the selection is actually read. }
	\item{[[}{\code{signature(i)}: reads band i and returns the values as a
	numeric vector}
  }
}
\usage{
\method{open}{SpatialGDAL}(con, \dots, silent = FALSE, allowedDrivers = NULL, options=NULL)
\method{close}{SpatialGDAL}(con, \dots)
copy.SpatialGDAL(dataset, fname, driver = getDriver(dataset@grod),
 strict = FALSE, options = NULL, silent = FALSE)
}
\arguments{
\item{con}{ file name of grid map for opening, SpatialGDAL object for closing }
\item{\dots}{ other arguments (currently ignored)}
\item{silent}{ logical; if TRUE, comment and non-fatal CPL driver errors suppressed }
\item{dataset}{ object of class SpatialGDAL}
\item{fname}{ file name of grid map }
\item{driver}{ GDAL driver name }
\item{strict}{TRUE if the copy must be strictly equivalent, or more normally FALSE indicating that the copy may adapt as needed for the output format}
\item{allowedDrivers}{a character vector of suggested driver short names may be provided starting from GDAL 2.0}
\item{options}{driver-specific options to be passed to the GDAL driver; only available for opening datasets from GDAL 2.0}
}
\note{Non-fatal CPL errors may be displayed for some drivers, currently for the AIG ArcInfo 9.3 binary raster driver using GDAL >= 1.6.2; the data has been read correctly, but the contents of the info directory did not meet the specifications used to reverse engineer the driver used in GDAL (see \url{https://trac.osgeo.org/gdal/ticket/3031})}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\seealso{
  \code{\link[sp]{SpatialGridDataFrame-class}}, which is actually sub-classed.
}
\examples{
x <- open.SpatialGDAL(system.file("external/test.ag", package="sp")[1])
image(x[])
image(as(x, "SpatialGridDataFrame"))
summary(as(x, "SpatialGridDataFrame"))
spplot(as(x, "SpatialGridDataFrame"))
# select first 50 rows:
summary(x[1:50])
# select first 50 columns:
summary(x[,1:50])
# select band 1:
summary(x[,,1])
# select first 50 rows, first 50 columns, band 1:
summary(x[1:50,1:50,1])
# get values of first band:
summary(x[[1]])
close(x)
}
\keyword{classes}
