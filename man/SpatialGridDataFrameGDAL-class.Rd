\name{SpatialGridDataFrameGDAL-class}
\docType{class}
\alias{SpatialGridDataFrameGDAL-class}
\alias{[,SpatialGridDataFrameGDAL-method}
\alias{summary,SpatialGridDataFrameGDAL-method}
%\alias{show,SpatialGridDataFrame-method}
%\alias{cbind.SpatialGridDataFrame}
\alias{open.SpatialGridDataFrameGDAL}
\alias{close.SpatialGridDataFrameGDAL}
\alias{print.summary.SpatialGridDataFrameGDAL}
\alias{coerce,SpatialGridDataFrameGDAL,SpatialGridDataFrame-method}
\alias{coerce,SpatialGridDataFrameGDAL,SpatialPixelsDataFrame-method}
\alias{$,SpatialGridDataFrameGDAL,character-method}
\alias{[[,SpatialGridDataFrameGDAL,ANY,missing-method}
\alias{$<-,SpatialGridDataFrameGDAL,character-method}
\alias{[[<-,SpatialGridDataFrameGDAL,ANY,missing-method}

\title{Class "SpatialGridDataFrameGDAL" }
\description{  Class for spatial attributes that have spatial locations
on a (full) regular grid on file, not (yet) actually read. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{open.
SpatialGridDataFrameGDAL(name), }, where \code{name} is the name of
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
\section{Extends}{
Class \link[sp]{"SpatialGridDataFrame"}, directly.
Class \link[sp]{"SpatialGrid"}, by class \link[sp]{"SpatialGridDataFrame"}.
Class \link[sp]{"Spatial"}, by class \link[sp]{"SpatialGrid"}.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SpatialGridDataFrameGDAL", i, j, ...)}: 
		selects rows (i), columns (j), and bands (third argument); returns an object of 
		class \link[sp]{SpatialGridDataFrame}. Only the selection is actually read. }
	\item{[[}{\code{signature(i)}: reads band i and returns the values as a
	numeric vector}
  }
}

\references{  }
\author{ Edzer J. Pebesma, \email{e.pebesma@geo.uu.nl} }
\note{  }

\seealso{
  \code{\link[sp]{SpatialGridDataFrame-class}}, which is actually sub-classed.
}
\examples{
x <- open.SpatialGridDataFrameGDAL(system.file("external/test.ag", package="sp")[1])
image(x[])
image(as(x, "SpatialGridDataFrame"))
summary(as(x, "SpatialGridDataFrame"))
spplot(x) # S4 method: will transform x
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
