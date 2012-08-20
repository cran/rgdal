\name{GDALReadOnlyDataset-methods}
\docType{methods}
\alias{GDALReadOnlyDataset-methods}
\alias{[,GDALReadOnlyDataset-method}
\alias{,GDALReadOnlyDataset-method}
\alias{coerce,GDALReadOnlyDataset,SpatialGridDataFrame-method}
\alias{sub.GDROD}
\title{subset methods for "GDALReadOnlyDataset"}
\description{subsets GDAL objects, returning a SpatialGridDataFrame object}
\section{Methods}{
  \describe{
     \item{"["}{\code{signature(.Object = "GDALReadOnlyDataset")}: requires package
 	sp; selects rows and columns, and returns an object of class SpatialGridDataFrame
 	if the grid is not rotated, or else of class SpatialPointsDataFrame. Any arguments
 	passed to getRasterData (or in case of rotation getRasterTable) may be passed as
 	named arguments; the first three unnamed arguments are row,col,band }
  }
}
\details{
The \code{[} method subsets a GDAL data set, returning a SpatialGridDataFrame object. Reading is
done on the GDAL side, and only the subset requested is ever read into memory.

Further named arguments to \code{[} are to either getRasterTable or getRasterData:
\describe{
 \item{as.is}{see \link{getRasterData}}
 \item{interleave}{see \link{getRasterData}}
 \item{output.dim}{see \link{getRasterData}}
}
the other arguments, \code{offset} and \code{region.dim} are
derived from row/column selection values.

An GDALReadOnlyDataset object can be coerced directly to a SpatialGridDataFrame
}

\author{Edzer Pebesma}

\seealso{See also \code{\link{readGDAL}} \code{\link{GDALDriver-class}},
\code{\link{GDALDataset-class}}, \code{\link{GDALTransientDataset-class}},
\code{\link[sp]{SpatialGridDataFrame-class}} .}

\examples{
library(grid)
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
dim(x)
x.sp = x[20:50, 20:50]
class(x.sp)
summary(x.sp)
spplot(x.sp)
GDAL.close(x)

logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x.gdal <- new("GDALReadOnlyDataset", logo)
x = x.gdal[,,3]
dim(x)
summary(x)
spplot(x)
spplot(x.gdal[])
GDAL.close(x.gdal)

logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x.gdal <- new("GDALReadOnlyDataset", logo)
x.as <- as(x.gdal, "SpatialGridDataFrame")
GDAL.close(x.gdal)
summary(x.as)
}
\keyword{classes}
