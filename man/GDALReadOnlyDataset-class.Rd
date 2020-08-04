\name{GDALReadOnlyDataset-class}
\docType{class}
\alias{GDALReadOnlyDataset-class}
\alias{getDriver}
%\alias{displayDataset}
\alias{GDAL.close}
\alias{GDAL.open}
\alias{getColorTable}
\alias{getGeoTransFunc}
\alias{initialize,GDALReadOnlyDataset-method}
\alias{dim,GDALReadOnlyDataset-method}
% \alias{[,GDALReadOnlyDataset-method}
% \alias{sub.GDROD}
\title{Class "GDALReadOnlyDataset"}
\description{\code{GDALReadOnlyDataset} is the base class for a GDAL Dataset
  classes. Only read operations are supported. Both \code{GDALDataset}
  and \code{GDALTransientDataset} inherit these read operations while
  providing additional write operations (see
  \code{\link{GDALDataset-class}}). \code{GDALReadOnlyDataset-class} inherits
  from \code{\link{GDALMajorObject-class}}.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GDALReadOnlyDataset", filename, handle)}.
    ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{handle}:}{Object of class \code{"externalptr", from class "GDALMajorObject"} ~~ }
  }
}
\section{Extends}{
Class \code{"GDALMajorObject"}, directly.
}
\section{Methods}{
  \describe{
    \item{closeDataset}{\code{signature(dataset = "GDALReadOnlyDataset")}: ... }
    \item{dim}{\code{signature(x = "GDALReadOnlyDataset")}: ... }
    \item{initialize}{\code{signature(.Object = "GDALReadOnlyDataset")}: ... }
%     \item{"["}{\code{signature(.Object = "GDALReadOnlyDataset")}: requires package
% 	sp; selects rows and columns, and returns an object of class SpatialGridDataFrame
% 	if the grid is not rotated, or else of class SpatialPointsDataFrame. Any arguments
% 	passed to getRasterData (or in case of rotation getRasterTable) may be passed as
% 	named arguments; the first three unnamed arguments are row,col,band }
  }
}
\usage{
GDAL.close(dataset)
GDAL.open(filename, read.only = TRUE, silent=FALSE,
                 allowedDrivers = NULL, options=NULL)
getDriver(dataset)
%displayDataset(x, offset = c(0, 0), region.dim = dim(x),
%                           reduction = 1, band = NULL, col = NULL,
%                           max.dim = 500, ...)
getColorTable(dataset, band = 1)
getGeoTransFunc(dataset)
}
\arguments{
 \item{dataset}{An object inheriting from class 'GDALReadOnlyDataset'}
 \item{filename}{name of file to contain raster data object; will be normalized with \code{\link{normalizePath}} if it is a file}
% \item{x}{An object inheriting from class 'GDALReadOnlyDataset'}
% \item{offset}{Number of rows and columns from the origin (usually the
%    upper left corner) to begin reading from}
% \item{region.dim}{The number of rows and columns to read from the
%    dataset}
% \item{reduction}{scaling parameter for region.dim}
 \item{band}{The band number (1-based) to read from}
% \item{col}{a vector of colours or a colour function such as rainbow}
% \item{max.dim}{another scaling parameter}
% \item{...}{other plotting parameters passed through}
% \item{}{}
% \item{}{}
\item{read.only}{A logical flag indicating whether to open the file as a
  \code{GDALReadOnlyDataset} or as a writable \code{GDALDataset}}
\item{silent}{ logical; if TRUE, comment and non-fatal CPL driver errors suppressed }
\item{allowedDrivers}{a character vector of suggested driver short names may be provided starting from GDAL 2.0}
\item{options}{open options may be passed to raster drivers starting from GDAL 2.0; very few drivers support these options}
}
\details{
\code{GDAL.open} and \code{GDAL.close} are shorter versions of \code{new("GDALReadOnlyDataset", ...)} and \code{closeDataset()}. Because \code{GDAL.close} through \code{closeDataset()} uses the finalization mechanism to destroy the handles to the dataset and its driver, messages such as:

"Closing GDAL dataset handle 0x8ff7900...  destroyed ... done."

may appear when \code{GDAL.close} is run, or at some later stage. 
\code{getDriver} returns an object inheriting from class 'GDALDriver'.
%\code{displayDataset} and its use in \code{plot} for a pixmap object to produce a plot of the dataset, returning a pixmap object.
\code{getColorTable} returns the dataset colour table (currently does not support RGB imaging).
\code{getGeoTransFunc} returns a warping function.
  
}
\references{\url{https://gdal.org/}}

\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{See also \code{\link{GDALDriver-class}}, \code{\link{GDALDataset-class}}, \code{\link{GDALTransientDataset-class}}.}

\examples{
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
dim(x)
plot(density(getRasterTable(x)$band1))
#displayDataset(x)
#displayDataset(x, col=function(x){rev(cm.colors(x))})
#im <- displayDataset(x, col=function(x){rev(cm.colors(x))}, reset.par=FALSE)
#contour(1:attr(im, "size")[2], 1:attr(im, "size")[1],
# t(attr(im, "index"))[,attr(im, "size")[1]:1], nlevels = 1,
# levels = 100, col = 'black', add = TRUE)
GDAL.close(x)
logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
dim(x)
#displayDataset(x)
GDAL.close(x)
}
\keyword{classes}
