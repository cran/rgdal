\name{getPixmapGDAL}
\alias{getPixmapGDAL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read GDAL datasets as pixmapGrey or pixmapRGB objects}
\description{
  A wrapper function for \code{getRasterData()} to insert the retrieved data into pixmap objects; only implemented now for greyscale and RGB data.
}
\usage{
getPixmapGDAL(dataset, col=NULL, band = NULL, offset = c(0, 0), region.dim = dim(dataset), output.dim = region.dim, interleave = c(0, 0), stretch.bands = TRUE, as.is = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{An object inheriting from class 'GDALReadOnlyDataset'}
  \item{col}{a vector of colours or a colour function such as rainbow}
  \item{band}{The band number (1-based) to read from}
  \item{offset}{Number of rows and columns from the origin (usually the
    upper left corner) to begin reading from}
  \item{region.dim}{The number of rows and columns to read from the
    dataset}
  \item{output.dim}{Number of rows and columns in the output data; if
    smaller than \code{region.dim} the data will be subsampled}
  \item{interleave}{Element and row stride while reading data; rarely
    needed}
  \item{stretch.bands}{if TRUE, stretch 3-band RGB images}
  \item{as.is}{If false, scale the data to its natural units; if the
    case of thematic data, return the data as factors}
}

\value{
 For single band data, returns a \code{"pixmapGrey"} object, for three band data, a \code{"pixmapRGB"} object.
}

\author{Roger Bivand}


 \seealso{\code{\link{GDALRasterBand-class}}}


\examples{
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
xGrey <- getPixmapGDAL(x)
xGrey
plot(xGrey)
xCm <- getPixmapGDAL(x, col=cm.colors)
xCm
plot(xCm)
GDAL.close(x)
Rlogo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
y <- new("GDALReadOnlyDataset", Rlogo)
yRGB <- getPixmapGDAL(y)
yRGB
plot(yRGB)
GDAL.close(y)
}
\keyword{hplot}
