\name{GDALRasterBand-class}
\docType{class}
\alias{GDALRasterBand-class}
\alias{getRasterData}
\alias{getRasterTable}
\alias{getProjectionRef}
\alias{initialize,GDALRasterBand-method}
\alias{dim,GDALRasterBand-method}
\alias{getRasterBand}
\alias{getRasterBlockSize}
\alias{toSigned}
\alias{toUnSigned}
\alias{get_OVERRIDE_PROJ_DATUM_WITH_TOWGS84}
\alias{set_OVERRIDE_PROJ_DATUM_WITH_TOWGS84}
\title{Class "GDALRasterBand"}
\description{Returns a two-dimensional array with data from a raster band, used internally within functions}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GDALRasterBand", dataset, band)}.
 
}
\section{Slots}{
  \describe{
    \item{\code{handle}:}{Object of class \code{"externalptr", from class "GDALMajorObject"}, used internally; not for public consumption }
  }
}
\section{Extends}{
Class \code{"GDALMajorObject"}, directly.
}
\section{Methods}{
  \describe{
    \item{dim}{\code{signature(x = "GDALRasterBand")}: ... }
    \item{initialize}{\code{signature(.Object = "GDALRasterBand")}: ... }
  }
}
\usage{
getRasterData(dataset, band = NULL, offset = c(0, 0),
              region.dim = dim(dataset), output.dim = region.dim,
              interleave = c(0, 0), as.is = FALSE, list_out=FALSE) 

getRasterTable(dataset, band = NULL, offset = c(0, 0), 
  region.dim = dim(dataset))

getProjectionRef(dataset, OVERRIDE_PROJ_DATUM_WITH_TOWGS84 = NULL, enforce_xy = NULL)

getRasterBand(dataset, band = 1)

getRasterBlockSize(raster)

toSigned(x, base)

toUnSigned(x, base)

get_OVERRIDE_PROJ_DATUM_WITH_TOWGS84()
set_OVERRIDE_PROJ_DATUM_WITH_TOWGS84(value)

}
\arguments{
  \item{dataset}{An object inheriting from class 'GDALReadOnlyDataset'}
  \item{band}{The band number (1-based) to read from}
  \item{offset}{Number of rows and columns from the origin (usually the
    upper left corner) to begin reading from; presently ordered (y,x) - this may change}
  \item{region.dim}{The number of rows and columns to read from the
    dataset; presently ordered (y,x) - this may change}
  \item{output.dim}{Number of rows and columns in the output data; if
    smaller than \code{region.dim} the data will be subsampled}
  \item{interleave}{Element and row stride while reading data; rarely
    needed}
  \item{as.is}{If false, scale the data to its natural units; if the
    case of thematic data, return the data as factors}
  \item{list_out}{default FALSE, return array, if TRUE, return a list of vector bands}
  \item{raster}{An object of class GDALRasterBand}
  \item{x}{integer variable for conversion}
  \item{base}{If Byte input, 8, if Int16 or UInt16, 16}
  \item{OVERRIDE_PROJ_DATUM_WITH_TOWGS84}{logical value, default NULL, which case the cached option set by \code{set_OVERRIDE_PROJ_DATUM_WITH_TOWGS84} is used. Ignored if the GDAL version is less than \dQuote{1.8.0} or if the CPLConfigOption variable is already set}
  \item{enforce_xy}{(PROJ6+/GDAL3+) either use global setting (default NULL) or override policy for coordinate ordering easting/x as first axis, northing/y as second axis.}
  \item{value}{logical value to set OVERRIDE_PROJ_DATUM_WITH_TOWGS84}
}

\details{
  \describe{
    \item{\code{getRasterData}:}{retrieves data from the dataset as an array or list of bands; will try to convert relevant bands to factor if category names are available in the GDAL driver when returning a list.}
    
    \item{\code{getRasterTable}:}{retrieves data from the dataset as data frame.}
    
    \item{\code{getProjectionRef}:}{returns the geodetic projection in
      Well Known Text format.}

    \item{\code{getRasterBand}:}{returns a raster band}

    \item{\code{getRasterBlockSize}:}{returns the natural block size of
      the raster band. Use this for efficient tiled IO.}

    \item{\code{toSigned}:}{used to convert a band read as unsigned integer to signed integer}
    \item{\code{toUnSigned}:}{used to convert a band read as signed integer to unsigned integer}

  }
}

\note{The OVERRIDE_PROJ_DATUM_WITH_TOWGS84 argument is used to revert GDAL behaviour to pre-1.8.0 status; from 1.8.0, any input datum may be discarded if the input also includes a towgs84 tag in conversion to the PROJ.4 representation, see \url{https://trac.osgeo.org/gdal/ticket/4880} and \url{https://lists.osgeo.org/pipermail/gdal-dev/2012-November/034550.html}. The cached value of OVERRIDE_PROJ_DATUM_WITH_TOWGS84 will also be used in \code{open.SpatialGDAL}, \code{sub.GDROD}, and \code{asGDALROD_SGDF}, which do not have a suitable argument}


\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{See also \code{\link{GDALDriver-class}}, \code{\link{GDALDataset-class}}, \code{\link{GDALTransientDataset-class}}}

\examples{
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
plot(density(getRasterTable(x)$band1))
GDAL.close(x)
}
\keyword{classes}
