\name{GDALDataset-class}
\docType{class}
\alias{GDALDataset-class}
\alias{putRasterData}
\alias{saveDataset}
\alias{copyDataset}
\alias{deleteDataset}
\alias{saveDatasetAs}
\alias{initialize,GDALDataset-method}
\title{Class "GDALDataset"}
\description{\code{GDALDataset} extends \code{\link{GDALReadOnlyDataset-class}} with data update commands.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GDALDataset", filename, handle)}, where name: a string giving the name of a GDAL driver, handle: used internally; not for public consumption (default = NULL).
}
\section{Slots}{
  \describe{
    \item{\code{handle}:}{Object of class \code{"externalptr", from class "GDALReadOnlyDataset"}, used internally; not for public consumption}
  }
}
\section{Extends}{
Class \code{"GDALReadOnlyDataset"}, directly.
Class \code{"GDALMajorObject"}, by class "GDALReadOnlyDataset".
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "GDALDataset")}: ... }
  }
}
\usage{
putRasterData(dataset, rasterData, band = 1, offset = c(0, 0))
saveDataset(dataset, filename, options=NULL)
copyDataset(dataset, driver, strict = FALSE, options = '')
deleteDataset(dataset)
saveDatasetAs(dataset, filename, driver = NULL, options=NULL)
}
\arguments{
  \item{dataset}{An object inheriting from class 'GDALDataset'}
  \item{rasterData}{A data array with \code{length(dim(rasterData)) = 2}}
  \item{band}{The band number (1-based) to read from}
  \item{offset}{Number of rows and columns from the origin (usually the
    upper left corner) to begin reading from}
  \item{filename}{name of file to contain raster data object}
  \item{driver}{GDAL driver name to use for saving raster data object}
  \item{strict}{Driver specific options (currently passed to GDAL)}
  \item{options}{Driver specific options (currently ignored)}
}
\details{
  \describe{
    
    \item{\code{putRasterData}:}{writes data contained in
      \code{rasterData} to the dataset, begining at \code{offset} rows
      and columns from the origin (usually the upper left corner). Data
      type conversion is automatic.}
    \item{\code{saveDataset}:}{saves a raster data object in a file using the driver of the object}
    \item{\code{saveDatasetAs}:}{saves a raster data object in a file using the specified driver}
    \item{\code{copyDataset}:}{make a copy of raster data object in a file using the specified driver}
    \item{\code{deleteDataset}:}{delete the file from which the raster data object was read (should only delete files opened as GDALDataset objects}
  }
}

\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{\code{\link{GDALDriver-class}},
  \code{\link{GDALReadOnlyDataset-class}}, \code{\link{GDALTransientDataset-class}}}

\examples{
}
\keyword{classes}
