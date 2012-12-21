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
saveDataset(dataset, filename, options=NULL, returnNewObj=FALSE)
copyDataset(dataset, driver, strict = FALSE, options = NULL, fname=NULL)
deleteDataset(dataset)
saveDatasetAs(dataset, filename, driver = NULL, options=NULL)
}
\arguments{
  \item{dataset}{An object inheriting from class 'GDALDataset'}
  \item{rasterData}{A data array with \code{length(dim(rasterData)) = 2}}
  \item{band}{The band number (1-based) to read from}
  \item{offset}{Number of rows and columns from the origin (usually the
    upper left corner) to begin reading from}
  \item{filename}{name of file to contain raster data object; will be normalized with \code{\link{normalizePath}}}
  \item{returnNewObj}{until and including 0.5-27, \code{saveDataset} returned an invisible copy of the new file handle, which was then only finalized when the garbage collector ran. The old behaviour can be retained by setting to FALSE, the default behaviour is to close the hande and not return it.}
  \item{driver}{GDAL driver name to use for saving raster data object}
  \item{strict}{TRUE if the copy must be strictly equivalent, or more normally FALSE indicating that the copy may adapt as needed for the output format}
  \item{options}{Driver specific options (currently passed to GDAL)}
  \item{fname}{default NULL, used internally to pass through a file name with a required extension (RST driver has this problem)}
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

\keyword{classes}
