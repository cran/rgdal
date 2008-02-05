\name{GDALMajorObject-class}
\docType{class}
\alias{GDALMajorObject-class}
\alias{getDescription}
\alias{getMetadata}
\alias{setMetadata}
\alias{appendMetadata}
\title{Class "GDALMajorObject" }

\description{"GDALMajorObject" is a virtual base class for all GDAL objects.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GDALMajorObject", ...)}, but are only created for classes that extend this class.}

\section{Slots}{
  \describe{
    \item{\code{handle}:}{Object of class \code{"externalptr"}, used internally; not for public consumption}
  }
}

\section{Methods}{
No methods defined with class "GDALMajorObject" in the signature.
}
\usage{
getDescription(object)

getMetadata(object, domain = "")

setMetadata(object, metadata)

appendMetadata(object, metadata)
}
\arguments{
  \item{object}{an object inheriting from "GDALMajorObject"}

  \item{domain}{the metadata domain (currently ignored)}

  \item{metadata}{a list of character strings}
}
\details{
  \describe{
    \item{\code{getDescription}:}{returns a descrption string associated
    with the object. No setter method is defined because GDAL dataset
    objects use the description to hold the filename attached to the
    dataset. It would not be good to change that mid-stream.}
  
    \item{\code{getMetadata}:}{returns a list of metadata strings. Keys
      are returned as item names in the list.}
  
    \item{\code{setMetadata}:}{sets the metadata to the supplied list of
      strings. List item names will be used to generate metadata key
      names.} 
  }
}

\references{\url{http://keittlab.bio.sunysb.edu/R/GDAL/}, \url{http://www.gdal.org/}}

\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{ \code{\link{GDALDriver-class}},
  \code{\link{GDALReadOnlyDataset-class}}, \code{\link{GDALDataset-class}} and
  \code{\link{GDALTransientDataset-class}} }

\examples{

driver <- new('GDALDriver', as.character(getGDALDriverNames()[1,1]))
setMetadata(driver, list(Key = 'Test metadata'))
getMetadata(driver)
rm(driver)
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
getDescription(x)
getMetadata(x)
dim(x)
GDAL.close(x)

}
\keyword{classes}
