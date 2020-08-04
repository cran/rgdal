\name{GDALMajorObject-class}
\docType{class}
\alias{GDALMajorObject-class}
\alias{getDescription}
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
}
\arguments{
  \item{object}{an object inheriting from "GDALMajorObject"}

}
\details{
  \describe{
    \item{\code{getDescription}:}{returns a descrption string associated
    with the object. No setter method is defined because GDAL dataset
    objects use the description to hold the filename attached to the
    dataset. It would not be good to change that mid-stream.}
  
  }
}

\references{\url{https://gdal.org/}}

\author{Timothy H. Keitt, modified by Roger Bivand}

\seealso{ \code{\link{GDALDriver-class}},
  \code{\link{GDALReadOnlyDataset-class}}, \code{\link{GDALDataset-class}} and
  \code{\link{GDALTransientDataset-class}} }

\examples{
driver <- new('GDALDriver', as.character(getGDALDriverNames()[1,1]))
driver
rm(driver)
logo <- system.file("pictures/logo.jpg", package="rgdal")[1]
x <- new("GDALReadOnlyDataset", logo)
x
getDescription(x)
dim(x)
GDAL.close(x)

}
\keyword{classes}
