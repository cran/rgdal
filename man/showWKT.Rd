\name{showWKT}
\alias{showWKT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Show Well-Known Text spatial reference system metadata}
\description{
  Use GDAL/OGR spatial reference objects to convert a PROJ.4 representation to a Well-Known Text representation.
}
\usage{
showWKT(p4s, file = NULL, morphToESRI = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p4s}{A valid PROJ.4 string representing a spatial reference system}
  \item{file}{if not NULL, a file name to which the output Well-Known Text representation should be written}
  \item{morphToESRI}{default TRUE, morph the WKT string to the representation used by ESRI}
}

\value{
  A character string containing the WKT representation of the PROJ.4 string.
}
\references{\url{http://www.gdal.org/ogr/osr_tutorial.html}}
\author{Roger Bivand}

\seealso{\code{\link[sp]{is.projected}}, \code{\link[sp]{CRS-class}}}
\examples{
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
readLines(system.file("vectors/cities.prj", package = "rgdal")[1])
showWKT(proj4string(cities))
showWKT("+init=epsg:28992")
}
\keyword{spatial}

