\name{readOGR}
\alias{readOGR}
\alias{ogrInfo}
\alias{print.ogrinfo}
\alias{ogrFIDs}
\alias{ogrDrivers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read OGR vector maps into Spatial objects}
\description{
  The function reads an OGR data source and layer into a suitable Spatial vector object. It can only handle layers with conformable geometry features (not mixtures of points, lines, or polygons in a single layer). It will set the spatial reference system if the layer has such metadata.
}
\usage{
readOGR(dsn, layer, verbose = TRUE, p4s=NULL, drop_unsupported_fields=FALSE,
 input_field_name_encoding=NULL)
ogrInfo(dsn, layer, input_field_name_encoding=NULL)
ogrFIDs(dsn, layer)
ogrDrivers()
\method{print}{ogrinfo}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{data source name (interpretation varies by driver --- for some drivers, dsn is a file name, but may also be a folder)}
  \item{layer}{layer name (varies by driver, may be a file name without extension)}
  \item{verbose}{report progress}
  \item{p4s}{PROJ4 string defining CRS, if default NULL, the value is read from the OGR data set} 
  \item{drop\_unsupported\_fields}{default FALSE, if TRUE skip fields other than string, integer, and real}
  \item{input\_field\_name\_encoding}{default NULL; if not NULL, should be a valid encoding name for the source platform of the data source and layer, see \code{\link[base]{iconv}}}
  \item{x}{ogrinfo object}
  \item{...}{other arguments to print method}
}
\details{
  The drivers available will depend on the installation of GDAL/OGR, and can vary; the \code{ogrDrivers()} function shows which are available, and which may be written (but all are assumed to be readable). Note that stray files in data source directories (such as *.dbf) may lead to suprious errors that accompanying *.shp are missing.
}
\value{
  A Spatial object is returned suiting the vector data source, either a SpatialPointsDataFrame (using an AttributeList for its data slot directly), a SpatialLinesDataFrame, or a SpatialPolygonsDataFrame.
}
\references{\url{http://www.gdal.org/ogr/}, \url{http://www.gdal.org/ogr/ogr_formats.html}, \url{http://examples.oreilly.com/webmapping/}}
\author{Roger Bivand}

\note{The bases for this implementation are taken from functions in Barry Rowlingson's draft Rmap package, and from Radim Blazek's v.in.ogr program in GRASS.

Please note that the OGR drivers used may not handle missing data gracefully, and be prepared to have to correct for this manually. From rgdal 0.5-27, missing value handling has been improved, and OGR unset field values are set to NA in R, but drivers and external files may vary in their representations of missing values.
}
\seealso{\code{\link[sp]{SpatialPointsDataFrame-class}}, \code{\link[sp]{SpatialLinesDataFrame-class}}, \code{\link[sp]{SpatialPolygonsDataFrame-class}}, \code{\link[maptools]{readShapePoly}}, \code{\link[base]{iconv}}}
\examples{
ogrDrivers()
ogrInfo(system.file("vectors", package = "rgdal")[1], "cities")
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
summary(cities)
ogrInfo(system.file("vectors", package = "rgdal")[1], "kiritimati_primary_roads")
kiritimati_primary_roads <- readOGR(system.file("vectors", package = "rgdal")[1], "kiritimati_primary_roads")
summary(kiritimati_primary_roads)
ogrInfo(system.file("vectors", package = "rgdal")[1], "scot_BNG")
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1], "scot_BNG")
summary(scot_BNG)
if ("GML" \%in\% ogrDrivers()$name) {
  airports <- try(readOGR(system.file("vectors/airports.gml", package = "rgdal")[1], "airports"))
  if (class(airports) != "try-error") summary(airports)
}
ogrInfo(system.file("vectors/ps_cant_31.MIF", package = "rgdal")[1], "ps_cant_31")
ps_cant_31 <- readOGR(system.file("vectors/ps_cant_31.MIF", package = "rgdal")[1], "ps_cant_31")
summary(ps_cant_31)
ogrInfo(system.file("vectors/Up.tab", package = "rgdal")[1], "Up")
Up <- readOGR(system.file("vectors/Up.tab", package = "rgdal")[1], "Up")
summary(Up)
}
\keyword{spatial}

