\name{readOGR}
\alias{readOGR}
\alias{ogrInfo}
\alias{print.ogrinfo}
\alias{ogrFIDs}
\alias{ogrDrivers}
\alias{OGRSpatialRef}
\alias{ogrListLayers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read OGR vector maps into Spatial objects}
\description{
  The function reads an OGR data source and layer into a suitable Spatial vector object. It can only handle layers with conformable geometry features (not mixtures of points, lines, or polygons in a single layer). It will set the spatial reference system if the layer has such metadata.

If reading a shapefile, the data source name (\code{dsn=} argument) is 
the folder (directory) where the shapefile is, and the layer is the 
name of the shapefile (without the .shp extension). For example to read 
\code{bounds.shp} from \code{C:/Maps}, do 
\code{map <- readOGR(dsn="C:/Maps", layer="bounds")}. The logic behind 
this is that typically one keeps all the shapefiles for a project in 
one folder (directory). 

As noted below, for other file type drivers, the \code{dsn=} argument is 
interpreted differently, and may be the file name itself, as for example 
with the \code{GPX} driver for reading GPS data as \code{layer="tracks"} 
lines or \code{layer="track_points"} points.

}
\usage{
readOGR(dsn, layer, verbose = TRUE, p4s=NULL, 
 stringsAsFactors=default.stringsAsFactors(), 
 drop_unsupported_fields=FALSE, input_field_name_encoding=NULL,
 pointDropZ=FALSE, dropNULLGeometries=TRUE,
 useC=TRUE, disambiguateFIDs=FALSE, addCommentsToPolygons=TRUE,
 encoding=NULL, use_iconv=NULL)
ogrInfo(dsn, layer, encoding=NULL, input_field_name_encoding=NULL,
 use_iconv=NULL)
ogrFIDs(dsn, layer)
ogrDrivers()
OGRSpatialRef(dsn, layer)
ogrListLayers(dsn)
\method{print}{ogrinfo}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{data source name (interpretation varies by driver --- for some drivers, dsn is a file name, but may also be a folder)}
  \item{layer}{layer name (varies by driver, may be a file name without extension)}
  \item{verbose}{report progress}
  \item{p4s}{PROJ4 string defining CRS, if default NULL, the value is read from the OGR data set} 
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? The \sQuote{factory-fresh} default is \code{TRUE}, but this can be changed by setting \code{\link{options}(stringsAsFactors = FALSE)} (see \code{link[base]{data.frame}}).}
  \item{drop\_unsupported\_fields}{default FALSE, if TRUE skip fields other than String, Integer, and Real; Date, Time and DateTime are converted to String}
  \item{input\_field\_name\_encoding}{default NULL; if not NULL, should be a valid encoding name for the source platform of the data source and layer, see \code{\link[base]{iconv}}}
  \item{pointDropZ}{default FALSE, if TRUE, discard third coordinates for point geometries; third coordinates are alway discarded for line and polygon geometries}
  \item{dropNULLGeometries}{default TRUE, drop both declared NULL geometries, and empty geometries with no coordinates; if FALSE, return a data frame with the attribute values of the NULL and empty geometries}
  \item{useC}{default TRUE, if FALSE use original interpreted code in a loop}
  \item{disambiguateFIDs}{default FALSE, if TRUE, and FID values are not unique, they will be set to unique values 1:N for N features; problem observed in GML files}
  \item{addCommentsToPolygons}{default TRUE, may be set FALSE for legacy behaviour; used to indicate which interior rings are holes in which exterior rings in conformance with OGC SFS specifications}
  \item{encoding}{default NULL, if set to a character string, and the driver is \dQuote{ESRI Shapefile}, it is passed to the CPL Option \dQuote{SHAPE_ENCODING} immediately before reading the DBF of a shapefile. The empty character string \dQuote{} turns off OGR's encoding functionality, and may be used when GDAL is not built with iconv support.}
  \item{use_iconv}{default NULL, in which case the use of iconv is controlled by the GDAL version (TRUE < 1.9). If set to a logical value, this test will be overridden.}
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

In addition, from 0.6-9 date and time fields are read as strings rather than being treated as unsupported; NULL geometries are identified and dropped. There are differences in the reporting of NULL geometries between \code{ogrInfo} and \code{readOGR} - in \code{ogrInfo}, only declared NULL geometries are reported, but in \code{readOGR}, any line or polygon geometries with no coordinates are assigned NULL geometry status as well. An attempt is made to close unclosed rings in polygon geometries. 

For reading GPX files, refer to the OGR GPX format documentation for the use of layer tags: \dQuote{waypoints}, \dQuote{tracks}, \dQuote{routes}, \dQuote{track_points} and \dQuote{route_points} - reading GPX files requires a build of GDAL/OGR with the expat XML library. 

From 0.6-10, attempts are made to detect deleted features still present in the layer, but not read. Apparently features deleted in Qgis are only marked as deleted, but are still in the layer. These are not NULL geometries, but still need to be handled. An attempt is made to check the FID values, and \code{ogrFIDs} now returns attributes permitting this oddity to be detected. Such deleted features were seen as NULL in 0.6-9, but are not such.

From 0.7-24, if the layer has no fields, a single field containing the FID values is placed in the data slot of the returned object.

From 0.7-24, attempts are begun to provide users with arguments to control reading from OGR/shapefile driver when the encoding is inappropriate (especially the setting of LDID in shapefile DBFs, and the SHAPE_ENCODING environment variable).
}
\seealso{\code{\link[sp]{SpatialPointsDataFrame-class}}, \code{\link[sp]{SpatialLinesDataFrame-class}}, \code{\link[sp]{SpatialPolygonsDataFrame-class}}, \code{\link[maptools]{readShapePoly}}, \code{\link[base]{iconv}}}
\examples{
ogrDrivers()
dsn <- system.file("vectors", package = "rgdal")[1]
ogrListLayers(dsn)
ogrInfo(dsn=dsn, layer="cities")
owd <- getwd()
setwd(dsn)
ogrInfo(dsn="cities.shp", layer="cities")
setwd(owd)
cities <- readOGR(dsn=dsn, layer="cities")
summary(cities)
ogrInfo(dsn=dsn, layer="kiritimati_primary_roads")
OGRSpatialRef(dsn=dsn, layer="kiritimati_primary_roads")
kiritimati_primary_roads <- readOGR(dsn=dsn, layer="kiritimati_primary_roads")
summary(kiritimati_primary_roads)
ogrInfo(dsn=dsn, layer="scot_BNG")
OGRSpatialRef(dsn=dsn, layer="scot_BNG")
scot_BNG <- readOGR(dsn=dsn, layer="scot_BNG")
summary(scot_BNG)
if ("GML" \%in\% ogrDrivers()$name) {
  dsn <- system.file("vectors/airports.gml", package = "rgdal")[1]
  airports <- try(readOGR(dsn=dsn, layer="airports"))
  if (class(airports) != "try-error") summary(airports)
}
dsn <- system.file("vectors/ps_cant_31.MIF", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="ps_cant_31")
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31")
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31", stringsAsFactors=FALSE)
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="Up")
Up <- readOGR(dsn=dsn, layer="Up")
summary(Up)
dsn <- system.file("vectors/test_trk2.gpx", package = "rgdal")[1]
test_trk2 <- try(readOGR(dsn=dsn, layer="tracks"))
if (class(test_trk2) != "try-error") summary(test_trk2)
test_trk2pts <- try(readOGR(dsn=dsn, layer="track_points"))
if (class(test_trk2pts) != "try-error") summary(test_trk2pts)
dsn <- system.file("vectors", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="trin_inca_pl03")
birds <- readOGR(dsn=dsn, layer="trin_inca_pl03")
summary(birds)
}
\keyword{spatial}

