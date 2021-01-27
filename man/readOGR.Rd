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
 stringsAsFactors=as.logical(NA), 
 drop_unsupported_fields=FALSE,
 pointDropZ=FALSE, dropNULLGeometries=TRUE,
 useC=TRUE, disambiguateFIDs=FALSE, addCommentsToPolygons=TRUE,
 encoding=NULL, use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType = NULL,
 integer64="no.loss", GDAL1_integer64_policy=FALSE, morphFromESRI = NULL,
 dumpSRS = FALSE, enforce_xy = NULL)
ogrInfo(dsn, layer, encoding=NULL,
 use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType = NULL,
 morphFromESRI = NULL, dumpSRS = FALSE, enforce_xy = NULL)
ogrFIDs(dsn, layer)
ogrDrivers()
OGRSpatialRef(dsn, layer, morphFromESRI=NULL, dumpSRS = FALSE, driver = NULL,
 enforce_xy = NULL)
ogrListLayers(dsn)
\method{print}{ogrinfo}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{data source name (interpretation varies by driver --- for some drivers, dsn is a file name, but may also be a folder)}
  \item{layer}{layer name (varies by driver, may be a file name without extension). From rgdal 1.2.*, layer may be missing, in which case ogrListLayers examines the dsn, and fails if there are no layers, silently reads the only layer if only one layer is found, and reads the first layer if multiple layers are present, issuing a warning that layer should be given explicitly.}
  \item{verbose}{report progress}
  \item{p4s}{PROJ4 string defining CRS, if default NULL, the value is read from the OGR data set} 
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? Default NA, which uses the deprecated \code{default.stringsAsFactors()} in R < 4.1.0 (see \code{link[base]{data.frame}}). Before R 4, strings were converted to factors by default, as argument value TRUE. See \url{https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html} for details of changes.}
  \item{drop\_unsupported\_fields}{default FALSE, if TRUE skip fields other than String, Integer, and Real; Date, Time and DateTime are converted to String}
  \item{pointDropZ}{default FALSE, if TRUE, discard third coordinates for point geometries; third coordinates are alway discarded for line and polygon geometries}
  \item{dropNULLGeometries}{default TRUE, drop both declared NULL geometries, and empty geometries with no coordinates; if FALSE, return a data frame with the attribute values of the NULL and empty geometries. From 1.3-6, setting FALSE also works when there are no geometries at all, returning a data.frame including all FIDs}
  \item{useC}{default TRUE, if FALSE use original interpreted code in a loop}
  \item{disambiguateFIDs}{default FALSE, if TRUE, and FID values are not unique, they will be set to unique values 1:N for N features; problem observed in GML files}
  \item{addCommentsToPolygons}{default TRUE, may be set FALSE for legacy behaviour; used to indicate which interior rings are holes in which exterior rings in conformance with OGC SFS specifications}
  \item{encoding}{default NULL, if set to a character string, and the driver is \dQuote{ESRI Shapefile}, and use_iconv is FALSE, it is passed to the CPL Option \dQuote{SHAPE_ENCODING} immediately before reading the DBF of a shapefile. If use_iconv is TRUE, and encoding is not NULL, it will be used to convert input strings from the given value to the native encoding for the system/platform.}
  \item{use_iconv}{default FALSE; if TRUE and encoding is not NULL, it will be used to convert input strings from the given value to the native encoding for the system/platform.}
  \item{swapAxisOrder}{default FALSE, if TRUE, treat y coordinate as Easting, x as Northing, that is the opposite to the assumed order; this may be needed if some OGR read drivers do not behave as expected}
  \item{require_geomType}{, default NULL, if one of: \code{c("wkbPoint", "wkbLineString", "wkbPolygon")}, then in input with multiple geometry types, the chosen type will be read}
  \item{integer64}{default \dQuote{no.loss} (from rgdal 1.2.*). From GDAL 2, fields to be read may also take Integer64 values. As R has no such storage mode, three options are offered, analogous with \code{\link{type.convert}} for numeric conversion: \dQuote{allow.loss} which clamps to 32-bit signed integer (default < rgdal 1.2), \dQuote{warn.loss} - as \dQuote{allow.loss} but warns when clamping occurs, and \dQuote{no.loss}, which reads as a character string using the formatting applied by default by GDAL (default >= rgdal 1.2). The use of 64-bit integers is usually a misunderstanding, as such data is almost always a long key ID.}
  \item{GDAL1_integer64_policy}{default FALSE, if TRUE, Integer64 fields are read as doubles}
  \item{morphFromESRI}{default NULL, morph from ESRI WKT1 dialect}
  \item{dumpSRS}{dump SRS to stdout from inside GDAL to debug conversion - developer use only}
  \item{enforce_xy}{(PROJ6+/GDAL3+) either use global setting (default NULL) or override policy for coordinate ordering easting/x as first axis, northing/y as second axis.}
  \item{driver}{default NULL, driver found using \code{ogrListLayers} from the data source; otherwise already known and passed through from a calling function}
  \item{x}{ogrinfo object}
  \item{...}{other arguments to print method}
}
\details{
  The drivers available will depend on the installation of GDAL/OGR, and can vary; the \code{ogrDrivers()} function shows which are available, and which may be written (but all are assumed to be readable). Note that stray files in data source directories (such as *.dbf) may lead to suprious errors that accompanying *.shp are missing.
}
\value{
  A Spatial object is returned suiting the vector data source, either a SpatialPointsDataFrame (using an AttributeList for its data slot directly), a SpatialLinesDataFrame, or a SpatialPolygonsDataFrame.
}
\references{\url{https://gdal.org/drivers/vector/index.html}, \url{https://resources.oreilly.com/examples/9780596008659}}
\author{Roger Bivand}

\note{The bases for this implementation are taken from functions in Barry Rowlingson's draft Rmap package, and from Radim Blazek's v.in.ogr program in GRASS.

Please note that the OGR drivers used may not handle missing data gracefully, and be prepared to have to correct for this manually. From rgdal 0.5-27, missing value handling has been improved, and OGR unset field values are set to NA in R, but drivers and external files may vary in their representations of missing values.

In addition, from 0.6-9 date and time fields are read as strings rather than being treated as unsupported; NULL geometries are identified and dropped. There are differences in the reporting of NULL geometries between \code{ogrInfo} and \code{readOGR} - in \code{ogrInfo}, only declared NULL geometries are reported, but in \code{readOGR}, any line or polygon geometries with no coordinates are assigned NULL geometry status as well. An attempt is made to close unclosed rings in polygon geometries. 

For reading GPX files, refer to the OGR GPX format documentation for the use of layer tags: \dQuote{waypoints}, \dQuote{tracks}, \dQuote{routes}, \dQuote{track_points} and \dQuote{route_points} - reading GPX files requires a build of GDAL/OGR with the expat XML library. 

From 0.6-10, attempts are made to detect deleted features still present in the layer, but not read. Apparently features deleted in Qgis are only marked as deleted, but are still in the layer. These are not NULL geometries, but still need to be handled. An attempt is made to check the FID values, and \code{ogrFIDs} now returns attributes permitting this oddity to be detected. Such deleted features were seen as NULL in 0.6-9, but are not such.

From 0.7-24, if the layer has no fields, a single field containing the FID values is placed in the data slot of the returned object.

From 0.7-24, attempts are begun to provide users with arguments to control reading from OGR/shapefile driver when the encoding is inappropriate (especially the setting of LDID in shapefile DBFs, and the SHAPE_ENCODING environment variable).

While there is no certainty, newer drivers such as KML, GML, SQLite and Geopackage (GPKG) may encode string fields as UTF-8. Users are advised to explore this on a case to case basis using \code{\link[base]{Encoding}} on string fields of input objects.

Because of the representation of DateTime data in OGR, decimal seconds in input data are rounded to integer seconds, see: \url{https://trac.osgeo.org/gdal/ticket/2680}.

Because some drivers support reading string, integer and real list fields, support has been introduced into \code{ogrInfo} from version 0.9-1 to report their presence and the maximum counts of list items. This may lead to the introduction of the \code{-splitlistfields} facility from the command line utility \code{ogrinfo}. In addition, \code{ogrInfo} reports that there are no features when counting FIDs in a while loop over features in \code{ogrFIDs} never enters the loop, despite the layer feature count reporting at least one feature.
}
\seealso{\code{\link[sp]{SpatialPointsDataFrame-class}}, \code{\link[sp]{SpatialLinesDataFrame-class}}, \code{\link[sp]{SpatialPolygonsDataFrame-class}}, \code{\link[maptools]{readShapePoly}}, \code{\link[base]{iconv}}}
\examples{
set_thin_PROJ6_warnings(TRUE)
ogrDrivers()
dsn <- system.file("vectors", package = "rgdal")[1]
ogrListLayers(dsn)
ogrInfo(dsn)
ogrInfo(dsn=dsn, layer="cities")
owd <- getwd()
setwd(dsn)
ogrInfo(dsn="cities.shp")
ogrInfo(dsn="cities.shp", layer="cities")
setwd(owd)
ow <- options("warn")$warn
options("warn"=1)
cities <- readOGR(dsn=dsn, layer="cities")
str(slot(cities, "data"))
if (new_proj_and_gdal()) comment(slot(cities, "proj4string"))
cities$POPULATION <- type.convert(as.character(cities$POPULATION),
  na.strings="-99", numerals="no.loss")
str(slot(cities, "data"))
cities <- readOGR(dsn=dsn, layer="cities", GDAL1_integer64_policy=TRUE)
str(slot(cities, "data"))
options("warn"=ow)
summary(cities)
table(Encoding(as.character(cities$NAME)))
ogrInfo(dsn=dsn, layer="kiritimati_primary_roads")
OGRSpatialRef(dsn=dsn, layer="kiritimati_primary_roads")
kiritimati_primary_roads <- readOGR(dsn=dsn, layer="kiritimati_primary_roads")
summary(kiritimati_primary_roads)
if (new_proj_and_gdal()) comment(slot(kiritimati_primary_roads, "proj4string"))
ogrInfo(dsn=dsn, layer="scot_BNG")
OGRSpatialRef(dsn=dsn, layer="scot_BNG")
scot_BNG <- readOGR(dsn=dsn, layer="scot_BNG")
summary(scot_BNG)
if (new_proj_and_gdal()) comment(slot(scot_BNG, "proj4string"))
if ("GML" \%in\% ogrDrivers()$name) {
  dsn <- system.file("vectors/airports.gml", package = "rgdal")[1]
  airports <- try(readOGR(dsn=dsn, layer="airports"))
  if (!inherits(airports, "try-error")) {
    summary(airports)
    if (new_proj_and_gdal()) comment(slot(airports, "proj4string"))
  }
}
dsn <- system.file("vectors/ps_cant_31.MIF", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="ps_cant_31")
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31")
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
if (new_proj_and_gdal()) comment(slot(ps_cant_31, "proj4string"))
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31", stringsAsFactors=FALSE)
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="Up")
Up <- readOGR(dsn=dsn, layer="Up")
summary(Up)
if (new_proj_and_gdal()) comment(slot(Up, "proj4string"))
dsn <- system.file("vectors/test_trk2.gpx", package = "rgdal")[1]
test_trk2 <- try(readOGR(dsn=dsn, layer="tracks"))
if (!inherits(test_trk2, "try-error")) {
  summary(test_trk2)
  if (new_proj_and_gdal()) comment(slot(test_trk2, "proj4string"))
}
test_trk2pts <- try(readOGR(dsn=dsn, layer="track_points"))
if (!inherits(test_trk2pts, "try-error")) {
  summary(test_trk2pts)
  if (new_proj_and_gdal()) comment(slot(test_trk2pts, "proj4string"))
}
dsn <- system.file("vectors", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="trin_inca_pl03")
birds <- readOGR(dsn=dsn, layer="trin_inca_pl03")
summary(birds)
if (new_proj_and_gdal()) comment(slot(birds, "proj4string"))
dsn <- system.file("vectors/PacoursIKA2.TAB", package = "rgdal")[1]
try(ogrInfo(dsn, "PacoursIKA2"))
ogrInfo(dsn, "PacoursIKA2", require_geomType="wkbPoint")
plot(readOGR(dsn, "PacoursIKA2", require_geomType="wkbLineString"), col="red")
plot(readOGR(dsn, "PacoursIKA2", require_geomType="wkbPoint"), add=TRUE)
odir <- getwd()
setwd(system.file("vectors", package = "rgdal")[1])
ow <- options("warn")$warn
options("warn"=1)
ogrInfo("test64.vrt", "test64")
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="allow.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="warn.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="no.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, stringsAsFactors=FALSE,
 integer64="no.loss")$val)
setwd(odir)
options("warn"=ow)
}
\keyword{spatial}

