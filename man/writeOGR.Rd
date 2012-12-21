\name{writeOGR}
\alias{writeOGR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write spatial vector data using OGR}
\description{
  The function is an interface with the OGR abstraction library for spatial vector data, allowing data to be written out using supported drivers. The drivers supported will depend on the local installation, and the capabilities of those drivers (many are read-only). The objects exported are SpatialPointsDataFrame, SpatialLinesDataFrame, or SpatialPolygonsDataFrame objects as defined in the sp package.
}
\usage{
writeOGR(obj, dsn, layer, driver, dataset_options = NULL,
 layer_options=NULL, verbose = FALSE, check_exists=NULL, overwrite_layer=FALSE, delete_dsn=FALSE, morphToESRI=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame object.}
  \item{dsn}{data source name (interpretation varies by driver --- for some drivers, dsn is a file name, but may also be a folder)}
  \item{layer}{layer name (varies by driver, may be a file name without extension)}
  \item{driver}{a character string equal to one of the driver names returned by \code{\link{ogrDrivers}}}
  \item{dataset_options}{a character vector of options, which vary by driver, and should be treated as experimental}
  \item{layer_options}{a character vector of options, which vary by driver, and should be treated as experimental}
  \item{verbose}{if TRUE, returns a list of information about the attempted write operation}
  \item{check_exists}{default NULL, which tests for the GDAL version, and sets FALSE if < 1.8.0, or TRUE for >= 1.8.0}
  \item{overwrite_layer}{default FALSE, if TRUE and \code{check_exists=TRUE}, delete the existing layer of the same name from the data source before writing the new layer; this will delete data and must be used with extreme caution, its behaviour varies between drivers, and accommodates changes that may appear in GDAL 1.8}
  \item{delete_dsn}{default FALSE, may be set to TRUE if \code{overwrite_layer} reports that the data source cannot be updated; this will delete data and must be used with extreme caution, its behaviour varies between drivers, and accommodates changes that may appear in GDAL 1.8}
  \item{morphToESRI}{default NULL, in which case set TRUE if driver is \dQuote{ESRI Shapefile} or FALSE otherwise; may be used to override this default}
}
\details{
  Working out which combination of dsn, layer, and driver (and option) values give the desired output takes time and care, and is constrained by the ability of drivers to write output; many are read-only. Use of the references given is highly advisable, with searches in the archives of other software using GDAL/OGR. Note that for the \dQuote{ESRI Shapefile} driver and GDAL >= 1.9, the \code{layer_options} value of \sQuote{ENCODING=\dQuote{LDID/CP1252}} or other values found on \url{http://www.autopark.ru/ASBProgrammerGuide/DBFSTRUC.HTM} to set the encoding byte of the output DBF file (link refered to in ogr/ogrsf_frmts/shape/ogrshapelayer.cpp. The effect of setting the LDID may vary depending on whether GDAL is built with iconv or not, and on the setting of the CPL Option \dQuote{SHAPE_ENCODING}.
}

\value{
 if verbose=TRUE, a list of information about the attempted write operation
}

\note{Only a subset of possible data slot column classes may be written out; if the function returns an error that the data type of stated columns is unknown, examine the classes and check that they are one of \code{c("numeric", "character", "factor", "POSIXt", "integer", "logical")}, and if not convert to such classes. Classes \code{c("factor", "POSIXt")} are converted to character strings, and \code{c("logical")} to integer internally.

For writing with the KML and GPX drivers, note that the geometries should be in geographical coordinates with datum WGS84.
}
\section{Warning}{
The \code{overwrite_layer} and \code{delete_dsn} arguments are provided only for experienced script writers who need to be able to destroy data, for example during repetetive simulation runs. They should never be used by anyone who is not confident about deleting files.
}
\references{\url{http://www.gdal.org/ogr/}, \url{http://www.gdal.org/ogr/ogr_formats.html}, \url{http://examples.oreilly.com/webmapping/}}
\author{Roger Bivand}

\seealso{\code{\link{readOGR}}}
\examples{
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
is.na(cities$POPULATION) <- cities$POPULATION == -99
summary(cities$POPULATION)
td <- tempdir()
if(nchar(Sys.getenv("OSGEO4W_ROOT") > 0)) {
    OLDPWD <- getwd()
    setwd(td)
    td <- "."
}
writeOGR(cities, td, "cities", driver="ESRI Shapefile")
cities2 <- readOGR(td, "cities")
summary(cities2$POPULATION)
all.equal(cities, cities2)
\dontrun{if ("GML" \%in\% ogrDrivers()$name) {
  airports <- try(readOGR(system.file("vectors/airports.gml",
    package = "rgdal")[1], "airports"))
  if (class(airports) != "try-error") {
    writeOGR(cities, paste(td, "cities.gml", sep="/"), "cities", driver="GML")
    cities3 <- readOGR(paste(td, "cities.gml", sep="/"), "cities")
    all.equal(cities, cities3)
  }
}}
# The GML driver does not support coordinate reference systems
if ("KML" \%in\% ogrDrivers()$name) {
  data(meuse)
  coordinates(meuse) <- c("x", "y")
  proj4string(meuse) <- CRS("+init=epsg:28992")
  meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
  writeOGR(meuse_ll["zinc"], paste(td, "meuse.kml", sep="/"), "zinc", "KML")
}
list.files(td)
roads <- readOGR(system.file("vectors", package = "rgdal")[1],
 "kiritimati_primary_roads")
summary(roads)
writeOGR(roads, td, "roads", driver="MapInfo File")
roads2 <- readOGR(paste(td, "roads.tab", sep="/"), "roads")
summary(roads2)
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1], "scot_BNG")
summary(scot_BNG)
writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File")
list.files(td)
scot_BNG2 <- readOGR(paste(td, "scot_BNG.tab", sep="/"), "scot_BNG", addCommentsToPolygons=FALSE)
summary(scot_BNG2)
writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File",
 dataset_options="FORMAT=MIF")
list.files(td)
scot_BNG3 <- readOGR(paste(td, "scot_BNG.mif", sep="/"), "scot_BNG")
summary(scot_BNG3)
if(nchar(Sys.getenv("OSGEO4W_ROOT") > 0)) {
    setwd(OLDPWD)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

