\name{rgdal-deprecated}
\alias{rgdal-deprecated}
\alias{project}
\alias{readGDAL}
\alias{asSGDF_GROD}
\alias{writeGDAL}
\alias{create2GDAL}
\alias{GDALinfo}
\alias{print.GDALobj}
\alias{GDALSpatialRef}
\alias{readOGR}
\alias{ogrInfo}
\alias{print.ogrinfo}
\alias{ogrFIDs}
\alias{ogrDrivers}
\alias{OGRSpatialRef}
\alias{ogrListLayers}
\alias{writeOGR}
\alias{CRSargs}
\alias{showWKT}
\alias{showEPSG}
\alias{getCPLConfigOption}
\alias{setCPLConfigOption}
\alias{GDALcall}
\alias{rawTransform}
\title{Deprecated functions and methods}
\description{Cumulative deprecated functions and methods from rgdal prior to package retirement/archiving during 2023.}
\usage{
project(xy, proj, inv = FALSE, use_ob_tran=FALSE, legacy=TRUE,
 allowNAs_if_not_legacy=FALSE, coordOp = NULL, verbose = FALSE,
 use_aoi=TRUE)
readGDAL(fname, offset, region.dim, output.dim, band, p4s=NULL, ...,
 half.cell=c(0.5, 0.5), silent = FALSE, OVERRIDE_PROJ_DATUM_WITH_TOWGS84=NULL,
 allowedDrivers = NULL, enforce_xy = NULL, options=NULL)
asSGDF_GROD(x, offset, region.dim, output.dim, p4s=NULL, ...,
 half.cell=c(0.5,0.5), OVERRIDE_PROJ_DATUM_WITH_TOWGS84=NULL, enforce_xy = NULL)
writeGDAL(dataset, fname, drivername = "GTiff", type = "Float32",
 mvFlag = NA, options=NULL, copy_drivername = "GTiff", setStatistics=FALSE,
 colorTables = NULL, catNames=NULL, enforce_xy = NULL)
create2GDAL(dataset, drivername = "GTiff", type = "Float32", mvFlag = NA,
 options=NULL, fname = NULL, setStatistics=FALSE, colorTables = NULL,
 catNames=NULL, enforce_xy = NULL)
GDALinfo(fname, silent=FALSE, returnRAT=FALSE, returnCategoryNames=FALSE,
 returnStats=TRUE, returnColorTable=FALSE,
 OVERRIDE_PROJ_DATUM_WITH_TOWGS84=NULL, returnScaleOffset=TRUE,
 allowedDrivers = NULL, enforce_xy = NULL, options=NULL)
GDALSpatialRef(fname, silent=FALSE, OVERRIDE_PROJ_DATUM_WITH_TOWGS84=NULL,
 allowedDrivers = NULL, enforce_xy = NULL, get_source_if_boundcrs=TRUE, options=NULL)
readOGR(dsn, layer, verbose = TRUE, p4s=NULL, 
 stringsAsFactors=as.logical(NA), 
 drop_unsupported_fields=FALSE,
 pointDropZ=FALSE, dropNULLGeometries=TRUE,
 useC=TRUE, disambiguateFIDs=FALSE, addCommentsToPolygons=TRUE,
 encoding=NULL, use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType = NULL,
 integer64="no.loss", GDAL1_integer64_policy=FALSE, morphFromESRI = NULL,
 dumpSRS = FALSE, enforce_xy = NULL, D3_if_2D3D_points=FALSE, missing_3D=0)
ogrInfo(dsn, layer, encoding=NULL,
 use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType = NULL,
 morphFromESRI = NULL, dumpSRS = FALSE, enforce_xy = NULL,
 D3_if_2D3D_points=FALSE)
ogrFIDs(dsn, layer)
ogrDrivers()
OGRSpatialRef(dsn, layer, morphFromESRI=NULL, dumpSRS = FALSE, driver = NULL,
 enforce_xy = NULL, get_source_if_boundcrs=TRUE)
ogrListLayers(dsn)
\method{print}{ogrinfo}(x, ...)
writeOGR(obj, dsn, layer, driver, dataset_options = NULL,
 layer_options=NULL, verbose = FALSE, check_exists=NULL,
 overwrite_layer=FALSE, delete_dsn=FALSE, morphToESRI=NULL,
 encoding=NULL, shp_edge_case_fix=FALSE, dumpSRS = FALSE)
checkCRSArgs(uprojargs)
showWKT(p4s, file = NULL, morphToESRI = FALSE, enforce_xy = NULL)
showEPSG(p4s, enforce_xy = NULL)
getCPLConfigOption(ConfigOption)
setCPLConfigOption(ConfigOption, value)
GDALcall(object, option, ...)
rawTransform(projfrom, projto, n, x, y, z=NULL, wkt=FALSE)
}
\arguments{
  \item{xy}{ 2-column matrix of coordinates }
  \item{proj}{ character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  \item{inv}{ default FALSE, if TRUE inverse projection to geographical coordinates }
  \item{use_ob_tran}{default FALSE, if TRUE and \dQuote{+proj=ob_tran}, use General Oblique Transformation with internalised from/to projection reversal; the user oblique transforms forward rather than inverse.}
  \item{legacy}{default TRUE, if FALSE, use transform C functions (enforced internally for Windows 32-bit platforms)}
  \item{allowNAs_if_not_legacy}{used if legacy is FALSE, default FALSE; introduced to handle use of NAs as object separators in \pkg{oce}}
  \item{coordOp}{default NULL, for PROJ >= 6 used to pass through a pre-defined coordinate operation}
  \item{verbose}{default FALSE, for PROJ >=6 used to show the coordinate operation used}
  \item{use_aoi}{With PROJ >= 6, use the area of interest defined as the range of \code{xy} in limiting the search for candidate coordinate operations; set FALSE if \code{use_ob_tran} is TRUE}
\item{fname}{ file name of grid map; in \code{create2GDAL} provides a way 
to pass through a file name with driver-required extension for sensitive 
drivers }
\item{x}{A GDALReadOnlyDataset object}
\item{offset}{Number of rows and columns from the origin (usually the upper 
left corner) to begin reading from; presently ordered (y,x) - this may change}
\item{region.dim}{The number of rows and columns to read from the dataset; 
presently ordered (y,x) - this may change}
\item{output.dim}{The number of rows and columns to return in the created 
object using GDAL's method to take care of image decimation / replication; 
presently ordered (y,x) - this may change}
\item{band}{if missing, all bands are read}
\item{p4s}{PROJ4 string defining CRS, if default (NULL), the value is read 
from the GDAL data set} 
\item{half.cell}{Used to adjust the intra-cell offset from corner to centre, 
usually as default, but may be set to c=(0,0) if needed; presently ordered (y,x) - this may change}
\item{silent}{ logical; if TRUE, comment and non-fatal CPL driver errors 
suppressed }
\item{OVERRIDE_PROJ_DATUM_WITH_TOWGS84}{logical value, default NULL, which case the cached option set by \code{set_OVERRIDE_PROJ_DATUM_WITH_TOWGS84} is used. Ignored if the GDAL version is less than \dQuote{1.8.0} or if the CPLConfigOption variable is already set; see \code{\link{getProjectionRef}} for further details}
\item{allowedDrivers}{a character vector of suggested driver short names may be provided starting from GDAL 2.0}
\item{...}{ arguments passed to either \code{getRasterData}, or
\code{getRasterTable}, depending on rotation angles (see below);
see the rgdal documentation for the available options (subsetting
etc.) }
\item{dataset}{ object of class \link[sp]{SpatialGridDataFrame-class} or 
\link[sp]{SpatialPixelsDataFrame-class}}
\item{drivername, copy_drivername}{ GDAL driver name; if the chosen driver 
does not support dataset creation, an attempt is made to use the 
\code{copy_drivername} driver to create a dataset, and \code{copyDatset} 
to copy to the target driver }
\item{type}{ GDAL write data type, one of: \sQuote{Byte}, \sQuote{Int16}, 
\sQuote{Int32}, \sQuote{Float32}, \sQuote{Float64}; \sQuote{UInt16}, 
\sQuote{UInt32} are available but have not been tests }
\item{ mvFlag }{default NA, missing value flag for output file; the default 
value works for \sQuote{Int32}, \sQuote{Float32}, \sQuote{Float64}, but 
suitable in-range value that fits the data type should be used for other 
data types, for example 255 for \sQuote{Byte}, -32768 for \sQuote{Int16}, 
and so on; see Details below.}
\item{enforce_xy}{(PROJ6+/GDAL3+) either use global setting (default NULL) or override policy for coordinate ordering easting/x as first axis, northing/y as second axis.}
\item{options}{driver-specific options to be passed to the GDAL driver; only available for opening datasets from GDAL 2.0; 
see copying and creation details below}
\item{setStatistics}{default FALSE, if TRUE, attempt to set per-band 
statistics in the output file (driver-dependent)}
\item{colorTables}{default NULL, if not NULL, a list of length equal to the number of bands, with NULL components for bands with no color table, or either an integer matrix of red, green, blue and alpha values (0-255), or a character vector of colours. The number of colours permitted may vary with driver.}
\item{catNames}{default NULL, if not NULL, a list of length equal to the number of bands, with NULL components for bands with no category names, or a string vector of category names}
\item{returnRAT}{default FALSE, if TRUE, return a list with a Raster 
Attribute Table or NULL for each band}
\item{returnCategoryNames}{default FALSE, if TRUE, return a list with a character vector of CategoryNames or NULL for each band}
\item{returnStats}{default TRUE, return band-wise statistics if avaliable (from 0.7-20 set to NA if not available)}
\item{returnColorTable}{default FALSE; if TRUE return band-wise colour tables in a list attribute \dQuote{ColorTables}}
\item{returnScaleOffset}{default TRUE, return a matrix of bandwise scales and offsets}
  \item{dsn}{data source name (interpretation varies by driver --- for some drivers, dsn is a file name, but may also be a folder)}
  \item{layer}{layer name (varies by driver, may be a file name without extension). From rgdal 1.2.*, layer may be missing, in which case ogrListLayers examines the dsn, and fails if there are no layers, silently reads the only layer if only one layer is found, and reads the first layer if multiple layers are present, issuing a warning that layer should be given explicitly.}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? Default NA, which uses the deprecated \code{default.stringsAsFactors()} in R < 4.1.0 (see \code{link[base]{data.frame}}). Before R 4, strings were converted to factors by default, as argument value TRUE. See \url{https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html} for details of changes.}
  \item{drop_unsupported_fields}{default FALSE, if TRUE skip fields other than String, Integer, and Real; Date, Time and DateTime are converted to String}
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
  \item{get_source_if_boundcrs}{The presence of the \code{+towgs84=} key in a Proj4 string \code{projargs=} argument value may promote the output WKT2 CRS to BOUNDCRS for PROJ >= 6 and GDAL >= 3, which is a coordinate operation from the input datum to WGS84. This is often unfortunate, so a PROJ function is called through \pkg{rgdal} to retrieve the underlying source definition.}
  \item{D3_if_2D3D_points}{https://github.com/r-spatial/sf/issues/1683 case of mixed 2D/3D track points - set TRUE to 3D to pass}
  \item{missing_3D}{default 0, may be finite real numbers; https://github.com/r-spatial/sf/issues/1683}
  \item{driver}{default NULL, driver found using \code{ogrListLayers} from the data source; otherwise already known and passed through from a calling function}
  \item{obj}{a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame object.}
  \item{dataset_options}{a character vector of options, which vary by driver, and should be treated as experimental}
  \item{layer_options}{a character vector of options, which vary by driver, and should be treated as experimental}
  \item{check_exists}{default NULL, which tests for the GDAL version, and sets FALSE if < 1.8.0, or TRUE for >= 1.8.0}
  \item{overwrite_layer}{default FALSE, if TRUE and \code{check_exists=TRUE}, delete the existing layer of the same name from the data source before writing the new layer; this will delete data and must be used with extreme caution, its behaviour varies between drivers, and accommodates changes that may appear in GDAL 1.8}
  \item{delete_dsn}{default FALSE, may be set to TRUE if \code{overwrite_layer} reports that the data source cannot be updated; this will delete data and must be used with extreme caution, its behaviour varies between drivers, and accommodates changes that may appear in GDAL 1.8}
  \item{morphToESRI}{default NULL, in which case set TRUE if driver is \dQuote{ESRI Shapefile} or FALSE otherwise; may be used to override this default}
  \item{shp_edge_case_fix}{default FALSE, if TRUE, attempt to work around MULTIPOLYGON to POLYGON degradation in ESRI Shapefile output with two touching exterior rings in a single feature (not yet implemented).}
  \item{uprojargs}{character string PROJ.4 projection arguments}
  \item{file}{if not NULL, a file name to which the output Well-Known Text representation should be written}
  \item{ConfigOption}{CPL configure option documented in \url{https://trac.osgeo.org/gdal/wiki/ConfigOptions} and elsewhere in GDAL source code}
  \item{value}{a string value to set a CPL option; NULL is used to unset the CPL option}
  \item{object}{GDALTransientDataset (option = 'SetGeoTransform', 'SetProject') or GDALRasterBand (the other options)}
  \item{option}{character. One of 'SetGeoTransform', 'SetProject', 'SetNoDataValue', 'SetStatistics', 'SetRasterColorTable' or 'SetCategoryNames') }
  \item{projfrom}{character. PROJ.4 coordinate reference system (CRS) description}
  \item{projto}{character. PROJ.4 CRS description}
  \item{n}{number of coordinates}
  \item{y}{y coordinates}
  \item{z}{z coordinates}
  \item{wkt}{default FALSE, if TRUE, the caller determines that projfrom and projto are wkt and that new_proj_and_gdal() returns TRUE to avoid multiple warnings when the function is called repetitively}
}
\examples{
data(state)
res <- project(cbind(state.center$x, state.center$y),
 "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
res1 <- project(res, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84",
 inv=TRUE)
summary(res1 - cbind(state.center$x, state.center$y))
plot(cbind(state.center$x, state.center$y), asp=1, type="n")
text(cbind(state.center$x, state.center$y), state.abb)
plot(res, asp=1, type="n")
text(res, state.abb)
broke_proj <- FALSE
pv <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]]
# https://github.com/OSGeo/PROJ/issues/1525
if (pv >= 600 && pv < 620) broke_proj <- TRUE
if (!broke_proj) {
crds <- matrix(data=c(9.05, 48.52), ncol=2)
a <- project(crds, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
a
#should be (-5.917698, -1.87195)
project(a, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 inv=TRUE, use_ob_tran=TRUE)
#added after posting by Martin Ivanov
}
#
getPROJ4VersionInfo()
# Test for UTM == TMERC (<= 4.9.2) or UTM == ETMERC (> 4.9.2)
nhh <- matrix(c(5.304234, 60.422311), ncol=2)
nhh_utm_32N_P4 <- project(nhh, "+init=epsg:3044")
nhh_tmerc_P4 <- project(nhh, paste("+proj=tmerc +k=0.9996 +lon_0=9",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
nhh_etmerc_P4 <- project(nhh, paste("+proj=etmerc +k=0.9996 +lon_0=9",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
all.equal(nhh_utm_32N_P4, nhh_tmerc_P4, tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(nhh_utm_32N_P4, nhh_etmerc_P4, tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
unis <- matrix(c(15.653453, 78.222504), ncol=2)
unis_utm_33N_P4 <- project(unis, "+init=epsg:3045")
unis_tmerc_P4 <- project(unis, paste("+proj=tmerc +k=0.9996 +lon_0=15",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
unis_etmerc_P4 <- project(unis, paste("+proj=etmerc +k=0.9996 +lon_0=15",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
all.equal(unis_utm_33N_P4, unis_tmerc_P4, tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(unis_utm_33N_P4, unis_etmerc_P4, tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
#pv <- attr(getPROJ4VersionInfo(), "short")
#if (pv < 500) {  
# valgrind leakages in some cases for PROJ >= 5; many non-projection proj values added
# available projections and their inverses if provided
# For >=4.9.3 returns non-finite points rather than needing crash protection
projs <- as.character(projInfo()$name)
res <- logical(length(projs))
names(res) <- projs
msgs <- character(length(projs))
names(msgs) <- projs
owarn <- options("warn")$warn
options(warn=2L)
for (i in seq(along=res)) {
  iprs <- paste("+proj=", projs[i], sep="")
  xy <- try(project(cbind(0, 0), iprs, legacy=TRUE, use_aoi=FALSE), silent=TRUE)
  if (inherits(xy, "try-error")) {
    res[i] <- NA
    msgs[i] <- paste("fwd:", strsplit(xy, "\n")[[1]][2])
  } else if(any(abs(xy) > 1e+08)) {
    res[i] <- NA
    msgs[i] <- paste("fwd: huge value")
  } else {
    out <- try(project(xy, iprs, inv=TRUE, legacy=TRUE, use_aoi=FALSE), silent=TRUE)
    if (inherits(out, "try-error")) {
      res[i] <- NA
      msgs[i] <- paste("inv:", strsplit(out, "\n")[[1]][2])
    } else {
      res[i] <- isTRUE(all.equal(cbind(0,0), out))
    }
  }
}
options(warn=owarn)
df <- data.frame(res=unname(res), msgs=unname(msgs), row.names=names(res))
# projection and inverse projection failures
# fwd: missing parameters
# inv: mostly inverse not defined
df[is.na(df$res),]
# inverse not equal to input
# (see http://lists.maptools.org/pipermail/proj/2011-November/006015.html)
df[!is.na(df$res) & !df$res,]
# inverse equal to input
row.names(df[!is.na(df$res) & df$res,])
#}
# oce data representation with NAs
ll <- structure(c(12.1823368669203, 11.9149630062421, 12.3186076188739, 
12.6207597184845, 12.9955172054652, 12.6316117692658, 12.4680041846297, 
12.4366882666609, NA, NA, -5.78993051516384, -5.03798674888479, 
-4.60623015708619, -4.43802336997614, -4.78110320396188, -4.99127125409291, 
-5.24836150474498, -5.68430388755925, NA, NA), .Dim = c(10L, 
2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(xy0 <- project(ll, "+proj=moll", legacy=TRUE))
if (!PROJis6ormore()) { # legacy=TRUE PROJ >= 6
try(xy1 <- project(ll, "+proj=moll", legacy=FALSE, allowNAs_if_not_legacy=FALSE))
try(xy2 <- project(ll, "+proj=moll", legacy=FALSE, allowNAs_if_not_legacy=TRUE))
if (exists("xy0")) all.equal(xy0, xy2)
}
if (!exists("xy0")) xy0 <- structure(c(1217100.8468177, 1191302.229156,
1232143.28841193, 1262546.27733232, 1299648.82357849, 1263011.18154638,
1246343.17808186, 1242654.33986052, NA, NA, -715428.207551599,
-622613.577983058, -569301.605757784, -548528.530156422, -590895.949857199,
-616845.926397351, -648585.161643274, -702393.1160979, NA, NA), 
.Dim = c(10L, 2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(ll0 <- project(xy0, "+proj=moll", inv=TRUE, legacy=TRUE))
if (!PROJis6ormore()) { # legacy=TRUE PROJ >= 6
try(ll1 <- project(xy0, "+proj=moll", inv=TRUE, legacy=FALSE, allowNAs_if_not_legacy=FALSE))
try(ll2 <- project(xy0, "+proj=moll", inv=TRUE, legacy=FALSE, allowNAs_if_not_legacy=TRUE))
if (exists("ll0")) all.equal(ll0, ll2)
}
if (exists("ll0")) all.equal(ll0, ll)
\dontrun{
set_thin_PROJ6_warnings(TRUE)
library(grid)
GDALinfo(system.file("external/test.ag", package="sp")[1])
x <- readGDAL(system.file("external/test.ag", package="sp")[1])
class(x)
image(x)
summary(x)
x@data[[1]][x@data[[1]] > 10000] <- NA
summary(x)
image(x)

x <- readGDAL(system.file("external/simple.ag", package="sp")[1])
class(x)
image(x)
summary(x)
x <- readGDAL(system.file("pictures/big_int_arc_file.asc", package="rgdal")[1])
summary(x)
cat("if the range is not 10000, 77590, your GDAL does not detect big\n")
cat("integers for this driver\n")
y = readGDAL(system.file("pictures/Rlogo.jpg", package = "rgdal")[1], band=1)
summary(y)
y = readGDAL(system.file("pictures/Rlogo.jpg", package = "rgdal")[1])
summary(y)
spplot(y, names.attr=c("red","green","blue"), 
	col.regions=grey(0:100/100),
	main="example of three-layer (RGB) raster image", as.table=TRUE)
data(meuse.grid)
gridded(meuse.grid) = ~x+y
proj4string(meuse.grid) = CRS("+init=epsg:28992")
fn <- tempfile()
writeGDAL(meuse.grid["dist"], fn)
GDALinfo(fn)
writeGDAL(meuse.grid["dist"], fn, setStatistics=TRUE)
GDALinfo(fn)
mg2 <- readGDAL(fn)
proj4string(mg2)
SP27GTIF <- readGDAL(system.file("pictures/SP27GTIF.TIF", 
package = "rgdal")[1], output.dim=c(100,100))
summary(SP27GTIF)
slot(SP27GTIF, "proj4string")
if (new_proj_and_gdal()) comment(slot(SP27GTIF, "proj4string"))
image(SP27GTIF, col=grey(1:99/100))
GDALinfo(system.file("pictures/cea.tif", package = "rgdal")[1])
(o <- GDALSpatialRef(system.file("pictures/cea.tif", package = "rgdal")[1]))
if (new_proj_and_gdal()) comment(o)
cea <- readGDAL(system.file("pictures/cea.tif", package = "rgdal")[1], 
output.dim=c(100,100))
summary(cea)
image(cea, col=grey(1:99/100))
slot(cea, "proj4string")
if (new_proj_and_gdal()) comment(slot(cea, "proj4string"))
fn <- system.file("pictures/erdas_spnad83.tif", package = "rgdal")[1]
erdas_spnad83 <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400), 
output.dim=c(100,100))
summary(erdas_spnad83)
slot(erdas_spnad83, "proj4string")
if (new_proj_and_gdal()) comment(slot(erdas_spnad83, "proj4string"))
image(erdas_spnad83, col=grey(1:99/100))
erdas_spnad83a <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400))
bbox(erdas_spnad83)
bbox(erdas_spnad83a)
gridparameters(erdas_spnad83)
gridparameters(erdas_spnad83a)
tf <- tempfile()
writeGDAL(erdas_spnad83, tf, drivername="GTiff", type="Byte", options=NULL)
erdas_spnad83_0 <- readGDAL(tf)
slot(erdas_spnad83_0, "proj4string")
if (new_proj_and_gdal()) comment(slot(erdas_spnad83_0, "proj4string"))
all.equal(erdas_spnad83, erdas_spnad83_0)
writeGDAL(erdas_spnad83, tf, drivername="GTiff", type="Byte", 
options="INTERLEAVE=PIXEL")
erdas_spnad83_1 <- readGDAL(tf)
slot(erdas_spnad83_1, "proj4string")
if (new_proj_and_gdal()) comment(slot(erdas_spnad83_1, "proj4string"))
all.equal(erdas_spnad83, erdas_spnad83_1)
writeGDAL(erdas_spnad83, tf, drivername="GTiff", type="Byte",
options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE"))
erdas_spnad83_2 <- readGDAL(tf)
slot(erdas_spnad83_2, "proj4string")
if (new_proj_and_gdal()) comment(slot(erdas_spnad83_2, "proj4string"))
all.equal(erdas_spnad83, erdas_spnad83_2)

x <- GDAL.open(system.file("pictures/erdas_spnad83.tif", package = "rgdal")[1])
erdas_spnad83 <- asSGDF_GROD(x, output.dim=c(100,100))
GDAL.close(x)
summary(erdas_spnad83)
image(erdas_spnad83, col=grey(1:99/100))

tf <- tempfile()
xx <- create2GDAL(erdas_spnad83, type="Byte")
xxx <- copyDataset(xx, driver="PNG")
saveDataset(xxx, tf)
GDAL.close(xx)
GDAL.close(xxx)
GDALinfo(tf)

tf2 <- tempfile()
writeGDAL(erdas_spnad83, tf2, drivername="PNG", type="Byte")
GDALinfo(tf2)

GT <- GridTopology(c(0.5, 0.5), c(1, 1), c(10, 10))
set.seed(1)
SGDF <- SpatialGridDataFrame(GT, data=data.frame(z=runif(100)))
opar <- par(mfrow=c(2,2), mar=c(1,1,4,1))
image(SGDF, "z", col=colorRampPalette(c("blue", "yellow"))(20))
title(main="input values")
pfunc <- colorRamp(c("blue","yellow"))
RGB <- pfunc(SGDF$z)
SGDF$red <- RGB[,1]
SGDF$green <- RGB[,2]
SGDF$blue <- RGB[,3]
image(SGDF, red="red", green="green", blue="blue")
title(main="input RGB")
tf <- tempfile()
writeGDAL(SGDF[c("red", "green", "blue")], tf, type="Byte", drivername="PNG")
t1 <- readGDAL(tf)
image(t1, red=1, green=2, blue=3)
title(main="output PNG RGB")
par(opar)

t0 <- meuse.grid["ffreq"]
fullgrid(t0) <- TRUE
t0$ffreq <- as.integer(t0$ffreq)-1
# convert factor to zero-base integer
CT <- c("red", "orange", "green", "transparent")
CT
cN <- c("annual", "2-5 years", "infrequent")
tf <- tempfile()
writeGDAL(t0, tf, type="Byte", colorTable=list(CT), catNames=list(cN),
 mvFlag=3L)
attr(GDALinfo(tf, returnStats=FALSE, returnCategoryNames=TRUE),
 "CATlist")[[1]]
ds <- GDAL.open(tf)
displayDataset(ds, reset.par=FALSE)
t(col2rgb(getColorTable(ds)[1:4]))
GDAL.close(ds)
fn <- system.file("pictures/test_envi_class.envi", package = "rgdal")[1]
Gi <- GDALinfo(fn, returnColorTable=TRUE, returnCategoryNames=TRUE)
CT <- attr(Gi, "ColorTable")[[1]]
CT
attr(Gi, "CATlist")[[1]]
with <- readGDAL(fn)
with <- readGDAL(fn, silent=TRUE)
table(with$band1)
table(as.numeric(with$band1))
with1 <- readGDAL(fn, as.is=TRUE)
table(with1$band1)
spplot(with, col.regions=CT)
tf <- tempfile()
cN <- levels(with$band1)
with$band1 <- as.integer(with$band1)-1
writeGDAL(with, tf, drivername="ENVI", type="Int16", colorTable=list(CT),
 catNames=list(cN), mvFlag=11L)
cat(paste(readLines(paste(tf, "hdr", sep=".")), "\n", sep=""), "\n")
wGi <- GDALinfo(tf, returnColorTable=TRUE, returnCategoryNames=TRUE)
CTN <- attr(wGi, "ColorTable")[[1]]
CTN
attr(wGi, "CATlist")[[1]]
withN <- readGDAL(tf)
table(withN$band1)
withN1 <- readGDAL(tf, as.is=TRUE)
table(withN1$band1)
spplot(withN, col.regions=CTN)


# a file with scale and offset
fn <- system.file("pictures/scaleoffset.vrt", package = "rgdal")[1]
g <- GDALinfo(fn)
attr(g, 'ScaleOffset')
g

fl <- system.file("pictures/MR5905167_372.nc", package="rgdal")[1]
if (file.exists(fl)) {
  flstr <- paste0("NETCDF:\"", fl, "\":TEMP")
  if ("netCDF" \%in\% gdalDrivers()$name) GDALinfo(flstr)
}

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
set_thin_PROJ6_warnings(TRUE)
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
is.na(cities$POPULATION) <- cities$POPULATION == -99
summary(cities$POPULATION)
td <- file.path(tempdir(), "rgdal_examples"); dir.create(td)
# BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
    OLDPWD <- getwd()
    setwd(td)
    td <- "."
}
writeOGR(cities, td, "cities", driver="ESRI Shapefile")
try(writeOGR(cities, td, "cities", driver="ESRI Shapefile"))
writeOGR(cities, td, "cities", driver="ESRI Shapefile", overwrite_layer=TRUE)
cities2 <- readOGR(td, "cities")
summary(cities2$POPULATION)
if ("SQLite" \%in\% ogrDrivers()$name) {
  tf <- tempfile()
  try(writeOGR(cities, tf, "cities", driver="SQLite", layer_options="LAUNDER=NO"))
}
if ("GeoJSON" \%in\% ogrDrivers()$name) {
  js <- '{
    "type": "MultiPolygon",
    "coordinates": [[[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0],
    [102.0, 2.0]]], [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0],
    [100.0, 0.0]]]]
  }'
  spdf <- readOGR(js, layer='OGRGeoJSON')
  in1_comms <- sapply(slot(spdf, "polygons"), comment)
  print(in1_comms)
  tf <- tempfile()
  writeOGR(spdf, tf, "GeoJSON", driver="GeoJSON")
  #spdf1 <- readOGR(tf, "GeoJSON")
  spdf1 <- readOGR(tf)
  in2_comms <- sapply(slot(spdf1, "polygons"), comment)
  print(in2_comms)
  print(isTRUE(all.equal(in1_comms, in2_comms)))
}
}

\dontrun{if ("GML" \%in\% ogrDrivers()$name) {
  airports <- try(readOGR(system.file("vectors/airports.gml",
    package = "rgdal")[1], "airports"))
  if (class(airports) != "try-error") {
    writeOGR(cities, paste(td, "cities.gml", sep="/"), "cities", driver="GML")
    cities3 <- readOGR(paste(td, "cities.gml", sep="/"), "cities")
  }
}}
if (!exists("td")) {
  td <- file.path(tempdir(), "rgdal_examples"); dir.create(td)
# BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
  if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
    OLDPWD <- getwd()
    setwd(td)
    td <- "."
  }
}
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
if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
# For GDAL >= 2, the TAB driver may need a BOUNDS layer option
  writeOGR(roads, td, "roads", driver="MapInfo File")
  roads2 <- readOGR(paste(td, "roads.tab", sep="/"), "roads")
  summary(roads2)
}
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1], "scot_BNG")
summary(scot_BNG)
if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
# For GDAL >= 2, the TAB driver may need a BOUNDS layer option
  writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File")
  list.files(td)
  scot_BNG2 <- readOGR(paste(td, "scot_BNG.tab", sep="/"), "scot_BNG",
    addCommentsToPolygons=FALSE)
  summary(scot_BNG2)
}
writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File",
 dataset_options="FORMAT=MIF")
list.files(td)
scot_BNG3 <- readOGR(paste(td, "scot_BNG.mif", sep="/"), "scot_BNG")
summary(scot_BNG3)
if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
    setwd(OLDPWD)
}
set_thin_PROJ6_warnings(TRUE)
CRSargs(CRS("+proj=longlat"))
try(CRS("+proj=longlat"))
CRSargs(CRS("+proj=longlat +datum=NAD27"))
CRSargs(CRS("+init=epsg:4267"))
CRSargs(CRS("+init=epsg:26978"))
CRSargs(CRS(paste("+proj=stere +lat_0=52.15616055555555", 
"+lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel", 
"+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812", 
"+units=m")))
# see http://trac.osgeo.org/gdal/ticket/1987
CRSargs(CRS("+init=epsg:28992"))
crs <- CRS("+init=epsg:28992")
CRSargs(CRS(CRSargs(crs)))
set_thin_PROJ6_warnings(TRUE)
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
readLines(system.file("vectors/cities.prj", package = "rgdal")[1])
showWKT(proj4string(cities))
showWKT("+init=epsg:28992")
showP4(showWKT("+init=epsg:28992"))
showEPSG("+proj=utm +zone=30")
showEPSG("+proj=longlat +ellps=WGS84")
}

