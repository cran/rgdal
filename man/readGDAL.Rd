\name{readGDAL}
\alias{readGDAL}
\alias{asSGDF_GROD}
\alias{writeGDAL}
\alias{create2GDAL}
\alias{GDALinfo}
\alias{print.GDALobj}
\alias{GDALSpatialRef}
\title{Read/write between GDAL grid maps and Spatial objects}

\description{The functions read or write GDAL grid maps. They will set
the spatial reference system if available. \code{GDALinfo} reports the
size and other parameters of the dataset. \code{create2GDAL} creates
a GDAL data set from a SpatialGridDataFrame object, in particular to
be able to save to GDAL driver formats that only permit copying rather
than creation.}
\usage{ 
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
 allowedDrivers = NULL, enforce_xy = NULL, options=NULL)
}

\arguments{
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

%\item{clone}{if not NULL, a grid file to clone}
}
\details{In \code{writeGDAL}, if types other than \sQuote{Int32},
\sQuote{Float32}, \sQuote{Float64} are used, the \dQuote{mvFlag}
argument should be used to set a no data value other than the default
NA. Note that the flag only replaces NA values in the data being exported
with the value of the argument - it does not mark data values equal to
\dQuote{mvFlag} as missing. The value is stored in the file being written
in driver-specific ways, and may be used when the file is read. When the
default \dQuote{mvFlag=NA} is used, no NoDataValue is written to the file,
and the input data is written as is.

Also in \code{writeGDAL}, the \dQuote{options} argument may be
used to pass a character vector of one or more options to the
driver, for example \sQuote{options=\dQuote{INTERLEAVE=PIXEL}},
or \sQuote{options=c(\dQuote{INTERLEAVE=PIXEL},
\dQuote{COMPRESS=DEFLATE})}. Typical cases are given in the examples
below; it may also be necessary in some cases to escape quotation markes
if included in the string passed to the driver.}

\value{
\code{read.GDAL} returns the data in the file as a Spatial object. 

Usually, GDAL maps will be north-south oriented, in which case the rgdal
function \code{getRasterData} is used to read the data, and an object
of class \link[sp]{SpatialGridDataFrame-class} is returned.

Some map formats supported by GDAL are not north-south oriented grids. If
this is the case, \code{readGDAL} returns the data as a set of point
data, being of class \link[sp]{SpatialPointsDataFrame-class}. If the points
are on a 45 or 90 degree rotated grid, you can try to enforce gridding
later on by e.g. using \code{\link[sp]{gridded-methods}(x)=TRUE}.
}
\note{Non-fatal CPL errors may be displayed for some drivers, currently
for the AIG ArcInfo 9.3 binary raster driver using GDAL >= 1.6.2; the data
has been read correctly, but the contents of the info directory did not
meet the specifications used to reverse engineer the driver used in GDAL
(see \url{https://trac.osgeo.org/gdal/ticket/3031})}

\section{Warning}{Some raster files may have an erroneous positive
y-axis resolution step, leading to the data being flipped on the
y-axis. \code{readGDAL} will issue a warning: Y axis resolution
positive, examine data for flipping, when the step is positive, but
this need not mean that the data are flipped. Examine a display of the
data compared with your knowledge of the file to determine whether this
is the case (one known case is interpolation files created under Qgis
up to February 2010 at least). To retreive the correct orientation,
use \code{\link[sp]{flip}}.}

\seealso{ \code{\link[sp]{image}}, \code{\link[maptools]{asciigrid}} }

\author{Edzer Pebesma, Roger Bivand}

\examples{
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
\dontrun{
ds <- GDAL.open(tf)
displayDataset(ds, reset.par=FALSE)
t(col2rgb(getColorTable(ds)[1:4]))
GDAL.close(ds)
}
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
}
\keyword{spatial}
