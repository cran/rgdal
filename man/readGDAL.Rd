\name{readGDAL}
\alias{readGDAL}
\alias{asSGDF_GROD}
\alias{writeGDAL}
\alias{create2GDAL}
\alias{GDALinfo}
\alias{print.GDALobj}
\title{Read/write between GDAL grid maps and Spatial objects}
\description{The functions read or write GDAL grid maps. They will set the spatial reference system if available. \code{GDALinfo} reports the size and other parameters of the dataset. \code{create2GDAL} creates a GDAL data set from a SpatialGridDataFrame object, in particular to be able to save to GDAL driver formats that only permit copying rather than creation.}
\usage{ 
readGDAL(fname, offset, region.dim, output.dim, band, p4s=NULL, ..., half.cell=c(0.5, 0.5), silent = FALSE)
asSGDF_GROD(x, offset, region.dim, output.dim, p4s=NULL, ..., half.cell=c(0.5,0.5))
writeGDAL(dataset, fname, drivername = "GTiff", type = "Float32", mvFlag = NA, options=NULL, copy_drivername = "GTiff")
create2GDAL(dataset, drivername = "GTiff", type = "Float32", mvFlag = NA, options=NULL, fname = NULL)
GDALinfo(fname, silent=FALSE)
}

\arguments{
\item{fname}{ file name of grid map; in \code{create2GDAL} provides a way to pass through a file name with driver-required extension for sensitive drivers }
\item{x}{A GDALReadOnlyDataset object}
\item{offset}{Number of rows and columns from the origin (usually the upper left corner) to begin reading from; presently ordered (y,x) - this may change}
\item{region.dim}{The number of rows and columns to read from the dataset; presently ordered (y,x) - this may change}
\item{output.dim}{The number of rows and columns to return in the created object using GDAL's method to take care of image decimation / replication; presently ordered (y,x) - this may change}
\item{band}{if missing, all bands are read}
\item{p4s}{PROJ4 string defining CRS, if default (NULL), the value is read from the GDAL data set} 
\item{half.cell}{Used to adjust the intra-cell offset from corner to centre, usually as default, but may be set to c=(0,0) if needed; presently ordered (y,x) - this may change}
\item{silent}{ logical; if TRUE, comment and non-fatal CPL driver errors suppressed }
\item{...}{ arguments passed to either \code{getRasterData}, or
\code{getRasterTable}, depending on rotation angles (see below);
see the rgdal documentation for the available options (subsetting
etc.) }
\item{dataset}{ object of class \link[sp]{SpatialGridDataFrame-class} or \link[sp]{SpatialPixelsDataFrame-class}}
\item{drivername, copy_drivername}{ GDAL driver name; if the chosen driver does not support dataset creation, an attempt is made to use the \code{copy_drivername} driver to create a dataset, and \code{copyDatset} to copy to the target driver }
\item{type}{ GDAL write data type (others than this default have not been tested) }
\item{ mvFlag }{ missing value flag for output file }
\item{options}{driver-specific options to be passed to the GDAL driver}
%\item{clone}{if not NULL, a grid file to clone}
}

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
\note{Non-fatal CPL errors may be displayed for some drivers, currently for the AIG ArcInfo 9.3 binary raster driver using GDAL >= 1.6.2; the data has been read correctly, but the contents of the info directory did not meet the specifications used to reverse engineer the driver used in GDAL (see \url{http://trac.osgeo.org/gdal/ticket/3031})}

\seealso{ \code{\link[sp]{image}}, \code{\link[maptools]{asciigrid}} }

\author{Edzer J.\ Pebesma, Roger Bivand}

\examples{
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
spplot(y, zcol=1:3, names.attr=c("red","green","blue"), 
	col.regions=grey(0:100/100),
	main="example of three-layer (RGB) raster image", as.table=TRUE)
data(meuse.grid)
gridded(meuse.grid) = ~x+y
proj4string(meuse.grid) = CRS("+init=epsg:28992")
fn <- tempfile()
writeGDAL(meuse.grid["dist"], fn)
mg2 <- readGDAL(fn)
proj4string(mg2)
SP27GTIF <- readGDAL(system.file("pictures/SP27GTIF.TIF", package = "rgdal")[1], output.dim=c(100,100))
summary(SP27GTIF)
image(SP27GTIF, col=grey(1:99/100))

GDALinfo(system.file("pictures/cea.tif", package = "rgdal")[1])
cea <- readGDAL(system.file("pictures/cea.tif", package = "rgdal")[1], output.dim=c(100,100))
summary(cea)
image(cea, col=grey(1:99/100))
fn <- system.file("pictures/erdas_spnad83.tif", package = "rgdal")[1]
erdas_spnad83 <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400), output.dim=c(100,100))
summary(erdas_spnad83)
image(erdas_spnad83, col=grey(1:99/100))
erdas_spnad83a <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400))
bbox(erdas_spnad83)
bbox(erdas_spnad83a)
gridparameters(erdas_spnad83)
gridparameters(erdas_spnad83a)
tf <- tempfile()
writeGDAL(erdas_spnad83, tf, drivername="GTiff", type="Byte", options=NULL)
all.equal(erdas_spnad83, readGDAL(tf))
writeGDAL(erdas_spnad83, tf, drivername="GTiff", type="Byte", options="INTERLEAVE=PIXEL")
all.equal(erdas_spnad83, readGDAL(tf))

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

}
\keyword{spatial}
