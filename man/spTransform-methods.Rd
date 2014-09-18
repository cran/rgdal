\name{spTransform-methods}
\docType{methods}
\title{ Methods for Function spTransform for map projection and datum transformation in package "rgdal" }
\alias{spTransform-methods}
\alias{spTransform}
%\alias{spTransform,ANY-method}
\alias{spTransform,SpatialPoints,CRS-method}
\alias{spTransform,SpatialPointsDataFrame,CRS-method}
\alias{spTransform,SpatialPixelsDataFrame,CRS-method}
\alias{spTransform,SpatialGridDataFrame,CRS-method}
%\alias{spTransform,Sline-method}
%\alias{spTransform,Slines-method}
\alias{spTransform,SpatialLines,CRS-method}
%\alias{spTransform,Sring-method}
%\alias{spTransform,Srings-method}
\alias{spTransform,SpatialPolygons,CRS-method}
\alias{spTransform.SpatialPoints}
\alias{spTransform.SpatialPointsDataFrame}
%\alias{spTransform.Sline}
%\alias{spTransform.Slines}
\alias{spTransform.SpatialLines}
\alias{spTransform.SpatialLinesDataFrame}
\alias{spTransform,SpatialLinesDataFrame,CRS-method}
%\alias{spTransform.Sring}
%\alias{spTransform.Srings}
\alias{spTransform.SpatialPolygons}
\alias{spTransform.SpatialPolygonsDataFrame}
\alias{spTransform,SpatialPolygonsDataFrame,CRS-method}
\section{Methods}{\describe{

\item{"ANY"}{default void method}

\item{"SpatialPoints", CRSobj = CRS}{returns transformed coordinates of an "SpatialPoints" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialPointsDataFrame", CRSobj = CRS}{returns transformed coordinates of an "SpatialPointsDataFrame" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialLines", CRSobj = CRS}{returns transformed coordinates of an "SpatialLines" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialLinesDataFrame", CRSobj = CRS}{returns transformed coordinates of an "SpatialLinesDataFrame" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialPolygons", CRSobj = CRS}{returns transformed coordinates of an "SpatialPolygons" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialPolygonsDataFrame", CRSobj = CRS}{returns transformed coordinates of an "SpatialPolygonsDataFrame" object using the projection arguments in "CRSobj", of class CRS}
\item{"SpatialPixelsDataFrame", CRSobj = CRS}{Because regular grids will usually not be regular after projection/datum transformation, the input object is coerced to a SpatialPointsDataFrame, and the transformation carried out on that object. A warning: \dQuote{Grid warping not available, coercing to points} is given.}
\item{"SpatialGridDataFrame", CRSobj = CRS}{Because regular grids will usually not be regular after projection/datum transformation, the input object is coerced to a SpatialPointsDataFrame, and the transformation carried out on that object. A warning: \dQuote{Grid warping not available, coercing to points} is given.}

}}
\description{The \code{spTransform} methods provide transformation between datum(s) and conversion between projections (also known as projection and/or re-projection), from one unambiguously specified coordinate reference system to another, using PROJ.4 projection arguments. For simple projection, when no +datum tags are used, datum projection does not occur. When datum transformation is required, the +datum tag should be present with a valid value both in the CRS of the object to be transformed, and in the target CRS. In general +datum= is to be prefered to +ellps=, because the datum always fixes the ellipsoid, but the ellipsoid never fixes the datum. 

In addition, the +towgs84 tag should be used where needed to make sure that datum transformation does take place. Parameters for +towgs84 will be taken from the bundled EPSG database if they are known unequivocally, but may be entered manually from known authorities. Not providing the appropriate +datum and +towgs84 tags may lead to coordinates being out by hundreds of metres. Unfortunately, there is no easy way to provide this information: the user has to know the correct metadata for the data being used, even if this can be hard to discover.}

\note{The projection arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks.
Note that warnings about different projections may be issued when the PROJ.4 library extends projection arguments; examine the warning to see if the differences are real.

Also note that re-projection and/or datum transformation will usually not work for regular grids. The term used for similar operations for regular grids is warping, which involved resampling to a regular grid in the target coordinate reference system.

The methods may take an optional argument \dQuote{use_ob_tran}, default FALSE, if TRUE and \dQuote{+proj=ob_tran}, use General Oblique Transformation with internalised from/to projection reversal (the user oblique transforms from longlat to oblique forward rather than inverse as suggested in PROJ.4 mailing list postings); these changes are intended to meet a need pointed out by Martin Ivanov (2012-08-15).

If a SpatialPoints object has three dimensions, the third will also be transformed, with the metric of the third dimension assumed to be meters if the vertical units metric is not given in the projection description with +vunits= or +vto_meter= (which is 1.0 by default) \url{http://trac.osgeo.org/proj/wiki/GenParms#VerticalUnits}.

Note that WGS84 is both an ellipse and a datum, and that since 1984 there have been changes in the relative positions of continents, leading to a number of modifications. This is discussed for example in \url{http://www.uvm.edu/giv/resources/WGS84_NAD83.pdf}; there are then multiple transformations between NAD83 and WGS84 depending on the WGS84 definition used. One would expect that \dQuote{+towgs84=} is a no-op for WGS84, but this only applies sometimes, and as there are now at least 30 years between now and 1984, things have shifted. It may be useful to note that \dQuote{+nadgrids=@null} can help, see these threads: \url{https://stat.ethz.ch/pipermail/r-sig-geo/2014-August/021611.html}, \url{http://lists.maptools.org/pipermail/proj/2014-August/006894.html}, with thanks to Hermann Peifer for assistance.
}

\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\examples{
data(state)
states <- data.frame(state.x77, state.center)
states <- states[states$x > -121,]
coordinates(states) <- c("x", "y")
proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
summary(states)
state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
summary(state.ll83)
state.merc <- spTransform(states, CRS=CRS("+proj=merc +ellps=GRS80"))
summary(state.merc)
state.merc <- spTransform(states,
 CRS=CRS("+proj=merc +ellps=GRS80 +units=us-mi"))
summary(state.merc)
if (projNAD()) {
states <- data.frame(state.x77, state.center)
states <- states[states$x > -121,]
coordinates(states) <- c("x", "y")
proj4string(states) <- CRS("+init=epsg:4267")
print(summary(states))
state.ll83 <- spTransform(states, CRS("+init=epsg:4269"))
print(summary(state.ll83))
state.kansasSlcc <- spTransform(states, CRS=CRS("+init=epsg:26978"))
print(summary(state.kansasSlcc))
SFpoint_NAD83 <- SpatialPoints(matrix(c(-103.869667, 44.461676), nrow=1),
 proj4string=CRS("+init=epsg:4269"))
SFpoint_NAD27 <- spTransform(SFpoint_NAD83, CRS("+init=epsg:4267"))
print(all.equal(coordinates(SFpoint_NAD83), coordinates(SFpoint_NAD27)))
print(coordinates(SFpoint_NAD27), digits=12)
print(coordinates(SFpoint_NAD83), digits=12)
}
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS(paste("+init=epsg:28992",
 "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))
# see http://trac.osgeo.org/gdal/ticket/1987
summary(meuse)
meuse.utm <- spTransform(meuse, CRS("+proj=utm +zone=32 +datum=WGS84"))
summary(meuse.utm)
cbind(coordinates(meuse), coordinates(meuse.utm))
kiritimati_primary_roads <- readOGR(system.file("vectors",
 package = "rgdal")[1], "kiritimati_primary_roads")
kiritimati_primary_roads_ll <- spTransform(kiritimati_primary_roads,
 CRS("+proj=longlat +datum=WGS84"))
opar <- par(mfrow=c(1,2))
plot(kiritimati_primary_roads, axes=TRUE)
plot(kiritimati_primary_roads_ll, axes=TRUE, las=1)
par(opar)
opar <- par(mfrow=c(1,2))
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1],
   "scot_BNG")
scot_LL <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84"))
plot(scot_LL, axes=TRUE)
grd_LL <- gridlines(scot_LL, ndiscr=100)
summary(grd_LL)
grd_BNG <- spTransform(grd_LL, CRS(proj4string(scot_BNG)))
grdtxt_LL <- gridat(scot_LL)
grdtxt_BNG <- spTransform(grdtxt_LL, CRS(proj4string(scot_BNG)))
plot(scot_BNG, axes=TRUE, las=1)
plot(grd_BNG, add=TRUE, lty=2)
text(coordinates(grdtxt_BNG),
   labels=parse(text=as.character(grdtxt_BNG$labels)))
par(opar)
crds <- matrix(data=c(9.05, 48.52), ncol=2)
spPoint <- SpatialPoints(coords=crds,
 proj4string=CRS("+proj=longlat +ellps=sphere +no_defs"))
a <- spTransform(spPoint, CRS(paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs")),
 use_ob_tran=TRUE)
a
#should be (-5.917698, -1.87195)
spTransform(a, CRS("+proj=longlat +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
crds1 <- matrix(data=c(7, 51, 8, 52, 9, 52, 10, 51, 7, 51), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
crds2 <- matrix(data=c(8, 48, 9, 49, 11, 49, 9, 48, 8, 48), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
crds3 <- matrix(data=c(6, 47, 6, 55, 15, 55, 15, 47, 6, 47), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
spLines <- SpatialLines(list(Lines(list(Line(crds1), Line(crds2),
Line(crds3)), ID="a")));
spLines@proj4string <- CRS("+proj=longlat +ellps=sphere +no_defs");
bbox(spLines);
spLines_tr <- spTransform(spLines, CRS("+proj=ob_tran +o_proj=longlat
+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
use_ob_tran=TRUE);
bbox(spLines_tr)
bbox(spTransform(spLines_tr, CRS("+proj=longlat"), use_ob_tran=TRUE))
spPolygons <- SpatialPolygons(list(Polygons(list(Polygon(crds1),
Polygon(crds2), Polygon(crds3)), ID="a")));
spPolygons@proj4string <- CRS("+proj=longlat +ellps=sphere +no_defs");
bbox(spPolygons);
spPolygons_tr <- spTransform(spPolygons, CRS("+proj=ob_tran +o_proj=longlat
+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
use_ob_tran=TRUE);
bbox(spPolygons_tr)
bbox(spTransform(spPolygons_tr, CRS("+proj=longlat"), use_ob_tran=TRUE))
#added after posting by Martin Ivanov
data(nor2k)
summary(nor2k)
nor2kNGO <- spTransform(nor2k, CRS("+init=epsg:4273"))
summary(nor2kNGO)
all.equal(coordinates(nor2k)[,3], coordinates(nor2kNGO)[,3])
#added after posting by Don MacQueen 
crds <- cbind(c(-121.524764291826, -121.523480804667), c(37.6600366036405, 37.6543604613483))
ref <- cbind(c(1703671.30566227, 1704020.20113366), c(424014.398045834, 421943.708664294))
crs.step1.cf <- CRS(paste("+proj=lcc +lat_1=38.43333333333333",
 "+lat_2=37.06666666666667 +lat_0=36.5 +lon_0=-120.5",
 "+x_0=2000000.0 +y_0=500000.0 +ellps=GRS80 +units=us-ft +no_defs",
 "+towgs84=-0.991,1.9072,0.5129,0.025789908,0.0096501,0.0116599,0.0"))
locs.step1.cf <- spTransform(SpatialPoints(crds,
 proj4string=CRS("+proj=longlat +datum=WGS84")), crs.step1.cf)
suppressWarnings(proj4string(locs.step1.cf) <- CRS(paste("+proj=lcc",
"+lat_1=38.43333333333333 +lat_2=37.06666666666667 +lat_0=36.5",
"+lon_0=-120.5 +x_0=2000000.0 +y_0=500000.0 +ellps=GRS80 +units=us-ft",
"+no_defs +nadgrids=@null")))
locs.step2.cfb <- spTransform(locs.step1.cf, CRS("+init=epsg:26743"))
coordinates(locs.step2.cfb) - ref
all.equal(unname(coordinates(locs.step2.cfb)), ref)
}

\keyword{methods}
\keyword{spatial}
