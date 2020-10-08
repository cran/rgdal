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
\alias{get_transform_wkt_comment}
\alias{set_transform_wkt_comment}
\alias{get_last_coordOp}
\alias{get_enforce_xy}
\alias{set_enforce_xy}

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

\usage{
get_transform_wkt_comment()
set_transform_wkt_comment(value)
get_enforce_xy()
set_enforce_xy(value)
get_last_coordOp()
}

\arguments{
  \item{value}{A non-NA logical value}
}

\description{The \code{spTransform} methods provide transformation between datum(s) and conversion between projections (also known as projection and/or re-projection), from one unambiguously specified coordinate reference system (CRS) to another, prior to version 1.5 using Proj4 projection arguments. From version 1.5, Well-Known Text 2 (WKT2 2019) strings are used. For simple projection, when no Proj4 +datum tags are used, datum projection does not occur. When datum transformation is required, the datum should be defined with a valid value both in the CRS of the object to be transformed, and in the target CRS. In general datum is to be prefered to ellipsoid, because the datum always fixes the ellipsoid, but the ellipsoid never fixes the datum. 

In addition, before version 1.5 the +towgs84 tag should have been used where needed to make sure that datum transformation would take place. Parameters for +towgs84 were taken from the legacy bundled EPSG file if they are known unequivocally, but could be entered manually from known authorities. Not providing the appropriate +datum and +towgs84 tags led to coordinates being out by hundreds of metres. Unfortunately, there is no easy way to provide this information: the user has to know the correct metadata for the data being used, even if this can be hard to discover.

From version 1.5, \code{spTransform} uses the modern PROJ coordinate operation framework for transformations. This avoids pivoting through WGS84 if possible, and uses WKT2 (2019) strings for source and target CRS often constructed from the bundled EPSG SQLite database. The database is searched for feasible candidate coordinate operations, and the most accurate available is chosen. More details are available in a vignette: \code{vignette("CRS_projections_transformations")}.}

\note{The projection arguments had to be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks.
Note that warnings about different projections may be issued when the PROJ.4 library extends projection arguments; examine the warning to see if the differences are real.

Also note that re-projection and/or datum transformation will usually not work for regular grids. The term used for similar operations for regular grids is warping, which involved resampling to a regular grid in the target coordinate reference system.

The methods may take an optional argument \dQuote{use_ob_tran}, default FALSE, if TRUE and \dQuote{+proj=ob_tran}, use General Oblique Transformation with internalised from/to projection reversal (the user oblique transforms from longlat to oblique forward rather than inverse as suggested in PROJ.4 mailing list postings); these changes are intended to meet a need pointed out by Martin Ivanov (2012-08-15). A subsequent point raised by Martin Ivanov (2017-04-28) was that use of a projected CRS with \dQuote{+proj=ob_tran} led to errors, so mixing projected CRS and \dQuote{+proj=ob_tran} is blocked. Transform first \dQuote{+proj=ob_tran} to or from \dQuote{+proj=longlat}, and then on from geographical coordinates to those desired or the reverse - see example.

If a SpatialPoints object has three dimensions, the third will also be transformed, with the metric of the third dimension assumed to be meters if the vertical units metric is not given in the projection description with +vunits= or +vto_meter= (which is 1.0 by default) \url{https://proj.org/faq.html}.

Note that WGS84 is both an ellipse and a datum, and that since 1984 there have been changes in the relative positions of continents, leading to a number of modifications. This is discussed for example in \url{http://www.uvm.edu/giv/resources/WGS84_NAD83.pdf}; there are then multiple transformations between NAD83 and WGS84 depending on the WGS84 definition used. One would expect that \dQuote{+towgs84=} is a no-op for WGS84, but this only applies sometimes, and as there are now at least 30 years between now and 1984, things have shifted. It may be useful to note that \dQuote{+nadgrids=@null} can help, see these threads: \url{https://stat.ethz.ch/pipermail/r-sig-geo/2014-August/021611.html}, \url{http://lists.maptools.org/pipermail/proj/2014-August/006894.html}, with thanks to Hermann Peifer for assistance.

Note that from PROJ.4 4.9.3, the definition of UTM is changed from TMERC to ETMERC; see example.
}

\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\examples{
set_thin_PROJ6_warnings(TRUE)
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
if (PROJis6ormore() || (!PROJis6ormore() && projNAD())) {
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
proj4string(meuse) <- CRS("+init=epsg:28992")
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
grdtxt_LL <- gridat(scot_LL)
grd_LL <- gridlines(scot_LL, ndiscr=100)
summary(grd_LL)
target <- CRS(proj4string(scot_BNG))
grd_BNG <- spTransform(grd_LL, target)
grdtxt_BNG <- spTransform(grdtxt_LL, target)
plot(scot_BNG, axes=TRUE, las=1)
plot(grd_BNG, add=TRUE, lty=2)
text(coordinates(grdtxt_BNG),
   labels=parse(text=as.character(grdtxt_BNG$labels)))
par(opar)
broke_proj <- FALSE
# https://github.com/OSGeo/PROJ/issues/1525
pv <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]]
if (pv >= 600 && pv < 620) broke_proj <- TRUE
if (!broke_proj) { 
crds <- matrix(data=c(9.05, 48.52), ncol=2)
spPoint <- SpatialPoints(coords=crds,
 proj4string=CRS("+proj=longlat +ellps=sphere +no_defs"))
ob_tran_def <- paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs")
tg <- CRS(ob_tran_def)
# proj4string not propagated in GDAL 3.0.0
a <- spTransform(spPoint, tg, use_ob_tran=TRUE)
a
}
#should be (-5.917698, -1.87195)
if (!broke_proj) {
spTransform(a, CRS("+proj=longlat +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
}
if (!broke_proj) {
try(spTransform(a, CRS(paste("+proj=tmerc +lat_0=0 +lon_0=9 +k=1",
"+x_0=3500000 +y_0=0 +ellps=bessel +units=m +no_defs")),
 use_ob_tran=TRUE))
}
if (!broke_proj) {
spTransform(spPoint, CRS(paste("+proj=tmerc +lat_0=0 +lon_0=9 +k=1",
"+x_0=3500000 +y_0=0 +ellps=bessel +units=m +no_defs")))
}
if (!broke_proj) {
spTransform(spTransform(a, CRS("+proj=longlat +ellps=sphere +no_defs"),
 use_ob_tran=TRUE), CRS(paste("+proj=tmerc +lat_0=0 +lon_0=9 +k=1",
"+x_0=3500000 +y_0=0 +ellps=bessel +units=m +no_defs")))
}
crds1 <- matrix(data=c(7, 51, 8, 52, 9, 52, 10, 51, 7, 51), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
crds2 <- matrix(data=c(8, 48, 9, 49, 11, 49, 9, 48, 8, 48), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
crds3 <- matrix(data=c(6, 47, 6, 55, 15, 55, 15, 47, 6, 47), ncol=2,
byrow=TRUE, dimnames=list(NULL, c("lon", "lat")));
spLines <- SpatialLines(list(Lines(list(Line(crds1), Line(crds2),
Line(crds3)), ID="a")));
slot(spLines, "proj4string") <- CRS("+proj=longlat +ellps=sphere +no_defs");
bbox(spLines);
if (!broke_proj) {
spLines_tr <- spTransform(spLines, tg, use_ob_tran=TRUE);
bbox(spLines_tr)
}
if (!broke_proj) {
bbox(spTransform(spLines_tr, CRS("+proj=longlat +ellps=sphere"),
 use_ob_tran=TRUE))
}
if (!broke_proj) {
spPolygons <- SpatialPolygons(list(Polygons(list(Polygon(crds1),
Polygon(crds2), Polygon(crds3)), ID="a")));
slot(spPolygons, "proj4string") <- CRS("+proj=longlat +ellps=sphere +no_defs");
bbox(spPolygons);
}
if (!broke_proj) {
spPolygons_tr <- spTransform(spPolygons, tg, use_ob_tran=TRUE);
bbox(spPolygons_tr)
}
if (!broke_proj) {
bbox(spTransform(spPolygons_tr, CRS("+proj=longlat +ellps=sphere"),
 use_ob_tran=TRUE))
}
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
run <- new_proj_and_gdal()
if (run) {
# Test for UTM == TMERC (<= 4.9.2) or UTM == ETMERC (> 4.9.2)
nhh <- SpatialPointsDataFrame(matrix(c(5.304234, 60.422311), ncol=2),
 proj4string=CRS(SRS_string="OGC:CRS84"), data=data.frame(office="RSB"))
nhh_utm_32N_P4 <- spTransform(nhh, CRS("+init=epsg:3044"))
nhh_tmerc_P4 <- spTransform(nhh, CRS(paste("+proj=tmerc +k=0.9996",
 "+lon_0=9 +x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
nhh_etmerc_P4 <- spTransform(nhh, CRS(paste("+proj=etmerc +k=0.9996",
 "+lon_0=9 +x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
all.equal(coordinates(nhh_utm_32N_P4), coordinates(nhh_tmerc_P4),
 tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(coordinates(nhh_utm_32N_P4), coordinates(nhh_etmerc_P4),
 tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
unis <- SpatialPointsDataFrame(matrix(c(15.653453, 78.222504), ncol=2),
 proj4string=CRS(SRS_string="OGC:CRS84"), data=data.frame(office="UNIS"))
unis_utm_33N_P4 <- spTransform(unis, CRS("+init=epsg:3045"))
unis_tmerc_P4 <- spTransform(unis, CRS(paste("+proj=tmerc +k=0.9996 +lon_0=15",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
unis_etmerc_P4 <- spTransform(unis, CRS(paste("+proj=etmerc +k=0.9996",
 "+lon_0=15 +x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
all.equal(coordinates(unis_utm_33N_P4), coordinates(unis_tmerc_P4),
 tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(coordinates(unis_utm_33N_P4), coordinates(unis_etmerc_P4),
 tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
}
}

\keyword{methods}
\keyword{spatial}
