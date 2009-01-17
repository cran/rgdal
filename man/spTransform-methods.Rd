\name{spTransform-methods}
\docType{methods}
\title{ Methods for Function spTransform in package "rgdal" }
\alias{spTransform-methods}
\alias{spTransform}
\alias{spTransform,ANY-method}
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

%\item{"Sline", CRSobj = CRS}{returns transformed coordinates of an "Sline" object using the projection arguments in "CRSobj", of class CRS}

%\item{"Slines", CRSobj = CRS}{returns transformed coordinates of an "Slines" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialLines", CRSobj = CRS}{returns transformed coordinates of an "SpatialLines" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialLinesDataFrame", CRSobj = CRS}{returns transformed coordinates of an "SpatialLinesDataFrame" object using the projection arguments in "CRSobj", of class CRS}

%\item{"Sring", CRSobj = CRS}{returns transformed coordinates of an "Sring" object using the projection arguments in "CRSobj", of class CRS}

%\item{"Srings", CRSobj = CRS}{returns transformed coordinates of an "Srings" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialPolygons", CRSobj = CRS}{returns transformed coordinates of an "SpatialPolygons" object using the projection arguments in "CRSobj", of class CRS}

\item{"SpatialPolygonsDataFrame", CRSobj = CRS}{returns transformed coordinates of an "SpatialPolygonsDataFrame" object using the projection arguments in "CRSobj", of class CRS}

}}
\description{Transformation in package "sp" is concerned with transformation between datum(s) and conversion between projections, from one unambiguously specified coordinate reference system to another, using PROJ.4 projection arguments. Note that warnings about different projections may be issued when the PROJ.4 library extends projection arguments; examine the warning to see if the differences are real.}

\note{The projection arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks.}

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
state.merc <- spTransform(states, CRS=CRS("+proj=merc +ellps=GRS80 +units=us-mi"))
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
proj4string(meuse) <- CRS("+proj=stere +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m")
summary(meuse)
meuse.utm <- spTransform(meuse, CRS("+proj=utm +zone=32 +ellps=WGS84"))
summary(meuse.utm)
cbind(coordinates(meuse), coordinates(meuse.utm))
kiritimati_primary_roads <- readOGR(system.file("vectors", package = "rgdal")[1], "kiritimati_primary_roads")
kiritimati_primary_roads_ll <- spTransform(kiritimati_primary_roads, CRS("+proj=longlat +ellps=WGS84"))
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
}

\keyword{methods}
\keyword{spatial}
