\name{transform-methods}
\docType{methods}
\title{ Methods for Function transform in package "spproj" }
\alias{transform-methods}
\alias{transform,ANY-method}
\alias{transform,SpatialPoints-method}
\alias{transform,SpatialPointsDataFrame-method}
\alias{transform,SpatialPixelsDataFrame-method}
\alias{transform,SpatialGridDataFrame-method}
%\alias{transform,Sline-method}
%\alias{transform,Slines-method}
\alias{transform,SpatialLines-method}
%\alias{transform,Sring-method}
%\alias{transform,Srings-method}
\alias{transform,SpatialPolygons-method}
\alias{transform.SpatialPoints}
\alias{transform.SpatialPointsDataFrame}
%\alias{transform.Sline}
%\alias{transform.Slines}
\alias{transform.SpatialLines}
\alias{transform.SpatialLinesDataFrame}
\alias{transform,SpatialLinesDataFrame-method}
%\alias{transform.Sring}
%\alias{transform.Srings}
\alias{transform.SpatialPolygons}
\alias{transform.SpatialPolygonsDataFrame}
\alias{transform,SpatialPolygonsDataFrame-method}
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
state.ll83 <- transform(states, CRS("+proj=longlat +ellps=GRS80"))
summary(state.ll83)
state.merc <- transform(states, CRS=CRS("+proj=merc +ellps=GRS80"))
summary(state.merc)
state.merc <- transform(states, CRS=CRS("+proj=merc +ellps=GRS80 +units=us-mi"))
summary(state.merc)
\dontrun{
states <- data.frame(state.x77, state.center)
states <- states[states$x > -121,]
coordinates(states) <- c("x", "y")
proj4string(states) <- CRS("+init=epsg:4267")
summary(states)
state.ll83 <- transform(states, CRS("+init=epsg:4269"))
summary(state.ll83)
state.kansasSlcc <- transform(states, CRS=CRS("+init=epsg:26978"))
summary(state.kansasSlcc)
}
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+proj=stere +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m")
summary(meuse)
meuse.utm <- transform(meuse, CRS("+proj=utm +zone=32 +ellps=WGS84"))
summary(meuse.utm)
cbind(coordinates(meuse), coordinates(meuse.utm))
kiritimati_primary_roads <- readOGR(system.file("vectors", package = "rgdal")[1], "kiritimati_primary_roads")
kiritimati_primary_roads_ll <- transform(kiritimati_primary_roads, CRS("+proj=longlat +ellps=WGS84"))
opar <- par(mfrow=c(1,2))
plot(kiritimati_primary_roads, axes=TRUE)
plot(kiritimati_primary_roads_ll, axes=TRUE, las=1)
par(opar)
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1], "scot_BNG")
scot_LL <- transform(scot_BNG, CRS("+proj=longlat +datum=WGS84"))
opar <- par(mfrow=c(1,2))
plot(scot_LL, axes=TRUE)
plot(scot_BNG, axes=TRUE, las=1)
toproj <- CRS(proj4string(scot_BNG))
easts <- pretty(bbox(scot_LL)[1,], min.n=4)
easts <- easts[easts > bbox(scot_LL)[1,1] & easts < bbox(scot_LL)[1,2]]
eastlist <- vector(mode="list", length=length(easts))
for (i in 1:length(easts)) eastlist[[i]] <- Line(cbind(rep(easts[i], 100), seq(bbox(scot_LL)[2,1], bbox(scot_LL)[2,2], length.out=100)))
plot(transform(SpatialLines(list(Lines(eastlist)), CRS("+proj=longlat +datum=WGS84")), toproj), add=TRUE, col="grey")
norths <- pretty(bbox(scot_LL)[2,], min.n=4)
norths <- norths[norths > bbox(scot_LL)[2,1] & norths < bbox(scot_LL)[2,2]]
northlist <- vector(mode="list", length=length(norths))
for (i in 1:length(norths)) northlist[[i]] <- Line(cbind(seq(bbox(scot_LL)[1,1], bbox(scot_LL)[1,2], length.out=100), rep(norths[i], 100)))
plot(transform(SpatialLines(list(Lines(northlist)), CRS("+proj=longlat +datum=WGS84")), toproj), add=TRUE, col="grey")
par(opar)
}

\keyword{methods}
\keyword{spatial}
