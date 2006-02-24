# Copyright (c) 2003-5 by Barry Rowlingson, Roger Bivand, and Edzer Pebesma

"project" <- function(xy, proj, inv=FALSE) {

    if (!is.numeric(xy)) stop("xy not numeric")
    if (is.matrix(xy)) nc <- dim(xy)[1]
    else if (length(xy) == 2) nc <- 1
    else stop("xy malformed")
    if(!inv) {
      res <- .C("project",
                as.integer(nc),
                as.double(xy[,1]),
                as.double(xy[,2]),
                x=double(nc),
                y=double(nc),
                proj,
                NAOK=TRUE,
                PACKAGE="rgdal")
    } else {
      res <- .C("project_inv",
                as.integer(nc),
                as.double(xy[,1]),
                as.double(xy[,2]),
                x=double(nc),
                y=double(nc),
                proj,
                NAOK=TRUE,
                PACKAGE="rgdal")
    }
    cbind(res$x, res$y)
}
if (as.numeric(version$minor) < 3) {
    "transform.SpatialPoints" <-  function(x, CRSobj, ...) {
	if (is.na(proj4string(x))) 
		stop("No transformation possible from NA reference system")
	if (is.na(CRSargs(CRSobj))) 
		stop("No transformation possible to NA reference system")
	crds <- coordinates(x)
	crds.names <- dimnames(crds)[[2]] # crds is matrix
	if (ncol(crds) != 2) 	
		warning("Only x- and y-coordinates are being transformed")
	n <- nrow(crds)
	res <- .Call("transform", proj4string(x), CRSargs(CRSobj), n,
		as.double(crds[,1]), as.double(crds[,2]),
		PACKAGE="rgdal")
	# make sure coordinate names are set back:
	crds[,1:2] <- cbind(res[[1]], res[[2]])
	dimnames(crds)[[2]] <- crds.names
	x <- SpatialPoints(coords=crds, proj4string=CRS(res[[4]]))
	x
    }
    setMethod("transform", signature("SpatialPoints", "CRS"), transform.SpatialPoints)

} else { 

    "transform.SpatialPoints" <-  function(`_data`, CRSobj, ...) {
	x <- `_data`
	if (is.na(proj4string(x))) 
		stop("No transformation possible from NA reference system")
	if (is.na(CRSargs(CRSobj))) 
		stop("No transformation possible to NA reference system")
	crds <- coordinates(x)
	crds.names <- dimnames(crds)[[2]] # crds is matrix
	if (ncol(crds) != 2) 	
		warning("Only x- and y-coordinates are being transformed")
	n <- nrow(crds)
	res <- .Call("transform", proj4string(x), CRSargs(CRSobj), n,
		as.double(crds[,1]), as.double(crds[,2]),
		PACKAGE="rgdal")
	# make sure coordinate names are set back:
	crds[,1:2] <- cbind(res[[1]], res[[2]])
	dimnames(crds)[[2]] <- crds.names
	x <- SpatialPoints(coords=crds, proj4string=CRS(res[[4]]))
	x
    }

    setMethod("transform", signature("SpatialPoints"), transform.SpatialPoints)
}


if (as.numeric(version$minor) < 3) {
    "transform.SpatialPointsDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialPoints")
	resSP <- transform(xSP, CRSobj)
	# xDF <- as(x, "data.frame")
	xDF <- x@data # little need to add unique row.names here!
	res <- SpatialPointsDataFrame(coords=coordinates(resSP), data=xDF,
		coords.nrs = numeric(0), proj4string = CRS(proj4string(resSP)))
	res
    }
    setMethod("transform", signature("SpatialPointsDataFrame", "CRS"), 
	transform.SpatialPointsDataFrame)


} else { 

    "transform.SpatialPointsDataFrame" <- function(`_data`, CRSobj, ...) {
	x <- `_data`
	xSP <- as(x, "SpatialPoints")
	resSP <- transform(xSP, CRSobj)
	# xDF <- as(x, "data.frame")
	xDF <- x@data # little need to add unique row.names here!
	res <- SpatialPointsDataFrame(coords=coordinates(resSP), data=xDF,
		coords.nrs = numeric(0), proj4string = CRS(proj4string(resSP)))
	res
    }
    setMethod("transform", signature("SpatialPointsDataFrame"), 
	transform.SpatialPointsDataFrame)


}


if (as.numeric(version$minor) < 3) {
    setMethod("transform", signature("SpatialPixelsDataFrame", "CRS"), 
	function(x, CRSobj, ...) 
		transform(as(x, "SpatialPointsDataFrame"), CRSobj, ...))

} else { 

    setMethod("transform", signature("SpatialPixelsDataFrame"), 
	function(`_data`, ...) 
		transform(as(`_data`, "SpatialPointsDataFrame"), ...))
}

if (as.numeric(version$minor) < 3) {
    setMethod("transform", signature("SpatialGridDataFrame", "CRS"), 
	function(x, CRSobj, ...) 
		transform(as(x, "SpatialPixelsDataFrame"), CRSobj, ...))

} else { 

    setMethod("transform", signature("SpatialGridDataFrame"), 
	function(`_data`, ...) 
		transform(as(`_data`, "SpatialPixelsDataFrame"), ...))
}

".transform_Line" <- function(x, to_args, from_args) {
#	crds <- getSlineCoordsSlot(x)
	crds <- x@coords
	n <- nrow(crds)
	res <- .Call("transform", from_args, to_args, n,
		as.double(crds[,1]), as.double(crds[,2]),
		PACKAGE="rgdal")
	crds <- cbind(res[[1]], res[[2]])
	x <- Line(coords=crds)
	x
}

#setMethod("transform", signature("Sline", "CRS"), transform.Sline)

".transform_Lines" <- function(x, to_args, from_args) {
	ID <- getLinesIDSlot(x)
	input <- x@Lines
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Line(input[[i]], 
		to_args=to_args, from_args=from_args)
	x <- Lines(output, ID)
	x
}

#setMethod("transform", signature("Slines", "CRS"), transform.Slines)

if (as.numeric(version$minor) < 3) {
    "transform.SpatialLines" <- function(x, CRSobj, ...) {
	from_args <- proj4string(x)
	if (is.na(from_args)) 
		stop("No transformation possible from NA reference system")
	to_args <- CRSargs(CRSobj)
	if (is.na(to_args)) 
		stop("No transformation possible to NA reference system")
#	input <- getSLlinesSlot(x)
	input <- x@lines
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Lines(input[[i]], 
		to_args=to_args, from_args=from_args)
	res <- SpatialLines(output, proj4string=CRS(to_args))
	res
    }
    setMethod("transform", signature("SpatialLines", "CRS"), transform.SpatialLines)
} else { 

    "transform.SpatialLines" <- function(`_data`, CRSobj, ...) {
	x <- `_data`
	from_args <- proj4string(x)
	if (is.na(from_args)) 
		stop("No transformation possible from NA reference system")
	to_args <- CRSargs(CRSobj)
	if (is.na(to_args)) 
		stop("No transformation possible to NA reference system")
#	input <- getSLlinesSlot(x)
	input <- x@lines
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Lines(input[[i]], 
		to_args=to_args, from_args=from_args)
	res <- SpatialLines(output, proj4string=CRS(to_args))
	res
    }
    setMethod("transform", signature("SpatialLines"), transform.SpatialLines)
}



if (as.numeric(version$minor) < 3) {
    "transform.SpatialLinesDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialLines")
	resSP <- transform(xSP, CRSobj)
	xDF <- as(x, "data.frame")
	res <- SpatialLinesDataFrame(sl=resSP, data=xDF)
	res
    }
    setMethod("transform", signature("SpatialLinesDataFrame", "CRS"), transform.SpatialLinesDataFrame)

} else { 

    "transform.SpatialLinesDataFrame" <- function(`_data`, CRSobj, ...) {
	x <- `_data`
	xSP <- as(x, "SpatialLines")
	resSP <- transform(xSP, CRSobj)
	xDF <- as(x, "data.frame")
	res <- SpatialLinesDataFrame(sl=resSP, data=xDF)
	res
    }
    setMethod("transform", signature("SpatialLinesDataFrame"), transform.SpatialLinesDataFrame)
}



".transform_Polygon" <- function(x, to_args, from_args) {
	crds <- getPolygonCoordsSlot(x)
	n <- nrow(crds)
	res <- .Call("transform", from_args, to_args, n,
		as.double(crds[,1]), as.double(crds[,2]),
		PACKAGE="rgdal")
	crds <- cbind(res[[1]], res[[2]])
	x <- Polygon(coords=crds)
	x
}

#setMethod("transform", signature("Sring", "CRS"), transform.Sring)

".transform_Polygons" <- function(x, to_args, from_args) {
	ID <- getPolygonsIDSlot(x)
	input <- getPolygonsPolygonsSlot(x)
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Polygon(input[[i]], 
		to_args=to_args, from_args=from_args)
	res <- Polygons(output, ID)
	res
}

#setMethod("transform", signature("Srings", "CRS"), transform.Srings)

if (as.numeric(version$minor) < 3) {
    "transform.SpatialPolygons" <- function(x, CRSobj, ...) {
	from_args <- proj4string(x)
	if (is.na(from_args)) 
		stop("No transformation possible from NA reference system")
	to_args <- CRSargs(CRSobj)
	if (is.na(to_args)) 
		stop("No transformation possible to NA reference system")
	input <- getSpPpolygonsSlot(x)
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Polygons(input[[i]], 
		to_args=to_args, from_args=from_args)
	res <- SpatialPolygons(output, pO=getSpPplotOrderSlot(x), 
		proj4string=CRSobj)
	res
    }
    setMethod("transform", signature("SpatialPolygons", "CRS"), transform.SpatialPolygons)

} else { 

    "transform.SpatialPolygons" <- function(`_data`, CRSobj, ...) {
	x <- `_data`
	from_args <- proj4string(x)
	if (is.na(from_args)) 
		stop("No transformation possible from NA reference system")
	to_args <- CRSargs(CRSobj)
	if (is.na(to_args)) 
		stop("No transformation possible to NA reference system")
	input <- getSpPpolygonsSlot(x)
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) output[[i]] <- .transform_Polygons(input[[i]], 
		to_args=to_args, from_args=from_args)
	res <- SpatialPolygons(output, pO=getSpPplotOrderSlot(x), 
		proj4string=CRSobj)
	res
    }
    setMethod("transform", signature("SpatialPolygons"), transform.SpatialPolygons)

}


if (as.numeric(version$minor) < 3) {
    "transform.SpatialPolygonsDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialPolygons")
	resSP <- transform(xSP, CRSobj)
	xDF <- as(x, "data.frame")
	res <- SpatialPolygonsDataFrame(Sr=resSP, data=xDF)
	res
    }
    setMethod("transform", signature("SpatialPolygonsDataFrame", "CRS"), transform.SpatialPolygonsDataFrame)
} else { 

    "transform.SpatialPolygonsDataFrame" <- function(`_data`, CRSobj, ...) {
	x <- `_data`
	xSP <- as(x, "SpatialPolygons")
	resSP <- transform(xSP, CRSobj)
	xDF <- as(x, "data.frame")
	res <- SpatialPolygonsDataFrame(Sr=resSP, data=xDF)
	res
    }
    setMethod("transform", signature("SpatialPolygonsDataFrame"), transform.SpatialPolygonsDataFrame)
}


