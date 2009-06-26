# Copyright 2006-9 Roger Bivand

readOGR <- function(dsn, layer, verbose=TRUE, p4s=NULL,
        drop_unsupported_fields=FALSE, input_field_name_encoding=NULL,
	pointDropZ=FALSE, dropNULLGeometries=TRUE) {
	if (missing(dsn)) stop("missing dsn")
	if (nchar(dsn) == 0) stop("empty name")
	if (missing(layer)) stop("missing layer")
	if (nchar(layer) == 0) stop("empty name")
	ogr_info <- ogrInfo(dsn=dsn, layer=layer,
            input_field_name_encoding=input_field_name_encoding)
        keep <- ogr_info$iteminfo$typeName %in% c("Integer", "Real",
            "String", "Date", "Time", "DateTime")
        if (drop_unsupported_fields) {
             iflds <- as.integer((1:ogr_info$nitems)-1)
             iflds <- iflds[keep]
             fldnms <- ogr_info$iteminfo$name[keep]
             if (any(!keep)) warning(paste("Fields dropped:", 
                 paste(ogr_info$iteminfo$name[!keep], collapse=" ")))
        } else {
             if (any(!keep)) stop(paste("Unsupported field type:", 
                 paste(ogr_info$iteminfo$typeName[!keep], collapse=" ")))
             iflds <- as.integer((1:ogr_info$nitems)-1)
             fldnms <- ogr_info$iteminfo$name
        }
	fids <- ogrFIDs(dsn=dsn, layer=layer)
	if (verbose) {
		cat("OGR data source with driver:", ogr_info$driver, "\n")
		cat("Source: \"", dsn, '\", layer: \"', layer, "\"", '\n',
			sep='')
		cat("with ", ogr_info$nrows," rows and ",
			ogr_info$nitems, " columns\n")
	}
# suggestion by Paul Hiemstra 070817
	if (is.null(p4s)) 
	    p4s <- .Call("ogrP4S", as.character(dsn), as.character(layer), 
		PACKAGE="rgdal")
	if (!is.na(p4s) && nchar(p4s) == 0) p4s <- as.character(NA)
	dlist <- .Call("ogrDataFrame", as.character(dsn), as.character(layer), 
		as.integer(fids), iflds, PACKAGE="rgdal")
	names(dlist) <- make.names(fldnms ,unique=TRUE)
	geometry <- .Call("R_OGR_CAPI_features", as.character(dsn), 
		as.character(layer), PACKAGE="rgdal")
	eType <- geometry[[4]]
	with_z <- geometry[[6]]
        isNULL <- as.logical(geometry[[7]])
        if (any(isNULL)) {
            eType <- eType[!isNULL]
            with_z <- with_z[!isNULL]
        }        
	u_eType <- unique(sort(eType))
	u_with_z <- unique(sort(with_z))
	if (length(u_with_z) != 1) stop(
		paste("Multiple # dimensions:", 
			paste((u_with_z + 2), collapse=":")))
	if (u_with_z < 0 || u_with_z > 1) stop(
		paste("Invalid # dimensions:", (u_with_z + 2)))
	if (length(u_eType) > 2) stop(
		paste("Multiple incompatible geometries:", 
			paste(u_eType, collapse=":")))
	if (length(u_eType) == 2) {
		if (u_eType[1] == 2 && u_eType[2] == 5) u_eType = 2
		else if (u_eType[1] == 3 && u_eType[2] == 6) u_eType = 3
		else stop(paste("Multiple incompatible geometries:", 
			paste(u_eType, collapse=":")))
	}
	WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
	    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
        if (verbose) cat("Feature type:", paste(WKB[u_eType], collapse=", "),
	    "with", u_with_z+2, "dimensions\n")
	if (u_eType == 5) u_eType <- 2
	if (u_eType == 6) u_eType <- 3

	gFeatures <- geometry[[5]]
	data <- data.frame(dlist, row.names=fids)
	if (length(gFeatures) != ogr_info$nrows) stop("Feature mismatch")

        if (any(isNULL)) {
            if (dropNULLGeometries) {
                warning(paste("Dropping null geometries:", paste(which(isNULL),
                    collapse=", ")))
                gFeatures <- gFeatures[!isNULL]
	        data <- data[!isNULL, , drop=FALSE]
                fids <- fids[!isNULL]
            } else {
                warning(paste("Null geometries found:", paste(which(isNULL),
                    collapse=", ")))
                warning("dropNULLGeometries FALSE, returning only data for null-geometry features")
                return(data[isNULL, , drop=FALSE])
            }
        }

	if (u_eType == 1) { # points
		if (u_with_z == 0 || pointDropZ) {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[1]], x[[1]][[2]])))
		} else {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[1]], x[[1]][[2]],
				x[[1]][[3]])))
		}
#		data <- data.frame(dlist)
		row.names(data) <- NULL
		res <- SpatialPointsDataFrame(coords=coords, data=data,
			proj4string=CRS(p4s))
	} else if (u_eType == 2) { # lines
		if (u_with_z != 0) warning("Z-dimension discarded")
		n <- length(gFeatures)
		lnList <- vector(mode="list", length=n)
		for (i in 1:n) {
			iG <- gFeatures[[i]]
			m <- length(iG)
			lnlist <- vector(mode="list", length=m)
			for (j in 1:m) {
				jG <- iG[[j]]
				lnlist[[j]] <- Line(cbind(jG[[1]], jG[[2]]))
			}
			lnList[[i]] <- Lines(lnlist, ID=as.character(fids[i]))
		}
		SL <- SpatialLines(lnList, proj4string=CRS(p4s))
#		data <- data.frame(dlist, row.names=fids)
		res <- SpatialLinesDataFrame(SL, data)
	} else if (u_eType == 3) { # polygons
		if (u_with_z != 0) warning("Z-dimension discarded")
		n <- length(gFeatures)
		plList <- vector(mode="list", length=n)
		for (i in 1:n) {
			iG <- gFeatures[[i]]
			m <- length(iG)
			pllist <- vector(mode="list", length=m)
			for (j in 1:m) {
				jG <- iG[[j]]
				cmat <- cbind(jG[[1]], jG[[2]])
				if (!identical(cmat[1,], cmat[nrow(cmat),])) {
				  cmat <- rbind(cmat, cmat[1,])
                                  warning(paste("Ring closed in Polygons",
				    i, "Polygon", j))
				}
				pllist[[j]] <- Polygon(cmat)
			}
			plList[[i]] <- Polygons(pllist, ID=as.character(fids[i]))
		}
		SP <- SpatialPolygons(plList, proj4string=CRS(p4s))
#		data <- data.frame(dlist, row.names=fids)
		res <- SpatialPolygonsDataFrame(SP, data)
	} else stop(paste("Incompatible geometry:", u_eType))

	res
}

showWKT <- function(p4s, file=NULL, morphToESRI=TRUE) {

	if (!is.character(p4s)) stop("invalid p4s object")
	if (!is.logical(morphToESRI)) stop("invalid morphToESRI object")
	res <- .Call("p4s_to_wkt", as.character(p4s), as.integer(morphToESRI), 
		PACKAGE="rgdal")
	if (!is.null(file)) cat(res, "\n", sep="", file=file)
	res
}

 
