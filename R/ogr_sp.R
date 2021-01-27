# Copyright 2006-2016 Roger Bivand

readOGR <- function(dsn, layer, verbose=TRUE, p4s=NULL, 
        stringsAsFactors=as.logical(NA),
        drop_unsupported_fields=FALSE,
	pointDropZ=FALSE, dropNULLGeometries=TRUE, useC=TRUE,
        disambiguateFIDs=FALSE, addCommentsToPolygons=TRUE, encoding=NULL,
        use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType=NULL,
        integer64="no.loss", GDAL1_integer64_policy=FALSE,
        morphFromESRI=NULL, dumpSRS=FALSE, enforce_xy=NULL) {
	if (missing(dsn)) stop("missing dsn")
        stopifnot(is.character(dsn))
        stopifnot(length(dsn) == 1L)
# copy sf::st_read.default usage
	if (length(dsn) == 1 && file.exists(dsn))
          dsn <- enc2utf8(normalizePath(dsn))
	if (nchar(dsn) == 0) stop("empty name")
	if (missing(layer)){
          layers <- ogrListLayers(dsn=dsn)
          if (length(layers) == 0L) stop("missing layer")
          if (length(layers) > 0L) layer <- c(layers[1])
          if (length(layers) > 1L)
            warning("First layer ", layer,
              " read; multiple layers present in\n", dsn,
              ", check layers with ogrListLayers()")
        }  else layer <- enc2utf8(layer)
# stop("missing dsn")
#	if (missing(layer)) stop("missing layer")
	if (nchar(layer) == 0) stop("empty name")
        integer64 <- match.arg(integer64,
          c("allow.loss", "warn.loss", "no.loss"))
        
        int64 <- switch(integer64,
          "allow.loss"=1L,
          "warn.loss"=2L, 
          "no.loss"=3L)
        if (GDAL1_integer64_policy) int64 <- 4L
# adding argument for SHAPE_ENCODING environment variable 121124
        stopifnot(is.logical(use_iconv))
        stopifnot(length(use_iconv) == 1)
        if (!is.null(encoding)) {
            stopifnot(is.character(encoding))
            stopifnot(length(encoding) == 1)
        }
        WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
          "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
        if (!is.null(require_geomType)) {
          stopifnot(is.character(require_geomType) &&
            length(require_geomType)==1)
          m_require_geomType <- match(require_geomType, WKB)
          stopifnot(!is.na(m_require_geomType) || m_require_geomType <= 3)
        }
        if(is.na(stringsAsFactors)) {
            stringsAsFactors <-
                if(getRversion() < "4.1.0")
                    default.stringsAsFactors()
                else
                    FALSE
        }
        
	suppressMessages(ogr_info <- ogrInfo(dsn=dsn, layer=layer,
            encoding=encoding, use_iconv=use_iconv,
            swapAxisOrder=swapAxisOrder, require_geomType=require_geomType,
            morphFromESRI=morphFromESRI, dumpSRS=dumpSRS))
        HAS_FEATURES <- TRUE
        if (!ogr_info$have_features) {
            if (dropNULLGeometries) {
                stop("no features found")
            } else {
                warning("no features found; proceeding to atttributes only")
                HAS_FEATURES <- FALSE
            }
        }
        if (is.null(ogr_info$nListFields)) nListFields <- 0
        else nListFields <- ogr_info$nListFields
# 121130 RSB trap no field case (from PostGIS, Mathieu Basille)
        if (ogr_info$nitems > 0) {
          nodata_flag <- FALSE
          if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
            keep <- ogr_info$iteminfo$typeName %in% c("Integer", "Real",
              "String", "Date", "Time", "DateTime", "IntegerList",
              "RealList", "StringList")
          } else {
            keep <- ogr_info$iteminfo$typeName %in% c("Integer", "Real",
              "String", "Date", "Time", "DateTime", "IntegerList",
              "RealList", "StringList", "Integer64", "Integer64List")
          }
          if (nListFields > 0)
              ListFields <- as.integer(ogr_info$iteminfo$maxListCount)
          if (drop_unsupported_fields) {
             iflds <- as.integer((1:ogr_info$nitems)-1)
             iflds <- iflds[keep]
             fldnms <- ogr_info$iteminfo$name[keep]
             if (nListFields > 0) ListFields <- ListFields[keep]
             if (any(!keep)) warning(paste("Fields dropped:", 
                 paste(ogr_info$iteminfo$name[!keep], collapse=" ")))
          } else {
             if (any(!keep)) stop(paste("Unsupported field type:", 
                 paste(ogr_info$iteminfo$typeName[!keep], collapse=" ")))
             iflds <- as.integer((1:ogr_info$nitems)-1)
             fldnms <- ogr_info$iteminfo$name
          }
          int64_found <- grep("Integer64", ogr_info$iteminfo$typeName)
        } else {
          int64_found <- integer(0L)
          nodata_flag <- TRUE
          iflds <- integer(0)
        }
	fids <- ogrFIDs(dsn=dsn, layer=layer)
        if (attr(fids, "i") != attr(fids, "nf")) {
            retain <- 1:attr(fids, "i")
            afids <- 0:(attr(fids, "nf")-1)
            deleted <- afids[!(afids %in% fids[retain])]
            warning(paste("Deleted feature IDs: ", paste(deleted,
                    collapse=", ")))
            fids <- fids[retain]
        } else {
            retain <- NULL
        }
        attributes(fids) <- NULL
# suggestion by Dylan Beaudette 110620
        non_unique_fids <- max(table(fids)) > 1
        if (non_unique_fids) {
            if (disambiguateFIDs) {
                fids <- seq_along(fids) # if present, make new FIDs
            } else {
                stop("FIDs not unique")
            }
        }
	if (verbose) {
		cat("OGR data source with driver:", ogr_info$driver, "\n")
		cat("Source: \"", dsn, '\", layer: \"', layer, "\"", '\n',
			sep='')
		cat("with", length(fids), "features")
                if (!is.null(attr(ogr_info, "require_geomType")))
                    cat(";\nSelected", attr(ogr_info, "require_geomType"),
                      "feature type, with", sum(attr(ogr_info, "keepGeoms")),
                      "rows")
                cat("\n")
		cat("It has", length(iflds), "fields")
                if (nListFields > 0)
                  cat(", of which", nListFields, "list fields")
                cat("\n")
                if (length(int64_found > 0L)) {
                    if (GDAL1_integer64_policy) {
                        cat("Integer64 fields read as doubles: ",
                            paste(fldnms[int64_found], collapse=" "), "\n")
                    } else {
                        if (integer64 == "no.loss") {
                            cat("Integer64 fields read as strings: ",
                                paste(fldnms[int64_found], collapse=" "), "\n")
                        } else {
                            cat("Integer64 fields read as signed 32-bit integers: ",
                                paste(fldnms[int64_found], collapse=" "), "\n")
                        }
                    }
            }

	}

# adding argument for SHAPE_ENCODING environment variable 121124
        if (!use_iconv && !is.null(encoding) && 
            ogr_info$driver == "ESRI Shapefile") {
            oSE <- getCPLConfigOption("SHAPE_ENCODING")
            tull <- setCPLConfigOption("SHAPE_ENCODING", encoding)
        }
	if (nodata_flag) {
            dlist <- list(FID=as.integer(fids))
        } else {
            attr(iflds, "nListFields") <- as.integer(nListFields)
            nflds <- length(iflds)
            if (nListFields > 0) {
                attr(iflds, "ListFields") <- ListFields
                nflds <- nflds + sum(ListFields) - nListFields
                fldnms1 <- NULL
                for (i in seq(along=ListFields)) {
                    if (ListFields[i] == 0) fldnms1 <- c(fldnms1, fldnms[i])
                    else fldnms1 <- c(fldnms1,
                        paste(fldnms[i], 1:ListFields[i], sep=""))
                }
                stopifnot(length(fldnms1) == nflds)
                fldnms <- fldnms1
            }
            attr(iflds, "nflds") <- as.integer(nflds)
            attr(iflds, "int64") <- as.integer(int64)
            dlist <- .Call("ogrDataFrame", as.character(dsn),
                enc2utf8(as.character(layer)), as.integer(fids), iflds, PACKAGE="rgdal")
	    names(dlist) <- make.names(fldnms ,unique=TRUE)

            if (use_iconv && !is.null(encoding)) {
                for (i in seq(along=dlist)) {
                    if (is.character(dlist[[i]])) {
                       dlist[[i]] <- iconv(dlist[[i]], from=encoding)
                    }
                }
            }
        }
        if (!use_iconv && !is.null(encoding) && 
            ogr_info$driver == "ESRI Shapefile") {
            tull <- setCPLConfigOption("SHAPE_ENCODING", oSE)
        }
	data <- data.frame(dlist, row.names=fids,
            stringsAsFactors=stringsAsFactors)
        rm(dlist)
        gc(verbose = FALSE)

        if (!HAS_FEATURES) {
            return(data)
        }

# suggestion by Paul Hiemstra 070817
#        if (is.null(morphFromESRI)) {
#            if (ogr_info$driver == "ESRI Shapefile") morphFromESRI <- TRUE
#            else morphFromESRI <- FALSE
#        }
#        if (ogr_info$driver != "ESRI Shapefile" && morphFromESRI)
#            morphFromESRI <- FALSE
#        stopifnot(is.logical(morphFromESRI))
#        stopifnot(length(morphFromESRI) == 1)
#        stopifnot(is.logical(dumpSRS))
#        stopifnot(length(dumpSRS) == 1)
#	prj <- .Call("ogrP4S", as.character(dsn), enc2utf8(as.character(layer)),		as.logical(morphFromESRI), as.logical(dumpSRS), PACKAGE="rgdal")

#        prj <- OGRSpatialRef(dsn=dsn, layer=layer, morphFromESRI=morphFromESRI,
#          dumpSRS=dumpSRS, driver=ogr_info$driver, enforce_xy=enforce_xy)

        prj <- ogr_info$p4s

	if (!is.null(p4s)) {
          if (!is.na(prj)) {
            warning("p4s= argument given as: ", p4s, "\n and read as: ",
              c(prj), "\n read string overridden by given p4s= argument value")
          }
        } else {
          p4s <- prj
        }

	if (!is.na(p4s) && nchar(p4s) == 0) p4s <- as.character(NA)
#        if (new_proj_and_gdal()) wkt2 <- ogr_info$wkt2
        oCRS <- new("CRS", projargs=p4s)
        if (new_proj_and_gdal()) comment(oCRS) <- ogr_info$wkt2

	geometry <- .Call("R_OGR_CAPI_features", as.character(dsn), 
		enc2utf8(as.character(layer)), comments=addCommentsToPolygons,
                PACKAGE="rgdal")
	if (is.null(retain)) {
	    eType <- geometry[[4]]
	    with_z <- geometry[[6]]
            isNULL <- as.logical(geometry[[7]])
	    gFeatures <- geometry[[5]]
            gComments <- geometry[[8]]
        } else {
	    eType <- geometry[[4]][retain]
	    with_z <- geometry[[6]][retain]
            isNULL <- as.logical(geometry[[7]])[retain]
	    gFeatures <- geometry[[5]][retain]
            gComments <- geometry[[8]][retain]
        }
        rm(geometry);
        gc(verbose = FALSE)
        if (any(isNULL)) {
            eType <- eType[!isNULL]
            with_z <- with_z[!isNULL]
        }   
     
	u_with_z <- unique(sort(with_z))
	if (length(u_with_z) != 1L) stop(
		paste("Multiple # dimensions:", 
			paste((u_with_z + 2), collapse=":")))
	if (u_with_z < 0 || u_with_z > 1) stop(
		paste("Invalid # dimensions:", (u_with_z + 2)))

        eType[eType == 5L] <- 2L
        eType[eType == 6L] <- 3L

	u_eType <- unique(sort(eType))

    t_eType <- table(eType)
    if (is.null(require_geomType)) {
      keepGeoms <- NULL
      if (length(u_eType) > 1L) stop(
        paste("Multiple incompatible geometries:", 
          paste(paste(WKB[as.integer(names(t_eType))], t_eType, sep=": "),
          collapse="; ")))
#  if (length(u_eType) == 2L) {
#    if (u_eType[1] == 2 && u_eType[2] == 5) u_eType = 2
#    else if (u_eType[1] == 3 && u_eType[2] == 6) u_eType = 3
#    else stop(paste("Multiple incompatible geometries:", 
#      paste(paste(WKB[as.integer(names(t_eType))], t_eType, sep=": "),
#        collapse="; ")))
#   }
    } else {
      if (!require_geomType %in% WKB[as.integer(names(t_eType))])
        stop(require_geomType, "not in", WKB[as.integer(names(t_eType))])
      u_eType <- match(require_geomType, WKB)
      keepGeoms <- WKB[eType] == require_geomType
      message("NOTE: keeping only ", sum(keepGeoms), " ", require_geomType,
        " of ", length(keepGeoms), " features\n")
    }

	if (length(gFeatures) != length(fids)) stop("Feature mismatch")

        if (any(isNULL)) {
            if (dropNULLGeometries) {
                warning(paste("Dropping null geometries:", paste(which(isNULL),
                    collapse=", ")))
                gFeatures <- gFeatures[!isNULL]
	        data <- data[!isNULL, , drop=FALSE]
                fids <- fids[!isNULL]
                gComments <- gComments[!isNULL]
            } else {
                warning(paste("Null geometries found:", paste(which(isNULL),
                    collapse=", ")))
                warning("dropNULLGeometries FALSE, returning only data for null-geometry features")
                return(data[isNULL, , drop=FALSE])
            }
        }

        if (!is.null(require_geomType)) {
                gFeatures <- gFeatures[keepGeoms]
	        data <- data[keepGeoms, , drop=FALSE]
                fids <- fids[keepGeoms]
                gComments <- gComments[keepGeoms]
        }

	if (u_eType == 1) { # points
		if (u_with_z == 0 || pointDropZ) {
                    if (swapAxisOrder) {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[2]], x[[1]][[1]])))
                    } else {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[1]], x[[1]][[2]])))
                    }
		} else {
                    if (swapAxisOrder) {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[2]], x[[1]][[1]],
				x[[1]][[3]])))
                    } else {
			coords <- do.call("rbind", lapply(gFeatures, 
				function(x) c(x[[1]][[1]], x[[1]][[2]],
				x[[1]][[3]])))
                    }  
		}
#		data <- data.frame(dlist)
		row.names(data) <- NULL
		res <- SpatialPointsDataFrame(coords=coords, data=data,
			proj4string=oCRS)
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
                                if (swapAxisOrder) {
				  lnlist[[j]] <- Line(cbind(jG[[2]], jG[[1]]))
                                } else {
				  lnlist[[j]] <- Line(cbind(jG[[1]], jG[[2]]))
                                }
			}
			lnList[[i]] <- Lines(lnlist, ID=as.character(fids[i]))
		}
		SL <- SpatialLines(lnList, proj4string=oCRS)
#		data <- data.frame(dlist, row.names=fids)
		res <- SpatialLinesDataFrame(SL, data)
	} else if (u_eType == 3) { # polygons
            if (u_with_z != 0) warning("Z-dimension discarded")
            if (useC) {
#                plList <- .Call("make_polygonslist", gFeatures,
#                    as.list(as.character(fids)), PACKAGE="rgdal")
		n <- length(gFeatures)
		plList <- vector(mode="list", length=n)
		for (i in 1:n) {
			iG <- gFeatures[[i]]
                        if (swapAxisOrder) {
                          iG <- lapply(iG, function(x) {
                            tmp <- x[[1]]; x[[1]] <- x[[2]]; x[[2]] <- tmp
                          })
                        }
                        if (addCommentsToPolygons) {
                            thisPL <- Polygons(.Call("make_Polygonlist",
                                iG, gComments[[i]], PACKAGE="rgdal"),
                                ID=as.character(fids[i]))
                            comment(thisPL) <- paste(gComments[[i]],
                                collapse=" ")
                        } else {
                            thisPL <- Polygons(.Call("make_Polygonlist",
                                iG, NULL, PACKAGE="rgdal"),
                                ID=as.character(fids[i]))
                        }
			plList[[i]] <- thisPL
                    }
            } else {
		n <- length(gFeatures)
		plList <- vector(mode="list", length=n)
		for (i in 1:n) {
			iG <- gFeatures[[i]]
                        if (swapAxisOrder) {
                          iG <- lapply(iG, function(x) {
                            tmp <- x[[1]]; x[[1]] <- x[[2]]; x[[2]] <- tmp
                          })
                        }
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
				t0 <- try(pllist[[j]] <- Polygon(cmat),
                                    silent=TRUE)
                                if (inherits(t0, "try-error")) {
                                     print(cmat)
                                     print(t0)
                                     stop("i: ", i, ", j: ", j,
                                       ", Polygon error exit")
                                }
			}
                        thisPL <- Polygons(pllist, ID=as.character(fids[i]))
                        if (addCommentsToPolygons) {
                            comment(thisPL) <- paste(gComments[[i]],
                                collapse=" ")
                            if (!isTRUE(all.equal(as.logical(gComments[[i]]),
                              sapply(slot(thisPL, "Polygons"), slot, "hole"))))
                              warning("comment/hole mismatch, geometry:", i)
                        }
			plList[[i]] <- thisPL
                    }
		}
                rm(gFeatures)
                gc(verbose = FALSE)
		SP <- SpatialPolygons(plList, proj4string=oCRS)
                rm(plList)
                gc(verbose = FALSE)
#		data <- data.frame(dlist, row.names=fids)
		res <- SpatialPolygonsDataFrame(SP, data, match.ID=FALSE)
	} else stop(paste("Incompatible geometry:", u_eType))

	res
}

showWKT <- function(p4s, file=NULL, morphToESRI=FALSE, enforce_xy=NULL) {

	if (!is.character(p4s)) stop("invalid p4s object")
        stopifnot(length(p4s) == 1)
	if (!is.logical(morphToESRI)) stop("invalid morphToESRI object")
        stopifnot(length(morphToESRI) == 1)
        stopifnot(!is.na(morphToESRI))
        if (!is.null(enforce_xy)) {
            stopifnot(is.logical(enforce_xy))
            stopifnot(length(enforce_xy) == 1L)
            stopifnot(!is.na(enforce_xy))
        } else {
            enforce_xy <- get_enforce_xy()
        }
        attr(morphToESRI, "enforce_xy") <- enforce_xy
	res <- .Call("p4s_to_wkt", as.character(p4s), morphToESRI, 
		PACKAGE="rgdal")
	if (!is.null(file)) cat(res, "\n", sep="", file=file)
	res
}

showP4 <- function(wkt, morphFromESRI=FALSE, enforce_xy=NULL) {

	if (!is.character(wkt)) stop("invalid wkt object")
        stopifnot(length(wkt) == 1)
	if (!is.logical(morphFromESRI)) stop("invalid morphFromESRI object")
        stopifnot(length(morphFromESRI) == 1)
        stopifnot(!is.na(morphFromESRI))
        if (!is.null(enforce_xy)) {
            stopifnot(is.logical(enforce_xy))
            stopifnot(length(enforce_xy) == 1L)
            stopifnot(!is.na(enforce_xy))
        } else {
            enforce_xy <- get_enforce_xy()
        }
        attr(morphFromESRI, "enforce_xy") <- enforce_xy
	res <- .Call("wkt_to_p4s", as.character(wkt),
                morphFromESRI, PACKAGE="rgdal")
	res
}


showEPSG <- function(p4s, enforce_xy=NULL) {

	if (!is.character(p4s)) stop("invalid p4s object")
        stopifnot(length(p4s) == 1L)
        stopifnot(!is.na(p4s))
        if (!is.null(enforce_xy)) {
            stopifnot(is.logical(enforce_xy))
            stopifnot(length(enforce_xy) == 1L)
            stopifnot(!is.na(enforce_xy))
        } else {
            enforce_xy <- get_enforce_xy()
        }
        attr(p4s, "enforce_xy") <- enforce_xy

	res <- .Call("ogrAutoIdentifyEPSG", p4s, PACKAGE="rgdal")
	res
}

get_thin_PROJ6_warnings <- function() {
    get("thin_PROJ6_warnings", envir=.RGDAL_CACHE)
}

set_thin_PROJ6_warnings <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("thin_PROJ6_warnings", value, envir=.RGDAL_CACHE)
}

get_rgdal_show_exportToProj4_warnings <- function() {
  get("rgdal_show_exportToProj4_warnings", envir=.RGDAL_CACHE)
}

set_rgdal_show_exportToProj4_warnings <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("rgdal_show_exportToProj4_warnings", value, envir=.RGDAL_CACHE)
}

get_PROJ6_warnings_count <- function() {
    get("PROJ6_warnings_count", envir=.RGDAL_CACHE)
}

get_enforce_xy <- function() {
    get("enforce_xy", envir=.RGDAL_CACHE)
}

set_enforce_xy <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("enforce_xy", value, envir=.RGDAL_CACHE)
}


get_prefer_proj <- function() {
    get("prefer_proj", envir=.RGDAL_CACHE)
}

set_prefer_proj <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("prefer_proj", value, envir=.RGDAL_CACHE)
}


showSRID <- function(inSRID, format="WKT2", multiline="NO", enforce_xy=NULL, EPSG_to_init=TRUE, prefer_proj=NULL) {
    valid_WKT_formats <- c("SFSQL", "WKT1_SIMPLE", "WKT1", "WKT1_GDAL",
        "WKT1_ESRI", "WKT2_2015", "WKT2_2018", "WKT2") # add WKT2_2019 ??
    valid_formats <- c("PROJ", valid_WKT_formats)
    stopifnot(is.character(inSRID))
    stopifnot(length(inSRID) == 1L)
    stopifnot(nzchar(inSRID))
    stopifnot(is.character(format))
    stopifnot(length(format) == 1L)
    if (!(format %in% valid_formats)) stop("invalid format value")
    out_format <- as.integer(NA)
    if (format %in% valid_WKT_formats) out_format <- 1L
    if (format == "PROJ") out_format <- 2L
    if (!is.na(multiline) && is.logical(multiline)) {
      multiline <- ifelse(multiline, "YES", "NO")
    }
    stopifnot(!is.na(multiline))
    stopifnot(is.character(multiline))
    stopifnot(length(multiline) == 1L)
    if (!(multiline %in% c("YES", "NO"))) stop("invalid multiline value")
    in_format <- as.integer(NA)
    if (substring(inSRID, 1, 1) == " ") stop("string starts with space")
    if (substring(inSRID, 1, 1) == "+") in_format = 1L
    if (substring(inSRID, 1, 3) == "urn") in_format = 2L
    if (substring(inSRID, 1, 2) == "PR") in_format = 3L
    if (substring(inSRID, 1, 2) == "GE") in_format = 3L
    if (substring(inSRID, 1, 2) == "BA") in_format = 3L
    if (substring(inSRID, 1, 2) == "BO") in_format = 3L
    if (substring(inSRID, 1, 1) == "S") in_format = 3L
    if (substring(inSRID, 1, 2) == "CO") in_format = 3L
    if (substring(inSRID, 1, 4) == "EPSG") in_format = 4L
    if (substring(inSRID, 1, 4) == "ESRI") in_format = 5L
    if (substring(inSRID, 1, 3) == "OGC") in_format = 5L
    epsg <- as.integer(NA)
    if (!is.null(prefer_proj)) {
      stopifnot(is.logical(prefer_proj))
      stopifnot(length(prefer_proj) == 1L)
      stopifnot(!is.na(prefer_proj))
    } else {
        prefer_proj <- get_prefer_proj()
    }
    if (in_format == 4L) {
        if (EPSG_to_init && !prefer_proj) {
            in_format = 1L
            inSRID <- paste0("+init=epsg:", substring(inSRID, 6, nchar(inSRID)))
        } else {
            epsg <- as.integer(substring(inSRID, 6, nchar(inSRID)))
        }
    }
    format <- paste0("FORMAT=", format)
    multiline <- paste0("MULTILINE=", multiline)
    if (!is.null(enforce_xy)) {
      stopifnot(is.logical(enforce_xy))
      stopifnot(length(enforce_xy) == 1L)
      stopifnot(!is.na(enforce_xy))
    } else {
        enforce_xy <- get_enforce_xy()
    }

    if (new_proj_and_gdal()) {
        if (!is.na(in_format)) {
            attr(in_format, "enforce_xy") <- enforce_xy
                if (prefer_proj) {
                  if (in_format == 1L && !grepl("\\+type\\=crs", inSRID))
                    inSRID <- paste0(inSRID, " +type=crs")
                  res <- try(.Call("P6_SRID_proj", as.character(inSRID),
                    as.character(format), as.character(multiline), 
                    in_format, as.integer(epsg),
                    as.integer(out_format), PACKAGE="rgdal"), silent=TRUE)
                } else {
                  res <- try(.Call("P6_SRID_show", as.character(inSRID),
                    as.character(format), as.character(multiline), 
                    in_format, as.integer(epsg),
                    as.integer(out_format), PACKAGE="rgdal"), silent=TRUE)
                }
                if (inherits(res, "try-error"))
                    stop(unclass(attr(res, "condition"))$message, "\n", inSRID) 
            no_towgs84 <- ((is.null(attr(res, "towgs84"))) || 
                (all(nchar(attr(res, "towgs84")) == 0)))
            if ((length(grep("towgs84|TOWGS84|Position Vector|Geocentric translations", c(res))) == 0L)
                && !no_towgs84) warning("TOWGS84 discarded")
            if ((!is.null(attr(res, "ellps"))) && 
                (nchar(attr(res, "ellps")) > 0L) && 
                (length(grep("ellps|ELLIPSOID", c(res))) == 0L)) {
                if (length(grep("datum|DATUM", c(res))) == 0L) {
                    msg <- paste0("Discarded ellps ", attr(res, "ellps"),
                        " in Proj4 definition: ", c(res))
                } else {
                    msg <- ""
                }
                if (get_rgdal_show_exportToProj4_warnings()) {
                  if (!get_thin_PROJ6_warnings()) {
                    if (nchar(msg) > 0) warning(msg)
                  } else {
                    if (nchar(msg) > 0 && get("PROJ6_warnings_count",
                        envir=.RGDAL_CACHE) == 0L) {
                        warning(paste0("PROJ/GDAL PROJ string degradation in workflow\n repeated warnings suppressed\n ", msg))
                      }
                      assign("PROJ6_warnings_count",
                            get("PROJ6_warnings_count",
                            envir=.RGDAL_CACHE) + 1L, envir=.RGDAL_CACHE)
                  }
                }
            }
#warning("Discarded ellps ", attr(res, "ellps"),
#                    " in Proj4 definition")
            if ((!is.null(attr(res, "datum"))) 
                && (nchar(attr(res, "datum")) > 0L)
                && (length(grep("datum|DATUM", c(res))) == 0L)) {
                msg <- paste0("Discarded datum ", attr(res, "datum"),
                    " in Proj4 definition")
                if (!no_towgs84 && (length(grep("towgs84", c(res))) > 0L))
                    msg <- paste0(msg, ",\n but +towgs84= values preserved")
                if (get_P6_datum_hard_fail()) stop(msg)
                else {
                  if (get_rgdal_show_exportToProj4_warnings()) {
                    if (!get_thin_PROJ6_warnings()) {
                        warning(msg)
                    } else {
                        if (get("PROJ6_warnings_count",
                            envir=.RGDAL_CACHE) == 0L) {
                            warning(paste0("PROJ/GDAL PROJ string degradation in workflow\n repeated warnings suppressed\n ", msg))
                          }
                          assign("PROJ6_warnings_count",
                                get("PROJ6_warnings_count",
                                envir=.RGDAL_CACHE) + 1L, envir=.RGDAL_CACHE)
                    }
                  }
                }
            }
            res <- c(res)
        } else {
            stop("unknown input format")
        }
    } else {
        if (!is.na(in_format) && in_format == 1L) {
            res <- showWKT(inSRID)
        } else {
            stop("unknown input format")
        }
    }
    res
}
 
