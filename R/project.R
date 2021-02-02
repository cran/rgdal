# Copyright (c) 2003-20 by Barry Rowlingson, Roger Bivand, and Edzer Pebesma

getPROJ4VersionInfo <- function() {
    res0 <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")
    res <- paste(res0[[1]], ", [PJ_VERSION: ", res0[[2]], "]", sep="")
    attr(res, "short") <- res0[[2]]
    res
}

PROJis6ormore <- function() {
    verno <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]]
    verno >= 600
}

GDALis3ormore <- function() {
    substring(getGDALVersionInfo(), 6, 6) >= "3"
}

new_proj_and_gdal <- function() {
    PROJis6ormore() && GDALis3ormore()
}

is_proj_CDN_enabled <- function() {
    .Call("proj_network_enabled", PACKAGE="rgdal")
}

enable_proj_CDN <- function() {
    .Call("enable_proj_network", PACKAGE="rgdal")
    paste0("Using: ", proj_CDN_user_writable_dir(), sep="")
}

disable_proj_CDN <- function() {
    invisible(.Call("disable_proj_network", PACKAGE="rgdal"))
}

proj_CDN_user_writable_dir <- function() {
    .Call("get_proj_user_writable_dir", PACKAGE="rgdal")
}


GDAL_OSR_PROJ <- function() {
    res <- .Call("R_GDAL_OSR_PROJ", PACKAGE="rgdal")
    if (is.null(res)) return(res)
    res1 <- (res[1]*100)+(res[2]*10)+res[3]
    verno <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]]
    if (res1 != verno) warning("GDAL built with PROJ ", res1,
        " not running ", verno)
    res1
}

getPROJ4libPath <- function() {
    res <- Sys.getenv("PROJ_LIB")
    if (PROJis6ormore()) {
        attr(res, "search_path") <- .Call("get_proj_search_path",
            PACKAGE="rgdal")
    }
    res
}

get_proj_search_paths <- function() {
    if (PROJis6ormore()) {
        res <- .Call("get_proj_search_path", PACKAGE="rgdal")
        res <- strsplit(res, .Platform$path.sep)[[1]]
    } else {
        res <- NULL
    }
    res
}

set_proj_search_paths <- function(paths) {
    if (PROJis6ormore()) {
        stopifnot(!missing(paths))
        stopifnot(is.character(paths))
        stopifnot(length(paths) > 0)
        n <- length(paths)
        for (i in 1:n) stopifnot(dir.exists(paths[i]))
        res <- .Call("set_proj_paths", paths, PACKAGE="rgdal")
    } else {
        res <- NULL
    }
    res
}

projNAD <- function() {
    .Call("PROJ4NADsInstalled", PACKAGE="rgdal")
}

get_P6_datum_hard_fail <- function() {
    get("P6_datum_hard_fail", envir=.RGDAL_CACHE)
}

set_P6_datum_hard_fail <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("P6_datum_hard_fail", value, envir=.RGDAL_CACHE)
}

get_transform_wkt_comment <- function() {
    get("transform_wkt_comment", envir=.RGDAL_CACHE)
}

set_transform_wkt_comment <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1L)
    stopifnot(!is.na(value))
    assign("transform_wkt_comment", value, envir=.RGDAL_CACHE)
}

get_last_coordOp <- function() {
    get(".last_coordOp", envir=.RGDAL_CACHE)
}

get_aoi <- function(obj, xy, inv, proj) {
    if (!new_proj_and_gdal()) return(NULL)
    if (missing(obj)) { # used in project
        bb <- cbind(range(xy[,1], na.rm=TRUE), range(xy[,2], na.rm=TRUE))
        if (inv) {
            o <- try(project(bb, proj, inv=inv, use_aoi=FALSE), silent=TRUE)
            if (inherits(o, "try-error")) return(NULL)
        } else {
            o <- bb
        }
    } else { # used in spTransform
        if (is.projected(obj)) { 
            tg <- wkt(obj)
            if (is.null(tg)) {
                tg <- proj4string(obj)
                if (length(grep("+init", tg)) > 0L) {
                    tg <- wkt(CRS(tg))
                }
            }
            o <- try(project(t(bbox(obj))[,1:2], tg, inv=TRUE, use_aoi=FALSE),
                silent=TRUE)
            if (inherits(o, "try-error")) return(NULL)
        } else {
            o <- t(bbox(obj))[,1:2]
        }
    }
    if (any(!is.finite(c(o)))) return(NULL)
    aoi <- o + c(-0.1, +0.1) # stretch envelope
    aoi[,1] <- sapply(aoi[,1], function(x) { if (x > 180) 180 - x else if (x < -180) 360 + x else x})
    aoi[,2] <- sapply(aoi[,2], function(y) { if (y > 90) 90 else if (y < -90) -90 else y}) # constrain to -180/180, -90/90
    c(t(aoi))
}


OSRIsProjected <- function(obj) {
    stopifnot(inherits(obj, "CRS"))
    wkt2 <- wkt(obj)
    if (!is.null(wkt2) && new_proj_and_gdal())
        return(.Call("OSR_is_projected", wkt2, PACKAGE="rgdal"))
    p4str <- slot(obj, "projargs")
    if (is.na(p4str) || !nzchar(p4str)) 
	return(as.logical(NA))    
    .Call("OSR_is_projected", p4str, PACKAGE="rgdal")
}


"project" <- function(xy, proj, inv=FALSE, use_ob_tran=FALSE, legacy=TRUE, allowNAs_if_not_legacy=FALSE, coordOp=NULL, verbose=FALSE, use_aoi=TRUE) {

#    if (new_proj_and_gdal()) 
#        warning("project() will not be adapted for PROJ 6 and is deprecated")

    if (!is.numeric(xy)) stop("xy not numeric")
    if (is.matrix(xy)) nc <- dim(xy)[1]
    else if (length(xy) == 2L) nc <- 1
    else stop("xy malformed")
# 111216 RSB
    stopifnot(is.character(proj))
    stopifnot(length(proj) == 1)
    stopifnot(is.logical(inv))
# 120816 RSB
    stopifnot(is.logical(use_ob_tran))
    if (use_ob_tran) {
        gp <- grep("proj=ob_tran", proj)
        if (length(gp) == 0) {
            use_ob_tran <- FALSE
            warning("project: use_ob_tran set FALSE")
# 120820 RSB
        } else inv <- !inv
    }
    if (.Platform$OS.type == "windows" && .Platform$r_arch == "i386" &&
        !new_proj_and_gdal()) legacy <- FALSE
    if (!legacy) {
      if (allowNAs_if_not_legacy) {
       nas <- is.na(xy[,1]) | is.na(xy[,2])
      }
      if (!inv) {
        attr(nc, "ob_tran") <- as.integer(use_ob_tran)
        if (attr(nc, "ob_tran") == 0L) {
          res <- .Call("transform", "+proj=longlat", proj, nc,
	    as.double(xy[,1]), as.double(xy[,2]), NULL, PACKAGE="rgdal")
        } else {
          res <- .Call("transform", proj, "+proj=longlat", nc,
	    as.double(xy[,1]), as.double(xy[,2]), NULL, PACKAGE="rgdal")
        }
	if (!allowNAs_if_not_legacy) {
           if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
	      k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]]))
	      cat("non finite transformation detected:\n")
	      print(cbind(xy, res[[1]], res[[2]])[k,])
	      stop(paste("failure in points", paste(k, collapse=":")))
	   }
        } else {
           if (any(nas)) {
             res[[1]][nas] <- as.double(NA)
             res[[2]][nas] <- as.double(NA)
           }
        }
     } else {
        attr(nc, "ob_tran") <- -as.integer(use_ob_tran)
        if (attr(nc, "ob_tran") == 0L) {
          res <- .Call("transform", proj, "+proj=longlat", nc,
	    as.double(xy[,1]), as.double(xy[,2]), NULL, PACKAGE="rgdal")
        } else {
          res <- .Call("transform", "+proj=longlat", proj, nc,
	    as.double(xy[,1]), as.double(xy[,2]), NULL, PACKAGE="rgdal")
        }
	if (!allowNAs_if_not_legacy) {
	   if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
	     k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]]))
	     cat("non finite transformation detected:\n")
	     print(cbind(xy, res[[1]], res[[2]])[k,])
	     stop(paste("failure in points", paste(k, collapse=":")))
	   }
        } else {
           if (any(nas)) {
             res[[1]][nas] <- as.double(NA)
             res[[2]][nas] <- as.double(NA)   
           }        
        }
     }
    } else {
        if (new_proj_and_gdal()) {
            if (is.null(coordOp)) {
                if (substring(proj, 1, 1) == "+") {
                    if (length(grep("\\+init", proj)) > 0L)
                        proj <- comment(CRS(proj))
                    if (length(grep(" \\+type=crs", proj)) == 0L)
                        proj <- paste0(proj, " +type=crs")
                }
                aoi <- NULL
                if (!use_ob_tran && use_aoi) {
                    aoi <- get_aoi(xy=xy, inv=inv, proj=proj)
                    if (!is.null(aoi)) {
                        stopifnot(length(aoi) == 4)
                        aoi <- as.numeric(aoi)
                    }
                }
                coordOp <- .Call("project_ng_coordOp", proj,
                    as.logical(inv), aoi, as.logical(use_ob_tran), PACKAGE="rgdal")
                s2t <- strsplit(coordOp, " ")[[1]]
                if (length(s2t) == 0L) stop("malformed pipeline found")
                coordOp <- paste("+", s2t, sep="", collapse=" ")
            }
            if (verbose) cat(strwrap(coordOp), sep="\n")
            res <- .Call("project_ng",
                as.integer(nc),
                as.double(xy[,1]),
                as.double(xy[,2]),
                NULL,
                coordOp,
                PACKAGE="rgdal")
        } else {
            if(!inv) {
# 160404 RSB convert to .Call()
            res <- .Call("RGDAL_project",
                as.integer(nc),
                as.double(xy[,1]),
                as.double(xy[,2]),
                proj,
                as.logical(use_ob_tran),
                PACKAGE="rgdal")
            } else {
                res <- .Call("project_inv",
                as.integer(nc),
                as.double(xy[,1]),
                as.double(xy[,2]),
                proj,
                as.logical(use_ob_tran),
                PACKAGE="rgdal")
            }
        }
    }
    out <- cbind(res[[1]], res[[2]])
    if (!is.null(colnames(xy))) colnames(out) <- colnames(xy)
    out
}

## Fri, 28 Apr 2017 10:40:18
## From: Martin Aleksandrov Ivanov <Martin.Ivanov@geogr.uni-giessen.de>
# ob_tran when isTRUE(is.projected())

#if (packageVersion("sp") < "1.4.2") {

#if (!isGeneric("is.projected"))
#	setGeneric("is.projected", function(obj)
#		standardGeneric("is.projected"))
#
#is_projected_crs <- function (obj) 
#{
#    p4str <- CRSargs(obj)
#    if (is.na(p4str) || !nzchar(p4str)) 
#        return(as.logical(NA))
#    else {
#        res <- grep("longlat", p4str, fixed = TRUE)
#        if (length(res) == 0) 
#            return(TRUE)
#        else return(FALSE)
#    }
#}

#setMethod("is.projected", signature("CRS"), is_projected_crs)

#}

if (!isGeneric("spTransform"))
	setGeneric("spTransform", function(x, CRSobj, ...)
		standardGeneric("spTransform"))

"spTransform.SpatialPoints" <-  function(x, CRSobj, ...) {
        if (new_proj_and_gdal()) {
          if (get_transform_wkt_comment()) {
            if (is.null(comment(slot(x, "proj4string")))) {
                from_args <- paste0(slot(slot(x, "proj4string"),
                  "projargs"), " +type=crs")
                warning("NULL source CRS comment, falling back to PROJ string")
	        if (is.na(from_args)) 
		    stop("No transformation possible from NA source CRS")
                if (length(grep("\\+init\\=", from_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(from_args, " "))
                    from_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                from_args <- comment(slot(x, "proj4string"))
            }
            if (is.null(comment(CRSobj))) {
                warning("NULL target CRS comment, falling back to PROJ string")
	        to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
	        if (is.na(to_args)) 
		    stop("No transformation possible to NA target CRS")
                if (length(grep("\\+init\\=", to_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(to_args, " "))
                    to_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                to_args <- comment(CRSobj)
            }
          } else {
            from_args <- paste0(slot(slot(x, "proj4string"), "projargs"),
              " +type=crs")
            to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
          }
        } else {
	    from_args <- proj4string(x)
	    if (is.na(from_args)) 
		stop("No transformation possible from NA from reference system")
	    to_args <- slot(CRSobj, "projargs")
	    if (is.na(to_args)) 
		stop("No transformation possible to NA to reference system")
        }
	dots = list(...)
        if (!is.null(dots$use_ob_tran)) {
          stopifnot(is.logical(dots$use_ob_tran))
          if (dots$use_ob_tran) {
            gpf <- grep("proj=ob_tran", slot(CRSobj, "projargs"))
            gpi <- grep("proj=ob_tran", proj4string(x))
            if (length(gpf) == 0 && length(gpi) == 0) {
              use_ob_tran <- 0L
              warning("project: use_ob_tran set FALSE")
            } else {
              if (length(gpf) > 0) use_ob_tran <- -1L
              else use_ob_tran <- 1L
            }
            if (use_ob_tran != 0L) {
              if (is.projected(x) || length(grep("longlat", slot(CRSobj, "projargs"))) == 0)
                stop("Use ob_tran to or from unprojected objects only")
            }
          } else {
            use_ob_tran <- 0L
          }
        } else {
          use_ob_tran <- 0L
        }
        if (!is.null(dots$enforce_xy)) {
          stopifnot(is.logical(dots$enforce_xy))
          stopifnot(length(dots$enforce_xy) == 1L)
          stopifnot(!is.na(dots$enforce_xy))
          enforce_xy <- dots$enforce_xy
        } else {
            enforce_xy <- get_enforce_xy()
        }
        coordOp <- NULL
        if (!is.null(dots$coordOp)) {
            coordOp <- dots$coordOp
        }
        
        use_aoi <- TRUE
        if (!is.null(dots$use_aoi)) {
            use_aoi <- dots$use_aoi
            if (use_ob_tran != 0L) use_aoi <- FALSE
            if (!is.null(coordOp)) use_aoi <- FALSE
            if (!new_proj_and_gdal()) use_aoi <- FALSE
        }
        
        aoi <- NULL
        if (use_aoi && new_proj_and_gdal()) {
            aoi <- get_aoi(x)
            if (!is.null(aoi)) {
                stopifnot(length(aoi) == 4)
                aoi <- as.numeric(aoi)
            }
        }

	crds <- coordinates(x)
	crds.names <- dimnames(crds)[[2]] # crds is matrix
	n <- nrow(crds)
        attr(n, "ob_tran") <- use_ob_tran
        if (ncol(crds) == 2) {
            if (new_proj_and_gdal()) {
                if (use_ob_tran == 0L) {
                    attr(n, "enforce_xy") <- enforce_xy
                    res <- .Call("transform_ng", from_args, to_args, coordOp, n,
		        as.double(crds[,1]), as.double(crds[,2]), NULL, aoi,
		        PACKAGE="rgdal")
                    out_coordOp <- res[[5]]
                } else {
                    if (use_ob_tran == -1L) {
                        inv <- TRUE
                        proj <- paste0(slot(CRSobj, "projargs"), " +type=crs")
                    }
                    if (use_ob_tran == 1L) {
                        inv <- FALSE
                        proj <- paste0(slot(slot(x, "proj4string"),
                            "projargs"), " +type=crs")
                    }
                    if (use_ob_tran == 0L) use_ob_tran1 <- FALSE
                    else use_ob_tran1 <- TRUE
                    if (is.null(coordOp)) {
                        out_coordOp <- .Call("project_ng_coordOp", proj,
                            as.logical(inv), NULL, as.logical(use_ob_tran1),
                            PACKAGE="rgdal")
                        s2t <- strsplit(out_coordOp, " ")[[1]]
                        if (length(s2t) == 0L) stop("malformed pipeline found")
                        out_coordOp <- paste("+", s2t, sep="", collapse=" ")
                    }
                    res <- .Call("project_ng", as.integer(n), 
                        as.double(crds[,1]), as.double(crds[,2]), NULL,
                        out_coordOp, PACKAGE="rgdal")
                }
            } else {
	        res <- .Call("transform", from_args, to_args, n,
                    as.double(crds[,1]), as.double(crds[,2]), NULL,
                    PACKAGE="rgdal")
            }
	    if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
		k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]])[k,])
		stop(paste("failure in points", paste(k, collapse=":")))
	    }
	    crds[,1:2] <- cbind(res[[1]], res[[2]])
        } else {
            if (new_proj_and_gdal()) {
                if (use_ob_tran == 0L) {
                    attr(n, "enforce_xy") <- enforce_xy
                    res <- .Call("transform_ng", from_args, to_args, coordOp, n,
		        as.double(crds[,1]), as.double(crds[,2]),
                        as.double(crds[,3]), aoi, PACKAGE="rgdal")
                    out_coordOp <- res[[6]]
                } else {
                    if (use_ob_tran == -1L) {
                        inv <- TRUE
                        proj <- paste0(slot(CRSobj, "projargs"), " +type=crs")
                    }
                    if (use_ob_tran == 1L) {
                        inv <- FALSE
                        proj <- paste0(slot(slot(x, "proj4string"),
                            "projargs"), " +type=crs")
                    }
                    if (use_ob_tran == 0L) use_ob_tran1 <- FALSE
                    else use_ob_tran1 <- TRUE
                    if (is.null(coordOp)) {
                        out_coordOp <- .Call("project_ng_coordOp", proj,
                            as.logical(inv), NULL, as.logical(use_ob_tran1),
                            PACKAGE="rgdal")
                        s2t <- strsplit(out_coordOp, " ")[[1]]
                        if (length(s2t) == 0L) stop("malformed pipeline found")
                        out_coordOp <- paste("+", s2t, sep="", collapse=" ")
                    }
                    res <- .Call("project_ng", as.integer(n), 
                        as.double(crds[,1]), as.double(crds[,2]),
                        as.double(crds[,3]), out_coordOp, PACKAGE="rgdal")
                }
            } else {
	    res <- .Call("transform", from_args, to_args, n,
		as.double(crds[,1]), as.double(crds[,2]), as.double(crds[,3]),
                PACKAGE="rgdal")
            }
	    if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))
                || any(!is.finite(res[[3]]))) {
		k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]])
                    | !is.finite(res[[3]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]], res[[3]])[k,])
		stop(paste("failure in points", paste(k, collapse=":")))
	    }
	    crds[,1:3] <- cbind(res[[1]], res[[2]], res[[3]])
        }
	# make sure coordinate names are set back:
        if (new_proj_and_gdal()) {
            if (substring(out_coordOp, 1, 1) != "+") {
                out_coordOp <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
            assign(".last_coordOp", out_coordOp, envir=.RGDAL_CACHE)
        }
	dimnames(crds)[[2]] <- crds.names
	x <- SpatialPoints(coords=crds, proj4string=CRSobj)
	x
}
setMethod("spTransform", signature("SpatialPoints", "CRS"), spTransform.SpatialPoints)



"spTransform.SpatialPointsDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialPoints")
	resSP <- spTransform(xSP, CRSobj, ...)
	# xDF <- as(x, "data.frame")
	xDF <- x@data # little need to add unique row.names here!
        if (packageVersion("sp") > "1.4.1") {
            crs <- rebuild_CRS(slot(resSP, "proj4string"))
        } else {
            rebuild_CRS <- NULL
            crs <- CRS(proj4string(resSP))
        }
        res <- SpatialPointsDataFrame(coords=coordinates(resSP), data=xDF,
		coords.nrs = numeric(0), proj4string = crs)
	res
}
setMethod("spTransform", signature("SpatialPointsDataFrame", "CRS"), 
	spTransform.SpatialPointsDataFrame)




setMethod("spTransform", signature("SpatialPixelsDataFrame", "CRS"), 
	function(x, CRSobj, ...) {
                warning("Grid warping not available, coercing to points")
		spTransform(as(x, "SpatialPointsDataFrame"), CRSobj, ...)})

setMethod("spTransform", signature("SpatialGridDataFrame", "CRS"), 
	function(x, CRSobj, ...) {
                warning("Grid warping not available, coercing to points")
		spTransform(as(x, "SpatialPixelsDataFrame"), CRSobj, ...)})


".spTransform_Line" <- function(x, to_args, from_args, ii, jj,
                use_ob_tran, coordOp, enforce_xy, aoi) {
	crds <- slot(x, "coords")
	n <- nrow(crds)
        attr(n, "ob_tran") <- use_ob_tran
        if (new_proj_and_gdal()) {
            if (use_ob_tran == 0L) {
                attr(n, "enforce_xy") <- enforce_xy
                res <- .Call("transform_ng", from_args, to_args, coordOp, n,
		    as.double(crds[,1]), as.double(crds[,2]), NULL, aoi,
		    PACKAGE="rgdal")
                out_coordOp <- res[[5]]
            } else {
                if (use_ob_tran == -1L) {
                    inv <- TRUE
                    if (is.null(attr(to_args, "p4s"))) proj <- to_args
                    else proj <- paste0(attr(to_args, "p4s"), " +type=crs")
                }
                if (use_ob_tran == 1L) {
                    inv <- FALSE
                    if (is.null(attr(from_args, "p4s"))) proj <- from_args
                    else proj <- paste0(attr(from_args, "p4s"), " +type=crs")
                }
                if (use_ob_tran == 0L) use_ob_tran1 <- FALSE
                else use_ob_tran1 <- TRUE
                if (is.null(coordOp)) {
                    coordOp <- .Call("project_ng_coordOp", proj,
                        as.logical(inv), NULL, as.logical(use_ob_tran1),
                        PACKAGE="rgdal")
                    s2t <- strsplit(coordOp, " ")[[1]]
                    if (length(s2t) == 0L) stop("malformed pipeline found")
                    coordOp <- paste("+", s2t, sep="", collapse=" ")
                }
                res <- .Call("project_ng", as.integer(n), 
                    as.double(crds[,1]), as.double(crds[,2]), NULL,
                    coordOp, PACKAGE="rgdal")
                out_coordOp <- coordOp
            }
        } else {
	    res <- .Call("transform", from_args, to_args, n,
		as.double(crds[,1]), as.double(crds[,2]), NULL,
		PACKAGE="rgdal")
        }
	if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
		k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]])[k,])
		stop(paste("failure in Lines", ii, "Line", jj, 
			"points", paste(k, collapse=":")))
	}
	crds <- cbind(res[[1]], res[[2]])
	x <- Line(coords=crds)
        if (new_proj_and_gdal()) {
            if (substring(out_coordOp, 1, 1) == "+") {
                attr(x, "coordOp") <- out_coordOp
            } else {
                attr(x, "coordOp") <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
        }
	x
}

#setMethod("spTransform", signature("Sline", "CRS"), spTransform.Sline)

".spTransform_Lines" <- function(x, to_args, from_args, ii,
                use_ob_tran, coordOp, enforce_xy, aoi) {
	ID <- slot(x, "ID")
	input <- slot(x, "Lines")
	n <- length(input)
	output <- vector(mode="list", length=n)
        out_coordOp <- NULL
	for (i in 1:n) {
            output[[i]] <- .spTransform_Line(input[[i]], 
		to_args=to_args, from_args=from_args, ii=ii, jj=i,
                use_ob_tran=use_ob_tran, coordOp=coordOp,
                enforce_xy=enforce_xy, aoi=aoi)
            if (is.null(coordOp) && new_proj_and_gdal() && i == 1) {
                coordOp <- attr(output[[i]], "coordOp")
#                coordOp <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
            attr(output[[i]], "coordOp") <- NULL
        }
	x <- Lines(output, ID)
        if (new_proj_and_gdal()) attr(x, "coordOp") <- out_coordOp
	x
}

#setMethod("spTransform", signature("Slines", "CRS"), spTransform.Slines)

"spTransform.SpatialLines" <- function(x, CRSobj, ...) {
        if (new_proj_and_gdal()) {
          if (get_transform_wkt_comment()) {
            if (is.null(comment(slot(x, "proj4string")))) {
                from_args <- paste0(slot(slot(x, "proj4string"),
                  "projargs"), " +type=crs")
                warning("NULL source CRS comment, falling back to PROJ string")
	        if (is.na(from_args)) 
		    stop("No transformation possible from NA source CRS")
                if (length(grep("\\+init\\=", from_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(from_args, " "))
                    from_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                from_args <- comment(slot(x, "proj4string"))
                attr(from_args, "p4s") <- slot(slot(x, "proj4string"),
                    "projargs")
            }
            if (is.null(comment(CRSobj))) {
                warning("NULL target CRS comment, falling back to PROJ string")
	        to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
	        if (is.na(to_args)) 
		    stop("No transformation possible to NA target CRS")
                if (length(grep("\\+init\\=", to_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(to_args, " "))
                    to_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                to_args <- comment(CRSobj)
                attr(to_args, "p4s") <- slot(CRSobj, "projargs")
            }
          } else {
            from_args <- paste0(slot(slot(x, "proj4string"), "projargs"),
              " +type=crs")
            to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
          }
        } else {
	    from_args <- proj4string(x)
	    if (is.na(from_args)) 
		stop("No transformation possible from NA from reference system")
	    to_args <- slot(CRSobj, "projargs")
	    if (is.na(to_args)) 
		stop("No transformation possible to NA to reference system")
        }
	dots = list(...)
        if (!is.null(dots$use_ob_tran)) {
          stopifnot(is.logical(dots$use_ob_tran))
          if (dots$use_ob_tran) {
            gpf <- grep("proj=ob_tran", slot(CRSobj, "projargs"))
            gpi <- grep("proj=ob_tran", proj4string(x))
            if (length(gpf) == 0 && length(gpi) == 0) {
              use_ob_tran <- 0L
              warning("project: use_ob_tran set FALSE")
            } else {
              if (length(gpf) > 0) use_ob_tran <- -1L
              else use_ob_tran <- 1L
            }
            if (use_ob_tran != 0L) {
              if (is.projected(x) || length(grep("longlat", slot(CRSobj, "projargs"))) == 0)
                stop("Use ob_tran to or from unprojected objects only")
            }
          } else {
            use_ob_tran <- 0L
          }
        } else {
          use_ob_tran <- 0L
        }
        if (!is.null(dots$enforce_xy)) {
          stopifnot(is.logical(dots$enforce_xy))
          stopifnot(length(dots$enforce_xy) == 1L)
          stopifnot(!is.na(dots$enforce_xy))
          enforce_xy <- dots$enforce_xy
        } else {
            enforce_xy <- get_enforce_xy()
        }
        coordOp <- NULL
        if (!is.null(dots$coordOp)) {
            coordOp <- dots$coordOp
        }
        use_aoi <- TRUE
        if (!is.null(dots$use_aoi)) {
            use_aoi <- dots$use_aoi
            if (use_ob_tran != 0L) use_aoi <- FALSE
            if (!is.null(coordOp)) use_aoi <- FALSE
            if (!new_proj_and_gdal()) use_aoi <- FALSE
        }
        
        aoi <- NULL
        if (use_aoi && new_proj_and_gdal()) {
            aoi <- get_aoi(x)
            if (!is.null(aoi)) {
                stopifnot(length(aoi) == 4)
                aoi <- as.numeric(aoi)
            }
        }

	input <- slot(x, "lines")
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) {
            output[[i]] <- .spTransform_Lines(input[[i]], 
		to_args=to_args, from_args=from_args, ii=i,
                use_ob_tran=use_ob_tran, coordOp=coordOp,
                enforce_xy=enforce_xy, aoi=aoi)
            if (is.null(coordOp) && new_proj_and_gdal() && i == 1) {
                coordOp <- attr(output[[i]], "coordOp")
#                coordOp <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
            attr(output[[i]], "coordOp") <- NULL
        }
        if (new_proj_and_gdal()) assign(".last_coordOp", coordOp,
            envir=.RGDAL_CACHE)
	res <- SpatialLines(output, proj4string=CRSobj)
	res
}
setMethod("spTransform", signature("SpatialLines", "CRS"), spTransform.SpatialLines)
"spTransform.SpatialLinesDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialLines")
	resSP <- spTransform(xSP, CRSobj, ...)
	xDF <- as(x, "data.frame")
	res <- SpatialLinesDataFrame(sl=resSP, data=xDF, match.ID = FALSE)
	res
}
setMethod("spTransform", signature("SpatialLinesDataFrame", "CRS"), spTransform.SpatialLinesDataFrame)




".spTransform_Polygon" <- function(x, to_args, from_args, ii, jj,
                use_ob_tran, coordOp, enforce_xy, aoi) {
	crds <- slot(x, "coords")
	n <- nrow(crds)
        attr(n, "ob_tran") <- use_ob_tran
        if (new_proj_and_gdal()) {
            if (use_ob_tran == 0L) {
                attr(n, "enforce_xy") <- enforce_xy
                res <- .Call("transform_ng", from_args, to_args, coordOp, n,
		    as.double(crds[,1]), as.double(crds[,2]), NULL, aoi,
		    PACKAGE="rgdal")
                out_coordOp <- res[[5]]
            } else {
                if (use_ob_tran == -1L) {
                    inv <- TRUE
                    if (is.null(attr(to_args, "p4s"))) proj <- to_args
                    else proj <- paste0(attr(to_args, "p4s"), " +type=crs")
                }
                if (use_ob_tran == 1L) {
                    inv <- FALSE
                    if (is.null(attr(from_args, "p4s"))) proj <- from_args
                    else proj <- paste0(attr(from_args, "p4s"), " +type=crs")
                }
                if (use_ob_tran == 0L) use_ob_tran1 <- FALSE
                else use_ob_tran1 <- TRUE
                if (is.null(coordOp)) {
                    coordOp <- .Call("project_ng_coordOp", proj,
                        as.logical(inv), NULL, as.logical(use_ob_tran1),
                        PACKAGE="rgdal")
                    s2t <- strsplit(coordOp, " ")[[1]]
                    if (length(s2t) == 0L) stop("malformed pipeline found")
                    coordOp <- paste("+", s2t, sep="", collapse=" ")
                }
                res <- .Call("project_ng", as.integer(n), 
                    as.double(crds[,1]), as.double(crds[,2]), NULL,
                    coordOp, PACKAGE="rgdal")
                out_coordOp <- coordOp
            }
        } else {
	    res <- .Call("transform", from_args, to_args, n,
		as.double(crds[,1]), as.double(crds[,2]), NULL,
		PACKAGE="rgdal")
        }
	if (any(!is.finite(res[[1]])) || any(!is.finite(res[[2]]))) {
		k <- which(!is.finite(res[[1]]) | !is.finite(res[[2]]))
		cat("non finite transformation detected:\n")
		print(cbind(crds, res[[1]], res[[2]])[k,])
		stop(paste("failure in Polygons", ii, "Polygon", jj, 
			"points", paste(k, collapse=":")))
	}
	crds <- cbind(res[[1]], res[[2]])
	x <- Polygon(coords=crds)
        if (new_proj_and_gdal()) {
            if (substring(out_coordOp, 1, 1) == "+") {
                attr(x, "coordOp") <- out_coordOp
            } else {
                attr(x, "coordOp") <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
        }
	x
}


".spTransform_Polygons" <- function(x, to_args, from_args, ii,
                use_ob_tran, coordOp, enforce_xy, aoi) {
	ID <- slot(x, "ID")
	input <- slot(x, "Polygons")
	n <- length(input)
	output <- vector(mode="list", length=n)
        out_coordOp <- NULL
	for (i in 1:n) {
            output[[i]] <- .spTransform_Polygon(input[[i]], 
		to_args=to_args, from_args=from_args, ii=ii, jj=i,
                use_ob_tran=use_ob_tran, coordOp=coordOp,
                enforce_xy=enforce_xy, aoi=aoi)
            if (is.null(coordOp) && new_proj_and_gdal() && i == 1) {
                coordOp <- attr(output[[i]], "coordOp")
#                coordOp <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
            attr(output[[i]], "coordOp") <- NULL
        }
	res <- Polygons(output, ID)
        if (!is.null(comment(x))) comment(res) <- comment(x)
        if (new_proj_and_gdal()) attr(res, "coordOp") <- coordOp
	res
}


"spTransform.SpatialPolygons" <- function(x, CRSobj, ...) {
        if (new_proj_and_gdal()) {
          if (get_transform_wkt_comment()) {
            if (is.null(comment(slot(x, "proj4string")))) {
                from_args <- paste0(slot(slot(x, "proj4string"), "projargs"),
                  " +type=crs")
                warning("NULL source CRS comment, falling back to PROJ string")
	        if (is.na(from_args)) 
		    stop("No transformation possible from NA source CRS")
                if (length(grep("\\+init\\=", from_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(from_args, " "))
                    from_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                from_args <- comment(slot(x, "proj4string"))
                attr(from_args, "p4s") <- slot(slot(x, "proj4string"),
                    "projargs")
            }
            if (is.null(comment(CRSobj))) {
                warning("NULL target CRS comment, falling back to PROJ string")
	        to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
	        if (is.na(to_args)) 
		    stop("No transformation possible to NA target CRS")
                if (length(grep("\\+init\\=", to_args)) > 0) {
                    warning("+init dropped in PROJ string")
                    strres <- unlist(strsplit(to_args, " "))
                    to_args <- paste(strres[-grep("\\+init\\=", strres)],
                        collapse=" ")
                }
            } else {
                to_args <- comment(CRSobj)
                attr(to_args, "p4s") <- slot(CRSobj, "projargs")
            }
          } else {
            from_args <- paste0(slot(slot(x, "proj4string"), "projargs"),
              " +type=crs")
            to_args <- paste0(slot(CRSobj, "projargs"), " +type=crs")
          }
        } else {
	    from_args <- proj4string(x)
	    if (is.na(from_args)) 
		stop("No transformation possible from NA from reference system")
	    to_args <- slot(CRSobj, "projargs")
	    if (is.na(to_args)) 
		stop("No transformation possible to NA to reference system")
        }
	dots = list(...)
        if (!is.null(dots$use_ob_tran)) {
          stopifnot(is.logical(dots$use_ob_tran))
          if (dots$use_ob_tran) {
            gpf <- grep("proj=ob_tran", slot(CRSobj, "projargs"))
            gpi <- grep("proj=ob_tran", proj4string(x))
            if (length(gpf) == 0 && length(gpi) == 0) {
              use_ob_tran <- 0L
              warning("project: use_ob_tran set FALSE")
            } else {
              if (length(gpf) > 0) use_ob_tran <- -1L
              else use_ob_tran <- 1L
            }
            if (use_ob_tran != 0L) {
              if (is.projected(x) || length(grep("longlat", slot(CRSobj, "projargs"))) == 0)
                stop("Use ob_tran to or from unprojected objects only")
            }
          } else {
            use_ob_tran <- 0L
          }
        } else {
          use_ob_tran <- 0L
        }
        if (!is.null(dots$enforce_xy)) {
          stopifnot(is.logical(dots$enforce_xy))
          stopifnot(length(dots$enforce_xy) == 1L)
          stopifnot(!is.na(dots$enforce_xy))
          enforce_xy <- dots$enforce_xy
        } else {
            enforce_xy <- get_enforce_xy()
        }
        coordOp <- NULL
        if (!is.null(dots$coordOp)) {
            coordOp <- dots$coordOp
        }
        use_aoi <- TRUE
        if (!is.null(dots$use_aoi)) {
            use_aoi <- dots$use_aoi
            if (use_ob_tran != 0L) use_aoi <- FALSE
            if (!is.null(coordOp)) use_aoi <- FALSE
            if (!new_proj_and_gdal()) use_aoi <- FALSE
        }
        
        aoi <- NULL
        if (use_aoi && new_proj_and_gdal()) {
            aoi <- get_aoi(x)
            if (!is.null(aoi)) {
                stopifnot(length(aoi) == 4)
                aoi <- as.numeric(aoi)
            }
        }

	input <- slot(x, "polygons")
	n <- length(input)
	output <- vector(mode="list", length=n)
	for (i in 1:n) {
            output[[i]] <- .spTransform_Polygons(input[[i]], 
		to_args=to_args, from_args=from_args, ii=i,
                use_ob_tran=use_ob_tran, coordOp=coordOp,
                enforce_xy=enforce_xy, aoi=aoi)
            if (is.null(coordOp) && new_proj_and_gdal() && i == 1) {
                coordOp <- attr(output[[i]], "coordOp")
#                coordOp <-  gsub(" ", " +", paste0("+", out_coordOp))
            }
            attr(output[[i]], "coordOp") <- NULL
        }
        if (new_proj_and_gdal()) assign(".last_coordOp", coordOp, 
            envir=.RGDAL_CACHE)
	res <- SpatialPolygons(output, pO=slot(x, "plotOrder"), 
		proj4string=CRSobj)
	res
}
setMethod("spTransform", signature("SpatialPolygons", "CRS"), spTransform.SpatialPolygons)

"spTransform.SpatialPolygonsDataFrame" <- function(x, CRSobj, ...) {
	xSP <- as(x, "SpatialPolygons")
	resSP <- spTransform(xSP, CRSobj, ...)
	xDF <- as(x, "data.frame")
	res <- SpatialPolygonsDataFrame(Sr=resSP, data=xDF, match.ID = FALSE)
	res
}
setMethod("spTransform", signature("SpatialPolygonsDataFrame", "CRS"), spTransform.SpatialPolygonsDataFrame)

projInfo <- function(type="proj") {
    opts <- c("proj", "ellps", "datum", "units")
    if (!(type %in% opts)) stop("unknown type")
    t <- as.integer(match(type[1], opts) - 1)
    if (is.na(t)) stop("unknown type")
    res <- .Call("RGDAL_projInfo", t, PACKAGE="rgdal")
    if (type == "proj") res$description <- sapply(strsplit(as.character(
        res$description), "\n"), function(x) x[1])
    if (type == "datum" && is.null(res)) warning("datum list not available for PROJ > 5")
    res <- data.frame(res)
    res
}
