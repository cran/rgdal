# Copyright (c) 2003-19 by Barry Rowlingson and Roger Bivand

#.valid.CRSobj <- function(object) {
#	if (exists("is.R") && is.function(is.R) && is.R()) {
#		projargNA <- is.na(object@projargs)
#	} else {
#		projargNA <- is.na(as.numeric(object@projargs))
#	}
#	if (!projargNA) {
#		res <- .Call("checkCRSArgs", object@projargs, 
#				PACKAGE="rgdal")
#	} else res <- list(TRUE, as.character(NA))
#	if (!res[[1]]) {
#	    	return(res[[2]])
#	} else {
#		return(res[[1]])
#	}
#
#}
#
#setValidity("CRS", .valid.CRSobj)


"CRSargs" <- function(object) {
	if (!is(object, "CRS")) stop("not a CRS object")

	if (!is.na(object@projargs)) {
            if (new_proj_and_gdal()) 
                res <- (checkCRSArgs_ng(object@projargs)[[2]])
            else res <- (checkCRSArgs(object@projargs)[[2]])
            res <- paste(unique(unlist(strsplit(res, " "))), 
		collapse=" ")
            return(sub("^\\s+", "", res))
	} else return(as.character(NA))
}

checkCRSArgs <- function(uprojargs=NA_character_) {
  if (packageVersion("rgdal") >= "1.5.1" && length(grep("ob_tran", uprojargs)) > 0L) {
    return(list(TRUE, uprojargs))
  }
# pkgdown work-around
  if (is.na(get("has_proj_def.dat", envir=.RGDAL_CACHE))) {
    assign("has_proj_def.dat", .Call("PROJ4_proj_def_dat_Installed",
      PACKAGE="rgdal"), envir=.RGDAL_CACHE)
  }
# RSB Web Mercator bug 180313 (for 5.0.0)
  drop_nadgrids <- FALSE
  if (strsplit(strsplit(getPROJ4VersionInfo(), ",")[[1]][1], " ")[[1]][2]
    == "5.0.0") {
    if (length(grep("+init=epsg:3857", uprojargs)) > 0L) drop_nadgrids <- TRUE
  }
# RSB 2015-05-21
# fix for omission of proj_defs.dat in PROJ.4 4.9.1
  if ((!get("has_proj_def.dat", envir=.RGDAL_CACHE)) && (!PROJis6ormore())) {
      message("NOTE: rgdal::checkCRSArgs: no proj_defs.dat in PROJ.4 shared files")
      uprojargs <- proj_def_bug_fix(uprojargs)
  }
  res <- .Call("RGDAL_checkCRSArgs", uprojargs, PACKAGE="rgdal")
  if (drop_nadgrids) {
    uuproj <- strsplit(res[[2]], " ")[[1]]
    hit_nad <- grep("nadgrids", uuproj)
    hit_init <- grep("init", uuproj)
    if (length(c(hit_nad, hit_init) > 0))
      res[[2]] <- paste(uuproj[-c(hit_nad, hit_init)], collapse=" ")
  }
  res[[2]] <- sub("^\\s+", "", res[[2]])
# fix for pj_get_def() +no_uoff/+no_off bug
  no_uoff <- length(grep("+no_uoff", uprojargs, fixed=TRUE) > 0)
  no_off <- length(grep("+no_off", uprojargs, fixed=TRUE) > 0)
  if (no_uoff) {
      if( length(grep("+no_uoff", res[[2]], fixed=TRUE)) == 0) 
          res[[2]] <- sub("+no_defs", "+no_uoff +no_defs", res[[2]], fixed=TRUE)
  }
  if (no_off) {
      if (length(grep("+no_off", res[[2]], fixed=TRUE)) == 0)
          res[[2]] <- sub("+no_defs", "+no_off +no_defs", res[[2]], fixed=TRUE)
  }
  res
}

checkCRSArgs_ng <- function(uprojargs=NA_character_, SRS_string=NULL,
  get_source_if_boundcrs=TRUE) {
  no_SRS <- is.null(SRS_string)
  no_PROJ <- is.na(uprojargs)
  prefer_proj <- get_prefer_proj()
  res <- vector(mode="list", length=3L)
  res[[1]] <- FALSE
  res[[2]] <- NA_character_
  res[[3]] <- NA_character_
  if (!no_SRS) {
    stopifnot(is.character(SRS_string))
    stopifnot(length(SRS_string) == 1L)
  }
  if (!no_PROJ) {
    stopifnot(is.character(uprojargs))
    stopifnot(length(uprojargs) == 1L)
    if (grepl("\\+init\\=", uprojargs)) prefer_proj <- FALSE
  }
  if (!no_SRS) {
    uprojargs1 <- try(showSRID(SRS_string, format="PROJ", multiline="NO",
        prefer_proj=prefer_proj), silent=TRUE)
    if (inherits(uprojargs1, "try-error")) {
      res[[1]] <- FALSE
      res[[2]] <- NA_character_
    } else {
      res[[1]] <- TRUE
      res[[2]] <- gsub(" \\+type\\=crs", "", uprojargs1)
    }
    wkt2 <- try(showSRID(SRS_string, format="WKT2", multiline="YES",
        prefer_proj=prefer_proj), silent=TRUE)
    if (!inherits(wkt2, "try-error")) {
      if (get_enforce_xy()) wkt2 <- try(.Call("proj_vis_order", wkt2,
        PACKAGE="rgdal"), silent=TRUE)
      if (!inherits(wkt2, "try-error")) {
        res[[3]] <- wkt2
      }
    }
  } else if (!no_PROJ) {
    uprojargs <- sub("^\\s+", "", uprojargs)
    if (prefer_proj && !grepl("\\+type\\=crs", uprojargs))
      uprojargs <- paste0(uprojargs, " +type=crs") 
    uprojargs1 <- try(showSRID(uprojargs, format="PROJ", multiline="NO",
        prefer_proj=prefer_proj), silent=TRUE)
    if (inherits(uprojargs1, "try-error")) {
      res[[1]] <- FALSE
      res[[2]] <- NA_character_
    } else {
      res[[1]] <- TRUE
      res[[2]] <- gsub(" \\+type\\=crs", "", uprojargs1)
    }
    wkt2 <- try(showSRID(uprojargs, format="WKT2", multiline="YES",
        prefer_proj=prefer_proj), silent=TRUE)
    if (!inherits(wkt2, "try-error")) {
      if (get_source_if_boundcrs) {
        if (length(grep("^BOUNDCRS", wkt2)) > 0L) {
          wkt2a <- try(.Call("get_source_crs", wkt2, PACKAGE="rgdal"),
            silent=TRUE)
          if (!inherits(wkt2a, "try-error")) wkt2 <- wkt2a
        }
      }
      if (get_enforce_xy()) {
        wkt2a <- try(.Call("proj_vis_order", wkt2, PACKAGE="rgdal"),
          silent=TRUE)
          if (!inherits(wkt2a, "try-error")) {
            wkt2 <- wkt2a
          }
      }
      res[[3]] <- wkt2
    }
  }
  res
}

compare_CRS <- function(CRS1, CRS2) {
    stopifnot(new_proj_and_gdal())
    stopifnot(inherits(CRS1, "CRS"))
    stopifnot(inherits(CRS2, "CRS"))
    type1 <- FALSE
    if (is.null(comment(CRS1))) {
        from_args <- paste0(slot(CRS1, "projargs"), " +type=crs")
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
       type1 <- TRUE
       from_args <- comment(CRS1)
    }
    type2 <- FALSE
    if (is.null(comment(CRS2))) {
        warning("NULL target CRS comment, falling back to PROJ string")
        to_args <- paste0(slot(CRS2, "projargs"), " +type=crs")
        if (is.na(to_args)) 
	    stop("No transformation possible to NA target CRS")
        if (length(grep("\\+init\\=", to_args)) > 0) {
            warning("+init dropped in PROJ string")
            strres <- unlist(strsplit(to_args, " "))
            to_args <- paste(strres[-grep("\\+init\\=", strres)], collapse=" ")
        }
    } else {
        type2 <- TRUE
        to_args <- comment(CRS2)
    }
    res0 <- .Call("CRS_compare", as.character(from_args), as.character(to_args),
        as.logical(type1), as.logical(type2), PACKAGE="rgdal")
    res <- as.logical(res0)
    names(res) <- c("strict", "equivalent", "equivalent_except_axis_order")
    res
}

proj_def_bug_fix <- function(uprojargs) {
    if (length(grep("no_defs", uprojargs)) == 0L && 
# corrected 20150904
        length(grep("init", uprojargs)) == 0L) {
        if (length(grep("ellps", uprojargs)) == 0L && 
# corrected 20150905
            length(grep("datum", uprojargs)) == 0L) {
            tags <- sapply(strsplit(strsplit("+proj=longlat +no_defs",
                "\\+")[[1]], "="), "[", 1)
# based on proj/src/pj_init.c lines 191-197
            if (!any(c("datum", "ellps", "a", "b", "rf", "f") %in% tags)) {
                uprojargs <- paste(uprojargs, "+ellps=WGS84", sep=" ")
           }
         }
    }
    uprojargs
}
