

# thin wrappers 121001 Robert J. Hijmans

.gd_SetNoDataValue <- function(object, NAflag) {
	.Call("RGDAL_SetNoDataValue", object, as.double(NAflag), PACKAGE="rgdal")
}


.gd_SetGeoTransform <- function(object, geotrans) {
   .Call("RGDAL_SetGeoTransform", object, geotrans, PACKAGE="rgdal")
}


.gd_SetProject <- function(object, proj4string) {
# NOT UPDATED FOR PROJ >= 6
if (new_proj_and_gdal()) warning("NOT UPDATED FOR PROJ >= 6")
    .Call("RGDAL_SetProject", object, proj4string, PACKAGE="rgdal")
}

#RH: not tested yet.
.gd_SetProjectWkt <- function(object, crs, enforce_xy=NULL) {
    if (new_proj_and_gdal()) {
        iCRS <- slot(crs, "projargs")
        wkt2 <- comment(crs)
        if (!is.null(wkt2)) {
            if (!is.null(enforce_xy)) {
                stopifnot(is.logical(enforce_xy))
                stopifnot(length(enforce_xy) == 1L)
                stopifnot(!is.na(enforce_xy))
            } else {
                enforce_xy <- get_enforce_xy()
            }
            .Call("RGDAL_SetProject_WKT2", object, wkt2, enforce_xy, PACKAGE = "rgdal")
		} else {
			warning("NO WKT AVAILABLE FOR PROJ >= 6")
			.Call("RGDAL_SetProject", object, proj4string, PACKAGE="rgdal")
		}
	} else {
		.Call("RGDAL_SetProject", object, proj4string, PACKAGE="rgdal")	
	}
}


.gd_SetStatistics <- function(object, statistics) {
	.Call("RGDAL_SetStatistics", object, as.double(statistics), PACKAGE="rgdal")
}

.gd_SetRasterColorTable <- function(object, icT) {
    if (!is.matrix(icT)) {
        stopifnot(is.character(icT))
        icT <- t(col2rgb(icT))
    }
    stopifnot(is.matrix(icT))
    stopifnot(storage.mode(icT) == "integer")
    ricT <- nrow(icT)
    cicT <- ncol(icT)
    stopifnot(cicT == 3 || cicT == 4)
    .Call("RGDAL_SetRasterColorTable", object, icT, ricT, cicT,
        PACKAGE = "rgdal")
}

.gd_SetCategoryNames <- function(object, icN) {
    stopifnot(is.character(icN))
    .Call("RGDAL_SetCategoryNames", object, icN, PACKAGE = "rgdal")
}


GDALcall <- function(object, option, ...) {
	if (option == 'SetNoDataValue') {
		.gd_SetNoDataValue(object, ...)
	} else if (option == 'SetGeoTransform') {
		.gd_SetGeoTransform(object, ...)
	} else if (option == 'SetProjectWkt') {
		.gd_SetProjectWkt(object, ...)
	} else if (option == 'SetProject') {
		.gd_SetProject(object, ...)
	} else if (option == 'SetStatistics') {
		.gd_SetStatistics(object, ...)
	} else if (option == 'SetRasterColorTable') {
		.gd_SetRasterColorTable(object, ...)
	} else if (option == 'SetCategoryNames') {
		.gd_SetCategoryNames(object, ...)
	} else {
		stop('invalid option')
	}
}




.gd_transform <- function(projfrom, projto, n, x, y, z=NULL) {
# pkgdown work-around
# NOT UPDATED FOR PROJ >= 6
if (new_proj_and_gdal()) warning("NOT UPDATED FOR PROJ >= 6")
  if (is.na(get("has_proj_def.dat", envir=.RGDAL_CACHE))) {
    assign("has_proj_def.dat", .Call("PROJ4_proj_def_dat_Installed",
      PACKAGE="rgdal"), envir=.RGDAL_CACHE)
  }
  if ((!get("has_proj_def.dat", envir=.RGDAL_CACHE)) && (!PROJis6ormore())) {
      projfrom <- proj_def_bug_fix(projfrom)
      projto <- proj_def_bug_fix(projto)
  }
  if (is.null(z)) .Call("transform", projfrom, projto, n, x, y, NULL, PACKAGE="rgdal")
  else .Call("transform", projfrom, projto, n, x, y, z, PACKAGE="rgdal")
}

# exported version
rawTransform <- function(projfrom, projto, n, x, y, z=NULL, wkt=FALSE) {
	if (wkt) { 
		# the caller determines that projfrom and projto are wkt 
		# and that new_proj_and_gdal() returns TRUE
		# to avoid multiple warnings when the function is called repetitively
		return( .Call("transform_ng", projfrom, projto, NULL,
			n, x, y, z, NULL, PACKAGE="rgdal") )
	}
		
# pkgdown work-around
          if (new_proj_and_gdal()) {
              if (get_rgdal_show_exportToProj4_warnings()) {
                 if (!get_thin_PROJ6_warnings()) {
                    warning("Using PROJ not WKT2 strings")
                 } else {
                    if (get("PROJ6_warnings_count", envir=.RGDAL_CACHE) == 0L) {
                        warning(paste0("PROJ/GDAL PROJ string degradation in workflow\n repeated warnings suppressed\n ", " Using PROJ not WKT2 strings"))
                    }
                    assign("PROJ6_warnings_count",
                        get("PROJ6_warnings_count", envir=.RGDAL_CACHE) + 1L,
                        envir=.RGDAL_CACHE)
              }
          }

          if (length(grep("+init", projfrom)) > 0)
              projfrom <- slot(CRS(projfrom),  "projargs")
          if (length(grep("+init", projto)) > 0)
              projto <- slot(CRS(projto),  "projargs")
          if (is.null(z)) res <- .Call("transform_ng", # redundant if/else?
              paste0(projfrom, " +type=crs"), paste0(projto, " +type=crs"),
              NULL, n, x, y, NULL, NULL, PACKAGE="rgdal")
          else res <- .Call("transform_ng", paste0(projfrom, " +type=crs"),
              paste0(projto, " +type=crs"), NULL, n, x, y, z, NULL, PACKAGE="rgdal")
          return(res)
        }
        if (is.na(get("has_proj_def.dat", envir=.RGDAL_CACHE))) {
          assign("has_proj_def.dat", .Call("PROJ4_proj_def_dat_Installed",
          PACKAGE="rgdal"), envir=.RGDAL_CACHE)
        }
        if (!get("has_proj_def.dat", envir=.RGDAL_CACHE)) {
            projfrom <- proj_def_bug_fix(projfrom)
            projto <- proj_def_bug_fix(projto)
        }
	if (is.null(z)) .Call("transform", projfrom, projto, n, x, y, NULL, PACKAGE="rgdal")
	else .Call("transform", projfrom, projto, n, x, y, z, PACKAGE="rgdal")
}

