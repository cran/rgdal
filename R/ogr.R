# Copyright 2003 (c) Barry Rowlingson
# Modified 2006-12 Roger Bivand
###
###
###  Routines for ogr layer data source 
###
###

#
ogrInfo <- function(dsn, layer, encoding=NULL,
  use_iconv=FALSE, swapAxisOrder=FALSE, require_geomType=NULL,
  morphFromESRI=NULL, dumpSRS=FALSE, enforce_xy=NULL) {
  if (missing(dsn)) stop("missing dsn")
  stopifnot(is.character(dsn))
  stopifnot(length(dsn) == 1L)
# copy sf::st_read.default usage
  if (length(dsn) == 1 && file.exists(dsn))
    dsn <- enc2utf8(normalizePath(dsn))
  if (nchar(dsn) == 0) stop("empty dsn")
  if (missing(layer)) {
    layers <- ogrListLayers(dsn=dsn)
    if (length(layers) == 0L) stop("missing layer")
    if (length(layers) > 0L) layer <- enc2utf8(c(layers[1]))
    if (length(layers) > 1L)
      warning("First layer ", layer,
        " read; multiple layers present in\n", dsn,
        ", check layers with ogrListLayers()")
    
  } else layer <- enc2utf8(layer)
  if (nchar(layer) == 0) stop("empty name")
  WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
  if (!is.null(require_geomType)) {
    stopifnot(is.character(require_geomType) && length(require_geomType)==1)
    m_require_geomType <- match(require_geomType, WKB)
    stopifnot(!is.na(m_require_geomType) || m_require_geomType <= 3)
  }
# a list with various ogr data source information
  
  stopifnot(is.logical(use_iconv))
  stopifnot(length(use_iconv) == 1)
  if (!is.null(encoding)) {
    stopifnot(is.character(encoding))
    stopifnot(length(encoding) == 1)
  }
  if (!use_iconv && !is.null(encoding)) {
    oSE <- getCPLConfigOption("SHAPE_ENCODING")
    tull <- setCPLConfigOption("SHAPE_ENCODING", encoding)
  }
  ogrinfo <- .Call("RGDAL_ogrInfo",as.character(dsn), as.character(layer),
    PACKAGE = "rgdal")
  if (!use_iconv && !is.null(encoding)) {
    tull <- setCPLConfigOption("SHAPE_ENCODING", oSE)
  }

  if (swapAxisOrder) ogrinfo[[5]] <- ogrinfo[[5]][c(2,1,4,3)]

  u_eType <- u_with_z <- null_geometries <- NULL
  deleted_geometries <- NULL
  retain <- NULL
  have_features <- NULL
  all_NULL <- FALSE
  keepGeoms <- NULL

  if (!is.na(ogrinfo[[1]])) {

  fids <- ogrFIDs(dsn=dsn, layer=layer)
  nrows_i <- attr(fids, "i")
  have_features <- nrows_i > 0
  if (have_features && (attr(fids, "i") != attr(fids, "nf"))) {
     retain <- 1:attr(fids, "i")
     afids <- 0:(attr(fids, "nf")-1)
     deleted <- afids[!(afids %in% fids[retain])]
     deleted_geometries <- paste("Deleted feature IDs:", paste(deleted,
        collapse=", "))
     fids <- fids[retain]
  } else {
     deleted_geometries <- NULL
     retain <- NULL
  }
#  attributes(fids) <- NULL
  if (have_features) {
    eTypes <- .Call("R_OGR_types",as.character(dsn), as.character(layer),
      PACKAGE = "rgdal")
    if (is.null(retain)) {
      eType <- eTypes[[4]]
      with_z <- eTypes[[5]]
      isNULL <- as.logical(eTypes[[6]])
   } else {
      eType <- eTypes[[4]][retain]
      with_z <- eTypes[[5]][retain]
      isNULL <- as.logical(eTypes[[6]])[retain]
    }
    null_geometries <- NULL
    if (any(isNULL)) {
      all_NULL <- (sum(isNULL) == length(eType))
      eType <- eType[!isNULL]
      with_z <- with_z[!isNULL]
      null_geometries <- paste("Null geometry IDs:", 
        paste(which(isNULL), collapse=", "))
    }        
    if (!all_NULL) {
      eType[eType == 5L] <- 2L
      eType[eType == 6L] <- 3L

      u_eType <- unique(sort(eType))
      u_with_z <- unique(sort(with_z))
      if (length(u_with_z) != 1L) stop(
        paste("Multiple # dimensions:", 
          paste((u_with_z + 2), collapse=":")))
      if (u_with_z < 0 || u_with_z > 1) stop(
        paste("Invalid # dimensions:", (u_with_z + 2)))

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
          " of ", length(keepGeoms), " features\n",
          "    note that extent applies to all features")
      }
    } else {
      have_features <- FALSE
      warning("ogrInfo: all features NULL")
    }
  }
  } else {
    ogrinfo[[1]] <- attr(ogrinfo[[1]], "dFIDs")
    warning("ogrInfo: feature count overflow")
  }
  names(ogrinfo) <- c("nrows", "nitems", "iteminfo", "driver", "extent",
    "nListFields")
  if (ogrinfo$driver == "ESRI Shapefile") {
      DSN <- dsn
      if (!file.info(DSN)$isdir) DSN <- dirname(enc2utf8(normalizePath(dsn)))
      DBF_fn <- paste(DSN, .Platform$file.sep, layer, ".dbf", sep = "")
      if (file.exists(DBF_fn)) {
        con <- file(DBF_fn, "rb")
        vr <- readBin(con, "raw", n=32L)
        ldid <- as.integer(vr[30])
        attr(ogrinfo, "LDID") <- ldid
        close(con)
      } else {
        warning("ogrInfo: ", DBF_fn, " not found", sep="")
      }
  }
  names(ogrinfo$iteminfo) <- c("name","type","length","typeName","maxListCount")
  if (use_iconv && !is.null(encoding))
    ogrinfo$iteminfo$name <- iconv(ogrinfo$iteminfo$name, from=encoding)
  ogrinfo$have_features <- have_features
  ogrinfo$eType <- u_eType
  ogrinfo$with_z <- u_with_z
  ogrinfo$null_geometries <- null_geometries
  ogrinfo$deleted_geometries <- deleted_geometries
  ogrinfo$dsn <- dsn
  ogrinfo$layer <- layer
  if (is.null(morphFromESRI)) {
    if (ogrinfo$driver == "ESRI Shapefile") morphFromESRI <- TRUE
    else morphFromESRI <- FALSE
  }
  if (ogrinfo$driver != "ESRI Shapefile" && morphFromESRI)
    morphFromESRI <- FALSE
  p4s0 <- OGRSpatialRef(dsn, layer, morphFromESRI=morphFromESRI,
    dumpSRS=dumpSRS, enforce_xy=enforce_xy)
  if (new_proj_and_gdal()) wkt2 <- comment(p4s0)
  ogrinfo$p4s <- c(p4s0)
  if (new_proj_and_gdal()) ogrinfo$wkt2 <- wkt2
  if (!is.null(require_geomType))
    attr(ogrinfo, "require_geomType") <- require_geomType
  if (!is.null(keepGeoms)) attr(ogrinfo, "keepGeoms") <- keepGeoms
  class(ogrinfo) <- "ogrinfo"
  ogrinfo
}

print.ogrinfo <- function(x, ...) {
  cat("Source: \"", x$dsn, '\", layer: \"', x$layer, "\"", '\n', sep='')
  cat("Driver:", x$driver)
  if (x$have_features) cat("; number of rows:", x$nrows, "\n")
  WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
  if (!is.null(attr(x, "require_geomType"))) {
    cat("  selected geometry type:", attr(x, "require_geomType"), "with",
      sum(attr(x, "keepGeoms")), "rows\n")
  }
  if (!x$have_features) cat(", no features found\n")
  if (x$have_features) cat("Feature type:", paste(WKB[x$eType],
    collapse=", "), "with", x$with_z+2, "dimensions\n")
  if (!is.null(x$extent)) cat("Extent: (", x$extent[1], " ",
    x$extent[2], ") - (", x$extent[3], " ", x$extent[4], ")\n", sep="")
  if (!is.null(x$null_geometries)) cat(x$null_geometries, "\n")
  if (!is.null(x$deleted_geometries)) cat(x$deleted_geometries, "\n")
  if ((nchar(x$p4s) > 1) && !is.na(x$p4s)) cat("CRS:", x$p4s, "\n")
  if (!is.null(attr(x, "LDID"))) cat("LDID:", attr(x, "LDID"), "\n")
  cat("Number of fields:", x$nitems, "\n")
  if (is.null(x$nListFields)) x$nListFields <- 0
  if (x$nListFields > 0) cat("Number of list fields:", x$nListFields, "\n")
  if (x$nitems > 0 && x$nListFields == 0) print(as.data.frame(x$iteminfo)[,1:4])
  if (x$nitems > 0 && x$nListFields > 0 && x$have_features)
    print(as.data.frame(x$iteminfo))
  if (x$nitems > 0 && x$nListFields > 0 && !x$have_features)
    print(as.data.frame(x$iteminfo)[,1:4])
  invisible(x)
}


ogrFIDs <- function(dsn, layer){
  if (missing(dsn)) stop("missing dsn")
  stopifnot(is.character(dsn))
  stopifnot(length(dsn) == 1L)
# copy sf::st_read.default usage
  if (length(dsn) == 1 && file.exists(dsn))
    dsn <- enc2utf8(normalizePath(dsn))
  if (nchar(dsn) == 0) stop("empty name")
  if (missing(layer)) stop("missing layer")
  layer <- enc2utf8(layer)
  if (nchar(layer) == 0) stop("empty name")
  fids <- .Call("RGDAL_ogrFIDs",as.character(dsn),as.character(layer), PACKAGE = "rgdal")
  if (attr(fids, "i") == 0L) warning("no features found")
  fids
}

ogrDrivers <- function() {
  if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
    res <- .Call("ogr_GetDriverNames", PACKAGE="rgdal")
    res <- as.data.frame(res, stringsAsFactors=FALSE)
  } else {
      res <- .Call('RGDAL_GetDriverNames', PACKAGE="rgdal")
      if (!is.null(attr(res, "isVector"))) res$isVector <- attr(res, "isVector")
      res <- as.data.frame(res, stringsAsFactors=FALSE)
      res <- res[res$isVector,]
      names(res)[3] <- "write"
  }
  res <- res[order(res$name),]
  row.names(res) <- NULL
  res
}

OGRSpatialRef <- function(dsn, layer, morphFromESRI=NULL, dumpSRS=FALSE,
  driver=NULL, enforce_xy=NULL) {
  stopifnot(is.character(dsn))
  stopifnot(length(dsn) == 1L)
  if (is.null(driver)) driver <- attr(ogrListLayers(dsn), "driver")
  if (is.null(morphFromESRI)) {
    if (driver == "ESRI Shapefile") morphFromESRI <- TRUE
    else morphFromESRI <- FALSE
  }
  if (driver != "ESRI Shapefile" && morphFromESRI)
    morphFromESRI <- FALSE
  stopifnot(is.logical(morphFromESRI))
  stopifnot(length(morphFromESRI) == 1L)
# copy sf::st_read.default usage
  if (length(dsn) == 1 && file.exists(dsn))
    dsn <- enc2utf8(normalizePath(dsn))
  layer <- enc2utf8(layer)
  stopifnot(is.logical(dumpSRS))
  stopifnot(length(dumpSRS) == 1)
  stopifnot(!is.na(dumpSRS))
  if (!is.null(enforce_xy)) {
    stopifnot(is.logical(enforce_xy))
    stopifnot(length(enforce_xy) == 1L)
    stopifnot(!is.na(enforce_xy))
  } else {
      enforce_xy <- get_enforce_xy()
  }
  attr(dumpSRS, "enforce_xy") <- enforce_xy
  wkt2 <- NULL
  no_ellps <- FALSE
  res <- .Call("ogrP4S", as.character(dsn), as.character(layer),
        as.logical(morphFromESRI), dumpSRS, PACKAGE="rgdal")
  if (!is.na(res) && new_proj_and_gdal()) {
    no_towgs84 <- ((is.null(attr(res, "towgs84"))) || 
      (all(nchar(attr(res, "towgs84")) == 0)))
    if ((length(grep("towgs84", c(res))) == 0L) && !no_towgs84)
      warning("TOWGS84 discarded")
    no_ellps <- (!is.null(attr(res, "ellps"))) &&
        (nchar(attr(res, "ellps")) > 0L) &&
        (length(grep("ellps", c(res))) == 0L) 
    no_ellps <- no_ellps && length(grep("datum", c(res))) == 0L
    if (no_ellps) {
      msg <- paste0("Discarded ellps ", attr(res, "ellps"),
            " in Proj4 definition: ", c(res))
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
    if ((!is.null(attr(res, "datum"))) && (nchar(attr(res, "datum")) > 0L)
      && (length(grep("datum", c(res))) == 0L)) {
      msg <- paste0("Discarded datum ", attr(res, "datum"),
          " in Proj4 definition: ", c(res))
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
#warning(msg)
    }
    if (new_proj_and_gdal()) wkt2 <- attr(res, "WKT2_2018")
  }
  res <- c(res)
  if (new_proj_and_gdal()) {
    if (no_ellps) res <- showSRID(wkt2, "PROJ")
    comment(res) <- wkt2
  }
  res
}

ogrListLayers <- function(dsn) {
  if (missing(dsn)) stop("missing dsn")
  stopifnot(is.character(dsn))
  stopifnot(length(dsn) == 1)
# copy sf::st_read.default usage
  if (length(dsn) == 1 && file.exists(dsn))
    dsn <- enc2utf8(normalizePath(dsn))
  if (nchar(dsn) == 0) stop("empty name")
  if (!is.null(attr(dsn, "debug"))) {
    stopifnot(is.logical(attr(dsn, "debug")))
    stopifnot(length(attr(dsn, "debug")) == 1)
  } else {
    attr(dsn, "debug") <- FALSE
  }
  layers <- .Call("RGDAL_ogrListLayers", dsn, PACKAGE = "rgdal")
  n <- length(layers)
  tmp <- layers[n]
  layers <- layers[-n]
  Encoding(layers) <- "UTF-8"
  layers <- enc2native(layers)
  attr(layers, "driver") <- tmp
  attr(layers, "nlayers") <- (n-1)
  layers
}
