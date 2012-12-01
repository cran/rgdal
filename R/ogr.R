# Copyright 2003 (c) Barry Rowlingson
# Modified 2006-12 Roger Bivand
###
###
###  Routines for ogr layer data source 
###
###

#
ogrInfo <- function(dsn, layer, encoding=NULL, input_field_name_encoding=NULL){
  if (missing(dsn)) stop("missing dsn")
  if (nchar(dsn) == 0) stop("empty name")
  if (missing(layer)) stop("missing layer")
  if (nchar(layer) == 0) stop("empty name")
# a list with various ogr data source information
  
  use_iconv <- ifelse(as.integer(getGDALVersionInfo("VERSION_NUM")) < 1900L,
    TRUE, FALSE)
  if (!is.null(encoding)) {
    stopifnot(is.character(encoding))
    stopifnot(length(encoding) == 1)
  }
  if (!is.null(input_field_name_encoding)) {
    warning("input_field_name_encoding= deprecated, use encoding=")
    stopifnot(is.character(input_field_name_encoding))
    stopifnot(length(input_field_name_encoding) == 1)
    if (!is.null(encoding) && (encoding != input_field_name_encoding))
       stop("encoding and input_field_name_encoding differ")
    if (is.null(encoding)) encoding <- input_field_name_encoding
  }
  if (!use_iconv && !is.null(encoding) && Sys.getenv("SHAPE_ENCODING") == "") {
    Sys.setenv("SHAPE_ENCODING"=encoding)
  }
  ogrinfo <- .Call("ogrInfo",as.character(dsn), as.character(layer),
    PACKAGE = "rgdal")
  if (!use_iconv && !is.null(encoding) && Sys.getenv("SHAPE_ENCODING") ==
    encoding) {
    Sys.unsetenv("SHAPE_ENCODING")
  }
  fids <- ogrFIDs(dsn=dsn, layer=layer)
  if (attr(fids, "i") != attr(fids, "nf")) {
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
  attributes(fids) <- NULL
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
      eType <- eType[!isNULL]
      with_z <- with_z[!isNULL]
      null_geometries <- paste("Null geometry IDs:", 
        paste(which(isNULL), collapse=", "))
  }        
  u_eType <- unique(sort(eType))
  u_with_z <- unique(sort(with_z))
  if (length(u_with_z) != 1L) stop(
    paste("Multiple # dimensions:", 
      paste((u_with_z + 2), collapse=":")))
  if (u_with_z < 0 || u_with_z > 1) stop(
    paste("Invalid # dimensions:", (u_with_z + 2)))
  if (length(u_eType) > 2L) stop(
    paste("Multiple incompatible geometries:", 
      paste(u_eType, collapse=":")))
  if (length(u_eType) == 2L) {
    if (u_eType[1] == 2 && u_eType[2] == 5) u_eType = 2
    else if (u_eType[1] == 3 && u_eType[2] == 6) u_eType = 3
    else stop(paste("Multiple incompatible geometries:", 
      paste(u_eType, collapse=":")))
  }
  names(ogrinfo) <- c("nrows","nitems","iteminfo","driver")
  if (ogrinfo$driver == "ESRI Shapefile") {
      con <- file(paste(dsn, .Platform$file.sep, layer, ".dbf", sep=""), "rb")
      vr <- readBin(con, "raw", n=32L)
      ldid <- as.integer(vr[30])
      attr(ogrinfo, "LDID") <- ldid
      close(con)
  }
  names(ogrinfo$iteminfo) <- c("name","type","length","typeName")
  if (use_iconv && !is.null(encoding))
    ogrinfo$iteminfo$name <- iconv(ogrinfo$iteminfo$name, from=encoding)
  ogrinfo$eType <- u_eType
  ogrinfo$with_z <- u_with_z
  ogrinfo$null_geometries <- null_geometries
  ogrinfo$deleted_geometries <- deleted_geometries
  ogrinfo$dsn <- dsn
  ogrinfo$layer <- layer
  ogrinfo$p4s <- OGRSpatialRef(dsn, layer)
  class(ogrinfo) <- "ogrinfo"
  ogrinfo
}

print.ogrinfo <- function(x, ...) {
  cat("Source: \"", x$dsn, '\", layer: \"', x$layer, "\"", '\n', sep='')
  cat("Driver:", x$driver, "number of rows", x$nrows, "\n")
  WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
  cat("Feature type:", paste(WKB[x$eType], collapse=", "), "with",
    x$with_z+2, "dimensions\n")
  if (!is.null(x$null_geometries)) cat(x$null_geometries, "\n")
  if (!is.null(x$deleted_geometries)) cat(x$deleted_geometries, "\n")
  if (nchar(x$p4s > 1)) cat(x$p4s, "\n")
  if (!is.null(attr(x, "LDID"))) cat("LDID:", attr(x, "LDID"), "\n")
  cat("Number of fields:", x$nitems, "\n")
  if (x$nitems > 0) print(as.data.frame(x$iteminfo))
  invisible(x)
}


ogrFIDs <- function(dsn, layer){
  if (missing(dsn)) stop("missing dsn")
  if (nchar(dsn) == 0) stop("empty name")
  if (missing(layer)) stop("missing layer")
  if (nchar(layer) == 0) stop("empty name")
  fids <- .Call("ogrFIDs",as.character(dsn),as.character(layer), PACKAGE = "rgdal")
  fids
}

ogrDrivers <- function() {
  res <- .Call("ogr_GetDriverNames", PACKAGE="rgdal")
  res <- as.data.frame(res)
  res <- res[order(res$name),]
  row.names(res) <- NULL
  res
}

"OGRSpatialRef" <- function(dsn, layer) {
    .Call("ogrP4S", as.character(dsn), as.character(layer),
        PACKAGE="rgdal")
}

ogrListLayers <- function(dsn) {
  if (missing(dsn)) stop("missing dsn")
  if (nchar(dsn) == 0) stop("empty name")
  layers <- .Call("ogrListLayers", as.character(dsn), PACKAGE = "rgdal")
  n <- length(layers)
  attr(layers, "driver") <- layers[n]
  layers <- layers[-n]
  layers
}
