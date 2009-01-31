# Copyright 2003 (c) Barry Rowlingson
# Modified 2006-9 Roger Bivand
###
###
###  Routines for ogr layer data source 
###
###

#
ogrInfo <- function(dsn, layer, input_field_name_encoding=NULL){
  if (missing(dsn)) stop("missing dsn")
  if (nchar(dsn) == 0) stop("empty name")
  if (missing(layer)) stop("missing layer")
  if (nchar(layer) == 0) stop("empty name")
# a list with various ogr data source information
  ogrinfo <- .Call("ogrInfo",as.character(dsn), as.character(layer),
    PACKAGE = "rgdal")
  eTypes <- .Call("R_OGR_types",as.character(dsn), as.character(layer),
    PACKAGE = "rgdal")
  eType <- eTypes[[4]]
  u_eType <- unique(sort(eType))
  with_z <- eTypes[[5]]
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
  names(ogrinfo) <- c("nrows","nitems","iteminfo","driver")
  names(ogrinfo$iteminfo) <- c("name","type","length","typeName")
  if (!is.null(input_field_name_encoding)) 
    ogrinfo$iteminfo$name <- iconv(ogrinfo$iteminfo$name,
      from=input_field_name_encoding)
  ogrinfo$eType <- u_eType
  ogrinfo$with_z <- u_with_z
  class(ogrinfo) <- "ogrinfo"
  ogrinfo
}

print.ogrinfo <- function(x, ...) {
  cat("Driver:", x$driver, "number of rows", x$nrows, "\n")
  WKB <- c("wkbPoint", "wkbLineString", "wkbPolygon", "wkbMultiPoint",
    "wkbMultiLineString", "wkbMultiPolygon", "wkbGeometryCollection")
  cat("Feature type:", paste(WKB[x$eType], collapse=", "), "with",
    x$with_z+2, "dimensions\n")
  cat("Number of fields:", x$nitems, "\n")
  print(as.data.frame(x$iteminfo))
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
