# Copyright 2003 (c) Barry Rowlingson
# Modified 2006 Roger Bivand
###
###
###  Routines for ogr layer data source 
###
###

#
ogrInfo <- function(dsn, layer){
  if (missing(dsn)) stop("missing dsn")
  if (nchar(dsn) == 0) stop("empty name")
  if (missing(layer)) stop("missing layer")
  if (nchar(layer) == 0) stop("empty name")
# a list with various ogr data source information
  ogrinfo <- .Call("ogrInfo",as.character(dsn),as.character(layer), PACKAGE = "rgdal")
  
  names(ogrinfo) <- c("nrows","nitems","iteminfo","driver")
  names(ogrinfo$iteminfo) <- c("name","type","length","typeName")
  class(ogrinfo) <- "ogrinfo"
  ogrinfo
}

print.ogrinfo <- function(x, ...) {
	cat("Driver:", x$driver, "number of rows", x$nrows, "\n")
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
