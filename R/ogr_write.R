writeOGR <- function(obj, dsn, layer, driver, dataset_options=NULL, layer_options=NULL, verbose=FALSE, check_exists=NULL, overwrite_layer=FALSE, delete_dsn=FALSE) {
    drvs <- ogrDrivers()
    mch <- match(driver, drvs$name)
    if (is.na(mch) || length(mch) > 1L)
        stop(paste("No such driver:", driver))
    if (!drvs$write[mch]) stop("Chosen driver cannot create files")
    if (driver == "KML" || driver == "GPX") {
      if (is.na(is.projected(obj))) warning(paste("Unknown coordinate", 
        "reference system: for KML driver should be geographical"))
      else if (is.projected(obj)) warning(paste("Projected coordinate",
        "reference system: for KML driver should be geographical"))
      proj4string(obj) <- CRS(as.character(NA))
# fix for over-eager internal checking in the KML driver 090114
    }
    stopifnot(inherits(obj, "Spatial"))
    if (gridded(obj)) {
        obj <- as(obj, "SpatialPointsDataFrame")
        if (verbose) warning("coercing gridded object to points")
    }
    if (!"data" %in% names(getSlots(class(obj))))
        stop("obj must be a SpatialPointsDataFrame, SpatialLinesDataFrame or\n    SpatialPolygonsDataFrame") 
    dfcls <- sapply(slot(obj, "data"), function(x) class(x)[1])
    dfcls <- gsub("POSIXct", "POSIXt", dfcls)
    dfcls <- gsub("POSIXlt", "POSIXt", dfcls)
    dfcls <- gsub("Date", "POSIXt", dfcls)
# fix for logical and better reporting Barry Rowlingson 091106
    known <- c("numeric", "character", "factor", "POSIXt", "integer", "logical")
    if (!all(dfcls %in% known)) 
        stop("Can't convert columns of class: ",
            paste(unique(dfcls[!dfcls %in% known]), collapse=","),
            "; column names: ", paste(names(obj@data)[!dfcls %in% known],
            collapse=","))
    dftof <- sapply(slot(obj, "data"), typeof)
    known <- c("double", "character", "integer", "logical")
    if (!all(dftof %in% known))
        stop("Can't convert columns of type: ", 
            paste(unique(dftof[!dftof %in% known]),collapse=","), 
            "; column names: ", paste(names(obj@data)[!dftof %in% known], 
            collapse=","))

    if (is.null(check_exists)) {
        if (getGDALVersionInfo("VERSION_NUM") >= "1800") {
            check_exists <- TRUE
        } else {
            check_exists <- FALSE
        }
    }

    if (check_exists) {
        already_exists <- FALSE
        ogrI <- try(.Call("ogrInfo", as.character(dsn),
            as.character(layer), PACKAGE = "rgdal"), silent=TRUE)
        if (class(ogrI) != "try-error") {
            if (driver == ogrI[[4]]) already_exists <- TRUE
        }
        if (already_exists) {
            if (overwrite_layer) {
                layer_del <- try(ogrDeleteLayer(dsn, layer, driver),
                    silent=TRUE)
                if (class(layer_del) == "try-error" && delete_dsn) {
                    ogrDeleteDataSource(dsn, driver)
                    if (verbose) warning("existing data source removed")
                }
                if (verbose) warning("existing layer removed")
            } else {
                stop("layer exists, use a new layer name")
            }
        }
    }

    nf <- length(dfcls)
    ldata <- vector(mode="list", length=nf)
    ogr_ftype <- integer(nf)
    for (i in 1:nf) {
        if (dfcls[i] == "numeric" && dftof[i] == "double") {
            ldata[[i]] <- slot(obj, "data")[,i]
            ogr_ftype[i] <- as.integer(2) #"OFTReal"
        } else if (dfcls[i] == "character" && dftof[i] == "character") {
            ldata[[i]] <- slot(obj, "data")[,i]
            ogr_ftype[i] <- as.integer(4) #"OFTString"
        } else if (dfcls[i] == "factor" && dftof[i] == "integer") {
            ldata[[i]] <- as.character(slot(obj, "data")[,i])
            ogr_ftype[i] <- as.integer(4) #"OFTString"
        } else if (dfcls[i] == "POSIXt" && dftof[i] == "integer") {
            ldata[[i]] <- as.character(format(slot(obj, "data")[,i]))
            ogr_ftype[i] <- as.integer(4) #"OFTString"
        } else if (dfcls[i] == "POSIXt" && dftof[i] == "double") {
            ldata[[i]] <- as.character(format(slot(obj, "data")[,i]))
            ogr_ftype[i] <- as.integer(4) #"OFTString"
        } else if (dfcls[i] == "integer" && dftof[i] == "integer") {
            ldata[[i]] <- slot(obj, "data")[,i]
            ogr_ftype[i] <- as.integer(0) #"OFTInteger"
        } else if (dfcls[i] == "logical" && dftof[i] == "logical") {
# fix for logical and better reporting Barry Rowlingson 091106
            ldata[[i]] <- as.integer(slot(obj, "data")[,i])
            ogr_ftype[i] <- as.integer(0) #"OFTInteger"
        } else stop(paste(dfcls[i], dftof[i], "unknown data type"))
    }
    fld_names <- names(dfcls)
    nobj <- nrow(slot(obj, "data"))
    
    pre <- list(obj, as.character(dsn), as.character(layer), 
        as.character(driver), as.integer(nobj), nf,
        as.character(fld_names), as.integer(ogr_ftype), ldata, 
        as.character(dataset_options), as.character(layer_options))
    res <- .Call("OGR_write", pre, PACKAGE="rgdal")
    if (verbose) {
        res <- list(object_type=res, output_dsn=dsn, output_layer=layer,
            output_diver=driver, output_n=nobj, output_nfields=nf,
            output_fields=fld_names, output_fclasses=ogr_ftype, 
            dataset_options=dataset_options, layer_options=layer_options)
        return(res)
    } 
}

ogrDeleteLayer <- function(dsn, layer, driver) {
    if (missing(dsn)) stop("missing dsn")
    if (nchar(dsn) == 0) stop("empty name")
    if (missing(layer)) stop("missing layer")
    if (nchar(layer) == 0) stop("empty name")
    if (missing(driver)) stop("missing layer")
    if (nchar(driver) == 0) stop("empty name")
    res <- .Call("ogrDeleteLayer", as.character(dsn), as.character(layer),
        as.character(driver), PACKAGE = "rgdal")
    invisible(res)
}

ogrDeleteDataSource <- function(dsn, driver) {
    if (missing(dsn)) stop("missing dsn")
    if (nchar(dsn) == 0) stop("empty name")
    if (missing(driver)) stop("missing layer")
    if (nchar(driver) == 0) stop("empty name")
    res <- .Call("ogrDeleteDataSource", as.character(dsn), as.character(driver),
        PACKAGE = "rgdal")
    invisible(res)

}

