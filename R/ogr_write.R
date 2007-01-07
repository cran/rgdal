writeOGR <- function(obj, dsn, layer, driver, dataset_options=NULL, layer_options=NULL, verbose=FALSE) {
    drvs <- ogrDrivers()
    mch <- match(driver, drvs$name)
    if (is.na(mch) || length(mch) > 1)
        stop(paste("No such driver:", driver))
    if (!drvs$write[mch]) stop("Chosen driver cannot create files")
    if (driver == "KML") {
      if (is.na(is.projected(obj))) warning(paste("Unknown coordinate", 
        "reference system: for KML driver should be geographical"))
      else if (is.projected(obj)) warning(paste("Projected coordinate",
        "reference system: for KML driver should be geographical"))
    }
    if (!"data" %in% names(getSlots(class(obj)))) stop("obj of wrong class") 
    dfcls <- sapply(slot(obj, "data"), function(x) class(x)[1])
    known <- c("numeric", "character", "factor", "POSIXt", "integer")
    if (!all(dfcls %in% known)) stop("unknown data type")

    dftof <- sapply(slot(obj, "data"), typeof)
    known <- c("double", "character", "integer")
    if (!all(dftof %in% known)) stop("unknown data type")

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
        } else if (dfcls[i] == "integer" && dftof[i] == "integer") {
            ldata[[i]] <- slot(obj, "data")[,i]
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

