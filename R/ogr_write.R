writeOGR <- function(obj, dsn, layer, driver, dataset_options=NULL, layer_options=NULL, verbose=FALSE) {
    drvs <- ogrDrivers()
    mch <- match(driver, drvs$name)
    if (is.na(mch) || length(mch) > 1)
        stop(paste("No such driver:", driver))
    if (!drvs$write[mch]) stop("Chosen driver cannot create files")
    if (!"data" %in% names(getSlots(class(obj)))) stop("obj of wrong class") 
    dfcls <- sapply(slot(obj, "data"), function(x) class(x)[1])
    dftof <- sapply(slot(obj, "data"), typeof)
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
        }
    }
    fld_names <- names(dfcls)
    nobj <- nrow(slot(obj, "data"))
    
    pre <- list(obj, dsn, layer, driver, nobj, nf, fld_names,
         ogr_ftype, ldata, dataset_options, layer_options)
    res <- .Call("OGR_write", pre, PACKAGE="rgdal")
    if (verbose) {
        res <- list(object_type=res, output_dsn=dsn, output_layer=layer,
            output_diver=driver, output_n=nobj, output_nfields=nf,
            output_fields=fld_names, output_fclasses=ogr_ftype, 
            dataset_options=dataset_options, layer_options=layer_options)
        return(res)
    } 
}

