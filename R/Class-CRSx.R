# Copyright (c) 2003-4 by Barry Rowlingson and Roger Bivand

.valid.CRSobj <- function(object) {
	if (exists("is.R") && is.function(is.R) && is.R()) {
		projargNA <- is.na(object@projargs)
	} else {
		projargNA <- is.na(as.numeric(object@projargs))
	}
	if (!projargNA) {
		res <- .Call("checkCRSArgs", object@projargs, 
				PACKAGE="rgdal")
	} else res <- list(TRUE, as.character(NA))
	if (!res[[1]]) {
	    	return(res[[2]])
	} else {
		return(res[[1]])
	}

}

setValidity("CRS", .valid.CRSobj)


"CRSargs" <- function(object) {
	if (!is(object, "CRS")) stop("not a CRS object")

	if (!is.na(object@projargs)) {
		res <- (.Call("checkCRSArgs", object@projargs, 
			PACKAGE="rgdal")[[2]])
		return(paste(unique(unlist(strsplit(res, " "))), 
			collapse=" "))

	} else return(as.character(NA))
}

