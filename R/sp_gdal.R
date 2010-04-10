GDALinfo <- function(fname, silent=FALSE) {
	if (nchar(fname) == 0) stop("empty file name")
	x <- GDAL.open(fname, silent=silent)
	d <- dim(x)[1:2]
        dr <- getDriverName(getDriver(x))
	p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt <- .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
        if (attr(gt, "CE_Failure")) warning("GeoTransform values not available")
	nbands <- .Call('RGDAL_GetRasterCount', x, PACKAGE="rgdal")
        mdata <- .Call('RGDAL_GetMetadata', x, NULL, PACKAGE="rgdal")
        subdsmdata <- .Call('RGDAL_GetMetadata', x, "SUBDATASETS",
            PACKAGE="rgdal")
        if (nbands < 1) {
#            warning("no bands in dataset")
            df <- NULL
        } else {
            band <- 1:nbands
            GDType <- character(nbands)
            Bmin <- numeric(nbands)
            Bmax <- numeric(nbands)
            Pix <- character(nbands)
            for (i in seq(along = band)) {

                raster <- getRasterBand(x, band[i])
                GDType[i] <- .GDALDataTypes[(.Call("RGDAL_GetBandType",
                    raster, PACKAGE="rgdal"))+1]
                Bmin[i] <- .Call("RGDAL_GetBandMinimum", raster,
                    PACKAGE="rgdal")
                Bmax[i] <- .Call("RGDAL_GetBandMaximum", raster,
                    PACKAGE="rgdal")
#                Pix[i] <- .Call("RGDAL_GetBandMetadataItem",
#                    raster, "PIXELTYPE", "IMAGE_STRUCTURE", PACKAGE="rgdal")
            }
            df <- data.frame(GDType=GDType, Bmin=Bmin, Bmax=Bmax)
        }
        
	GDAL.close(x)
#	res <- c(rows=d[1], columns=d[2], bands=nbands, ll.x=gt[1], ll.y=gt[4],
#		res.x=abs(gt[2]), res.y=abs(gt[6]), oblique.x=abs(gt[3]), 
#		oblique.y=abs(gt[5]))
### Modified: MDSumner 22 November 2008
        cellsize = abs(c(gt[2], gt[6]))
        ysign <- sign(gt[6])
        offset.y <- ifelse(ysign < 0, gt[4] + ysign * d[1] * abs(cellsize[2]),
            gt[4] +   abs(cellsize[2]))
        res <- c(rows = d[1], columns = d[2], bands = nbands, ll.x = gt[1],
            ll.y = offset.y, res.x = abs(gt[2]), res.y = abs(gt[6]),
            oblique.x = abs(gt[3]), oblique.y = abs(gt[5]))
#### end modification
	attr(res, "ysign") <- ysign
	attr(res, "driver") <- dr 
	attr(res, "projection") <- p4s 
	attr(res, "file") <- fname
        attr(res, "df") <- df
        attr(res, "mdata") <- mdata
        attr(res, "subdsmdata") <- subdsmdata
	class(res) <- "GDALobj"
	res
}

print.GDALobj <- function(x, ...) {
	cat("rows       ", x[1], "\n")
	cat("columns    ", x[2], "\n")
	cat("bands      ", x[3], "\n")
	cat("origin.x       ", x[4], "\n")
	cat("origin.y       ", x[5], "\n")
	cat("res.x      ", x[6], "\n")
	cat("res.y      ", x[7], "\n")
        cat("ysign      ", attr(x, "ysign"), "\n")
	cat("oblique.x  ", x[8], "\n")
	cat("oblique.y  ", x[9], "\n")
	cat("driver     ", attr(x, "driver"), "\n")
	cat("projection ", paste(strwrap(attr(x, "projection")),
		collapse="\n"), "\n")
	cat("file       ", attr(x, "file"), "\n")
        if (!is.null(attr(x, "df"))) {
            cat("apparent band summary:\n")
            print(attr(x, "df"))
        }
        if (!is.null(attr(x, "mdata"))) {
            cat("Metadata:\n")
            cv <- attr(x, "mdata")
            for (i in 1:length(cv)) cat(cv[i], "\n")
        }
        if (!is.null(attr(x, "subdsmdata"))) {
            cat("Subdatasets:\n")
            cv <- attr(x, "subdsmdata")
            for (i in 1:length(cv)) cat(cv[i], "\n")
        }
	invisible(x)
}

asGDALROD_SGDF <- function(from) {
	x <- from
	d = dim(x)
	half.cell <- c(0.5,0.5)
	offset <- c(0,0)
	output.dim <- d[1:2]
	p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt = .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
        if (attr(gt, "CE_Failure")) warning("GeoTransform values not available")
	if (any(gt[c(3,5)] != 0.0)) stop("Diagonal grid not permitted")
	data = getRasterData(x)
	cellsize = abs(c(gt[2],gt[6]))
	ysign <- sign(gt[6])
	co.x <- gt[1] + (offset[2] + half.cell[2]) * cellsize[1]
	co.y <- ifelse(ysign < 0, gt[4] + (ysign*((output.dim[1] + 
		offset[1]) + (ysign*half.cell[1]))) * abs(cellsize[2]),
		gt[4] + (ysign*((offset[1]) + (ysign*half.cell[1]))) * 
		abs(cellsize[2]))
	cellcentre.offset <- c(x=co.x, y=co.y)
	grid = GridTopology(cellcentre.offset, cellsize, rev(output.dim))
	if (length(d) == 2)
		df = list(band1 = as.vector(data))
	else {
		df <- vector(mode="list", length=d[3])
		df[[1]] <- as.vector(data[,,1, drop = FALSE])
		for (band in 2:d[3])
			df[[band]] <- as.vector(data[,,band, drop = FALSE])
		names(df) = paste("band", 1:d[3], sep="")
	}
	return(SpatialGridDataFrame(grid = grid, 
		data = data.frame(df), proj4string=CRS(p4s)))
}

setAs("GDALReadOnlyDataset", "SpatialGridDataFrame", asGDALROD_SGDF)

asSGDF_GROD <- function(x, offset, region.dim, output.dim, p4s=NULL, ..., half.cell=c(0.5,0.5)) {
	if (!extends(class(x), "GDALReadOnlyDataset"))
		stop("x must be or extend a GDALReadOnlyDataset")
	d = dim(x)
	if (missing(offset)) offset <- c(0,0)
	if (missing(region.dim)) region.dim <- dim(x)[1:2]
	odim_flag <- NULL
	if (!missing(output.dim)) odim_flag <- TRUE
	else {
		output.dim <- region.dim
		odim_flag <- FALSE
	}

# suggestion by Paul Hiemstra 070817
	if (is.null(p4s)) 
	    p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt = .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
        if (attr(gt, "CE_Failure")) warning("GeoTransform values not available")
	if (any(gt[c(3,5)] != 0.0)) stop("Diagonal grid not permitted")
	data = getRasterData(x, offset=offset, 
		region.dim=region.dim, output.dim=output.dim, ...)
	if (!odim_flag) cellsize = abs(c(gt[2],gt[6]))
	else {
		icellsize = abs(c(gt[2],gt[6]))
		span <- icellsize * rev(d)
		cellsize <- span / rev(output.dim)
	}
	ysign <- sign(gt[6])
	co.x <- gt[1] + (offset[2] + half.cell[2]) * cellsize[1]
	co.y <- ifelse(ysign < 0, gt[4] + (ysign*((output.dim[1] + 
		offset[1]) + (ysign*half.cell[1]))) * abs(cellsize[2]),
		gt[4] + (ysign*((offset[1]) + (ysign*half.cell[1]))) * 
		abs(cellsize[2]))
	cellcentre.offset <- c(x=co.x, y=co.y)
	grid = GridTopology(cellcentre.offset, cellsize, rev(output.dim))
	if (length(d) == 2)
		df = list(band1 = as.vector(data))
	else {
		df <- vector(mode="list", length=d[3])
		df[[1]] <- as.vector(data[,,1, drop = FALSE])
		for (band in 2:d[3])
			df[[band]] <- as.vector(data[,,band, drop = FALSE])
		names(df) = paste("band", 1:d[3], sep="")
	}
	df1 <- data.frame(df)
	data = SpatialGridDataFrame(grid = grid, 
		data = df1, proj4string=CRS(p4s))
	return(data)
}

readGDAL = function(fname, offset, region.dim, output.dim, band, p4s=NULL, ..., half.cell=c(0.5,0.5), silent = FALSE) {
	if (nchar(fname) == 0) stop("empty file name")
	x = GDAL.open(fname, silent=silent)
	d = dim(x)
	if (missing(offset)) offset <- c(0,0)
	if (missing(region.dim)) region.dim <- dim(x)[1:2] # rows=nx, cols=ny
#	else d <- region.dim
	odim_flag <- NULL
        if (missing(band)) band <- NULL
        else {
                if (length(band) > 1) d[3] <- length(band)
                else d <- d[1:2]
        }
# bug report Mike Sumner 070522
	if (!missing(output.dim)) odim_flag <- TRUE
	else {
		output.dim <- region.dim
		odim_flag <- FALSE
	}
	if (!silent) {
		cat(paste(fname, "has GDAL driver", getDriverName(getDriver(x)),"\n"))
		cat(paste("and has", d[1], "rows and", d[2], "columns\n"))
	}
# suggestion by Paul Hiemstra 070817
	if (is.null(p4s)) 
	    p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt = .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
        if (attr(gt, "CE_Failure")) warning("GeoTransform values not available")
	# [1] 178400     40      0 334000      0    -40
	if (any(gt[c(3,5)] != 0.0)) {
		data = getRasterTable(x, band=band, offset=offset, 
			region.dim=region.dim, ...)
		GDAL.close(x)
		coordinates(data) = c(1,2)
		proj4string(data) = CRS(p4s)
	} else {
#		cellsize = abs(c(gt[2],gt[6]))
		if (!odim_flag) cellsize = abs(c(gt[2],gt[6]))
		else {
			icellsize = abs(c(gt[2],gt[6]))
# bug report Jose M. Blanco Moreno 091004
			span <- icellsize * rev(region.dim)
# bug report Mike Sumner 070215
			cellsize <- span / rev(output.dim)
		}
		ysign <- sign(gt[6])
                if (ysign > 0) 
                    warning("Y axis resolution positive, examine data for flipping")
#		cells.dim = c(d[1], d[2]) # c(d[2],d[1])
# bug report Jose M. Blanco Moreno 091004
		co.x <- gt[1] + ((offset[2]/(cellsize[1]/abs(gt[2]))) + 
                    half.cell[2]) * cellsize[1]
		co.y <- ifelse(ysign < 0, gt[4] + (ysign*((output.dim[1] + 
# bug report Jose M. Blanco Moreno 091004
			(offset[1]/(cellsize[2]/abs(gt[6]))) + 
                        (ysign*half.cell[1])))) * abs(cellsize[2]),
			gt[4] + (ysign*((offset[1]) + (ysign*half.cell[1]))) * 
			abs(cellsize[2]))
		cellcentre.offset <- c(x=co.x, y=co.y)
#		cellcentre.offset = c(x = gt[1] + 0.5 * cellsize[1], 
#			y = gt[4] - (d[2] - 0.5) * abs(cellsize[2]))
		grid = GridTopology(cellcentre.offset, cellsize, 
			rev(output.dim))
#			rev(region.dim))
		data = getRasterData(x, band=band, offset=offset, 
			region.dim=region.dim, output.dim=output.dim, ...)
		GDAL.close(x)
		if (length(d) == 2)
			df = list(band1 = as.vector(data))
		else {
			df <- vector(mode="list", length=d[3])
			df[[1]] <- as.vector(data[,,1, drop = FALSE])
			for (band in 2:d[3])
				df[[band]] <- as.vector(data[,,band, drop = FALSE])
#			df = as.data.frame(df)
			names(df) = paste("band", 1:d[3], sep="")
		}
		data = SpatialGridDataFrame(grid = grid, 
			data = data.frame(df), proj4string=CRS(p4s))
	}
	return(data)
}

writeGDAL = function(dataset, fname, drivername = "GTiff", type = "Float32", 
		mvFlag = NA, options=NULL, copy_drivername = "GTiff")
{
	if (nchar(fname) == 0) stop("empty file name")
        x <- gdalDrivers()
	copy_only <- as.character(x[!x$create & x$copy, 1])
        if (drivername %in% copy_only) {
	    tds.create <- create2GDAL(dataset=dataset,
                drivername=copy_drivername, type=type,
                mvFlag=mvFlag, fname=NULL)
            tds.copy <- copyDataset(tds.create, driver=drivername, fname=fname)
            GDAL.close(tds.create)
	    saveDataset(tds.copy, fname, options=options)
        } else {
	    tds.out <- create2GDAL(dataset=dataset, drivername=drivername, 
		type=type, mvFlag=mvFlag, options=options, fname=fname)
	    saveDataset(tds.out, fname, options=options)
        }
# RSB 081030 GDAL.close cleanup
#	tmp.obj <- saveDataset(tds.out, fname, options=options)
#        GDAL.close(tmp.obj)
	invisible(fname)
}

create2GDAL = function(dataset, drivername = "GTiff", type = "Float32", mvFlag = NA, options=NULL, fname=NULL)
{
	stopifnot(gridded(dataset))
	fullgrid(dataset) = TRUE
	if (is.na(match(type, .GDALDataTypes)))
            stop(paste("Invalid type:", type, "not in:",
                paste(.GDALDataTypes, collapse="\n")))
#	d.dim = dim(as.matrix(dataset[1])) RSB 081106
	gp = gridparameters(dataset)
	cellsize = gp$cellsize
	offset = gp$cellcentre.offset
	dims = gp$cells.dim
	d.drv = new("GDALDriver", drivername)
	nbands = length(names(slot(dataset, "data")))
        if (!is.null(options) && !is.character(options))
                stop("options not character")
	tds.out = new("GDALTransientDataset", driver = d.drv, 
		rows = dims[2], cols = dims[1],
        	bands = nbands, type = type, options = options, fname = fname,
		handle = NULL)
	gt = c(offset[1] - 0.5 * cellsize[1], cellsize[1], 0.0, 
		offset[2] + (dims[2] -0.5) * cellsize[2], 0.0, -cellsize[2])
	.Call("RGDAL_SetGeoTransform", tds.out, gt, PACKAGE = "rgdal")

	p4s <- proj4string(dataset)
	if (!is.na(p4s) && nchar(p4s) > 0) {
	    .Call("RGDAL_SetProject", tds.out, p4s, PACKAGE = "rgdal")
        } else {
            if (getDriverName(getDriver(tds.out)) == "RST") 
                stop("RST files must have a valid coordinate reference system")
        }
	for (i in 1:nbands) {
		band = as.matrix(dataset[i])
		if (!is.numeric(band)) stop("Numeric bands required")
		if (!is.na(mvFlag))
			band[is.na(band)] = mvFlag
		putRasterData(tds.out, band, i)
		if (!is.na(mvFlag)) {
		    tds.out_b <- getRasterBand(dataset=tds.out, band=i)
		    .Call("RGDAL_SetNoDataValue", tds.out_b, as.double(mvFlag),
		        PACKAGE = "rgdal")
		}
	}
	tds.out
}

gdalDrivers <- function() getGDALDriverNames()

toSigned <- function(x, base) {
    if (any(x < 0)) stop("already signed")
    if (storage.mode(x) != "integer") stop("band not integer")
    b_2 <- (2^(base-1)-1)
    b <- 2^base
    x[x > b_2] <- x[x > b_2] - b
    as.integer(x)
}

toUnSigned <- function(x, base) {
    if (all(x >= 0)) stop("already unsigned")
    if (storage.mode(x) != "integer") stop("band not integer")
    b <- 2^base
    x[x < 0] <- x[x < 0] + b
    as.integer(x)
}

"GDALSpatialRef" <- function(fname, silent=FALSE) {
	if (nchar(fname) == 0) stop("empty file name")
	x <- GDAL.open(fname, silent=silent)
        p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	GDAL.close(x)
        p4s
}

