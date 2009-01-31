GDALinfo <- function(fname) {
	if (nchar(fname) == 0) stop("empty file name")
	x <- GDAL.open(fname)
	d <- dim(x)[1:2]
        dr <- getDriverName(getDriver(x))
	p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt <- .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
	nbands <- .Call('RGDAL_GetRasterCount', x, PACKAGE="rgdal")
        if (nbands < 1) warning("no bands in dataset")
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
	attr(res, "driver") <- dr 
	attr(res, "projection") <- p4s 
	attr(res, "file") <- fname
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
	cat("oblique.x  ", x[8], "\n")
	cat("oblique.y  ", x[9], "\n")
	cat("driver     ", attr(x, "driver"), "\n")
	cat("projection ", paste(strwrap(attr(x, "projection")),
		collapse="\n"), "\n")
	cat("file       ", attr(x, "file"), "\n")
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
	x = GDAL.open(fname)
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
			span <- icellsize * rev(d[1:2])
# bug report Mike Sumner 070215
			cellsize <- span / rev(output.dim)
		}
		ysign <- sign(gt[6])
#		cells.dim = c(d[1], d[2]) # c(d[2],d[1])
		co.x <- gt[1] + (offset[2] + half.cell[2]) * cellsize[1]
		co.y <- ifelse(ysign < 0, gt[4] + (ysign*((output.dim[1] + 
			offset[1]) + (ysign*half.cell[1]))) * abs(cellsize[2]),
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
		mvFlag = NA, options=NULL)
{
	if (nchar(fname) == 0) stop("empty file name")
	tds.out <- create2GDAL(dataset=dataset, drivername=drivername, 
		type=type, mvFlag=mvFlag, options=options)
	saveDataset(tds.out, fname, options=options)
# RSB 081030 GDAL.close cleanup
#	tmp.obj <- saveDataset(tds.out, fname, options=options)
#        GDAL.close(tmp.obj)
	invisible(fname)
}

create2GDAL = function(dataset, drivername = "GTiff", type = "Float32", mvFlag = NA, options=NULL)
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
        	bands = nbands, type = type, options = options, 
		handle = NULL)
	gt = c(offset[1] - 0.5 * cellsize[1], cellsize[1], 0.0, 
		offset[2] + (dims[2] -0.5) * cellsize[2], 0.0, -cellsize[2])
	.Call("RGDAL_SetGeoTransform", tds.out, gt, PACKAGE = "rgdal")

	p4s <- proj4string(dataset)
	if (!is.na(p4s) && nchar(p4s) > 0)
		.Call("RGDAL_SetProject", tds.out, p4s, PACKAGE = "rgdal")
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
