readGDAL = function(fname, offset, region.dim, ..., half.cell=c(0.5,0.5), silent = FALSE) {
#	if (!require(rgdal))
#		stop("read.gdal needs package rgdal to be properly installed")
	if (nchar(fname) == 0) stop("empty file name")
	x = GDAL.open(fname)
	d = dim(x)
	if (missing(offset)) offset <- c(0,0)
	if (missing(region.dim)) region.dim <- dim(x)[1:2] # rows=nx, cols=ny
#	else d <- region.dim
	if (!silent) {
		cat(paste(fname, "has GDAL driver", getDriverName(getDriver(x)),"\n"))
		cat(paste("and has", d[1], "rows and", d[2], "columns\n"))
	}
	p4s <- .Call("RGDAL_GetProjectionRef", x, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt = .Call('RGDAL_GetGeoTransform', x, PACKAGE="rgdal")
	# [1] 178400     40      0 334000      0    -40
	if (any(gt[c(3,5)] != 0.0)) {
		data = getRasterTable(x, offset=offset, 
			region.dim=region.dim, ...)
		GDAL.close(x)
		coordinates(data) = c(1,2)
	} else {
		data = getRasterData(x, offset=offset, 
			region.dim=region.dim, ...)
		GDAL.close(x)
		cellsize = abs(c(gt[2],gt[6]))
		ysign <- sign(gt[6])
#		cells.dim = c(d[1], d[2]) # c(d[2],d[1])
		co.x <- gt[1] + (offset[2] + half.cell[2]) * cellsize[1]
		co.y <- ifelse(ysign < 0, gt[4] + (ysign*((region.dim[1] + 
			offset[1]) + (ysign*half.cell[1]))) * abs(cellsize[2]),
			gt[4] + (ysign*((offset[1]) + (ysign*half.cell[1]))) * 
			abs(cellsize[2]))
		cellcentre.offset <- c(x=co.x, y=co.y)
#		cellcentre.offset = c(x = gt[1] + 0.5 * cellsize[1], 
#			y = gt[4] - (d[2] - 0.5) * abs(cellsize[2]))
		grid = GridTopology(cellcentre.offset, cellsize, 
			rev(region.dim))
		if (length(d) == 2)
			df = list(band1 = as.vector(data))
		else {
			df <- vector(mode="list", length=d[3])
			df[[1]] <- as.vector(data[,,1])
			for (band in 2:d[3])
				df[[band]] <- as.vector(data[,,band])
#			df = as.data.frame(df)
			names(df) = paste("band", 1:d[3], sep="")
		}
		data = SpatialGridDataFrame(grid = grid, 
			data = AttributeList(df), proj4string=CRS(p4s))
	}
	return(data)
}

writeGDAL = function(dataset, fname, drivername = "GTiff", type = "Float32", 
		mvFlag = NA, options=NULL)
#, clone = NULL
#) 
{
	if (nchar(fname) == 0) stop("empty file name")
	# stop("write.gdal is not working (yet>")
	stopifnot(gridded(dataset))
	fullgrid(dataset) = TRUE
	d.dim = dim(as.matrix(dataset[1]))
	d.drv = new("GDALDriver", drivername)
	nbands = length(names(slot(dataset, "data")))
#	if (!is.null(clone) && is.character(clone))
#		tds.out = new("GDALDataset", clone)
#	else {
		tds.out = new("GDALTransientDataset", driver = d.drv, 
			rows = d.dim[2], cols = d.dim[1],
        	bands = nbands, type = type, options = options, 
			handle = NULL)
		gp = gridparameters(dataset)
		cellsize = gp$cellsize
		offset = gp$cellcentre.offset
		dims = gp$cells.dim
		gt = c(offset[1] - 0.5 * cellsize[1], cellsize[1], 0.0, 
			offset[2] + (dims[2] -0.5) * cellsize[2], 0.0, -cellsize[2])
		.Call("RGDAL_SetGeoTransform", tds.out, gt, PACKAGE = "rgdal")
#	}
	if (!is.na(mvFlag))
		.Call("RGDAL_SetNoDataValue", tds.out, as.double(mvFlag), PACKAGE = "rgdal")
	p4s <- proj4string(dataset)
	if (!is.na(p4s) && nchar(p4s) > 0)
		.Call("RGDAL_SetProject", tds.out, p4s, PACKAGE = "rgdal")
	for (i in 1:nbands) {
		band = as.matrix(dataset[i])
		if (!is.na(mvFlag))
			band[is.na(band)] = mvFlag
		putRasterData(tds.out, band, i)
	}
	saveDataset(tds.out, fname)
	invisible(fname)
}
