SGDF2RGB <- function(x, ncolors=256) {
	if (!inherits(x, "SpatialGridDataFrame")) 
		stop("SpatialGridDataFrame required")
	if (length(names(slot(x, "data"))) != 3)
		stop("Three data columns (red, green, blue) required")
	if (ncolors > 256) {
		warning("ncolors reset to maximum 256")
		ncolors <- 256
	}
	if (ncolors < 2) {
		warning("ncolors reset to minimum 2")
		ncolors <- 2
	}

	fullgrid(x) = TRUE
	d.dim <- dim(as.matrix(x[1]))
	d.drv <- new("GDALDriver", "GTiff")
	GTiff3B <- new("GDALTransientDataset", driver = d.drv, 
		rows = d.dim[2], cols = d.dim[1], bands = 3, type = "Byte", 
		handle = NULL)
	gp <- gridparameters(x)
	cellsize <- gp$cellsize
	offset <- gp$cellcentre.offset
	dims <- gp$cells.dim
	gt <- c(offset[1] - 0.5 * cellsize[1], cellsize[1], 0.0, 
		offset[2] + (dims[2] -0.5) * cellsize[2], 0.0, -cellsize[2])
	.Call("RGDAL_SetGeoTransform", GTiff3B, gt, PACKAGE = "rgdal")
	for (i in 1:3) {
		band = as.matrix(x[i])
		if (!is.numeric(band)) stop("Numeric bands required")
		bmax <- max(band)
		bmin <- min(band)
		if (bmax == bmin) bmax <- bmin + ncolors
		band <- floor((band - bmin)/((bmax-bmin)/(255)))
		storage.mode(band) <- "integer"
		putRasterData(GTiff3B, band, i)
	}
	GTiff3B.tif <- tempfile()
	GTiff3Bs <- saveDataset(GTiff3B, GTiff3B.tif)
	GTiff1I.tif <- as.character(tempfile())

	cmd <- paste("rgb2pct.py -n", ncolors, GTiff3B.tif, GTiff1I.tif)
	system(cmd)

	GTiff1Is <- GDAL.open(GTiff1I.tif)
	output <- getRasterData(GTiff1Is, band=1)
	idx <- as.vector(output)
	ct <- getColorTable(GTiff1Is, band=1)
	res <- list(idx=idx, ct=ct[1:ncolors])
	GDAL.close(GTiff3B)
	GDAL.close(GTiff3Bs)
	GDAL.close(GTiff1Is)
	res
}
