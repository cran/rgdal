setClass("SpatialGridDataFrameGDAL",
    representation("SpatialGridDataFrame", grod = "GDALReadOnlyDataset", name = "character"),
    validity = function(object) {
        if (is.null(object@grod))
            stop("grod is NULL; this should not happen")
		if (nrow(object@data) > 0)
			stop("data slot should have zero rows")
        return(TRUE)
    }
)

open.SpatialGridDataFrameGDAL = function(con, ..., silent = FALSE) {
	if (nchar(con) == 0) stop("empty file name")

	grod = GDAL.open(con)

	d = dim(grod)
	if (!silent) {
		cat(paste(con, "has GDAL driver", getDriverName(getDriver(grod)),"\n"))
		cat(paste("and has", d[1], "rows and", d[2], "columns\n"))
	}
	p4s <- .Call("RGDAL_GetProjectionRef", grod, PACKAGE="rgdal")
	if (nchar(p4s) == 0) p4s <- as.character(NA)
	gt = .Call('RGDAL_GetGeoTransform', grod, PACKAGE="rgdal")
	# [1] 178400     40      0 334000      0    -40
	if (any(gt[c(3,5)] != 0.0)) 
		stop("rotated grid cannot be read; try readGDAL to read as points")
	cellsize = abs(c(gt[2],gt[6]))
	ysign <- sign(gt[6])
	output.dim <- dim(grod)[1:2] # rows=nx, cols=ny
	half.cell <- c(0.5, 0.5)
	co.x <- gt[1] + (half.cell[2]) * cellsize[1]
	co.y <- ifelse(ysign < 0, gt[4] + (ysign*((output.dim[1]) + (ysign*half.cell[1]))) * abs(cellsize[2]),
		gt[4] + (ysign*((offset[1]) + (ysign*half.cell[1]))) * 
		abs(cellsize[2]))
	cellcentre.offset <- c(x=co.x, y=co.y)
	grid = GridTopology(cellcentre.offset, cellsize, rev(output.dim))
	data = new("SpatialGridDataFrameGDAL", 
		bbox = bbox(SpatialGrid(grid)),
		proj4string = CRS(p4s), 
		grid = grid,
		data = data.frame(), 
		grod = grod,
		name = con)
	return(data)
}

close.SpatialGridDataFrameGDAL = function(con, ...) {
	GDAL.close(con@grod)
	invisible(NULL)
}

setAs("SpatialGridDataFrameGDAL", "SpatialGridDataFrame",
	function(from) from[]
)

setAs("SpatialGridDataFrameGDAL", "SpatialPixelsDataFrame",
	function(from) as(from[], "SpatialPixelsDataFrame")
)

setMethod("[", "SpatialGridDataFrameGDAL",
	function(x, i, j, ... , drop = FALSE)
		x@grod[i = i, j = j, ...]
)

setMethod("[[", c("SpatialGridDataFrameGDAL", "ANY", "missing"),
    function(x, i, j, ...)
        x[,,i][[1]] # efficient: reads only band i
)

# avoid inheritance:
#setMethod("$", c("SpatialGridDataFrameGDAL", "ANY"),
#    function(x, name)
#        stop("use [[ with numeric index to select single band")
#)

# avoid inheritance:
#setReplaceMethod("$", c("SpatialGridDataFrameGDAL", "character", "ANY"),
#    function(x, name, value)
#		stop("no replacement method available")
#)

# avoid inheritance:
#setReplaceMethod("[[", c("SpatialGridDataFrameGDAL", "ANY", "missing", "ANY"),
#    function(x, i, j, value)
#		stop("no replacement method available")
#)

setMethod("summary", "SpatialGridDataFrameGDAL",
	function(object, ...) {
		obj = list()
		obj$grid = object@grid
		obj$name = object@name
		class(obj) = "summary.SpatialGridDataFrameGDAL"
		obj
	}
)

print.summary.SpatialGridDataFrameGDAL = function(x, ...) {
	cat(paste("object of class", class(x), "\n"))
	cat(paste("file name:", x$name, "\n"))
	print(x$grid)
}
