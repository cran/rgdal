#require(methods, quietly = TRUE, warn.conflicts = FALSE)

.setCollectorFun <- function(object, fun) {

  if (is.null(fun)) fun <- function(obj) obj
  reg.finalizer(object, fun)

}

.assertClass <- function(object, class) {
  
  if (class %in% is(object))
    invisible(object)
  else
    stop(paste('Object is not a member of class', class))

}

.GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32',
                    'Int32', 'Float32', 'Float64', 'CInt16', 'CInt32',
                    'CFloat32', 'CFloat64')

setClass('GDALMajorObject',
         representation(handle = 'externalptr'))
 
getDescription <- function(object) {

  .assertClass(object, 'GDALMajorObject')

  .Call('RGDAL_GetDescription', object, PACKAGE="rgdal")

}

getMetadata <- function(object, domain = "") {

  .assertClass(object, 'GDALMajorObject')

  metadata <- .Call('RGDAL_GetMetadata', object,
                    as.character(domain), PACKAGE="rgdal")

  if (is.null(metadata))
    metadata
  else
    noquote(metadata)

}

setMetadata <- function(object, metadata) {

  .assertClass(object, 'GDALMajorObject')

  metadata <- lapply(as.list(metadata), as.character)

  .Call('RGDAL_SetMetadata', object, metadata, PACKAGE="rgdal")

  invisible(object)
  
}

appendMetadata <- function(object, metadata) {

  .assertClass(object, 'GDALMajorObject')

  setMetadata(object, append(getMetadata(object), metadata))

}

setClass('GDALDriver', 'GDALMajorObject')

setClass('GDALReadOnlyDataset', 'GDALMajorObject')

setClass('GDALDataset', 'GDALReadOnlyDataset')

setClass('GDALTransientDataset', 'GDALDataset')
         
setClass('GDALRasterBand', 'GDALMajorObject')

getGDALDriverNames <- function() .Call('RGDAL_GetDriverNames', PACKAGE="rgdal")

setMethod('initialize', 'GDALDriver',
          def = function(.Object, name, handle = NULL) {
            if (is.null(handle)) {
              slot(.Object, 'handle') <- {
                .Call('RGDAL_GetDriver', as.character(name), PACKAGE="rgdal")
              }
            } else {
              slot(.Object, 'handle') <- handle
            }
            .Object
          })

getDriverName <- function(driver) {

  .assertClass(driver, 'GDALDriver')

  .Call('RGDAL_GetDriverShortName', driver, PACKAGE="rgdal")

}

getDriverLongName <- function(driver) {

  .assertClass(driver, 'GDALDriver')

  .Call('RGDAL_GetDriverLongName', driver, PACKAGE="rgdal")

}

setMethod('initialize', 'GDALReadOnlyDataset',
          def = function(.Object, filename, handle = NULL) {
            if (is.null(handle)) {
	      if (nchar(filename) == 0) stop("empty file name")
              slot(.Object, 'handle') <- {
                .Call('RGDAL_OpenDataset', as.character(filename), 
			TRUE, PACKAGE="rgdal")
              }
            } else {
              slot(.Object, 'handle') <- handle
            }
            cfn <- function(handle) .Call('RGDAL_CloseHandle', 
		handle, PACKAGE="rgdal")
            .setCollectorFun(slot(.Object, 'handle'), cfn)
            .Object
          })

setMethod('initialize', 'GDALDataset',
          def = function(.Object, filename, handle = NULL) {
            if (is.null(handle)) {
	      if (nchar(filename) == 0) stop("empty file name")
              slot(.Object, 'handle') <- {
                .Call('RGDAL_OpenDataset', as.character(filename), 
			FALSE, PACKAGE="rgdal")
              }
            } else {
              slot(.Object, 'handle') <- handle
            }
            cfn <- function(handle) .Call('RGDAL_CloseHandle', 
		handle, PACKAGE="rgdal")
            .setCollectorFun(slot(.Object, 'handle'), cfn)
            .Object
          })

setMethod('initialize', 'GDALTransientDataset',
          def = function(.Object, driver, rows, cols, bands = 1,
            type = 'Byte', options = '', handle = NULL) {
            if (is.null(handle)) {
              typeNum <- match(type, .GDALDataTypes, 1) - 1
	      my_tempfile <- tempfile()
	      if (nchar(my_tempfile) == 0) stop("empty file name")
              slot(.Object, 'handle') <- .Call('RGDAL_CreateDataset', driver,
                                              as.integer(c(cols, rows, bands)),
                                              as.integer(typeNum),
                                              as.character(options),
                                              my_tempfile, PACKAGE="rgdal")
            } else {
              slot(.Object, 'handle') <- handle
            }
            cfn <- function(handle) .Call('RGDAL_CloseHandle', 
		handle, PACKAGE="rgdal")
            .setCollectorFun(slot(.Object, 'handle'), cfn)
            .Object
          })

getDriver <- function(dataset) {

  .assertClass(dataset, 'GDALReadOnlyDataset')

  new('GDALDriver',
      handle = .Call('RGDAL_GetDatasetDriver', dataset, PACKAGE="rgdal"))

}

copyDataset <- function(dataset, driver, strict = FALSE, options = '') {

  .assertClass(dataset, 'GDALReadOnlyDataset')
  
  if (missing(driver)) driver <- getDriver(dataset)
  
  my_tempfile <- tempfile()
  if (nchar(my_tempfile) == 0) stop("empty file name")
#  my_tempfile <- as.character(tempfile(tmpdir=.my_tempdir()))
  new.obj <- new('GDALTransientDataset',
                 handle = .Call('RGDAL_CopyDataset',
                   dataset, driver,
                   as.integer(strict),
                   as.character(options),
                   my_tempfile, PACKAGE="rgdal"))

  new.obj
  
}

saveDataset <- function(dataset, filename) {

  .assertClass(dataset, 'GDALReadOnlyDataset')
  
  new.class <- ifelse(class(dataset) == 'GDALTransientDataset',
                      'GDALDataset', class(dataset))
  
  if (nchar(filename) == 0) stop("empty file name")
  new.obj <- new(new.class,
                 handle = .Call('RGDAL_CopyDataset',
                   dataset, getDriver(dataset),
                   FALSE, NULL, filename, PACKAGE="rgdal"))

  invisible(new.obj)
  
}

setGeneric('closeDataset', function(dataset) standardGeneric('closeDataset'))

"closeDataset.default" <- function(dataset) 
	stop("No default method for closeDataset")

setMethod("closeDataset", signature("ANY"), closeDataset.default)

setMethod('closeDataset', 'GDALReadOnlyDataset',
          def = function(dataset) {
            .setCollectorFun(slot(dataset, 'handle'), NULL)
            .Call('RGDAL_CloseDataset', dataset, PACKAGE="rgdal")
            invisible()
          })

setMethod('closeDataset', 'GDALTransientDataset',
          def = function(dataset) {
            driver <- getDriver(dataset)
            filename <- getDescription(dataset)
            .Call('RGDAL_CloseDataset', driver, filename, PACKAGE="rgdal")
            callNextMethod()
          })


saveDatasetAs <- function(dataset, filename, driver = NULL) {

  .assertClass(dataset, 'GDALReadOnlyDataset')
  
  if (is.null(driver)) driver <- getDriver(dataset)
  
  new.obj <- new('GDALReadOnlyDataset',
                 handle = .Call('RGDAL_CopyDataset',
                   dataset, driver, FALSE, NULL, filename, PACKAGE="rgdal"))
  
  closeDataset(new.obj)
  
  err.opt <- getOption('show.error.messages')

  options(show.error.messages = FALSE)

  new.obj <- try(new('GDALDataset', filename))

  options(show.error.messages = err.opt)

  if (inherits(new.obj, 'try-error'))
    new.obj <- new('GDALReadOnlyDataset', filename)

  closeDataset(dataset)

  eval.parent(dataset <- new.obj)

  invisible(new.obj)
  
}


deleteDataset <- function(dataset) {

  .assertClass(dataset, 'GDALDataset')
  
  driver <- getDriver(dataset)
  
  filename <- getDescription(dataset)
  
  .Call('RGDAL_DeleteFile', driver, filename, PACKAGE="rgdal")
  
  closeDataset(dataset)

}

GDAL.open <- function(filename) {
  	if (nchar(filename) == 0) stop("empty file name")
	res <- new("GDALReadOnlyDataset", filename)
	res
}

GDAL.close <- function(dataset) {
            .setCollectorFun(slot(dataset, 'handle'), NULL)
            .Call('RGDAL_CloseDataset', dataset, PACKAGE="rgdal")
            invisible()
}

setMethod('dim', 'GDALReadOnlyDataset',
          def = function(x) {
            nrows <- .Call('RGDAL_GetRasterYSize', x, PACKAGE="rgdal")
            ncols <- .Call('RGDAL_GetRasterXSize', x, PACKAGE="rgdal")
            nbands <- .Call('RGDAL_GetRasterCount', x, PACKAGE="rgdal")
            if (nbands > 1)
              c(nrows, ncols, nbands)
            else
              c(nrows, ncols)
          })

getProjectionRef <- function(dataset) {

  .assertClass(dataset, 'GDALReadOnlyDataset')

  noquote(.Call('RGDAL_GetProjectionRef', dataset, PACKAGE="rgdal"))

}

putRasterData <- function(dataset,
                          rasterData,
                          band = 1,
                          offset = c(0, 0)) {

  .assertClass(dataset, 'GDALDataset')

  offset <- rep(offset, length.out = 2)
  
  rasterBand <- new('GDALRasterBand', dataset, band)
  
  .Call('RGDAL_PutRasterData', rasterBand, rasterData, 
	as.integer(offset), PACKAGE="rgdal")

}

getRasterTable <- function(dataset,
                           band = NULL,
                           offset = c(0, 0),
                           region.dim = dim(dataset)) {

  .assertClass(dataset, 'GDALReadOnlyDataset')

  offset <- rep(offset, length.out = 2)
  region.dim <- rep(region.dim, length.out = 2)

  rasterData <- getRasterData(dataset, band,
                              offset = offset,
                              region = region.dim)

  nbands <- .Call('RGDAL_GetRasterCount', dataset, PACKAGE="rgdal")

  if (is.null(band)) band <- 1:nbands

  dim(rasterData) <- c(region.dim, nbands)

  geoTrans <- getGeoTransFunc(dataset)

  # EJP, 06/01/05:
  #x.i <- 1:region.dim[1] + offset[1]
  #y.i <- 1:region.dim[2] + offset[2]
  y.i <- 1:region.dim[1] - 0.5 + offset[1]
  x.i <- 1:region.dim[2] - 0.5 + offset[2]

  y.i <- rep(y.i, each = length(x.i))
  x.i <- rep(x.i, len = prod(region.dim))

  out <- geoTrans(x.i, y.i)

  out <- cbind(out$x, out$y)
    for (b in band) { 
        vec <- as.numeric(rasterData[, , b])
        out <- cbind(out, vec)
    }
#for (b in band) out <- cbind(out, as.vector(rasterData[,,b]))

  out <- as.data.frame(out)
    
  # EJP, 06/01/05:
  #names(out) <- c('row', 'column', paste('band', 1:nbands, sep = ''))
  names(out) <- c('x', 'y', paste('band', 1:nbands, sep = ''))

  out

}
                           
getRasterData <- function(dataset,
                          band = NULL,
                          offset = c(0, 0),
                          region.dim = dim(dataset),
                          output.dim = region.dim,
                          interleave = c(0, 0),
                          as.is = FALSE) {

  .assertClass(dataset, 'GDALReadOnlyDataset')

  offset <- rep(offset, length.out = 2)
  region.dim <- rep(region.dim, length.out = 2)
  output.dim <- rep(output.dim, length.out = 2)
  interleave <- rep(interleave, length.out = 2)

  nbands <- .Call('RGDAL_GetRasterCount', dataset, PACKAGE="rgdal")

  if (is.null(band)) band <- 1:nbands
  
  x <- array(dim = as.integer(c(rev(output.dim), length(band))))

  for (i in seq(along=band)) {
  
    rasterBand <- new('GDALRasterBand', dataset, band[i])

    x[,,i] <- .Call('RGDAL_GetRasterData', rasterBand,
                      as.integer(c(offset, region.dim)),
                      as.integer(output.dim),
                      as.integer(interleave), PACKAGE="rgdal")
  
  }

  x <- drop(x)

  if (!as.is) {
  
    scale <- .Call('RGDAL_GetScale', rasterBand, PACKAGE="rgdal")
    offset <- .Call('RGDAL_GetOffset', rasterBand, PACKAGE="rgdal")

    if (scale != 1) x <- x * scale
    if (offset != 0) x <- x + offset
    
    catNames <- .Call('RGDAL_GetCategoryNames', rasterBand, PACKAGE="rgdal")
  
    if (!is.null(catNames)) {
      levels <- rep(min(x):max(x), len = length(catNames))
      x <- array(factor(x, levels, catNames), dim = dim(x),
                 dimnames = dimnames(x))
    }

  }

  x

}

getColorTable <- function(dataset, band = 1) {

  .assertClass(dataset, 'GDALReadOnlyDataset')

  if (length(band) > 1) stop("choose one band only")
  nbands <- .Call('RGDAL_GetRasterCount', dataset, PACKAGE="rgdal")
  if (!(band %in% 1:nbands)) stop("no such band")

  rasterBand <- new('GDALRasterBand', dataset, band)
  
  ctab <- .Call('RGDAL_GetColorTable', rasterBand, PACKAGE="rgdal") / 255

  if (length(ctab) == 0) return(NULL)

  if (.Call('RGDAL_GetColorInterp', rasterBand, PACKAGE="rgdal") == 'Palette')
    switch(.Call('RGDAL_GetPaletteInterp', rasterBand, PACKAGE="rgdal"),  
           RGB = rgb(ctab[,1], ctab[,2], ctab[,3]),
           HSV = hsv(ctab[,1], ctab[,2], ctab[,3]), # Doesn't actually exist
           Gray = gray(ctab[,1]),
           gray(apply(ctab, 2, mean)))
  else
    gray(ctab[,1])

}

displayDataset <- function(x, offset = c(0, 0), region.dim = dim(x),
                           reduction = 1, band = NULL, col = NULL,
                           max.dim = 500, ...) {

  .assertClass(x, 'GDALReadOnlyDataset')

  offset <- rep(offset, length.out = 2)
  region.dim <- rep(region.dim, length.out = 2)
  reduction <- rep(reduction, length.out = 2)

  nbands <- .Call('RGDAL_GetRasterCount', x, PACKAGE="rgdal")

  if (is.null(band)) band <- 1:nbands

  if (!length(band) %in% c(1, 3))
    stop("Only single band or 3 band RGB plotting supported")
  
  offset <- offset %% dim(x)[1:2]
  
  outOfBounds <- (region.dim + offset) > dim(x)[1:2]
  
  if (any(outOfBounds))
    region.dim[outOfBounds]  <- {
      dim(x)[outOfBounds] - offset[outOfBounds]
    }

  if (any(reduction < 1)) reduction[reduction < 1] <- 1

  plot.dim <- region.dim / reduction
            
  if (any(plot.dim > max.dim))
    plot.dim <- max.dim * plot.dim / max(plot.dim)

  if (any(plot.dim < 3))
    plot.dim <- 3 * plot.dim / max(plot.dim)

  pm <- getPixmapGDAL(x, col, band, offset, region.dim, plot.dim)

  plot(pm)

  invisible(pm)

}

setMethod('initialize', 'GDALRasterBand',
          def =  function(.Object, dataset, band = 1) {
            slot(.Object, 'handle') <- .Call('RGDAL_GetRasterBand',
                                            dataset, as.integer(band), 
					    PACKAGE="rgdal")
            .Object
          })

setMethod('dim', 'GDALRasterBand',
          def = function(x) {
            c(.Call('RGDAL_GetYSize', x, PACKAGE="rgdal"),
              .Call('RGDAL_GetXSize', x, PACKAGE="rgdal"))
          })

getGeoTransFunc <- function(dataset) {

  geoTrans <- .Call('RGDAL_GetGeoTransform', dataset, PACKAGE="rgdal")

  # EJP, 06/01/05:
  #rotMat <- matrix(geoTrans[c(6, 5, 3, 2)], 2)
  rotMat <- matrix(geoTrans[c(2, 3, 5, 6)], 2)

  # EJP, 06/01/05:
  #offset <- geoTrans[c(4, 1)]
  offset <- geoTrans[c(1, 4)]

  function(x, y) {

    x <- cbind(x, y)

    x <- x %*% rotMat

    list(x = x[,1] + offset[1],
         y = x[,2] + offset[2])

  }

}

.First.lib <- function(lib, pkg) {

  require(methods, quietly = TRUE, warn.conflicts = FALSE)

  library.dynam('rgdal', pkg, lib)

  .Call('RGDAL_Init', PACKAGE="rgdal")

  cat('Geospatial Data Abstraction Library ')
  cat('extensions to R successfully loaded\n')
  
}
