getPixmapGDAL <- function(dataset,
                          col = NULL,
                          band = NULL,
                          offset = c(0, 0),
                          region.dim = dim(dataset),
                          output.dim = region.dim,
                          interleave = c(0, 0),
                          stretch.bands = TRUE,
                          as.is = FALSE) {
	.assertClass(dataset, 'GDALReadOnlyDataset')
	require(pixmap)
	nbands <- .Call('RGDAL_GetRasterCount', dataset, PACKAGE="rgdal")
	if (is.null(band)) band <- 1:nbands
        nbands <- length(band)
	if (nbands == 1) {
                if (is.null(col)) col <- getColorTable(dataset)
                data <- t(getRasterData(dataset=dataset, band=1,
                           offset=offset, region.dim=region.dim,
                           output.dim=output.dim, interleave=interleave,
			   as.is=as.is))
                res <- if(is.null(col))
                  pixmapGrey(data)
                else
                  pixmapIndexed(data, col)
	} else if (nbands == 3) {
		require(abind)
		bb <- vector(mode="list", length=3)
		for (i in 1:3) {
			bb[[i]] <- t(getRasterData(dataset=dataset,
					 band=band[i],
				offset=offset, region.dim=region.dim,
				output.dim=output.dim, interleave=interleave,
				as.is=as.is))
		}
                if (stretch.bands) {
                  f <- function(x) {
                    dr <- diff(range(x))
                    if(dr > 0) (x - min(x)) / dr else x
                  }
                  bb <- lapply(bb, f)
                }
		res <- abind(bb[[1]], bb[[2]], bb[[3]], along=3)
		res <- pixmapRGB(res)
		if (!is.null(col)) warning("Three RGB bands used, col argument discarded")
	} else stop (paste("Number of bands", nbands, "neither 1 nor 3"))
	res
}
