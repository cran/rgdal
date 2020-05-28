\name{GDALcall}

\alias{GDALcall}
\alias{rawTransform}

\title{Wrapper functions to allow more direct calling of rgdal C code}

\description{
These functions allow more direct access to some of the rgdal C API. These are advanced methods intended for package developers only. 
}


\usage{
GDALcall(object, option, ...)
rawTransform(projfrom, projto, n, x, y, z=NULL, wkt=FALSE)
}


\arguments{
  \item{object}{GDALTransientDataset (option = 'SetGeoTransform', 'SetProject') or GDALRasterBand (the other options)}
  \item{option}{character. One of 'SetGeoTransform', 'SetProject', 'SetNoDataValue', 'SetStatistics', 'SetRasterColorTable' or 'SetCategoryNames') }
  \item{...}{additional arguments. The values to be set}
  \item{projfrom}{character. PROJ.4 coordinate reference system (CRS) description}
  \item{projto}{character. PROJ.4 CRS description}
  \item{n}{number of coordinates}
  \item{x}{x coordinates}
  \item{y}{y coordinates}
  \item{z}{z coordinates}
  \item{wkt}{default FALSE, if TRUE, the caller determines that projfrom and projto are wkt and that new_proj_and_gdal() returns TRUE to avoid multiple warnings when the function is called repetitively}
}

\value{
GDALcall does not return anything. 
rawTransform returns a matrix of transformed coordinates.
}

\author{Robert Hijmans}

\keyword{spatial}

