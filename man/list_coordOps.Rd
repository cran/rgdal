\name{list_coordOps}
\alias{list_coordOps}
\alias{best_instantiable_coordOp}
\alias{print.coordOps}

\title{List PROJ 6 coordinate operations}
\description{List PROJ 6 coordinate operations for a pair of source/target coordinate reference systems}
\usage{
list_coordOps(src_crs, tgt_crs, area_of_interest = as.numeric(NA), 
 strict_containment = FALSE, visualization_order = NULL)
best_instantiable_coordOp(x)
\method{print}{coordOps}(x, ...)
}

\arguments{
  \item{src_crs}{Source coordinate reference system string}
  \item{tgt_crs}{Target coordinate reference system string}
  \item{area_of_interest}{Numeric vector; either \code{NA}, or the xmin, ymin, xmax, ymax of the bounding box of the area of interest. This may be used to restrict the search for coordinate operations}
  \item{strict_containment}{default FALSE, permit partial matching of the area of interest; if TRUE strictly contain the area of interest. The area of interest is either as given, or as implied by the source/target coordinate reference systems (FIXME)}
  \item{visualization_order}{default NULL, taking the value of \code{get_enforce_xy()}; if TRUE always choose \code{x} or longitude for the first axis; if FALSE, follow the axis orders given by the coordinate reference systems when constructing the coordinate operation}
  \item{x}{an object of class \code{"coordOps"}}
  \item{...}{arguments possibly passed through, unused}
}
\details{(FIXME)}
\value{
A data frame with rows showing the coordinate operations found, and columns:
  \item{description}{String describing the operation}
  \item{definition}{PROJ pipeline for executing the operation}
  \item{accuracy}{Accuracy in meters, if negative, unknown}
  \item{instantiable}{Can this operation be carried out with available resources}
  \item{ballpark}{Does this operation only have ballpark accuracy}
  \item{number_grids}{The number of grids required for the operation}
The object has a \code{"grids"} attribute containing a nested list of grids for each coordinate operations found; if \code{number_grids == 0}, NULL, otherwise a list of grids. For each grid required, the short and long names of the grid are given, the package name if available in a PROJ grid package, and the download URL for that package. Three logical variables report whether the grid may be downloaded directly, whether it has an open license, and whether it is available. 
}
\references{
\url{https://proj.org/}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{
Fragile: work in progress
}
\examples{
run <- new_proj_and_gdal()
if (run) {
  discarded_datum <- showSRID("EPSG:27700", "PROJ")
  (x <- list_coordOps(paste0(discarded_datum, " +type=crs"), "OGC:CRS84"))
}
if (run) {
  best_instantiable_coordOp(x)
}
if (run) {
  restored_datum <- showSRID("EPSG:27700", "PROJ")
  list_coordOps(paste0(restored_datum, " +datum=OSGB36 +type=crs"), "OGC:CRS84")
}
if (run) {
  wkt_datum <- showSRID("EPSG:27700", "WKT2")
  (x <- list_coordOps(wkt_datum, "OGC:CRS84"))
}
if (run) {
  best_instantiable_coordOp(x)
}
if (run) {
  list_coordOps("EPSG:27700", "OGC:CRS84")
}
if (run) {
}
if (run) {
  discarded_datum <- showSRID("EPSG:22525", "PROJ")
  list_coordOps(paste0(discarded_datum, " +type=crs"), "EPSG:31985")
}
if (run) {
}
if (run) {
  wkt_datum <- showSRID("EPSG:22525", "WKT2")
  list_coordOps(wkt_datum, "EPSG:31985")
}
if (run) {
  (x <- list_coordOps("EPSG:22525", "EPSG:31985"))
}
if (run) {
  best_instantiable_coordOp(x)
}
}

\keyword{spatial}
