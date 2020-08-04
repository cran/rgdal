\name{displayDataset}
\alias{displayDataset}
\title{Display a GDAL dataset}
\description{Display a GDAL dataset allowing for subscenes and decimation, allowing very large images to be browsed}
\usage{
displayDataset(x, offset=c(0, 0), region.dim=dim(x), reduction = 1, 
  band = 1, col = NULL, reset.par = TRUE, max.dim = 500, ...)
}
\arguments{
  \item{x}{a three-band GDALReadOnlyDataset object}
  \item{offset}{Number of rows and columns from the origin (usually the
    upper left corner) to begin reading from; presently ordered (y,x) - this may change}
  \item{region.dim}{The number of rows and columns to read from the
    dataset; presently ordered (y,x) - this may change}
  \item{reduction}{a vector of length 1 or 2 recycled to 2 for decimating the input data, 1 retains full resultion, higher values decimate}
  \item{band}{The band number (1-based) to read from}
  \item{col}{default NULL, attempt to use band colour table and default to grey scale if not available}
  \item{reset.par}{default TRUE - reset par() settings on completion}
  \item{max.dim}{default 500, forcing the image to a maximum dimension of the value}
  \item{...}{arguments passed to image.default()}
}
\value{
a list of the image data, the colour table, and the par() values on entry.
}
\references{\url{https://gdal.org/}}
\author{Tim Keitt}

\examples{
\dontrun{
logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x <- GDAL.open(logo)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
displayDataset(x, band=1, reset.par=FALSE)
displayDataset(x, band=2, reset.par=FALSE)
#displayDataset(x, band=3, reset.par=TRUE)
par(opar)
dx <- RGB2PCT(x, band=1:3)
displayDataset(dx, reset.par=FALSE)
GDAL.close(x)
GDAL.close(dx)
}
}
\keyword{spatial}

