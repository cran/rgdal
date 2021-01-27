\name{RGB2PCT}
\alias{RGB2PCT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert RGB three band to single band colour table}
\description{
  This function converts a three-band GDALReadOnlyDataset into a single band of colour indices as a GDALTransientDataset. 
}
\usage{
RGB2PCT(x, band, driver.name = 'MEM', ncolors = 256, set.ctab = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a three-band GDALReadOnlyDataset object}
  \item{band}{a vector of numbers, recycled up to 3 in length}
  \item{driver.name}{default MEM}
  \item{ncolors}{a number of colours between 2 and 256}
  \item{set.ctab}{default TRUE, when the dithered dataset handle is returned, otherwise a list of the dataset and the PCT colour table}
}

\value{
The value returned is a either GDALTransientDataset or a list of a GDALTransientDataset and a colour table.
}
\references{\url{https://gdal.org/}}
\author{Tim Keitt}

\examples{
\dontrun{
logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
x <- GDAL.open(logo)
dim(x)
dx <- RGB2PCT(x, band=1:3)
displayDataset(dx, reset.par=FALSE)
dim(dx)
GDAL.close(x)
GDAL.close(dx)
}
}
\keyword{spatial}
