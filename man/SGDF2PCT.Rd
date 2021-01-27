\name{SGDF2PCT}
\alias{SGDF2PCT}
\alias{vec2RGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert RGB three band to single band colour table}
\description{
  This function converts a three-band SpatialGridDataFrame into a single band of colour indices and a colour look-up table using \code{RGB2PCT}. \code{vec2RGB} uses given breaks and colours (like \code{image}) to make a three column matrix of red, green, and blue values for a numeric vector. 
}
\usage{
SGDF2PCT(x, ncolors = 256, adjust.bands=TRUE)
vec2RGB(vec, breaks, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a three-band SpatialGridDataFrame object}
  \item{ncolors}{a number of colours between 2 and 256}
  \item{adjust.bands}{default TRUE; if FALSE the three bands must lie each between 0 and 255, but will not be streched within those bounds}
  \item{vec}{a numeric vector}
  \item{breaks}{a set of breakpoints for the colours: must give one more breakpoint than colour}
  \item{col}{a list of colors}
}

\value{
The value returned is a list:
  \item{idx}{a vector of colour indices in the same spatial order as the input object}
  \item{ct}{a vector of RGB colours}
}
\references{\url{https://gdal.org/}}
\author{Roger Bivand}

\examples{
logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
SGlogo <- readGDAL(logo)
cols <- SGDF2PCT(SGlogo)
SGlogo$idx <- cols$idx
image(SGlogo, "idx", col=cols$ct)
SGlogo <- readGDAL(logo)
cols <- SGDF2PCT(SGlogo, ncolors=64)
SGlogo$idx <- cols$idx
image(SGlogo, "idx", col=cols$ct)
SGlogo <- readGDAL(logo)
cols <- SGDF2PCT(SGlogo, ncolors=8)
SGlogo$idx <- cols$idx
image(SGlogo, "idx", col=cols$ct)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- TRUE
fullgrid(meuse.grid) <- TRUE
summary(meuse.grid$dist)
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2), mar=c(1,1,1,1)+0.1)
image(meuse.grid, "dist", breaks=seq(0,1,1/10), col=bpy.colors(10))
RGB <- vec2RGB(meuse.grid$dist, breaks=seq(0,1,1/10), col=bpy.colors(10))
summary(RGB)
meuse.grid$red <- RGB[,1]
meuse.grid$green <- RGB[,2]
meuse.grid$blue <- RGB[,3]
cols <- SGDF2PCT(meuse.grid[c("red", "green", "blue")], ncolors=10,
 adjust.bands=FALSE)
is.na(cols$idx) <- is.na(meuse.grid$dist)
meuse.grid$idx <- cols$idx
image(meuse.grid, "idx", col=cols$ct)
par(opar)
# Note: only one wrongly classified pixel after NA handling/dropping
# The functions are not written to be reversible
sort(table(findInterval(meuse.grid$dist, seq(0,1,1/10), all.inside=TRUE)))
sort(table(cols$idx))
}
\keyword{spatial}
