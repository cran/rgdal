\name{llgridlines}
\alias{llgridlines}
\title{Plot long-lat grid over projected data}
\description{ Plot long-lat grid over projected data }
\usage{
llgridlines(obj, easts, norths, ndiscr = 20, lty = 2, offset=0.5, side="WS",
llcrs = "+proj=longlat +datum=WGS84", plotLines = TRUE, plotLabels =
TRUE, ...)
}
\arguments{
  \item{obj}{object, deriving from \link[sp]{Spatial-class} having projection
  specified }
  \item{easts}{numeric; see \link[sp]{gridlines}}
  \item{norths}{numeric; see \link[sp]{gridlines}}
  \item{ndiscr}{numeric; see \link[sp]{gridlines}}
  \item{offset}{numeric; see \link[sp]{gridlines}}
  \item{side}{character, default \dQuote{WS}; see \link[sp]{gridlines}; available from \pkg{sp} 0.9-84}
  \item{lty}{line type to be used for grid lines}
\item{llcrs}{proj4string of longitude - latitude}
\item{plotLines}{logical; plot lines?}
\item{plotLabels}{logical; plot labels?}
\item{...}{ graphics arguments passed to plot function for lines
and text function for labels }
}

\value{
	none; side effect is that grid lines and lables are plotted
}
\seealso{\code{\link[sp]{is.projected}}, \code{\link[sp]{CRS-class}}}
\examples{
set_thin_PROJ6_warnings(TRUE)
data(meuse)
coordinates(meuse) = ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
plot(meuse) 
llgridlines(meuse, lty=3)
plot(meuse) 
llgridlines(meuse, lty=3, side = "EN", offset = 0.2)
}
\keyword{spatial}

