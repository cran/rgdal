\name{SGDF2RGB}
\alias{SGDF2RGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert RGB three band to single band colour table}
\description{
  This function converts a three-band SpatialGridDataFrame into a single band of colour indices and a colour look-up table. The function is in preliminary form only.
}
\usage{
SGDF2RGB(x, ncolors = 256)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a three-band SpatialGridDataFrame object}
  \item{ncolors}{a number of colours between 2 and 256}
}
\details{
  The function makes a \code{system()} call to the GDAL utility rgb2pct.py, which is only available if GDAL has been built with Python support. It is hoped to replace this extra dependency with compiled code if the function proves useful.
}
\value{
The value returned is a list:
  \item{idx}{a vector of colour indices in the same spatial order as the input object}
  \item{ct}{a vector of RGB colours}
}
\references{\url{http://www.remotesensing.org/gdal/}}
\author{Roger Bivand}

\examples{
\dontrun{
  logo <- system.file("pictures/Rlogo.jpg", package="rgdal")[1]
  SGlogo <- readGDAL(logo)
  cols <- SGDF2RGB(SGlogo)
  SGlogo$idx <- cols$idx
  image(SGlogo, "idx", col=cols$ct)
  SGlogo <- readGDAL(logo)
  cols <- SGDF2RGB(SGlogo, ncolors=64)
  SGlogo$idx <- cols$idx
  image(SGlogo, "idx", col=cols$ct)
}
}
\keyword{spatial}
