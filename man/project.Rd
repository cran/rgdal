\name{project}
\alias{project}

\title{Projection of coordinate matrices}
\description{
  Interface to the PROJ.4 library of projection functions for geographical position data, no datum transformation possible. Use \code{spTransform()} for extended support.
}
\usage{
project(xy, proj, inv = FALSE, use_ob_tran=FALSE)
}

\arguments{
  \item{xy}{ 2-column matrix of coordinates }
  \item{proj}{ character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  \item{inv}{ default FALSE, if TRUE inverse projection to geographical coordinates }
  \item{use_ob_tran}{default FALSE, if TRUE and \dQuote{+proj=ob_tran} and inv=TRUE, use General Oblique Transformation with from/to projection reversal}
}
\details{
  Full details of projection arguments available from website below, and examples in file "epsg" in the data directory installed with PROJ.4.
}
\value{
  A two column matrix with projected coordinates.
}
\references{\url{http://proj.maptools.org/}}
\author{Barry Rowlingson, Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{ The locations of Hawaii and Alaska in the data source are (putting it mildly) arbitrary, please avoid airlines using these positions.}

\seealso{ \code{\link[sp]{CRS-class}}, \code{\link{spTransform-methods}} }

\examples{
data(state)
res <- project(cbind(state.center$x, state.center$y),
 "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100")
res1 <- project(res, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100", inv=TRUE)
summary(res1 - cbind(state.center$x, state.center$y))
plot(cbind(state.center$x, state.center$y), asp=1, type="n")
text(cbind(state.center$x, state.center$y), state.abb)
plot(res, asp=1, type="n")
text(res, state.abb)
crds <- matrix(data=c(9.05, 48.52), ncol=2)
project(crds*(pi/180), paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 inv=TRUE)
project(crds, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 inv=TRUE, use_ob_tran=TRUE)
#should be (-5.917698  -1.87195)
#added after posting by Martin Ivanov
}
\keyword{spatial}
