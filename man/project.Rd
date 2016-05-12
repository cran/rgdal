\name{project}
\alias{project}

\title{Projection of coordinate matrices}
\description{
  Interface to the PROJ.4 library of projection functions for geographical position data, no datum transformation possible. Use \code{spTransform()} for extended support.
}
\usage{
project(xy, proj, inv = FALSE, use_ob_tran=FALSE, legacy=TRUE)
}

\arguments{
  \item{xy}{ 2-column matrix of coordinates }
  \item{proj}{ character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  \item{inv}{ default FALSE, if TRUE inverse projection to geographical coordinates }
  \item{use_ob_tran}{default FALSE, if TRUE and \dQuote{+proj=ob_tran}, use General Oblique Transformation with internalised from/to projection reversal; the user oblique transforms forward rather than inverse.}
  \item{legacy}{default TRUE, if FALSE, use transform C functions (enforced internally for Windows 32-bit platforms)}
}
\details{
  Full details of projection arguments available from website below, and examples in file "epsg" in the data directory installed with PROJ.4.

Note that from PROJ.4 4.9.3, the definition of UTM is changed from TMERC to ETMERC; see example.
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
 "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
res1 <- project(res, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84",
 inv=TRUE)
summary(res1 - cbind(state.center$x, state.center$y))
plot(cbind(state.center$x, state.center$y), asp=1, type="n")
text(cbind(state.center$x, state.center$y), state.abb)
plot(res, asp=1, type="n")
text(res, state.abb)
crds <- matrix(data=c(9.05, 48.52), ncol=2)
a <- project(crds, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
a
#should be (-5.917698, -1.87195)
project(a, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 inv=TRUE, use_ob_tran=TRUE)
#added after posting by Martin Ivanov
#
# Test for UTM == TMERC (<= 4.9.2) or UTM == ETMERC (> 4.9.2)
nhh <- matrix(c(5.304234, 60.422311), ncol=2)
nhh_utm_32N_P4 <- project(nhh, "+init=epsg:3044")
nhh_tmerc_P4 <- project(nhh, paste("+proj=tmerc +k=0.9996 +lon_0=9",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
nhh_etmerc_P4 <- project(nhh, paste("+proj=etmerc +k=0.9996 +lon_0=9",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
all.equal(nhh_utm_32N_P4, nhh_tmerc_P4, tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(nhh_utm_32N_P4, nhh_etmerc_P4, tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
unis <- matrix(c(15.653453, 78.222504), ncol=2)
unis_utm_33N_P4 <- project(unis, "+init=epsg:3045")
unis_tmerc_P4 <- project(unis, paste("+proj=tmerc +k=0.9996 +lon_0=15",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
unis_etmerc_P4 <- project(unis, paste("+proj=etmerc +k=0.9996 +lon_0=15",
 "+x_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
all.equal(unis_utm_33N_P4, unis_tmerc_P4, tolerance=1e-9, scale=1)
# UTM == TMERC: PROJ4 <=4.9.2
all.equal(unis_utm_33N_P4, unis_etmerc_P4, tolerance=1e-9, scale=1)
# UTM == ETMERC: PROJ4 > 4.9.2
}
\keyword{spatial}
