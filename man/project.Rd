\name{project}
\alias{project}

\title{Projection of coordinate matrices}
\description{
  Interface to the PROJ.4 library of projection functions for geographical position data, no datum transformation possible. Use \code{spTransform()} for extended support.
}
\usage{
project(xy, proj, inv = FALSE, use_ob_tran=FALSE, legacy=TRUE,
 allowNAs_if_not_legacy=FALSE, coordOp = NULL, verbose = FALSE,
 use_aoi=TRUE)
}

\arguments{
  \item{xy}{ 2-column matrix of coordinates }
  \item{proj}{ character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation, in particular there cannot be any white space in +<arg>=<value> strings, and successive such strings can only be separated by blanks. }
  \item{inv}{ default FALSE, if TRUE inverse projection to geographical coordinates }
  \item{use_ob_tran}{default FALSE, if TRUE and \dQuote{+proj=ob_tran}, use General Oblique Transformation with internalised from/to projection reversal; the user oblique transforms forward rather than inverse.}
  \item{legacy}{default TRUE, if FALSE, use transform C functions (enforced internally for Windows 32-bit platforms)}
  \item{allowNAs_if_not_legacy}{used if legacy is FALSE, default FALSE; introduced to handle use of NAs as object separators in \pkg{oce}}
  \item{coordOp}{default NULL, for PROJ >= 6 used to pass through a pre-defined coordinate operation}
  \item{verbose}{default FALSE, for PROJ >=6 used to show the coordinate operation used}
  \item{use_aoi}{With PROJ >= 6, use the area of interest defined as the range of \code{xy} in limiting the search for candidate coordinate operations; set FALSE if \code{use_ob_tran} is TRUE}
}
\details{
  Full details of projection arguments available from website below, and examples in file "epsg" in the data directory installed with PROJ.4.

Note that from PROJ.4 4.9.3, the definition of UTM is changed from TMERC to ETMERC; see example.
}
\value{
  A two column matrix with projected coordinates.
}
\references{\url{https://proj.org/}}
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
broke_proj <- FALSE
pv <- .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]]
# https://github.com/OSGeo/PROJ/issues/1525
if (pv >= 600 && pv < 620) broke_proj <- TRUE
if (!broke_proj) {
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
}
#
getPROJ4VersionInfo()
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
#pv <- attr(getPROJ4VersionInfo(), "short")
#if (pv < 500) {  
# valgrind leakages in some cases for PROJ >= 5; many non-projection proj values added
# available projections and their inverses if provided
# For >=4.9.3 returns non-finite points rather than needing crash protection
projs <- as.character(projInfo()$name)
res <- logical(length(projs))
names(res) <- projs
msgs <- character(length(projs))
names(msgs) <- projs
owarn <- options("warn")$warn
options(warn=2L)
for (i in seq(along=res)) {
  iprs <- paste("+proj=", projs[i], sep="")
  xy <- try(project(cbind(0, 0), iprs, legacy=TRUE, use_aoi=FALSE), silent=TRUE)
  if (inherits(xy, "try-error")) {
    res[i] <- NA
    msgs[i] <- paste("fwd:", strsplit(xy, "\n")[[1]][2])
  } else if(any(abs(xy) > 1e+08)) {
    res[i] <- NA
    msgs[i] <- paste("fwd: huge value")
  } else {
    out <- try(project(xy, iprs, inv=TRUE, legacy=TRUE, use_aoi=FALSE), silent=TRUE)
    if (inherits(out, "try-error")) {
      res[i] <- NA
      msgs[i] <- paste("inv:", strsplit(out, "\n")[[1]][2])
    } else {
      res[i] <- isTRUE(all.equal(cbind(0,0), out))
    }
  }
}
options(warn=owarn)
df <- data.frame(res=unname(res), msgs=unname(msgs), row.names=names(res))
# projection and inverse projection failures
# fwd: missing parameters
# inv: mostly inverse not defined
df[is.na(df$res),]
# inverse not equal to input
# (see http://lists.maptools.org/pipermail/proj/2011-November/006015.html)
df[!is.na(df$res) & !df$res,]
# inverse equal to input
row.names(df[!is.na(df$res) & df$res,])
#}
# oce data representation with NAs
ll <- structure(c(12.1823368669203, 11.9149630062421, 12.3186076188739, 
12.6207597184845, 12.9955172054652, 12.6316117692658, 12.4680041846297, 
12.4366882666609, NA, NA, -5.78993051516384, -5.03798674888479, 
-4.60623015708619, -4.43802336997614, -4.78110320396188, -4.99127125409291, 
-5.24836150474498, -5.68430388755925, NA, NA), .Dim = c(10L, 
2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(xy0 <- project(ll, "+proj=moll", legacy=TRUE))
if (!PROJis6ormore()) { # legacy=TRUE PROJ >= 6
try(xy1 <- project(ll, "+proj=moll", legacy=FALSE, allowNAs_if_not_legacy=FALSE))
try(xy2 <- project(ll, "+proj=moll", legacy=FALSE, allowNAs_if_not_legacy=TRUE))
if (exists("xy0")) all.equal(xy0, xy2)
}
if (!exists("xy0")) xy0 <- structure(c(1217100.8468177, 1191302.229156,
1232143.28841193, 1262546.27733232, 1299648.82357849, 1263011.18154638,
1246343.17808186, 1242654.33986052, NA, NA, -715428.207551599,
-622613.577983058, -569301.605757784, -548528.530156422, -590895.949857199,
-616845.926397351, -648585.161643274, -702393.1160979, NA, NA), 
.Dim = c(10L, 2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(ll0 <- project(xy0, "+proj=moll", inv=TRUE, legacy=TRUE))
if (!PROJis6ormore()) { # legacy=TRUE PROJ >= 6
try(ll1 <- project(xy0, "+proj=moll", inv=TRUE, legacy=FALSE, allowNAs_if_not_legacy=FALSE))
try(ll2 <- project(xy0, "+proj=moll", inv=TRUE, legacy=FALSE, allowNAs_if_not_legacy=TRUE))
if (exists("ll0")) all.equal(ll0, ll2)
}
if (exists("ll0")) all.equal(ll0, ll)
}
\keyword{spatial}
