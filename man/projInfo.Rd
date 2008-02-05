\name{projInfo}
\alias{projInfo}
\alias{getPROJ4VersionInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List PROJ.4 tag information}
\description{
  The \code{projInfo} function lists known values and descriptions for PROJ.4 tags for tag in \code{c("proj", "ellps", "datum")}; \code{getPROJ4VersionInfo} returns the version of the underlying PROJ.4 release.
}
\usage{
projInfo(type = "proj")
getPROJ4VersionInfo()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{One of these tags: \code{c("proj", "ellps", "datum")}}
}
\details{
 The output data frame lists the information given by the proj application with flags -lp, -le or -ld.
}
\value{
  A data frame with a name and description column, and two extra columns for the "ellps" and "datum" tags.
}
\references{\url{http://proj.maptools.org/}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{Loading the rgdal package changes the \code{PROJ\_LIB} environmental variable to the PROJ.4 support files bundles with the package.}

\examples{
getPROJ4VersionInfo()
projInfo()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

