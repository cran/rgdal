\name{projInfo}
\alias{projInfo}
\alias{getPROJ4VersionInfo}
\alias{getPROJ4libPath}
\alias{projNAD}
\alias{GDALis3ormore}
\alias{PROJis6ormore}
\alias{new_proj_and_gdal}
\alias{GDAL_OSR_PROJ}
%\alias{checkCRSArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List PROJ.4 tag information}
\description{
  The \code{projInfo} function lists known values and descriptions for PROJ.4 tags for tag in \code{c("proj", "ellps", "datum", "units")}; \code{getPROJ4VersionInfo} returns the version of the underlying PROJ.4 release, \code{getPROJ4libPath} returns the value of the PROJ\_LIB environment variable, \code{projNAD} detects the presence of NAD datum conversion tables (looking for conus).
}
\usage{
projInfo(type = "proj")
getPROJ4VersionInfo()
getPROJ4libPath()
projNAD()
GDAL_OSR_PROJ()
GDALis3ormore()
PROJis6ormore()
new_proj_and_gdal()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{One of these tags: \code{c("proj", "ellps", "datum", "units")}}
}
\details{
 The output data frame lists the information given by the proj application with flags -lp, -le, -ld or -lu. From PROJ 6, \code{"datum"} is not available. From PROJ 7.1.0, \code{"units"} returns the conversion factor as numeric, not character.
}
\value{
  A data frame with a name and description column, and two extra columns for the "ellps" and "datum" tags.
}
\references{\url{https://proj.org/}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\note{Loading the rgdal package may change the \code{PROJ\_LIB} environmental variable to the PROJ.4 support files if bundled with binary packages.}

\examples{
getPROJ4VersionInfo()
projInfo()
projInfo("ellps")
projInfo("units")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

