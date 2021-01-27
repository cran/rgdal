\name{showWKT}
\alias{showWKT}
\alias{showEPSG}
\alias{showP4}
\alias{showSRID}
\alias{get_P6_datum_hard_fail}
\alias{set_P6_datum_hard_fail}
\alias{get_thin_PROJ6_warnings}
\alias{set_thin_PROJ6_warnings}
\alias{get_prefer_proj}
\alias{set_prefer_proj}
\alias{get_rgdal_show_exportToProj4_warnings}
\alias{set_rgdal_show_exportToProj4_warnings}
\alias{get_PROJ6_warnings_count}
\alias{OSRIsProjected}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Show Well-Known Text spatial reference system metadata}
\description{
In modern workflows with PROJ >= 6 and GDAL >= 3, use only \code{showSRID()}
  DEPRECATED: Use GDAL/OGR spatial reference objects to convert a PROJ.4 representation to a Well-Known Text representation, and report an EPSG code if it can be determined by OGR SRS services.
}
\usage{
showWKT(p4s, file = NULL, morphToESRI = FALSE, enforce_xy = NULL)
showP4(wkt, morphFromESRI=FALSE, enforce_xy = NULL)
showEPSG(p4s, enforce_xy = NULL)
showSRID(inSRID, format="WKT2", multiline="NO", enforce_xy = NULL, EPSG_to_init=TRUE,
 prefer_proj=NULL)
get_P6_datum_hard_fail()
set_P6_datum_hard_fail(value)
get_thin_PROJ6_warnings()
set_thin_PROJ6_warnings(value)
get_prefer_proj()
set_prefer_proj(value)
get_rgdal_show_exportToProj4_warnings()
set_rgdal_show_exportToProj4_warnings(value)
get_PROJ6_warnings_count()
OSRIsProjected(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p4s}{A valid PROJ.4 string representing a spatial reference system}
  \item{file}{if not NULL, a file name to which the output Well-Known Text representation should be written}
  \item{morphToESRI}{default FALSE, morph the WKT string to the representation used by ESRI; set FALSE by default 2020-03-04 to avoid occasional Windows crashes seen when passing a very modern WKT string to an old version of PROJ: \url{https://github.com/edzer/sp/issues/75}}
  \item{enforce_xy}{(PROJ6+/GDAL3+) either use global setting (default NULL) or override policy for coordinate ordering easting/x as first axis, northing/y as second axis.}
  \item{wkt}{A valid WKT character string representing a spatial reference system}
  \item{morphFromESRI}{default TRUE, morph the WKT string from the representation used by ESRI}
  \item{inSRID}{Input coordinate reference string}
  \item{obj}{valid CRS object}
  \item{format}{Output format, default WKT2}
  \item{multiline}{Multiline output, either \code{"NO"} or \code{"YES"}}
  \item{EPSG_to_init}{default TRUE, workaround for PROJ 6.3.0 frailty leading to the dropping of +ellps= and +units=; DATUM seems to disappear in the internal definition}
  \item{prefer_proj}{default NULL, if TRUE, use PROJ compiled code directly, rather than FALSE using PROJ via GDAL SRS; if NULL, uses value shown by \code{get_prefer_proj()} set on startup to TRUE.}
%  \item{prefer_proj}{default FALSE, if TRUE, use the PROJ library directly rather than through GDAL}
  \item{value}{a logical value. For \code{set_P6_datum_hard_fail()}: by default, a deprecated/ignored input DATUM key/value pair on reading a file with PROJ6 will give a warning (default FALSE); if TRUE, an error is triggered, which may be trapped using \code{try}. For \code{set_thin_PROJ6_warnings()} default FALSE, can be set to TRUE to report only once and count the number of non-issues warnings, retrieved by \code{get_PROJ6_warnings_count()}. For \code{set_rgdal_show_exportToProj4_warnings()}, default in \pkg{rgdal} version 1.5.* TRUE, from 1.6 FALSE. The \code{options("rgdal_show_exportToProj4_warnings"="x")} may be used before loading \pkg{rgdal} to set the internal logical variables; if the option is set to \code{"all"}, all warnings reporting CRS degradation stemming from the GDAL OSR function \code{exportToProj4()} even if trivial are reported; if set to \code{"thin"}, all warnings are detected but thinned so that one report is given per function call; if set to "none", the degradations are detected but not reported.
}
}

\note{The \code{options("rgdal_show_exportToProj4_warnings"="x")} may be used before loading \pkg{rgdal} to set the internal logical variables; if the option is set to \code{"all"}, all warnings reporting CRS degradation stemming from the GDAL OSR function \code{exportToProj4()} even if trivial are reported; if set to \code{"thin"}, all warnings are detected but thinned so that one report is given per function call; if set to \code{"none"}, the degradations are detected but not reported.}

\value{
  For \code{showWKT}, a character string containing the WKT representation of the PROJ.4 string.
}
\references{\url{https://gdal.org/tutorials/osr_api_tut.html}}
\author{Roger Bivand}

\seealso{\code{\link[sp]{is.projected}}, \code{\link[sp]{CRS-class}}}
\examples{
set_thin_PROJ6_warnings(TRUE)
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
readLines(system.file("vectors/cities.prj", package = "rgdal")[1])
showWKT(proj4string(cities))
showWKT("+init=epsg:28992")
showP4(showWKT("+init=epsg:28992"))
showEPSG("+proj=utm +zone=30")
showEPSG("+proj=longlat +ellps=WGS84")
exts <- rgdal_extSoftVersion()
run <- new_proj_and_gdal()
if (run) {
  cat(showSRID("EPSG:27700", multiline="YES"), "\n")
}
if (run) {
(prj <- showSRID("EPSG:27700", "PROJ"))
}
if (run) {
showSRID(paste0(prj, " +datum=OSGB36"), "WKT1")
}
if (run) {
showSRID(paste0(prj, " +towgs84=370.936,-108.938,435.682"), "WKT1")
}
if (run) {
showSRID(paste0(prj, " +nadgrids=OSTN15_NTv2_OSGBtoETRS.gsb"), "WKT1")
}
if (run) {
showSRID(paste0(prj, " +datum=OSGB36"), "WKT2")
}
if (run) {
showSRID(paste0(prj, " +towgs84=370.936,-108.938,435.682"), "WKT2")
}
if (run) {
showSRID(paste0(prj, " +nadgrids=OSTN15_NTv2_OSGBtoETRS.gsb"), "WKT2")
}
if (run) {
showSRID("ESRI:102761", "WKT2")
}
if (run) {
showSRID("OGC:CRS84", "WKT2")
}
if (run) {
showSRID("urn:ogc:def:crs:OGC:1.3:CRS84", "WKT2")
}
if (run) {
try(showSRID("", "WKT2"))
}

OSRIsProjected(CRS("+proj=longlat"))
OSRIsProjected(CRS("+proj=geocent"))
OSRIsProjected(CRS("+proj=geocent +units=km"))
}
\keyword{spatial}

