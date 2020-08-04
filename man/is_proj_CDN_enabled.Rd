\name{is_proj_CDN_enabled}
\alias{is_proj_CDN_enabled}
\alias{enable_proj_CDN}
\alias{disable_proj_CDN}
\alias{proj_CDN_user_writable_dir}
\alias{get_proj_search_paths}
\alias{set_proj_search_paths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PROJ search paths and content download network handling}
\description{
From PROJ 7 (and partly 7.1), it is becoming possible to use transformation grids downloaded on demand to improve coordinate operation accuracy from a content download network (CDN). These functions report on and control the use of the CDN.
}
\usage{
is_proj_CDN_enabled()
enable_proj_CDN()
disable_proj_CDN()
proj_CDN_user_writable_dir()
get_proj_search_paths()
set_proj_search_paths(paths)
}

\arguments{
  \item{paths}{a character vector of existing directories}
}

\details{
The PROJ user-writable CDN directory is set as soon as the internal search path is queried, and for most uses, the default value will allow all programs using PROJ such as R packages, QGIS, GRASS, etc., to access any downloaded grids. Grids are checked for staleness at regular intervals. This directory may be set to a non-default value with the PROJ_USER_WRITABLE_DIRECTORY environment variable before \pkg{rgdal} (and any other package using PROJ) is loaded and attached, from PROJ >= 7.1.0.
}
\value{
Logical values and/or character vector search paths, often NULL for earlier versions of PROJ. 
}
\references{\url{https://cdn.proj.org/}.}
\author{Roger Bivand}
\examples{
is_proj_CDN_enabled()
proj_CDN_user_writable_dir()
get_proj_search_paths()
}
\keyword{spatial}

