\name{projInfo}
\alias{projInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List PROJ.4 tag information}
\description{
  The function lists known values and descriptions for PROJ.4 tags for tag in \code{c("proj", "ellps", "datum")}.
}
\usage{
projInfo(type = "proj")
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
\references{\url{http://www.remotesensing.org/proj/}}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\examples{
projInfo()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

