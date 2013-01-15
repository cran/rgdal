\name{nor2k}
\alias{nor2k}
\docType{data}
\title{Norwegian peaks over 2000m}
\description{Norwegian peaks over 2000m, 3D SpatialPoints data.}
\usage{data(nor2k)}
\format{
  The format is:
Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	300 obs. of  3 variables:
  .. ..$ Nr.    : int [1:300] 1 2 3 4 5 6 7 8 9 10 ...
  .. ..$ Navn   : chr [1:300] "Galdh?piggen" "Glittertinden" "Skagast?lstinden, Store (Storen)" "Styggedalstinden, Store, ?sttoppen" ...
  .. ..$ Kommune: chr [1:300] "Lom" "Lom" "Luster / Ardal" "Luster" ...
  ..@ coords.nrs : num(0) 
  ..@ coords     : num [1:300, 1:3] 463550 476550 439850 441450 441100 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:3] "East" "North" "Height"
  ..@ bbox       : num [1:3, 1:2] 404700 6804200 2001 547250 6910050 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:3] "East" "North" "Height"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr "+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
}
\details{
Norwegian peaks over 2000m, coordinates in EUREF89/WGS84 UTM32N, names not fully updated, here converted to ASCII. 
}
\source{
\url{http://www.nfo2000m.no/}; \url{http://www.nfo2000m.no/Excel/2000m_data.xls}
}
\examples{
data(nor2k)
summary(nor2k)
## maybe str(nor2k) ; plot(nor2k) ...
}
\keyword{datasets}
