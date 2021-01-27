## -----------------------------------------------------------------------------
library(sp)
packageVersion("sp")

## -----------------------------------------------------------------------------
## [1] '1.3.3'

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 [1] '1.3.2'

## -----------------------------------------------------------------------------
getClass("CRS")

## -----------------------------------------------------------------------------
rgdal::rgdal_extSoftVersion()

## -----------------------------------------------------------------------------
##           GDAL GDAL_with_GEOS           PROJ             sp 
##        "3.0.2"         "TRUE"        "6.2.1"        "1.3-3"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2           GDAL GDAL_with_GEOS         PROJ.4             sp 
## PROJ4/GDAL2        "2.2.3"         "TRUE"        "4.9.3"        "1.3-1"

## -----------------------------------------------------------------------------
packageVersion("rgdal")

## -----------------------------------------------------------------------------
## [1] '1.5.1'

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 [1] '1.4.7'

## -----------------------------------------------------------------------------
(crs <- CRS("+proj=longlat +ellps=WGS84"))

## -----------------------------------------------------------------------------
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"):
## Discarded datum Unknown_based_on_WGS84_ellipsoid in CRS definition

## CRS arguments: +proj=longlat +ellps=WGS84 +no_defs

## -----------------------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "GEOGCRS[\"unknown\", DATUM[\"Unknown based on WGS84 ellipsoid\",
## ELLIPSOID[\"WGS 84\", 6378137, 298.257223563, LENGTHUNIT[\"metre\", 1],
## ID[\"EPSG\", 7030]]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\",
## 0.0174532925199433], ID[\"EPSG\", 8901]], CS[ellipsoidal, 2], AXIS[\"longitude\",
## east, ORDER[1], ANGLEUNIT[\"degree\", 0.0174532925199433, ID[\"EPSG\", 9122]]],
## AXIS[\"latitude\", north, ORDER[2], ANGLEUNIT[\"degree\", 0.0174532925199433,
## ID[\"EPSG\", 9122]]]]"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 CRS arguments: +proj=longlat +ellps=WGS84

## -----------------------------------------------------------------------------
(crs <- CRS("+proj=longlat +datum=WGS84"))

## -----------------------------------------------------------------------------
## CRS arguments: +proj=longlat +datum=WGS84 +no_defs

## -----------------------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "GEOGCRS[\"unknown\", DATUM[\"World Geodetic System 1984\",
## ELLIPSOID[\"WGS 84\", 6378137, 298.257223563, LENGTHUNIT[\"metre\", 1]], 
## ID[\"EPSG\", 6326]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\",
## 0.0174532925199433], ID[\"EPSG\", 8901]], CS[ellipsoidal, 2], AXIS[\"longitude\",
## east, ORDER[1], ANGLEUNIT[\"degree\", 0.0174532925199433, ID[\"EPSG\", 9122]]],
## AXIS[\"latitude\", north, ORDER[2], ANGLEUNIT[\"degree\", 0.0174532925199433,
## ID[\"EPSG\", 9122]]]]"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 CRS arguments:
## PROJ4/GDAL2  +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0

## -----------------------------------------------------------------------------
(crs <- CRS("+proj=longlat +towgs84=0,0,0"))

## -----------------------------------------------------------------------------
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded datum WGS_1984 in CRS definition,
##  but +towgs84= values preserved

## CRS arguments:
##  +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs

## -----------------------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "BOUNDCRS[SOURCECRS[GEOGCRS[\"unknown\", DATUM[\"World Geodetic System 1984\",
## ELLIPSOID[\"WGS 84\", 6378137, 298.257223563, LENGTHUNIT[\"metre\", 1]], ID[\"EPSG\",
## 6326]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\", 0.0174532925199433],
## ID[\"EPSG\", 8901]], CS[ellipsoidal, 2], AXIS[\"longitude\", east, ORDER[1],
## ANGLEUNIT[\"degree\", 0.0174532925199433, ID[\"EPSG\", 9122]]], AXIS[\"latitude\",
## north, ORDER[2], ANGLEUNIT[\"degree\", 0.0174532925199433, ID[\"EPSG\", 9122]]]]],
## TARGETCRS[GEOGCRS[\"WGS 84\", DATUM[\"World Geodetic System 1984\", ELLIPSOID[\"WGS
## 84\", 6378137, 298.257223563, LENGTHUNIT[\"metre\", 1]]], PRIMEM[\"Greenwich\", 0,
## ANGLEUNIT[\"degree\", 0.0174532925199433]], CS[ellipsoidal, 2], AXIS[\"latitude\",
## north, ORDER[1], ANGLEUNIT[\"degree\", 0.0174532925199433]], AXIS[\"longitude\", east,
## ORDER[2], ANGLEUNIT[\"degree\", 0.0174532925199433]], ID[\"EPSG\", 4326]]],
## ABRIDGEDTRANSFORMATION[\"Transformation from unknown to WGS84\", METHOD[\"Geocentric
## translations (geog2D domain)\", ID[\"EPSG\", 9603]], PARAMETER[\"X-axis translation\",
## 0, ID[\"EPSG\", 8605]], PARAMETER[\"Y-axis translation\", 0, ID[\"EPSG\", 8606]],
## PARAMETER[\"Z-axis translation\", 0, ID[\"EPSG\", 8607]]]]"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 CRS arguments:
## PROJ4/GDAL2  +proj=longlat +towgs84=0,0,0 +ellps=WGS84

## -----------------------------------------------------------------------------
(crs <- CRS("+init=epsg:4326"))

## -----------------------------------------------------------------------------
## CRS arguments: +proj=longlat +datum=WGS84 +no_defs

## -----------------------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "GEOGCRS[\"WGS 84\", DATUM[\"World Geodetic System 1984\", ELLIPSOID[\"WGS 84\", 
## 6378137, 298.257223563, LENGTHUNIT[\"metre\", 1]], ID[\"EPSG\", 6326]],
## PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\", 0.0174532925199433], ID[\"EPSG\",
## 8901]], CS[ellipsoidal, 2], AXIS[\"longitude\", east, ORDER[1], ANGLEUNIT[\"degree\",
## 0.0174532925199433, ID[\"EPSG\", 9122]]], AXIS[\"latitude\", north, ORDER[2],
## ANGLEUNIT[\"degree\", 0.0174532925199433, ID[\"EPSG\", 9122]]],
## USAGE[SCOPE[\"unknown\"], AREA[\"World\"], BBOX[-90, -180, 90, 180]]]"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 CRS arguments:
## PROJ4/GDAL2  +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
## PROJ4/GDAL2 +towgs84=0,0,0

## ---- eval=FALSE--------------------------------------------------------------
#  (crs <- CRS("+init=epsg:27700"))

## -----------------------------------------------------------------------------
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"):
## Discarded datum OSGB_1936 in CRS definition

## CRS arguments:
##  +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
## +y_0=-100000 +ellps=airy +units=m +no_defs

## ---- eval=FALSE--------------------------------------------------------------
#  cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "PROJCRS[\"OSGB 1936 / British National Grid\", BASEGEOGCRS[\"OSGB 1936\",
## DATUM[\"OSGB 1936\", ELLIPSOID[\"Airy 1830\", 6377563.396, 299.3249646,
## LENGTHUNIT[\"metre\", 1]]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\",
## 0.0174532925199433]], ID[\"EPSG\", 4277]], CONVERSION[\"British National Grid\",
## METHOD[\"Transverse Mercator\", ID[\"EPSG\", 9807]], PARAMETER[\"Latitude of natural
## origin\", 49, ANGLEUNIT[\"degree\", 0.0174532925199433], ID[\"EPSG\", 8801]],
## PARAMETER[\"Longitude of natural origin\", -2, ANGLEUNIT[\"degree\",
## 0.0174532925199433], ID[\"EPSG\", 8802]], PARAMETER[\"Scale factor at natural
## origin\", 0.9996012717, SCALEUNIT[\"unity\", 1], ID[\"EPSG\", 8805]],
## PARAMETER[\"False easting\", 400000, LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8806]],
## PARAMETER[\"False northing\", -100000, LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8807]],
## ID[\"EPSG\", 19916]], CS[Cartesian, 2], AXIS[\"(E)\", east, ORDER[1],
## LENGTHUNIT[\"metre\", 1, ID[\"EPSG\", 9001]]], AXIS[\"(N)\", north, ORDER[2],
## LENGTHUNIT[\"metre\", 1, ID[\"EPSG\", 9001]]], USAGE[SCOPE[\"unknown\"], AREA[\"UK -
## Britain and UKCS 49°46'N to 61°01'N,  7°33'W to 3°33'E\"], BBOX[49.75, -9.2, 61.14, 2.88]]]"

## -----------------------------------------------------------------------------
## PROJ4/GDAL2 CRS arguments:
## PROJ4/GDAL2  +init=epsg:27700 +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717
## PROJ4/GDAL2 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
## PROJ4/GDAL2 +ellps=airy
## PROJ4/GDAL2 +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894

## -----------------------------------------------------------------------------
run <- rgdal::new_proj_and_gdal()

## ---- eval=run----------------------------------------------------------------
(crs <- CRS(SRS_string = "EPSG:27700"))

## -----------------------------------------------------------------------------
## Warning in showSRID(SRS_string, format = "PROJ", multiline = "NO"):
## Discarded datum OSGB_1936 in CRS definition

## CRS arguments:
##  +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
## +y_0=-100000 +ellps=airy +units=m +no_defs

## ---- eval=run----------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "PROJCRS[\"OSGB 1936 / British National Grid\", BASEGEOGCRS[\"OSGB 1936\",
## DATUM[\"OSGB 1936\", ELLIPSOID[\"Airy 1830\", 6377563.396, 299.3249646,
## LENGTHUNIT[\"metre\", 1]]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\",
## 0.0174532925199433]], ID[\"EPSG\", 4277]], CONVERSION[\"British National Grid\",
## METHOD[\"Transverse Mercator\", ID[\"EPSG\", 9807]], PARAMETER[\"Latitude of natural
## origin\", 49, ANGLEUNIT[\"degree\", 0.0174532925199433], ID[\"EPSG\", 8801]],
## PARAMETER[\"Longitude of natural origin\", -2, ANGLEUNIT[\"degree\",
## 0.0174532925199433], ID[\"EPSG\", 8802]], PARAMETER[\"Scale factor at natural
## origin\", 0.9996012717, SCALEUNIT[\"unity\", 1], ID[\"EPSG\", 8805]],
## PARAMETER[\"False easting\", 400000, LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8806]],
## PARAMETER[\"False northing\", -100000, LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8807]]],
## CS[Cartesian, 2], AXIS[\"(E)\", east, ORDER[1], LENGTHUNIT[\"metre\", 1]],
## AXIS[\"(N)\", north, ORDER[2], LENGTHUNIT[\"metre\", 1]], USAGE[SCOPE[\"unknown\"],
## AREA[\"UK - Britain and UKCS 49°46'N to 61°01'N,  7°33'W to 3°33'E\"], BBOX[49.75,
## -9.2, 61.14, 2.88]], ID[\"EPSG\", 27700]]"

## ---- eval=run----------------------------------------------------------------
(crs <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"))

## -----------------------------------------------------------------------------
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"):
## Discarded datum OSGB_1936 in CRS definition

## CRS arguments:
##  +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
## +y_0=-100000 +ellps=airy +units=m +no_defs

## ---- eval=run----------------------------------------------------------------
cat(strwrap(gsub(",", ", ", (comment(crs)))), sep="\n")

## -----------------------------------------------------------------------------
## [1] "PROJCRS[\"unknown\", BASEGEOGCRS[\"unknown\", DATUM[\"OSGB 1936\",
## ELLIPSOID[\"Airy 1830\", 6377563.396, 299.3249646, LENGTHUNIT[\"metre\", 1]],
## ID[\"EPSG\", 6277]], PRIMEM[\"Greenwich\", 0, ANGLEUNIT[\"degree\",
## 0.0174532925199433], ID[\"EPSG\", 8901]]], CONVERSION[\"unknown\", METHOD[\"Transverse
## Mercator\", ID[\"EPSG\", 9807]], PARAMETER[\"Latitude of natural origin\", 49,
## ANGLEUNIT[\"degree\", 0.0174532925199433], ID[\"EPSG\", 8801]], PARAMETER[\"Longitude
## of natural origin\", -2, ANGLEUNIT[\"degree\", 0.0174532925199433], ID[\"EPSG\",
## 8802]], PARAMETER[\"Scale factor at natural origin\", 0.9996012717,
## SCALEUNIT[\"unity\", 1], ID[\"EPSG\", 8805]], PARAMETER[\"False easting\", 400000,
## LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8806]], PARAMETER[\"False northing\", -100000,
## LENGTHUNIT[\"metre\", 1], ID[\"EPSG\", 8807]]], CS[Cartesian, 2], AXIS[\"(E)\", east,
## ORDER[1], LENGTHUNIT[\"metre\", 1, ID[\"EPSG\", 9001]]], AXIS[\"(N)\", north,
## ORDER[2], LENGTHUNIT[\"metre\", 1, ID[\"EPSG\", 9001]]]]"

## ---- eval=run----------------------------------------------------------------
if (packageVersion("rgdal") >= "1.5.1") cat(rgdal::showSRID("+init=epsg:27700", format="WKT2_2018", multiline="YES", prefer_proj=FALSE), "\n")

## -----------------------------------------------------------------------------
## PROJCRS["OSGB 1936 / British National Grid",
##     BASEGEOGCRS["OSGB 1936",
##         DATUM["OSGB 1936",
##             ELLIPSOID["Airy 1830",6377563.396,299.3249646,
##                 LENGTHUNIT["metre",1]]],
##         PRIMEM["Greenwich",0,
##             ANGLEUNIT["degree",0.0174532925199433]],
##         ID["EPSG",4277]],
##     CONVERSION["British National Grid",
##         METHOD["Transverse Mercator",
##             ID["EPSG",9807]],
##         PARAMETER["Latitude of natural origin",49,
##             ANGLEUNIT["degree",0.0174532925199433],
##             ID["EPSG",8801]],
##         PARAMETER["Longitude of natural origin",-2,
##             ANGLEUNIT["degree",0.0174532925199433],
##             ID["EPSG",8802]],
##         PARAMETER["Scale factor at natural origin",0.9996012717,
##             SCALEUNIT["unity",1],
##             ID["EPSG",8805]],
##         PARAMETER["False easting",400000,
##             LENGTHUNIT["metre",1],
##             ID["EPSG",8806]],
##         PARAMETER["False northing",-100000,
##             LENGTHUNIT["metre",1],
##             ID["EPSG",8807]],
##         ID["EPSG",19916]],
##     CS[Cartesian,2],
##         AXIS["(E)",east,
##             ORDER[1],
##             LENGTHUNIT["metre",1,
##                 ID["EPSG",9001]]],
##         AXIS["(N)",north,
##             ORDER[2],
##             LENGTHUNIT["metre",1,
##                 ID["EPSG",9001]]],
##     USAGE[
##         SCOPE["unknown"],
##         AREA["UK - Britain and UKCS 49°46'N to 61°01'N, 7°33'W to 3°33'E"],
##         BBOX[49.75,-9.2,61.14,2.88]]]

## -----------------------------------------------------------------------------
library(rgdal)

## -----------------------------------------------------------------------------
## rgdal: version: 1.5-1, (SVN revision 870)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 3.0.2, released 2019/10/28
##  Path to GDAL shared files: 
##  GDAL binary built with GEOS: TRUE 
##  Loaded PROJ.4 runtime: Rel. 6.2.1, November 1st, 2019, [PJ_VERSION: 621]
##  Path to PROJ.4 shared files: (autodetected)
##  Linking to sp version: 1.3-3

## -----------------------------------------------------------------------------
run <- new_proj_and_gdal()

## ---- eval=run----------------------------------------------------------------
(discarded_datum <- showSRID("EPSG:27700", "PROJ"))

## -----------------------------------------------------------------------------
## Warning in showSRID("EPSG:27700", "PROJ"): Discarded datum OSGB_1936 in CRS
## definition

## ---- eval=run----------------------------------------------------------------
(x <- list_coordOps(paste0(discarded_datum, " +type=crs"), "EPSG:4326"))

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  1 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
##         +y_0=-100000 +ellps=airy +units=m +no_defs +type=crs
## Target: EPSG:4326 
## Best instantiable operation has only ballpark accuracy 
## Description: Inverse of unknown + Ballpark geographic offset from unknown to
##              WGS 84 + axis order change (2D)
## Definition:  +proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2
##              +k=0.9996012717 +x_0=400000 +y_0=-100000
##              +ellps=airy +step +proj=unitconvert +xy_in=rad
##              +xy_out=deg

## ---- eval=run----------------------------------------------------------------
best_instantiable_coordOp(x)

## -----------------------------------------------------------------------------
## Warning in best_instantiable_coordOp(x): Best instantiable operation has only
## ballpark accuracy
## [1] "+proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717
## +x_0=400000 +y_0=-100000 +ellps=airy +step +proj=unitconvert +xy_in=rad +xy_out=deg"
## attr(,"description")
## [1] "Inverse of unknown + Ballpark geographic offset from unknown to WGS 84 + 
## axis order change (2D)"

## ---- eval=run----------------------------------------------------------------
list_coordOps(paste0(discarded_datum, " +datum=OSGB36 +type=crs"), "EPSG:4326")

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  7 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
##         +y_0=-100000 +ellps=airy +units=m +no_defs
##         +datum=OSGB36 +type=crs
## Target: EPSG:4326 
## Best instantiable operation has accuracy: 2 m
## Description: Inverse of unknown + axis order change (2D) + OSGB 1936 to WGS
##              84 (6) + axis order change (2D)
## Definition:  +proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2
##              +k=0.9996012717 +x_0=400000 +y_0=-100000
##              +ellps=airy +step +proj=push +v_3 +step +proj=cart
##              +ellps=airy +step +proj=helmert +x=446.448
##              +y=-125.157 +z=542.06 +rx=0.15 +ry=0.247 +rz=0.842
##              +s=-20.489 +convention=position_vector +step +inv
##              +proj=cart +ellps=WGS84 +step +proj=pop +v_3 +step
##              +proj=unitconvert +xy_in=rad +xy_out=deg
## Operation 6 is lacking 1 grid with accuracy 1 m
## Missing grid: OSTN15_NTv2_OSGBtoETRS.gsb 
## URL: https://download.osgeo.org/proj/proj-datumgrid-europe-latest.zip

## ---- eval=run----------------------------------------------------------------
wkt_source_datum <- showSRID("EPSG:27700", "WKT2")
wkt_target_datum <- showSRID("EPSG:4326", "WKT2")
(x <- list_coordOps(wkt_source_datum, wkt_target_datum))

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  6 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: PROJCRS["OSGB 1936 / British National Grid", BASEGEOGCRS["OSGB
##         1936", DATUM["OSGB 1936", ELLIPSOID["Airy 1830",
##         6377563.396, 299.3249646, LENGTHUNIT["metre", 1]]],
##         PRIMEM["Greenwich", 0, ANGLEUNIT["degree",
##         0.0174532925199433]], ID["EPSG", 4277]],
##         CONVERSION["British National Grid", METHOD["Transverse
##         Mercator", ID["EPSG", 9807]], PARAMETER["Latitude of
##         natural origin", 49, ANGLEUNIT["degree",
##         0.0174532925199433], ID["EPSG", 8801]],
##         PARAMETER["Longitude of natural origin", -2,
##         ANGLEUNIT["degree", 0.0174532925199433], ID["EPSG",
##         8802]], PARAMETER["Scale factor at natural origin",
##         0.9996012717, SCALEUNIT["unity", 1], ID["EPSG", 8805]],
##         PARAMETER["False easting", 400000, LENGTHUNIT["metre",
##         1], ID["EPSG", 8806]], PARAMETER["False northing",
##         -100000, LENGTHUNIT["metre", 1], ID["EPSG", 8807]]],
##         CS[Cartesian, 2], AXIS["(E)", east, ORDER[1],
##         LENGTHUNIT["metre", 1]], AXIS["(N)", north, ORDER[2],
##         LENGTHUNIT["metre", 1]], USAGE[SCOPE["unknown"],
##         AREA["UK - Britain and UKCS 49°46'N to 61°01'N, 7°33'W
##         to 3°33'E"], BBOX[49.75, -9.2, 61.14, 2.88]],
##         ID["EPSG", 27700]]
## Target: GEOGCRS["WGS 84", DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84", 6378137, 298.257223563,
##         LENGTHUNIT["metre", 1]]], PRIMEM["Greenwich", 0,
##         ANGLEUNIT["degree", 0.0174532925199433]],
##         CS[ellipsoidal, 2], AXIS["geodetic latitude (Lat)",
##         north, ORDER[1], ANGLEUNIT["degree",
##         0.0174532925199433]], AXIS["geodetic longitude (Lon)",
##         east, ORDER[2], ANGLEUNIT["degree",
##         0.0174532925199433]], USAGE[SCOPE["unknown"],
##         AREA["World"], BBOX[-90, -180, 90, 180]], ID["EPSG",
##         4326]]
## Best instantiable operation has accuracy: 2 m
## Description: Inverse of British National Grid + OSGB 1936 to WGS 84 (6) +
##              axis order change (2D)
## Definition:  +proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2
##              +k=0.9996012717 +x_0=400000 +y_0=-100000
##              +ellps=airy +step +proj=push +v_3 +step +proj=cart
##              +ellps=airy +step +proj=helmert +x=446.448
##              +y=-125.157 +z=542.06 +rx=0.15 +ry=0.247 +rz=0.842
##              +s=-20.489 +convention=position_vector +step +inv
##              +proj=cart +ellps=WGS84 +step +proj=pop +v_3 +step
##              +proj=unitconvert +xy_in=rad +xy_out=deg
## Operation 6 is lacking 1 grid with accuracy 1 m
## Missing grid: OSTN15_NTv2_OSGBtoETRS.gsb 
## URL: https://download.osgeo.org/proj/proj-datumgrid-europe-latest.zip

## ---- eval=run----------------------------------------------------------------
best_instantiable_coordOp(x)

## -----------------------------------------------------------------------------
## [1] "+proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717
## +x_0=400000 +y_0=-100000 +ellps=airy +step +proj=push +v_3 +step +proj=cart
## +ellps=airy +step +proj=helmert +x=446.448 +y=-125.157 +z=542.06 +rx=0.15 +ry=0.247
## +rz=0.842 +s=-20.489 +convention=position_vector +step +inv +proj=cart +ellps=WGS84
## +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg"
## attr(,"description")
## [1] "Inverse of British National Grid + OSGB 1936 to WGS 84 (6) + axis order change (2D)"

## ---- eval=run----------------------------------------------------------------
discarded_datum <- showSRID("EPSG:22525", "PROJ")

## -----------------------------------------------------------------------------
## Warning in showSRID("EPSG:22525", "PROJ"): Discarded datum Corrego_Alegre_1970-72 in CRS definition,
##  but +towgs84= values preserved

## ---- eval=run----------------------------------------------------------------
(x <- list_coordOps(paste0(discarded_datum, " +type=crs"), "EPSG:31985"))

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  1 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: +proj=utm +zone=25 +south +ellps=intl
##         +towgs84=-205.57,168.77,-4.12,0,0,0,0 +units=m +no_defs
##         +type=crs
## Target: EPSG:31985 
## Best instantiable operation has only ballpark accuracy 
## Description: Inverse of UTM zone 25S + Transformation from unknown to WGS84
##              + Inverse of SIRGAS 2000 to WGS 84 (1) + UTM zone
##              25S
## Definition:  +proj=pipeline +step +inv +proj=utm +zone=25 +south +ellps=intl
##              +step +proj=push +v_3 +step +proj=cart +ellps=intl
##              +step +proj=helmert +x=-205.57 +y=168.77 +z=-4.12
##              +rx=0 +ry=0 +rz=0 +s=0 +convention=position_vector
##              +step +inv +proj=cart +ellps=GRS80 +step +proj=pop
##              +v_3 +step +proj=utm +zone=25 +south +ellps=GRS80

## ---- eval=run----------------------------------------------------------------
best_instantiable_coordOp(x)

## -----------------------------------------------------------------------------
## Warning in best_instantiable_coordOp(x): Best instantiable operation has only
## ballpark accuracy
## [1] "+proj=pipeline +step +inv +proj=utm +zone=25 +south +ellps=intl +step
## +proj=push +v_3 +step +proj=cart +ellps=intl +step +proj=helmert +x=-205.57
## +y=168.77 +z=-4.12 +rx=0 +ry=0 +rz=0 +s=0 +convention=position_vector +step +inv
## +proj=cart +ellps=GRS80 +step +proj=pop +v_3 +step +proj=utm +zone=25 +south
## +ellps=GRS80"
## attr(,"description")
## [1] "Inverse of UTM zone 25S + Transformation from unknown to WGS84 + Inverse of
## SIRGAS 2000 to WGS 84 (1) + UTM zone 25S"

## ---- eval=run----------------------------------------------------------------
wkt_source_datum <- showSRID("EPSG:22525", "WKT2")
wkt_target_datum <- showSRID("EPSG:31985", "WKT2")
(x <- list_coordOps(wkt_source_datum, wkt_target_datum))

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  1 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: BOUNDCRS[SOURCECRS[PROJCRS["Corrego Alegre 1970-72 / UTM zone
##         25S", BASEGEOGCRS["Corrego Alegre 1970-72",
##         DATUM["Corrego Alegre 1970-72",
##         ELLIPSOID["International 1924", 6378388, 297,
##         LENGTHUNIT["metre", 1]]], PRIMEM["Greenwich", 0,
##         ANGLEUNIT["degree", 0.0174532925199433]], ID["EPSG",
##         4225]], CONVERSION["UTM zone 25S", METHOD["Transverse
##         Mercator", ID["EPSG", 9807]], PARAMETER["Latitude of
##         natural origin", 0, ANGLEUNIT["degree",
##         0.0174532925199433], ID["EPSG", 8801]],
##         PARAMETER["Longitude of natural origin", -33,
##         ANGLEUNIT["degree", 0.0174532925199433], ID["EPSG",
##         8802]], PARAMETER["Scale factor at natural origin",
##         0.9996, SCALEUNIT["unity", 1], ID["EPSG", 8805]],
##         PARAMETER["False easting", 500000, LENGTHUNIT["metre",
##         1], ID["EPSG", 8806]], PARAMETER["False northing",
##         10000000, LENGTHUNIT["metre", 1], ID["EPSG", 8807]]],
##         CS[Cartesian, 2], AXIS["(E)", east, ORDER[1],
##         LENGTHUNIT["metre", 1]], AXIS["(N)", north, ORDER[2],
##         LENGTHUNIT["metre", 1]], USAGE[SCOPE["unknown"],
##         AREA["Brazil - east of 36°W onshore"], BBOX[-10.1, -36,
##         -4.99, -34.74]], ID["EPSG", 22525]]],
##         TARGETCRS[GEOGCRS["WGS 84", DATUM["World Geodetic
##         System 1984", ELLIPSOID["WGS 84", 6378137,
##         298.257223563, LENGTHUNIT["metre", 1]]],
##         PRIMEM["Greenwich", 0, ANGLEUNIT["degree",
##         0.0174532925199433]], CS[ellipsoidal, 2],
##         AXIS["latitude", north, ORDER[1], ANGLEUNIT["degree",
##         0.0174532925199433]], AXIS["longitude", east, ORDER[2],
##         ANGLEUNIT["degree", 0.0174532925199433]], ID["EPSG",
##         4326]]], ABRIDGEDTRANSFORMATION["Corrego Alegre 1970-72
##         to WGS 84 (3)", VERSION["PBS-Bra 1983"],
##         METHOD["Geocentric translations (geog2D domain)",
##         ID["EPSG", 9603]], PARAMETER["X-axis translation",
##         -205.57, ID["EPSG", 8605]], PARAMETER["Y-axis
##         translation", 168.77, ID["EPSG", 8606]],
##         PARAMETER["Z-axis translation", -4.12, ID["EPSG",
##         8607]], USAGE[SCOPE["Medium and small scale mapping."],
##         AREA["Brazil - Corrego Alegre 1970-1972"], BBOX[-33.78,
##         -58.16, -2.68, -34.74]], ID["EPSG", 6192],
##         REMARK["Formed by concatenation of tfms codes 6191 and
##         1877. Used by Petrobras and ANP until February 2005
##         when replaced by Corrego Alegre 1970-72 to WGS 84 (4)
##         (tfm code 6194)."]]]
## Target:
## BOUNDCRS[SOURCECRS[PROJCRS["SIRGAS 2000 / UTM zone 25S",
##         BASEGEOGCRS["SIRGAS 2000", DATUM["Sistema de Referencia
##         Geocentrico para las AmericaS 2000", ELLIPSOID["GRS
##         1980", 6378137, 298.257222101, LENGTHUNIT["metre",
##         1]]], PRIMEM["Greenwich", 0, ANGLEUNIT["degree",
##         0.0174532925199433]], ID["EPSG", 4674]],
##         CONVERSION["UTM zone 25S", METHOD["Transverse
##         Mercator", ID["EPSG", 9807]], PARAMETER["Latitude of
##         natural origin", 0, ANGLEUNIT["degree",
##         0.0174532925199433], ID["EPSG", 8801]],
##         PARAMETER["Longitude of natural origin", -33,
##         ANGLEUNIT["degree", 0.0174532925199433], ID["EPSG",
##         8802]], PARAMETER["Scale factor at natural origin",
##         0.9996, SCALEUNIT["unity", 1], ID["EPSG", 8805]],
##         PARAMETER["False easting", 500000, LENGTHUNIT["metre",
##         1], ID["EPSG", 8806]], PARAMETER["False northing",
##         10000000, LENGTHUNIT["metre", 1], ID["EPSG", 8807]]],
##         CS[Cartesian, 2], AXIS["(E)", east, ORDER[1],
##         LENGTHUNIT["metre", 1]], AXIS["(N)", north, ORDER[2],
##         LENGTHUNIT["metre", 1]], USAGE[SCOPE["unknown"],
##         AREA["Brazil - 36°W to 30°W"], BBOX[-23.8, -36, 4.19,
##         -29.99]], ID["EPSG", 31985]]], TARGETCRS[GEOGCRS["WGS
##         84", DATUM["World Geodetic System 1984", ELLIPSOID["WGS
##         84", 6378137, 298.257223563, LENGTHUNIT["metre", 1]]],
##         PRIMEM["Greenwich", 0, ANGLEUNIT["degree",
##         0.0174532925199433]], CS[ellipsoidal, 2],
##         AXIS["latitude", north, ORDER[1], ANGLEUNIT["degree",
##         0.0174532925199433]], AXIS["longitude", east, ORDER[2],
##         ANGLEUNIT["degree", 0.0174532925199433]], ID["EPSG",
##         4326]]], ABRIDGEDTRANSFORMATION["SIRGAS 2000 to WGS 84
##         (1)", VERSION["OGP-C&S America"], METHOD["Geocentric
##         translations (geog2D domain)", ID["EPSG", 9603]],
##         PARAMETER["X-axis translation", 0, ID["EPSG", 8605]],
##         PARAMETER["Y-axis translation", 0, ID["EPSG", 8606]],
##         PARAMETER["Z-axis translation", 0, ID["EPSG", 8607]],
##         USAGE[SCOPE["Accuracy 1m."], AREA["Latin America -
##         SIRGAS 2000 by country"], BBOX[-59.87, -122.19, 32.72,
##         -25.28]], ID["EPSG", 15894]]]
## Best instantiable operation has only ballpark accuracy 
## Description: Inverse of UTM zone 25S + Corrego Alegre 1970-72 to WGS 84 (3)
##              + Inverse of SIRGAS 2000 to WGS 84 (1) + UTM zone
##              25S
## Definition:  +proj=pipeline +step +inv +proj=utm +zone=25 +south +ellps=intl
##              +step +proj=push +v_3 +step +proj=cart +ellps=intl
##              +step +proj=helmert +x=-205.57 +y=168.77 +z=-4.12
##              +step +inv +proj=cart +ellps=GRS80 +step +proj=pop
##              +v_3 +step +proj=utm +zone=25 +south +ellps=GRS80

## ---- eval=run----------------------------------------------------------------
(x <- list_coordOps("EPSG:22525", "EPSG:31985"))

## -----------------------------------------------------------------------------
## Candidate coordinate operations found:  2 
## Strict containment:  FALSE 
## Visualization order:  TRUE 
## Source: EPSG:22525 
## Target: EPSG:31985 
## Best instantiable operation has accuracy: 5 m
## Description: Inverse of UTM zone 25S + Corrego Alegre 1970-72 to SIRGAS 2000
##              (2) + UTM zone 25S
## Definition:  +proj=pipeline +step +inv +proj=utm +zone=25 +south +ellps=intl
##              +step +proj=push +v_3 +step +proj=cart +ellps=intl
##              +step +proj=helmert +x=-206.05 +y=168.28 +z=-3.82
##              +step +inv +proj=cart +ellps=GRS80 +step +proj=pop
##              +v_3 +step +proj=utm +zone=25 +south +ellps=GRS80
## Operation 2 is lacking 1 grid with accuracy 2 m
## Missing grid: CA7072_003.gsb

## ---- eval=run----------------------------------------------------------------
best_instantiable_coordOp(x)

## -----------------------------------------------------------------------------
## [1] "+proj=pipeline +step +inv +proj=utm +zone=25 +south +ellps=intl +step
## +proj=push +v_3 +step +proj=cart +ellps=intl +step +proj=helmert +x=-206.05
## +y=168.28 +z=-3.82 +step +inv +proj=cart +ellps=GRS80 +step +proj=pop +v_3 +step
## +proj=utm +zone=25 +south +ellps=GRS80"
## attr(,"description")
## [1] "Inverse of UTM zone 25S + Corrego Alegre 1970-72 to SIRGAS 2000 (2) + 
## UTM zone 25S"

## -----------------------------------------------------------------------------
scot_BNG <- readOGR(system.file("vectors", package="rgdal"), "scot_BNG")

## -----------------------------------------------------------------------------
## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS =
## dumpSRS): Discarded datum OSGB_1936 in CRS definition: +proj=tmerc +lat_0=49
## +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs
## OGR data source with driver: ESRI Shapefile 
## Source: "/home/rsb/lib/r_libs/rgdal/vectors", layer: "scot_BNG"
## with 56 features
## It has 13 fields
## Warning in OGRSpatialRef(dsn = dsn, layer = layer, morphFromESRI =
## morphFromESRI, : Discarded datum OSGB_1936 in CRS definition: +proj=tmerc
## +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy
## +units=m +no_defs
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
## datum Unknown_based_on_Airy_1830_ellipsoid in CRS definition

## -----------------------------------------------------------------------------
(load_status <- get_transform_wkt_comment())

## -----------------------------------------------------------------------------
## [1] TRUE

## -----------------------------------------------------------------------------
set_transform_wkt_comment(FALSE)
scot_LL <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84"))
(b0 <- bbox(scot_LL))

## -----------------------------------------------------------------------------
##         min       max
## x -8.621387 -0.753056
## y 54.626555 60.843843

## -----------------------------------------------------------------------------
(crs <- slot(scot_BNG, "proj4string"))

## -----------------------------------------------------------------------------
## CRS arguments:
##  +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000
## +y_0=-100000 +ellps=airy +units=m +no_defs

## -----------------------------------------------------------------------------
slot(crs, "projargs")

## -----------------------------------------------------------------------------
## [1] "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
## +ellps=airy +units=m +no_defs"

## -----------------------------------------------------------------------------
slot(crs, "projargs") <- paste0(slot(crs, "projargs"), " +datum=OSGB36")
slot(scot_BNG, "proj4string") <- crs
scot_LL1 <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84"))
(b1 <- bbox(scot_LL1))

## -----------------------------------------------------------------------------
##         min       max
## x -8.622158 -0.755071
## y 54.626633 60.843232

## -----------------------------------------------------------------------------
all.equal(b0, b1, scale=1)

## -----------------------------------------------------------------------------
## [1] "Mean absolute difference: 0.0008689328"

## -----------------------------------------------------------------------------
diag(spDists(t(b0), t(b1), longlat=TRUE))*1000

## -----------------------------------------------------------------------------
## [1]  50.56708 128.99003

## -----------------------------------------------------------------------------
set_transform_wkt_comment(load_status)

## -----------------------------------------------------------------------------
scot_BNG <- readOGR(system.file("vectors", package="rgdal"), "scot_BNG")

## -----------------------------------------------------------------------------
## Warning in OGRSpatialRef(dsn, layer, morphFromESRI = morphFromESRI, dumpSRS =
## dumpSRS): Discarded datum OSGB_1936 in CRS definition: +proj=tmerc +lat_0=49
## +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs
## OGR data source with driver: ESRI Shapefile 
## Source: "/home/rsb/lib/r_libs/rgdal/vectors", layer: "scot_BNG"
## with 56 features
## It has 13 fields
## Warning in OGRSpatialRef(dsn = dsn, layer = layer, morphFromESRI =
## morphFromESRI, : Discarded datum OSGB_1936 in CRS definition: +proj=tmerc
## +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy
## +units=m +no_defs
## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
## datum Unknown_based_on_Airy_1830_ellipsoid in CRS definition

## -----------------------------------------------------------------------------
system.time(scot_LL2 <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84")))

## -----------------------------------------------------------------------------
##    user  system elapsed 
##   0.095   0.000   0.096

## -----------------------------------------------------------------------------
##    user  system elapsed 
##   3.431   0.037   3.546

## -----------------------------------------------------------------------------
(b2 <- bbox(scot_LL2))

## -----------------------------------------------------------------------------
##         min        max
## x -8.622158 -0.7550709
## y 54.626633 60.8432318

## -----------------------------------------------------------------------------
all.equal(b1, b2, scale=1)

## -----------------------------------------------------------------------------
## [1] "Mean absolute difference: 3.361822e-08"

## -----------------------------------------------------------------------------
get_last_coordOp()

## -----------------------------------------------------------------------------
## [1] "+proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717
## +x_0=400000 +y_0=-100000 +ellps=airy +step +proj=push +v_3 +step +proj=cart
## +ellps=airy +step +proj=helmert +x=446.448 +y=-125.157 +z=542.06 +rx=0.15 +ry=0.247
## +rz=0.842 +s=-20.489 +convention=position_vector +step +inv +proj=cart +ellps=WGS84
## +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg"

## ---- eval=run----------------------------------------------------------------
system.time(scot_LL3 <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84"),
                                    coordOp=get_last_coordOp()))

## -----------------------------------------------------------------------------
##    user  system elapsed 
##   0.066   0.002   0.070

## ---- eval=run----------------------------------------------------------------
all.equal(b2, bbox(scot_LL3), scale=1)

## -----------------------------------------------------------------------------
## [1] TRUE

## ---- eval=run----------------------------------------------------------------
wkt_source_datum <- comment(slot(scot_BNG, "proj4string"))
wkt_target_datum <- comment(CRS("+proj=longlat +datum=WGS84"))
x <- list_coordOps(wkt_source_datum, wkt_target_datum)
system.time(scot_LL4 <- spTransform(scot_BNG, CRS("+proj=longlat +datum=WGS84"),
                                    coordOp=best_instantiable_coordOp(x)))

## -----------------------------------------------------------------------------
##    user  system elapsed 
##   0.066   0.002   0.069

## ---- eval=run----------------------------------------------------------------
all.equal(b2, bbox(scot_LL4), scale=1)

## -----------------------------------------------------------------------------
## [1] TRUE

## -----------------------------------------------------------------------------
library(rgdal)

## -----------------------------------------------------------------------------
packageVersion("rgdal")
rgdal_extSoftVersion()

## -----------------------------------------------------------------------------
pt0_lonlat <- SpatialPoints(matrix(c(10,46), nrow=1), proj4string= CRS("+init=epsg:4326"))
pt0_laea89 <- spTransform(pt0_lonlat, CRS("+init=epsg:3035"))
pt0_wgs32n <- spTransform(pt0_lonlat, CRS("+init=epsg:32632"))
pt0_psmerc <- spTransform(pt0_lonlat, CRS("+init=epsg:3857"))

## -----------------------------------------------------------------------------
coordinates(pt0_lonlat)

## -----------------------------------------------------------------------------
coordinates(pt0_laea89)

## -----------------------------------------------------------------------------
coordinates(pt0_wgs32n)

## -----------------------------------------------------------------------------
coordinates(pt0_psmerc)

## -----------------------------------------------------------------------------
from <- list(pt0_lonlat=pt0_lonlat, pt0_psmerc=pt0_psmerc, pt0_wgs32n=pt0_wgs32n, pt0_laea89=pt0_laea89)
names(from)

## -----------------------------------------------------------------------------
sapply(from, proj4string)

## -----------------------------------------------------------------------------
to <- c("4326", "3857", "32632", "3035")

## -----------------------------------------------------------------------------
get_enforce_xy()
set_enforce_xy(FALSE)
get_enforce_xy()

## -----------------------------------------------------------------------------
run <- TRUE
if (packageVersion("sp") < "1.3.3") run <- FALSE
if (packageVersion("rgdal") < "1.5.3") run <- FALSE
if (run && !rgdal::new_proj_and_gdal()) run <- FALSE

## ---- warning=FALSE, message=FALSE, eval=run----------------------------------
out_EPSG_non_viz <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_EPSG_non_viz) <- names(from)
rownames(out_EPSG_non_viz) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    pt1 <- spTransform(from[[j]], CRS(SRS_string = paste0("EPSG:", to[i])))
    out_EPSG_non_viz[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_EPSG_non_viz

## -----------------------------------------------------------------------------
set_enforce_xy(TRUE)
get_enforce_xy()

## ---- warning=FALSE, message=FALSE, eval=run----------------------------------
out_EPSG <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_EPSG) <- names(from)
rownames(out_EPSG) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    pt1 <- spTransform(from[[j]], CRS(SRS_string = paste0("EPSG:", to[i])))
    out_EPSG[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_EPSG

## -----------------------------------------------------------------------------
get_enforce_xy()
set_enforce_xy(FALSE)
get_enforce_xy()

## ---- warning=FALSE, message=FALSE--------------------------------------------
out_init_non_viz <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_init_non_viz) <- names(from)
rownames(out_init_non_viz) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    pt1 <- spTransform(from[[j]], CRS(paste0("+init=epsg:", to[i])))
    out_init_non_viz[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_init_non_viz

## -----------------------------------------------------------------------------
set_enforce_xy(TRUE)
get_enforce_xy()

## ---- warning=FALSE, message=FALSE--------------------------------------------
out_init <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_init) <- names(from)
rownames(out_init) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    pt1 <- spTransform(from[[j]], CRS(paste0("+init=epsg:", to[i])))
    out_init[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_init

## ---- warning=FALSE, message=FALSE, eval=run----------------------------------
out_EPSG <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_EPSG) <- names(from)
rownames(out_EPSG) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    coo1 <- list_coordOps(comment(slot(from[[j]], "proj4string")),
                          comment(CRS(SRS_string = paste0("EPSG:", to[i]))),
                          visualization_order=TRUE)
    pt1 <- spTransform(from[[j]], CRS(SRS_string = paste0("EPSG:", to[i])),
      coordOp=best_instantiable_coordOp(coo1))
    out_EPSG[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_EPSG

## ---- warning=FALSE, message=FALSE, eval=run----------------------------------
out_EPSG_non_viz1 <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_EPSG_non_viz1) <- names(from)
rownames(out_EPSG_non_viz1) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    coo1 <- list_coordOps(comment(slot(from[[j]], "proj4string")),
                          comment(CRS(SRS_string = paste0("EPSG:", to[i]))),
                          visualization_order=FALSE)
    pt1 <- spTransform(from[[j]], CRS(SRS_string = paste0("EPSG:", to[i])),
      coordOp=best_instantiable_coordOp(coo1))
    out_EPSG_non_viz1[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_EPSG_non_viz1

## ---- warning=FALSE, message=FALSE, eval=run----------------------------------
out_EPSG_non_viz2 <- matrix(as.logical(NA), nrow=4, ncol=4)
colnames(out_EPSG_non_viz2) <- names(from)
rownames(out_EPSG_non_viz2) <- to
for (j in seq(along=from)) {
  for (i in seq(along=to)) {
    pt1 <- spTransform(from[[j]], CRS(SRS_string = paste0("EPSG:", to[i])),
      enforce_xy=FALSE)
    out_EPSG_non_viz2[i, j] <- isTRUE(all.equal(coordinates(from[[i]]),
      coordinates(pt1)))
  }
}
out_EPSG_non_viz2

## -----------------------------------------------------------------------------
ll <- structure(c(12.1823368669203, 11.9149630062421, 12.3186076188739, 
12.6207597184845, 12.9955172054652, 12.6316117692658, 12.4680041846297, 
12.4366882666609, NA, NA, -5.78993051516384, -5.03798674888479, 
-4.60623015708619, -4.43802336997614, -4.78110320396188, -4.99127125409291, 
-5.24836150474498, -5.68430388755925, NA, NA), .Dim = c(10L, 
2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(xy0 <- project(ll, "+proj=moll", legacy=TRUE, verbose=TRUE))
if (!exists("xy0")) xy0 <- structure(c(1217100.8468177, 1191302.229156,
1232143.28841193, 1262546.27733232, 1299648.82357849, 1263011.18154638,
1246343.17808186, 1242654.33986052, NA, NA, -715428.207551599,
-622613.577983058, -569301.605757784, -548528.530156422, -590895.949857199,
-616845.926397351, -648585.161643274, -702393.1160979, NA, NA), 
.Dim = c(10L, 2L), .Dimnames = list(NULL, c("longitude", "latitude")))
try(ll0 <- project(xy0, "+proj=moll", inv=TRUE, legacy=TRUE, verbose=TRUE))
if (exists("ll0")) all.equal(ll, ll0)

## -----------------------------------------------------------------------------
try(xy1 <- project(ll, "+proj=moll", legacy=TRUE, coordOp=paste("+proj=pipeline +step",
"+proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=moll +lon_0=0 +x_0=0 +y_0=0 ellps=WGS84")))
try(ll1 <- project(xy1, "+proj=moll", inv=TRUE, legacy=TRUE, coordOp=paste("+proj=pipeline +step", 
"+inv +proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +step +proj=unitconvert +xy_in=rad +xy_out=deg")))
if (exists("ll1")) all.equal(ll, ll1)

## -----------------------------------------------------------------------------
WKT <- CRS("+proj=moll")
try(xy2 <- project(ll, comment(WKT), legacy=TRUE, verbose=TRUE))
try(ll2 <- project(xy1, comment(WKT), inv=TRUE, legacy=TRUE, verbose=TRUE))
if (exists("ll2")) all.equal(ll, ll2)

