## -----------------------------------------------------------------------------
td <- tempfile()
dir.create(td)
Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)
library(rgdal)
data("GridsDatums")
GridsDatums[grep("Netherlands", GridsDatums$country),]

## ---- echo=FALSE--------------------------------------------------------------
mvrun <- FALSE
#if (require(mapview, quietly=TRUE) && .Platform$OS.type == "unix" && require(curl, quietly=TRUE) && curl::has_internet()) mvrun <- TRUE

## ---- eval=FALSE--------------------------------------------------------------
#  demo(meuse, ask=FALSE, package="sp", echo=FALSE)
#  library(mapview)
#  x <- mapview(meuse, zcol="zinc")
#  mapshot(x, file="meuse.png")

## -----------------------------------------------------------------------------
knitr::include_graphics(system.file("misc/meuse.png", package="rgdal"))

## ---- echo=FALSE--------------------------------------------------------------
odd_run <- FALSE
if (new_proj_and_gdal()) odd_run <- TRUE

## ---- eval=odd_run------------------------------------------------------------
ellps <- projInfo("ellps")
(clrk66 <- ellps[ellps$name=="clrk66",])

## ---- eval=odd_run------------------------------------------------------------
#eval(parse(text=clrk66$major))
#eval(parse(text=clrk66$ell))
a <- 6378206.4
b <- 6356583.8
print(sqrt((a^2-b^2)/a^2), digits=10)

## ---- eval=odd_run------------------------------------------------------------
(shpr <- get_proj_search_paths())

## ---- echo=FALSE, results='hide', eval=odd_run--------------------------------
if (is.null(shpr)) odd_run <- FALSE

## ---- echo=FALSE, results='hide'----------------------------------------------
run <- FALSE
if (require("RSQLite", quietly=TRUE)) run <- TRUE

## ---- eval=run && odd_run-----------------------------------------------------
library(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(shpr[length(shpr)], "proj.db"))
dbListTables(db)

## ---- eval=run && odd_run-----------------------------------------------------
(metadata <- dbReadTable(db, "metadata"))

## ---- warning=TRUE, eval=odd_run----------------------------------------------
cat(wkt(CRS(SRS_string="OGC:CRS84")), "\n")

## ---- warning=TRUE, eval=odd_run----------------------------------------------
b_pump <- readOGR(system.file("vectors/b_pump.gpkg", package="rgdal"))

## ---- warning=TRUE, eval=odd_run----------------------------------------------
proj4string(b_pump)

## ---- eval=odd_run------------------------------------------------------------
if (packageVersion("sp") > "1.4.1") {
  WKT <- wkt(b_pump)
} else {
  WKT <- comment(slot(b_pump, "proj4string"))
}
cat(WKT, "\n")

## ---- eval=run && odd_run-----------------------------------------------------
cov <- dbReadTable(db, "coordinate_operation_view")
cov[grep("OSGB", cov$name), c("table_name", "code", "name", "method_name", "accuracy")]

## ---- eval=odd_run------------------------------------------------------------
list_coordOps(paste0(proj4string(b_pump), " +type=crs"), "EPSG:4326")

## ---- eval=odd_run------------------------------------------------------------
set_enforce_xy(FALSE)
list_coordOps(paste0(proj4string(b_pump), " +type=crs"), "EPSG:4326")
set_enforce_xy(TRUE)

## ---- eval=odd_run------------------------------------------------------------
set_transform_wkt_comment(FALSE)
isballpark <- spTransform(b_pump, CRS(SRS_string="OGC:CRS84"))
get_last_coordOp()

## ---- eval=odd_run------------------------------------------------------------
print(coordinates(isballpark), digits=10)

## ---- eval=run && odd_run-----------------------------------------------------
helm <- dbReadTable(db, "helmert_transformation_table")
helm[helm$code == "1314", c("auth_name", "code", "name", "tx", "ty", "tz", "rx", "ry", "rz", "scale_difference")]
dbDisconnect(db)

## ---- eval=odd_run------------------------------------------------------------
set_transform_wkt_comment(TRUE)
is2m <- spTransform(b_pump, CRS(SRS_string="OGC:CRS84"))
get_last_coordOp()

## ---- eval=odd_run------------------------------------------------------------
print(coordinates(is2m), digits=10)

## ---- eval=odd_run------------------------------------------------------------
c(spDists(isballpark, is2m)*1000)

## ---- echo=FALSE--------------------------------------------------------------
mrun <- FALSE
if (suppressPackageStartupMessages(require(maptools, quietly=TRUE))) mrun <- TRUE

## ---- eval=mrun && odd_run----------------------------------------------------
c(maptools::gzAzimuth(coordinates(isballpark), coordinates(is2m)))

## ---- eval=odd_run------------------------------------------------------------
(a <- project(coordinates(b_pump), proj4string(b_pump), inv=TRUE, verbose=TRUE))
(b <- project(coordinates(b_pump), WKT, inv=TRUE))

## ---- eval=odd_run------------------------------------------------------------
all.equal(a, b)
c(spDists(coordinates(isballpark), a)*1000)

## ---- eval=odd_run------------------------------------------------------------
list_coordOps(WKT, "OGC:CRS84")

## ---- eval=run && odd_run-----------------------------------------------------
if (is.projected(b_pump)) { 
  o <- project(t(unclass(bbox(b_pump))), wkt(b_pump), inv=TRUE)
} else {
  o <- t(unclass(bbox(b_pump)))
}
(aoi <- c(t(o + c(-0.1, +0.1))))

## ---- eval=run && odd_run-----------------------------------------------------
nrow(list_coordOps(WKT, "OGC:CRS84", area_of_interest=aoi))

## ---- eval=run && odd_run-----------------------------------------------------
nrow(list_coordOps(WKT, "OGC:CRS84", strict_containment=TRUE, area_of_interest=aoi))

## ---- eval=FALSE--------------------------------------------------------------
#  td <- tempfile()
#  dir.create(td)
#  Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)
#  library(rgdal)

## ---- echo=FALSE, results='hide', eval=odd_run--------------------------------
run <- run && (attr(getPROJ4VersionInfo(), "short") >= 710)

## ---- eval=run && odd_run-----------------------------------------------------
is_proj_CDN_enabled()

## ---- eval=run && odd_run-----------------------------------------------------
enable_proj_CDN()
is_proj_CDN_enabled()

## ---- eval=run && odd_run-----------------------------------------------------
shpr[1]
try(file.size(file.path(shpr[1], "cache.db")))

## ---- eval=run && odd_run-----------------------------------------------------
list_coordOps(WKT, "OGC:CRS84", area_of_interest=aoi)

## ---- eval=run && odd_run-----------------------------------------------------
system.time(is1m <- spTransform(b_pump, CRS(SRS_string="OGC:CRS84")))

## ---- eval=run && odd_run-----------------------------------------------------
get_last_coordOp()

## ---- eval=run && odd_run-----------------------------------------------------
print(coordinates(is1m), digits=10)

## ---- eval=run && odd_run-----------------------------------------------------
c(spDists(is2m, is1m)*1000)

## ---- eval=mrun && run && odd_run---------------------------------------------
c(maptools::gzAzimuth(coordinates(is1m), coordinates(is2m)))

## ---- eval=run && odd_run-----------------------------------------------------
try(file.size(file.path(shpr[1], "cache.db")))

## ---- eval=run && odd_run-----------------------------------------------------
library(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(shpr[1], "cache.db"))
dbListTables(db)
dbReadTable(db, "chunks")
dbDisconnect(db)

## ---- eval=run && odd_run-----------------------------------------------------
disable_proj_CDN()
is_proj_CDN_enabled()

## ---- eval=FALSE--------------------------------------------------------------
#  library(mapview)
#  x <- mapview(is2m, map.type="OpenStreetMap", legend=FALSE) + mapview(is1m, col.regions="green", legend=FALSE) + mapview(isballpark, col.regions="red", legend=FALSE)
#  mapshot(x, file="snow.png")

## -----------------------------------------------------------------------------
knitr::include_graphics(system.file("misc/snow.png", package="rgdal"))

## ---- eval=odd_run------------------------------------------------------------
# library(CARBayesdata)
library(sp)
# data(GGHB.IG)
# orig <- slot(GGHB.IG, "proj4string")
(load(system.file("misc/GGHB.IG_CRS.rda", package="rgdal")))
orig

## ---- eval=odd_run, warning=FALSE---------------------------------------------
sp_version_ok <- length(grep("get_source_if_boundcrs", deparse(args(sp::CRS)))) > 0L

## ---- eval=odd_run && sp_version_ok, warning=FALSE----------------------------
orig1 <- CRS(slot(orig, "projargs"), get_source_if_boundcrs=FALSE)
cat(wkt(orig1), "\n")

## ---- eval=odd_run, warning=FALSE---------------------------------------------
orig1a <- CRS(slot(orig, "projargs"))
cat(wkt(orig1a), "\n")

## ---- eval=odd_run, warning=FALSE---------------------------------------------
orig1b <- rebuild_CRS(orig)
cat(wkt(orig1b), "\n")

