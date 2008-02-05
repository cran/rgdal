# following upgrade request 070122

   if(!exists("Sys.setenv", envir = baseenv()))
     Sys.setenv <- Sys.putenv
   .rgdal_old.PROJ_LIB <- NULL
   .rgdal_old.GDAL_DATA <- NULL

#.First.lib <- function(lib, pkg) {
.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  .rgdal_old.PROJ_LIB <- Sys.getenv("PROJ_LIB")
  Sys.setenv("PROJ_LIB"=system.file("proj", package = "rgdal")[1])
  .rgdal_old.GDAL_DATA <- Sys.getenv("GDAL_DATA")
  Sys.setenv("GDAL_DATA"=system.file("gdal", package = "rgdal")[1])
  .zzz_xxx <- paste(.rgdal_old.PROJ_LIB, .rgdal_old.GDAL_DATA)
  rm(.zzz_xxx)

  library.dynam('rgdal', pkg, lib)

  .Call('RGDAL_Init', PACKAGE="rgdal")

  gdl <- Sys.getenv("GDAL_DATA")
  pl <- Sys.getenv("PROJ_LIB")

  Smess <- paste('Geospatial Data Abstraction Library ',
    'extensions to R successfully loaded\n',
    'Loaded GDAL runtime: ', getGDALVersionInfo(), '\n',
    paste(names(gdl[1]), ": ", gdl[1], sep=""), "\n",
    'Loaded PROJ.4 runtime: ', getPROJ4VersionInfo(), '\n',
    paste(names(pl[1]), ": ", pl[1], sep=""), "\n", sep="")
  packageStartupMessage(Smess, appendLF = FALSE)
}

#.Last.lib <- function(lib, pkg) {
.onUnload <- function(libpath) {
    Sys.setenv("PROJ_LIB"=.rgdal_old.PROJ_LIB)
    Sys.setenv("GDAL_DATA"=.rgdal_old.GDAL_DATA)
}

