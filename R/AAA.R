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
  .rgdal_old.GDAL_DATA <- Sys.getenv("GDAL_DATA")
  if (file.exists(system.file("proj/proj_def.dat", package = "rgdal")[1])) {
    Sys.setenv("PROJ_LIB"=system.file("proj", package = "rgdal")[1])
    Sys.setenv("GDAL_DATA"=system.file("gdal", package = "rgdal")[1])
    .zzz_xxx <- paste(.rgdal_old.PROJ_LIB, .rgdal_old.GDAL_DATA)
    rm(.zzz_xxx)
  } else if (.Platform$OS.type == "windows") {
    .rgdal_OSGeo4W <- Sys.getenv("OSGEO4W_ROOT")
  }

  library.dynam('rgdal', pkg, lib)

  .Call('RGDAL_Init', PACKAGE="rgdal")

  gdl <- getGDAL_DATA_Path()
  pl <- getPROJ4libPath()
  if (nchar(pl) == 0) pl <- "(autodetected)"

  Smess <- paste('Geospatial Data Abstraction Library ',
    'extensions to R successfully loaded\n',
    'Loaded GDAL runtime: ', getGDALVersionInfo(), '\n',
    paste("Path to GDAL shared files: ", gdl[1], sep=""), "\n",
    'Loaded PROJ.4 runtime: ', getPROJ4VersionInfo(), '\n',
    paste("Path to PROJ.4 shared files: ", pl[1], sep=""), "\n", sep="")
  packageStartupMessage(Smess, appendLF = FALSE)
}

#.Last.lib <- function(lib, pkg) {
.onUnload <- function(libpath) {
    Sys.setenv("PROJ_LIB"=.rgdal_old.PROJ_LIB)
    Sys.setenv("GDAL_DATA"=.rgdal_old.GDAL_DATA)
}

