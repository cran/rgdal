# following upgrade request 070122

if(!exists("Sys.setenv", envir = baseenv()))
     Sys.setenv <- Sys.putenv
.RGDAL_CACHE <- new.env(FALSE, parent=globalenv())
assign(".rgdal_old.PROJ_LIB", "", envir=.RGDAL_CACHE)
assign(".rgdal_old.GDAL_DATA", "", envir=.RGDAL_CACHE)

#.First.lib <- function(lib, pkg) {
.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  assign(".rgdal_old.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.RGDAL_CACHE)
  assign(".rgdal_old.GDAL_DATA", Sys.getenv("GDAL_DATA"), envir=.RGDAL_CACHE)
  assign(".rgdal_old.NEEDED", FALSE, envir=.RGDAL_CACHE)
  if (file.exists(system.file("proj/proj_def.dat", package = "rgdal")[1])) {
    Sys.setenv("PROJ_LIB"=system.file("proj", package = "rgdal")[1])
    Sys.setenv("GDAL_DATA"=system.file("gdal", package = "rgdal")[1])
    assign(".rgdal_old.NEEDED", TRUE, envir=.RGDAL_CACHE)
  } else if (.Platform$OS.type == "windows") {
    assign(".rgdal_OSGeo4W", Sys.getenv("OSGEO4W_ROOT"), envir=.RGDAL_CACHE)
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
  if (get(".rgdal_old.NEEDED", envir=.RGDAL_CACHE)) {
    Sys.setenv("PROJ_LIB"=get(".rgdal_old.PROJ_LIB", envir=.RGDAL_CACHE))
    Sys.setenv("GDAL_DATA"=get(".rgdal_old.GDAL_DATA", envir=.RGDAL_CACHE))
  }
  .Call('RGDAL_Exit', PACKAGE="rgdal")
}

