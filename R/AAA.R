# following upgrade request 070122

if(!exists("Sys.setenv", envir = baseenv()))
     Sys.setenv <- Sys.putenv
.RGDAL_CACHE <- new.env(FALSE, parent=globalenv())
assign(".rgdal_old.PROJ_LIB", "", envir=.RGDAL_CACHE)
assign(".rgdal_old.GDAL_DATA", "", envir=.RGDAL_CACHE)
assign(".rgdal_set.PROJ_LIB", "", envir=.RGDAL_CACHE)
assign(".rgdal_set.GDAL_DATA", "", envir=.RGDAL_CACHE)

#.First.lib <- function(lib, pkg) {
.onLoad <- function(lib, pkg) {
  load_stuff()
}

load_stuff <- function() {
  assign(".rgdal_old.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.RGDAL_CACHE)
  assign(".rgdal_old.GDAL_DATA", Sys.getenv("GDAL_DATA"), envir=.RGDAL_CACHE)
  assign(".rgdal_old.NEEDED", FALSE, envir=.RGDAL_CACHE)
  if (file.exists(system.file("proj/nad.lst", package = "rgdal")[1])) {
  prj = system.file("proj", package = "rgdal")[1]
    if (PROJis6ormore() && 
      .Call("PROJ4VersionInfo", PACKAGE="rgdal")[[2]] < 700) {
      set_is <- set_proj_search_paths(prj)
    } else {
      Sys.setenv("PROJ_LIB"=prj)
    }
    assign(".rgdal_set.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.RGDAL_CACHE)
    Sys.setenv("GDAL_DATA"=system.file("gdal", package = "rgdal")[1])
    assign(".rgdal_set.GDAL_DATA", Sys.getenv("GDAL_DATA"),
      envir=.RGDAL_CACHE)
    assign(".rgdal_old.NEEDED", TRUE, envir=.RGDAL_CACHE)
  } else if (.Platform$OS.type == "windows") {
    assign(".rgdal_OSGeo4W", Sys.getenv("OSGEO4W_ROOT"), envir=.RGDAL_CACHE)
  }
  assign("OVERRIDE_PROJ_DATUM_WITH_TOWGS84", TRUE, envir=.RGDAL_CACHE)
  assign("silent", TRUE, envir=.RGDAL_CACHE)
  assign("has_proj_def.dat", as.logical(NA), envir=.RGDAL_CACHE)
  assign("P6_datum_hard_fail", FALSE, envir=.RGDAL_CACHE)
  assign("transform_wkt_comment", new_proj_and_gdal(), envir=.RGDAL_CACHE)
  assign(".last_coordOp", "", envir=.RGDAL_CACHE)
  assign("thin_PROJ6_warnings", FALSE, envir=.RGDAL_CACHE)
  assign("PROJ6_warnings_count", 0L, envir=.RGDAL_CACHE)
  assign("enforce_xy", TRUE, envir=.RGDAL_CACHE)
  assign("prefer_proj", TRUE, envir=.RGDAL_CACHE)
  rgdal_show_exportToProj4_warnings <- options("rgdal_show_exportToProj4_warnings")
  if (!is.null(rgdal_show_exportToProj4_warnings)) {
    if (!(rgdal_show_exportToProj4_warnings %in% c("all", "thin", "none"))) {
# CURRENT DEFAULT: "all"
      rgdal_show_exportToProj4_warnings <- "all"
    }
  } else {
# CURRENT DEFAULT: "all"
    rgdal_show_exportToProj4_warnings <- "all"
  }
  if (rgdal_show_exportToProj4_warnings == "all") {
    assign("rgdal_show_exportToProj4_warnings", TRUE, envir=.RGDAL_CACHE)
  } else if (rgdal_show_exportToProj4_warnings == "thin") {
    assign("rgdal_show_exportToProj4_warnings", TRUE, envir=.RGDAL_CACHE)
    assign("thin_PROJ6_warnings", TRUE, envir=.RGDAL_CACHE)
  } else {
    assign("rgdal_show_exportToProj4_warnings", FALSE, envir=.RGDAL_CACHE)
  }
  local_RGDAL_Init() #.Call('RGDAL_Init', PACKAGE="rgdal")
}

local_RGDAL_Init <- function() .Call('RGDAL_Init', PACKAGE="rgdal")

.onAttach <- function(lib, pkg) {
  ver_ok <- getGDALCheckVersion()
  rver <- getGDALVersionInfo()

  gdl <- getGDAL_DATA_Path()
#  skip_writable <- Sys.getenv("PROJ_SKIP_READ_USER_WRITABLE_DIRECTORY")
#  if (nchar(skip_writable) == 0)
#    Sys.setenv("PROJ_SKIP_READ_USER_WRITABLE_DIRECTORY"="true")
  pl <- getPROJ4libPath()
  if (nchar(pl) == 0) {
    if (is.null(attr(pl, "search_path"))) pl <- "(autodetected)"
    else pl <- attr(pl, "search_path")
  }
  fn <- system.file("SVN_VERSION", package="rgdal")
  if (file.exists(fn)) {
    svn_version <- scan(fn, what=character(1), sep="\n", quiet=TRUE)
  } else {
    svn_version <- "(unknown)"
  }
  pdd <- .Call("PROJ4_proj_def_dat_Installed", PACKAGE="rgdal")
  if (is.null(pdd) && PROJis6ormore()) pdd <- TRUE
  if (is.na(get("has_proj_def.dat", envir=.RGDAL_CACHE))) {
    assign("has_proj_def.dat", pdd, envir=.RGDAL_CACHE)
  }

  Smess <- paste('rgdal: version: ',
    utils::packageDescription("rgdal")$Version,
    ', (SVN revision ', svn_version, ')\n',
    'Geospatial Data Abstraction Library ',
    'extensions to R successfully loaded\n',
    'Loaded GDAL runtime: ', rver, ifelse(ver_ok, '\n',
    '\n   but rgdal build and GDAL runtime not in sync:\n   ... consider re-installing rgdal!!\n'),
    paste("Path to GDAL shared files: ", gdl[1], sep=""), "\n",
    ifelse(GDAL_iconv(), "",
        paste(" GDAL does not use iconv for recoding strings.\n")),
    paste('GDAL binary built with GEOS:', getGDALwithGEOS(), "\n"),
    'Loaded PROJ runtime: ', getPROJ4VersionInfo(), '\n',
    paste("Path to PROJ shared files: ", pl[1], sep=""), "\n",
    ifelse((get("has_proj_def.dat", envir=.RGDAL_CACHE)  || (PROJis6ormore())), "", "WARNING: no proj_defs.dat in PROJ.4 shared files\n"), sep="")
  CDN_enabled <- is_proj_CDN_enabled()
  if (!is.null(CDN_enabled)) {
    Smess <- paste(Smess, "PROJ CDN enabled: ", CDN_enabled, "\n", sep="")
    if (CDN_enabled) paste(Smess, "PROJ CDN directory: ",
        proj_CDN_user_writable_dir(), "\n", sep="")
  }
  splVersion <- version_sp_linkingTo()
  Smess <- paste(Smess, "Linking to sp version:", splVersion, "\n", sep="")
  spVcheck <- NULL
  if("sp" %in% .packages()) 
    spVcheck <- utils::packageVersion("sp") == splVersion
  if (!is.null(spVcheck) && !spVcheck) paste(Smess, 
    "sp version used to install rgdal and loaded sp version differ\n")
  if (PROJis6ormore()) Smess <- paste(Smess, "To mute warnings of possible GDAL/OSR exportToProj4() degradation,\nuse options(\"rgdal_show_exportToProj4_warnings\"=\"none\") before loading rgdal.\n", sep="")
  if (nzchar(get(".rgdal_set.PROJ_LIB", envir=.RGDAL_CACHE))) {
    Smess <- paste(Smess, "Overwritten PROJ_LIB was ",
      get(".rgdal_set.PROJ_LIB", envir=.RGDAL_CACHE), "\n", sep="")
  }
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

rgdal_extSoftVersion <- function() {
  res <- c("GDAL"=strsplit(strsplit(getGDALVersionInfo(), ",")[[1]][1], " ")[[1]][2], "GDAL_with_GEOS"=as.character(getGDALwithGEOS()), "PROJ"=strsplit(strsplit(getPROJ4VersionInfo(), ",")[[1]][1], " ")[[1]][2], "sp"=version_sp_linkingTo())
  res
}

check_sp_version <- function() {
  splVersion <- version_sp_linkingTo()
  spVcheck <- NULL
  if("sp" %in% .packages()) 
    spVcheck <- utils::packageVersion("sp") == splVersion
  if (!is.null(spVcheck) && !spVcheck)
    warning("sp version used to install rgdal and loaded sp version differ")
}

