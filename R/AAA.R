

#require(methods)
#require(sp)
#
#.First.lib <- function(libname, pkgname){
#	library.dynam("spproj", pkgname, libname)
#}

# .noGenerics <- TRUE

#if (!isGeneric("transform"))
#	setGeneric("transform", function(x, ...)
#		standardGeneric("transform"))

#.onLoad <- function(lib, pkg) {
#	require(methods)
#	require(sp)
#	.spproj_old.PROJ_LIB <- Sys.getenv("PROJ_LIB")
#	Sys.putenv("PROJ_LIB"=system.file("proj", package = "spproj")[1])
#}
#
#.onUnload <- function(libpath) {
#    Sys.putenv("PROJ_LIB"=.spproj_old.PROJ_LIB)
#    library.dynam.unload("spproj", libpath)
#}
