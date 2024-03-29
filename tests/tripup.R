suppressPackageStartupMessages(library(rgdal))
load(system.file("etc/test_dfs.RData", package="rgdal"))
load(system.file("etc/obj_with_comments.RData", package="rgdal"))
Ps1_nc <- Ps1
comment(Ps1_nc) <- NULL
comment(slot(Ps1_nc, "polygons")[[1]]) <- NULL
drivers <- c("GeoJSON", "ESRI Shapefile")
drivers <- drivers[drivers %in% ogrDrivers()$name]
tfbase <- tempfile()
for (driver in drivers) {
tf <- paste0(tfbase, driver, "P", sep=".")
writeOGR(SpatialPolygonsDataFrame(P, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("P with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(P, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(P, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(P, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "Ph", sep=".")
writeOGR(SpatialPolygonsDataFrame(Ph, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Ph with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(Ph, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(Ph, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Ph, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "Phs", sep=".")
writeOGR(SpatialPolygonsDataFrame(Phs, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Phs with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(Phs, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(Phs, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Phs, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "MP", sep=".")
writeOGR(SpatialPolygonsDataFrame(MP, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("MP with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(MP, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(MP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(MP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "MPh", sep=".")
writeOGR(SpatialPolygonsDataFrame(MPh, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("MPh with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(MPh, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(MPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(MPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
load(system.file("etc/obj_without_comments.RData", package="rgdal"))
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "aP", sep=".")
writeOGR(SpatialPolygonsDataFrame(aP, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("aP with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(aP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(aP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "aPh", sep=".")
writeOGR(SpatialPolygonsDataFrame(aPh, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("aPh with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(aPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(aPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "aMP", sep=".")
writeOGR(SpatialPolygonsDataFrame(aMP, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("aMP with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(aMP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(aMP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "aMPh", sep=".")
writeOGR(SpatialPolygonsDataFrame(aMPh, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("aMPh with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(aMPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(aMPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "Ps1", sep=".")
writeOGR(SpatialPolygonsDataFrame(Ps1, data=df1), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Ps1 with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(Ps1, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Ps1, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
if (suppressPackageStartupMessages(requireNamespace("rgeos", quietly=TRUE))) {
tf <- paste0(tfbase, driver, "Ps1_nc", sep=".")
writeOGR(SpatialPolygonsDataFrame(Ps1_nc, data=df1), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Ps1_nc with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" coords", isTRUE(all.equal(lapply(slot(Ps1, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Ps1, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
}
}
