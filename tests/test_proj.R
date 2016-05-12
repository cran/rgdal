suppressPackageStartupMessages(library(rgdal))
data(state)
xy <- cbind(state.center$x, state.center$y)
res <- project(xy, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=GRS80")
res1 <- project(res, "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=GRS80",
 inv=TRUE)
stopifnot(isTRUE(all.equal(res1, xy)))
crds <- matrix(data=c(9.05, 48.52), ncol=2)
a <- project(crds, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
stopifnot(isTRUE(all.equal(a, matrix(c(-5.917698, -1.87195), ncol=2), tolerance=.Machine$double.eps ^ 0.25)))
a1 <- project(a, paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"),
 inv=TRUE, use_ob_tran=TRUE)
stopifnot(isTRUE(all.equal(a1, crds, tolerance=.Machine$double.eps ^ 0.25)))
states <- data.frame(state.x77, state.center)
states <- states[states$x > -121,]
coordinates(states) <- c("x", "y")
proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
state.ll <- spTransform(state.ll83, CRS("+proj=longlat +ellps=clrk66"))
stopifnot(isTRUE(all.equal(coordinates(states), coordinates(state.ll))))
spPoint <- SpatialPoints(coords=crds,
 proj4string=CRS("+proj=longlat +ellps=sphere +no_defs"))
a <- spTransform(spPoint, CRS(paste("+proj=ob_tran +o_proj=longlat",
 "+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs")),
 use_ob_tran=TRUE)
stopifnot(isTRUE(all.equal(unname(coordinates(a)), matrix(c(-5.917698, -1.87195), ncol=2), tolerance=.Machine$double.eps ^ 0.25)))
a1 <- spTransform(a, CRS("+proj=longlat +ellps=sphere +no_defs"),
 use_ob_tran=TRUE)
stopifnot(isTRUE(all.equal(unname(coordinates(a1)), unname(coordinates(spPoint)), tolerance=.Machine$double.eps ^ 0.25)))
projs <- as.character(projInfo()$name)
res <- logical(length(projs))
names(res) <- projs
msgs <- character(length(projs))
names(msgs) <- projs
for (i in seq(along=res)) {
  iprs <- paste("+proj=", projs[i], sep="")
  xy <- try(project(cbind(0, 0), iprs, legacy=TRUE), silent=TRUE)
  if (class(xy) == "try-error") {
    res[i] <- NA
    msgs[i] <- paste("fwd:", strsplit(xy, "\n")[[1]][2])
  } else {
    out <- try(project(xy, iprs, inv=TRUE, legacy=TRUE), silent=TRUE)
    if (class(out) == "try-error") {
      res[i] <- NA
      msgs[i] <- paste("inv:", strsplit(out, "\n")[[1]][2])
    } else res[i] <- isTRUE(all.equal(cbind(0,0), out))
  }
}
res
msgs
