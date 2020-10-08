list_coordOps <- function(src_crs, tgt_crs, area_of_interest=as.numeric(NA), strict_containment=FALSE, visualization_order=NULL) {
    stopifnot(is.character(src_crs))
    stopifnot(length(src_crs) == 1L)
    stopifnot(is.character(tgt_crs))
    stopifnot(length(tgt_crs) == 1L)
    stopifnot(is.numeric(area_of_interest))
    stopifnot((length(area_of_interest) == 1L && is.na(area_of_interest)) ||
        (length(area_of_interest) == 4L))
    stopifnot(is.logical(strict_containment))
    stopifnot(length(strict_containment) == 1L)
    if (!is.null(visualization_order)) {
        stopifnot(is.logical(visualization_order))
        stopifnot(length(visualization_order) == 1L)
        stopifnot(!is.na(visualization_order))
    } else {
        visualization_order <- get_enforce_xy()
    }
    res <- .Call("list_coordinate_ops", src_crs, tgt_crs, area_of_interest, 
        strict_containment, visualization_order, PACKAGE="rgdal")
    if (is.null(res)) stop("function not available without PROJ 6")
    names(res) <- c("description", "definition", "accuracy", "instantiable", "ballpark", "number_grids")
    is.na(res$accuracy) <- res$accuracy < 0
    grids <- res[[7]]
    res[[7]] <- NULL
    defs <- res$definition
    nzdefs <- nzchar(defs)
    res$definition[nzdefs] <- unname(sapply(defs[nzdefs], function(x) gsub(" ",
        " +", paste0("+", x))))
    res$definition[!nzdefs] <- as.character(NA)
    res <- as.data.frame(res, stringsAsFactors=FALSE)
    attr(res, "grids") <- grids
    attr(res, "src_crs") <- src_crs
    attr(res, "tgt_crs") <- tgt_crs
    attr(res, "area_of_interest") <- area_of_interest
    attr(res, "strict_containment") <- strict_containment
    attr(res, "visualization_order") <- visualization_order
    class(res) <- c("coordOps", class(res))
    res
}

prettify_wkt <- function(inSRID) {
    if (substring(inSRID, 1, 1) == " ") stop("string starts with space")
    if (substring(inSRID, 1, 1) == "+")
        res <- strwrap(inSRID, exdent=8, width=0.8*getOption("width"))
    if (substring(inSRID, 1, 3) == "urn") res <- inSRID
    if (substring(inSRID, 1, 1) == "P" || substring(inSRID, 1, 1) == "G" || substring(inSRID, 1, 1) == "B" || substring(inSRID, 1, 1) == "S") 
        res <- strwrap(gsub(",", ", ", inSRID), exdent=8,
            width=0.8*getOption("width"))
    if (substring(inSRID, 1, 4) == "EPSG") res <- inSRID
    if (substring(inSRID, 1, 4) == "ESRI") res <- inSRID
    if (substring(inSRID, 1, 3) == "OGC") res <- inSRID
    res
}

print.coordOps <- function(x, ...) {
    n <- nrow(x)
    cat("Candidate coordinate operations found: ", n, "\n")
    if (!all(is.na(attr(x, "area_of_interest"))))
        cat("Search specification:", paste("Area of interest: ", 
            paste(attr(x, "area_of_interest"),
            collapse=", ")), "\n")
    cat("Strict containment: ", attr(x, "strict_containment"), "\n")
    cat("Visualization order: ", attr(x, "visualization_order"), "\n")
    in_str <- prettify_wkt(attr(x, "src_crs"))
    if (length(in_str) == 1) cat("Source:", in_str, "\n")
    else {cat("Source: "); cat(in_str, sep="\n")}
    out_str <- prettify_wkt(attr(x, "tgt_crs"))
    if (length(out_str) == 1) cat("Target:", out_str, "\n")
    else cat("Target:", out_str, sep="\n")
    nos <- which(!x$instantiable)
    if (length(nos) > 0L) xx <- x[-nos,]
    else xx <- x
    xx <- xx[order(xx$accuracy),]
    if (is.na(xx$accuracy[1]))
      cat("Best instantiable operation has only ballpark accuracy", "\n")
    else cat("Best instantiable operation has accuracy:", xx$accuracy[1], "m\n")
    cat("Description: ")
    desc <- strwrap(xx$description[1], exdent=13, width=0.8*getOption("width"))
    if (length(desc) == 1L) cat(desc, "\n")
    else cat(desc, sep="\n")
    cat("Definition:  ")
    def <- strwrap(xx$definition[1], exdent=13, width=0.8*getOption("width"))
    if (length(def) == 1L) cat(def, "\n")
    else cat(def, sep="\n")
    if (length(nos) > 0L) {
        grds <- attr(x, "grids")
        for (i in seq(along=nos)) {
            grd <- grds[[nos[i]]]
            ii <- length(grd)
            if (ii > 0L) {
                cat("Operation", nos[i], "is lacking", ii,
                    ifelse(ii == 1L, "grid", "grids"),
                    "with accuracy", x$accuracy[nos[i]], "m\n")
                for (j in 1:ii) {
                    cat("Missing grid:", grd[[j]][[1]], "\n")
                    if (nzchar(grd[[j]][[2]])) cat("Name:", grd[[j]][[2]],
                        "\n")
                    if (nzchar(grd[[j]][[4]])) cat("URL:", grd[[j]][[4]],
                        "\n")
                }
            }
        }
    }
    
    invisible(x)    
}

best_instantiable_coordOp <- function(x) {
    stopifnot(inherits(x, "coordOps"))
    nos <- which(!x$instantiable)
    if (length(nos) > 0L) xx <- x[-nos,]
    else xx <- x
    xx <- xx[order(xx$accuracy),]
    if (is.na(xx$accuracy[1]))
      warning("Best instantiable operation has only ballpark accuracy")
    res <- xx$definition[1]
    attr(res, "description") <- xx$description[1]
    res
} 
