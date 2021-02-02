make_EPSG <- function(file) {
        if (.Call("PROJ4VersionInfo", PACKAGE = "rgdal")[[2]] >= 600L &&
            !missing(file)) {
            warning("New proj interface does not use file argument")
            file <- NULL
        }
        metadata <- NULL
        if (missing(file) || is.null(file)) {
            tf <- tempfile()
            n <- .Call("PROJcopyEPSG", tf, PACKAGE="rgdal")
            if (PROJis6ormore()) {
                if (n <= 0) stop("PROJ 6 database empty")
                EPSG <- read.csv(tf, header=TRUE, stringsAsFactors=FALSE)
                if (requireNamespace("DBI", quietly=TRUE) && 
                    requireNamespace("RSQLite", quietly=TRUE)) {
                    shpr <- get_proj_search_paths()
                    db <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname=file.path(shpr[length(shpr)], "proj.db"))
                    md <- DBI::dbReadTable(db, "metadata")
                    DBI::dbDisconnect(db)
                    metadata <- md[md$key == "EPSG.VERSION", "value"]
                }
                attr(EPSG, "metadata") <- metadata
                return(EPSG)
            }
            if (n > 0) file <- tf
            else stop("Error opening epsg file")
       } else {
# bug fix 20150914 Alain Benard
           if (file.exists(file)) {
               n <- length(readLines(file))
           } else {
               stop("Error opening", file)
           }
       }
       pre5 <- .Call("PROJ4VersionInfo", PACKAGE = "rgdal")[[2]] < 500L
       if (!pre5) n <- n - 1
#	epsg <- file(system.file("proj/epsg", package="rgdal"), open="r")
# report Michael Sumner 071017
	epsg <- file(file, open="r")
	note <- character(n)
	prj4 <- character(n)
	code <- integer(n)
        if (!pre5) metadata <- readLines(epsg, n=1)
	for (i in seq(along=code)) { # FIXME - broken in 5.0.0
		res <- strsplit(paste(readLines(epsg, n=2), collapse=""), 
			"[<>]")
		note[i] <- res[[1]][1]
		prj4[i] <- res[[1]][3]
		code[i] <- as.integer(res[[1]][2])
		if (is.na(code[i])) res <- readLines(epsg, n=1)
	}
	close(epsg)
        prj4 <- scan(con <- textConnection(prj4), what=character(0), sep="\n",
                 strip.white=TRUE, quiet=TRUE)
        close(con)
#	prj4 <- scan(textConnection(prj4), what=character(0), sep="\n", 
#		strip.white=TRUE, quiet=TRUE)
	EPSG <- data.frame(code=I(code), note=I(note), prj4=I(prj4))
	EPSG <- EPSG[-which(is.na(EPSG$note)),]
        attr(EPSG, "metadata") <- metadata
	EPSG
}

