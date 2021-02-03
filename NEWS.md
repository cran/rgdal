# Version 1.5-23 (development, rev. 1120-1121)

* Further fallout after removing valgrind issues.

# Version 1.5-22 (2021-02-02, rev. 1106-1119)

* Attempt to remove further valgrind leak in proj6.cpp: PROJcopyEPSG() and in ogr_proj.cpp, both wrongly placed object destructors.

* Modified roundtripping all declared projections in ?project examples  because some listed projections for PROJ >= 5 provoke valgrind leakages by returning very large out-of-scope values for input coordinates (0, 0); inversion of these is not attempted; some listed projections are not projections.

# Version 1.5-21 (2021-01-27, rev. 1093-1105)

* Suggest **rgeos** to write pre-SFS multipolygon objects to avoid unpleasant workaround.

* Try to eliminate current valgrind leaks, starting from (https://github.com/r-spatial/gstat/issues/82).

* Try to increase robustness to installation with early PROJ 6 versions, which often lack functionality found necessary later (for example visualization order); the code had assumed that this function always was available and behaved as it now does. There are now graceful failures when not available.

# Version 1.5-19 (2021-01-05, rev. 1083-1092)

* Dan Baston: raster speedups 

* PROJ 7.2.1 includes a bug-fix for `+proj=ob_tran` cases that required changes in detection and handling of target/source CRS reversal, https://lists.osgeo.org/pipermail/proj/2020-December/009999.html

# Version 1.5-18 (2020-10-13, rev. 1071-1082)

* condition `tests/test_enforce_xy.R` on PROJ >= 6 and GDAL >= 3 (email BDR, I forgot to re-check with PROJ-5.2.0/GDAL-2.2.4).

* Adaptation to EPSG 10 database format started (from PROJ 7.2); choose DB columns by name not number in vignette.

# Version 1.5-17 (2020-10-08, rev. 1051-1070)

* `"CRS"` instantiation now prefers PROJ: use `rgdal::set_prefer_proj(FALSE)` to return to earlier behaviour. It seems that access from C/C++ code to mechanisms in PROJ offers more depth than going through GDAL to PROJ. This `"CRS"` instantiation in **sp** and **raster**; Proj4 and WKT2 strings may differ depending on whether instantiation is straight from PROJ or goes via GDAL. Confirmed with multiple reverse dependency checks over almost 900 CRAN packages.

* By default use PROJ function to extract the source CRS from a `"BOUNDCRS"`. When `+towgs84=` is given, PROJ and GDAL see the apparent source Proj4 string as implicitly implying a coordinate operation transforming to target WGS84, leading to the WKT2 representation being a `"BOUNDCRS"`, not a `"PROJCRS"` or `"GEOGCRS"`, and thus causing misunderstandings later in searching for the most accurate coordinate operation for a transformation. May be controlled by setting the `get_source_if_boundcrs=` in `sp::CRS()` from **sp** 1.4-4 (2020-10-07). Confirmed with multiple reverse dependency checks over almost 900 CRAN packages.

* Add support for instantiating from `"OGC:CRS84"` to provide a guaranteed GIS/visualization axis order WGS84 instantiator (preferred to `"EPSG:4326"`).

* Permit empty string in `SRS_string=` argument to `sp::CRS()` and functions called by it.

* Use GDAL `ORSIsProjected()` instead of simply looking for `"+proj=longlat"` in the Proj4 string representation where possible.

# Version 1.5-16 (2020-08-07, rev. 1047-1050)

* Typo in C code; use `try()` around Area-of-Interest calculation for coordinate operations (email BDR, I forgot to re-check with PROJ-5.2.0/GDAL-2.2.4).

# Version 1.5-15 (2020-08-04, rev. 1020-1046)

* Add support for instantiating from `"ESRI:"`.

* Add Area-of-Interest to coordinate operation search (reduces the number of candidates found in many cases), and use in `rgdal::spTransform()` by default (`use_aoi=FALSE` to suppress); illustrate in vignette https://cran.r-project.org/web/packages/rgdal/vignettes/CRS_projections_transformations.html.

* Harden to condition on PROJ functions only available from 6.2.0; block `"+proj=ob_tran` tests for PROJ 6.0.0-6.1.1.

* Support PROJ CDN https://cdn.proj.org for on-demand download of transformation grids if requested by user; document in vignette https://cran.r-project.org/web/packages/rgdal/vignettes/CRS_projections_transformations.html.

# Version 1.5-12 (2020-06-26, rev. 1007-1019)

* Further corrections to `configure.ac` for older PROJ/GDAL versions

# Version 1.5-10 (2020-06-09, rev. 991-1006)

* Corrections to `configure.ac` for older PROJ/GDAL versions

# Version 1.5-8 (2020-05-28, rev. 846-990)

* Released to match **sp** 1.4.0 (2020-02-21) to 1.4-2 (2020-05-20) following months of development adapting to breaking changes in the external libraries used here: PROJ and GDAL; see also https://cran.r-project.org/web/packages/sp/news.html.

* Expose `options("rgdal_show_exportToProj4_warnings"="none")` to mute Proj4 string degradation warnings.

* Add new vignette https://cran.r-project.org/web/packages/rgdal/vignettes/CRS_projections_transformations.html.

* CRAN Windows binary uses PROJ >= 6 and GDAL >= 3

* Add PROJ-based CRS comparison: `compare_CRS()`.

* `project()` and `spTransform()` use WKT2 comment if available, fallback to Proj4 representation if not.

* List coordinate operations (based on pyproj code): `list_coordOps()`.

* Add `enforce_xy=` arguments to try to ensure that only GIS/visualization axis order is present.

* Add `"CRS"` object comment carrying WKT2 (2019) multiline string representation on read operations.

* Use `"CRS"` object comment carrying WKT2 (2019) multiline string representation on write operations.

# Versions 1.4-7 and 1.4-8

* Fixed configure issue for R 4.0.0

* Fixed length of `class()` errors for matrices becoming arrays in R 4.0.0

# Version 1.4-6 (Final pre-WKT2 release)

