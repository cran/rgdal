# Please note that **rgdal** will be retired during 2023, plan transition to sf/stars/terra functions using GDAL and PROJ at your earliest convenience (see https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs for guidance).

# Version 1.6-7 (development, rev. 1202-)

* Make retirement in October 2023 explicit, move package start-up messages to load from attach

# Version 1.6-6 (2023-04-18, rev. 1200-1201)

* add SharpYuv condition in src/Makevars.ucrt

# Version 1.6-5 (2023-03-02, rev. 1198-1199)

* Upgrade CXX11 to CXX17 (may cause problems for archaic OS)

# Version 1.6-4 (2023-01-12, rev. 1188-1197)

* Update GridsDatums data set

* Warn if retracted GDAL 3.6.0 is being used

* Update `makevars.ucrt` to Rtools 4.3

# Version 1.6-3 (2022-12-14, rev. 1184-1187)

* CRAN error in possibly writing outside tempdir

* CRAN error in printing stored stale `sessionInfo()` output in vignette

# Version 1.6-2 (2022-11-09, rev. 1177-1183)

* deprecation of `project()`, `CRSargs()`, `asSGDF_GROD()`, `readGDAL()`, `writeGDAL()`, `create2GDAL()`, `showWKT()`, `showEPSG()`, `readOGR()`, `ogrFIDs()`, `ogrInfo()`, `ogrDrivers()`, `writeOGR()`, `ogrListLayers()`, `GDALinfo()`, `print.GDALobj()`, `GDALcall()`, `rawTransform()`, `OGRSpatialRef()` in favour of functions and methods ib **sf** and **terra**. 

* handle -Wstrict-prototypes `int main()` to `int main(void)` in `configure.ac`

* update `src/Makevars.ucrt` to upgraded MXE geospatial libraries

* address https://github.com/r-spatial/sf/pull/2009 in `src/Makevars.ucrt`

* set PROJ4 warnings default to "none"

* fix -Wstrict-prototypes warnings in code

# Version 1.5-32 (2022-05-09, rev. 1173-1176)

* update src/Makefile.ucrt to accommodate the GDAL raster Geospatial PDF driver for reading

# Version 1.5-31 (2022-04-18, rev. 1172)

* unescape underscores in help pages

# Version 1.5-30 (2022-04-08, rev. 1166-1171)

* restore PROJ path assignment for PROJ >= 6 not using PROJ_LIB (was used for PROJ 6 but not > 6) https://github.com/rsbivand/rgrass/issues/52

* Update src/Makevars.win to GDAL 3.4.1 for R < 4.2 https://github.com/r-spatial/sf/pull/1919

# Version 1.5-29 (2022-03-15, rev. 1159-1165)

* also convert BOUNDCRS to PROJCRS by default in OGRSpatialRef() and getProjectionRef()

* test for chunks table in cache.db in vignette

* quietening tests only differing by `proj.db` version.

# Version 1.5-28 (2021-12-15, rev. 1157-1158)

* Adapting to R 4.2 Windows UCRT

# Version 1.5-27 (2021-09-16, rev. 1149-1156)

* Correcting logic error in check for MXE UCRT builds (temporary)

# Version 1.5-26 (2021-09-15, rev. 1141-1148)

* Mute use of PROJ CDN for MXE UCRT builds (temporary)

* Run autoupdate on configure.ac to handle obsolete AC_HELP_STRING etc.

# Version 1.5-25 (2021-09-08, rev. 1122-1140)

* Add environment variable access to --with-data-copy by PROJ_GDAL_DATA_COPY (https://github.com/r-spatial/sf/issues/1605)

* Adaptations for PROJ 8.

* Handle mixed 2D/3D in readOGR(): (https://github.com/r-spatial/sf/issues/1683)

* Add tweak for UCRT builds.

* Thin examples for spTransform().

# Version 1.5-23 (2021-02-03, rev. 1120-1121)

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

