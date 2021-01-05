#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <cpl_string.h>
#include <cpl_csv.h>
#include <cpl_error.h>
#include <cpl_conv.h>

#ifdef __cplusplus
extern "C" {
#endif
#define ROFFSET 1
#define SP_XPORT(x) RGDAL_ ## x
#include "sp.h"

#ifndef GDALV2
#if GDAL_VERSION_MAJOR >= 2
# define GDALV2 1
#endif
#endif


SEXP make_Polygonlist(SEXP iG, SEXP iGc);
SEXP make_Polygon(SEXP jG, SEXP jGc);
SEXP rgdal_sp_linkingTo_version(void);

void installErrorHandler();
void uninstallErrorHandlerAndTriggerError();

SEXP RGDAL_Init(void);
SEXP RGDAL_Exit(void);

SEXP RGDAL_GDALVersionInfo(SEXP str);
SEXP RGDAL_GDALCheckVersion(void);
SEXP RGDAL_GDAL_DATA_Info(void);
SEXP RGDAL_GDALwithGEOS(void);

SEXP RGDAL_GetDriverNames(void);
SEXP RGDAL_GetDriver(SEXP sxpDriverName);
SEXP RGDAL_GetDriverShortName(SEXP sxpDriver);
SEXP RGDAL_GetDriverLongName(SEXP sxpDriver);

SEXP RGDAL_CloseHandle(SEXP sxpHandle);
//SEXP RGDAL_DeleteHandle(SEXP sxpHandle);

SEXP RGDAL_OpenDataset(SEXP filename, SEXP read_only, SEXP silent,
  SEXP allowedDr, SEXP options);
SEXP RGDAL_CreateDataset(SEXP sxpDriver, SEXP sDim, SEXP sType,
  SEXP sOpts, SEXP sFile);
SEXP RGDAL_GetDatasetDriver(SEXP sDataset);
SEXP RGDAL_CopyDataset(SEXP sxpDataset, SEXP sxpDriver,
  SEXP sxpStrict,  SEXP sxpOpts,
  SEXP sxpFile);
SEXP RGDAL_CloseDataset(SEXP sxpDataset);

SEXP RGDAL_DeleteFile(SEXP sxpDriver, SEXP sxpFileName);

SEXP RGDAL_GetDescription(SEXP sxpObj);
SEXP RGDAL_GetProjectionRef(SEXP sDataset, SEXP enforce_xy);
SEXP RGDAL_GetYSize(SEXP sRasterBand);
SEXP RGDAL_GetXSize(SEXP sRasterBand);
SEXP RGDAL_GetGeoTransform(SEXP sxpDataset);
SEXP RGDAL_SetGeoTransform(SEXP sxpDataset, SEXP GeoTransform);
SEXP RGDAL_GetMetadata(SEXP sDataset, SEXP tag);
SEXP RGDAL_SetProject(SEXP sxpDataset, SEXP proj4string);
SEXP RGDAL_SetProject_WKT2(SEXP sxpDataset, SEXP WKT2string, SEXP enforce_xy);
SEXP RGDAL_SetNoDataValue(SEXP sxpRasterBand, SEXP NoDataValue);

SEXP RGDAL_GetRasterYSize(SEXP sDataset);
SEXP RGDAL_GetRasterXSize(SEXP sDataset);
SEXP RGDAL_GetRasterCount(SEXP sDataset);
SEXP RGDAL_GetRasterBlockSize(SEXP rasterObj);

SEXP RGDAL_PutRasterData(SEXP sxpRasterBand, SEXP sxpData, SEXP sxpOffset);
SEXP RGDAL_GetRasterData(SEXP sxpRasterBand, SEXP sxpRegion, SEXP sxpDimOut,
    SEXP sxpInterleave);
SEXP RGDAL_GetScale(SEXP sxpRasterBand);
SEXP RGDAL_GetOffset(SEXP sxpRasterBand);
SEXP RGDAL_GetCategoryNames(SEXP sxpRasterBand);
SEXP RGDAL_GetColorTable(SEXP rasterObj);
SEXP RGDAL_GetColorInterp(SEXP sxpRasterBand);
SEXP RGDAL_GetPaletteInterp(SEXP sxpRasterBand);
SEXP RGDAL_GenCMap(SEXP input1, SEXP input2, SEXP input3, SEXP output,
  SEXP nColors, SEXP setCMap);
SEXP RGDAL_GetRasterBand(SEXP sDataset, SEXP sBand);
SEXP RGDAL_GetNoDataValue(SEXP sxpRasterBand);
SEXP RGDAL_GetBandType(SEXP sxpRasterBand);
SEXP RGDAL_GetBandStatistics(SEXP sxpRasterBand, SEXP silent);
SEXP RGDAL_GetBandMinimum(SEXP sxpRasterBand);
SEXP RGDAL_GetBandMaximum(SEXP sxpRasterBand);
SEXP RGDAL_GetBandNoDataValue(SEXP sxpRasterBand);
SEXP RGDAL_SetStatistics(SEXP sxpRasterBand, SEXP statistics);
SEXP RGDAL_SetRasterColorTable(SEXP raster, SEXP icT, SEXP ricT, SEXP cicT);
SEXP RGDAL_SetCategoryNames(SEXP sxpRasterBand, SEXP sxpNames);
SEXP isGDALObjPtrNULL(SEXP sxpObj);

SEXP RGDAL_GetRAT(SEXP sxpRasterBand);
SEXP RGDAL_CPLSetConfigOption(SEXP inOption, SEXP value);
SEXP RGDAL_CPLGetConfigOption(SEXP inOption);
SEXP RGDAL_CPL_RECODE_ICONV(void);

SEXP RGDAL_ogrInfo(SEXP ogrsourcename, SEXP Layer);
SEXP R_OGR_types(SEXP dsn, SEXP layer);
SEXP RGDAL_ogrFIDs(SEXP filename, SEXP layer);
SEXP ogr_GetDriverNames(void);
SEXP RGDAL_ogrListLayers (SEXP ogrSource);
SEXP ogrDataFrame(SEXP ogrSource, SEXP Layer, SEXP FIDs, SEXP iFields);
SEXP R_OGR_CAPI_features(SEXP dsn, SEXP layer, SEXP comments);
//SEXP make_Polygonlist(SEXP iG);
SEXP OGR_write(SEXP inp);
SEXP RGDAL_ogrDeleteLayer (SEXP ogrSource, SEXP Layer, SEXP ogrDriver);
SEXP RGDAL_ogrDeleteDataSource (SEXP ogrSource, SEXP ogrDriver);
SEXP ogrCheckExists (SEXP ogrSource, SEXP Layer);

SEXP RGDAL_checkCRSArgs(SEXP args); // both
SEXP PROJcopyEPSG(SEXP tf); // both
SEXP ogrP4S(SEXP ogrsourcename, SEXP Layer, SEXP morphFromESRI, SEXP dumpSRS); // GDAL
SEXP p4s_to_wkt(SEXP p4s, SEXP esri);// GDAL
SEXP wkt_to_p4s(SEXP wkt, SEXP esri);// GDAL
SEXP P6_SRID_show(SEXP inSRID, SEXP format, SEXP multiline, SEXP in_format, SEXP epsg, SEXP out_format);// GDAL
SEXP P6_SRID_proj(SEXP inSRID, SEXP format, SEXP multiline, SEXP in_format, SEXP epsg, SEXP out_format);
SEXP ogrAutoIdentifyEPSG(SEXP p4s);// GDAL
SEXP list_coordinate_ops(SEXP source, SEXP target, SEXP area_of_interest, SEXP strict_containment, SEXP vis_order); // both
SEXP PROJ4VersionInfo(void); // different
SEXP PROJ4NADsInstalled(void); // both, different at R level ??
SEXP PROJ4_proj_def_dat_Installed(void); // both, different at R level ??
SEXP get_proj_search_path(void); // both
SEXP set_proj_paths(SEXP path); // both
SEXP get_proj_user_writable_dir(); // both
SEXP transform(SEXP fromargs, SEXP toargs, SEXP npts, SEXP x, SEXP y, SEXP z); // both
SEXP transform_ng(SEXP fromargs, SEXP toargs, SEXP coordOp, SEXP npts, SEXP x, SEXP y, SEXP z, SEXP aoi); // both
SEXP CRS_compare(SEXP fromargs, SEXP toargs, SEXP type1, SEXP type2);
SEXP proj_network_enabled(); // both
SEXP enable_proj_network(); // both
SEXP disable_proj_network(); // both
SEXP RGDAL_projInfo(SEXP type); // both
SEXP R_GDAL_OSR_PROJ();// GDAL
SEXP RGDAL_project(SEXP n, SEXP xlon, SEXP ylat, SEXP projarg, SEXP ob_tran); // both
SEXP project_inv(SEXP n, SEXP x, SEXP y, SEXP projarg, SEXP ob_tran); // both
SEXP project_ng(SEXP n, SEXP xlon, SEXP ylat, SEXP zz, SEXP coordOp); // both
SEXP project_ng_coordOp(SEXP proj, SEXP inv, SEXP aoi, SEXP ob_tran); // both
SEXP get_source_crs(SEXP source); // both
SEXP proj_vis_order(SEXP wkt2); // both
SEXP OSR_is_projected(SEXP inSRID);

#ifdef __cplusplus
}
#endif

