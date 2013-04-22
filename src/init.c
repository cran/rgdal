#include <R.h>
#include <Rinternals.h>
#include "rgdal.h"

#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[] = {
    {"project", (DL_FUNC) &project, 7},
    {"project_inv", (DL_FUNC) &project_inv, 7},
    {NULL, NULL, 0} 
};


static R_CallMethodDef CallEntries[] = {

    {"RGDAL_Init", (DL_FUNC) &RGDAL_Init, 0},
    {"RGDAL_Exit", (DL_FUNC) &RGDAL_Exit, 0},
    {"checkCRSArgs", (DL_FUNC) &checkCRSArgs, 1},
    {"RGDAL_GetDescription", (DL_FUNC) &RGDAL_GetDescription, 1},
    {"RGDAL_GDALVersionInfo", (DL_FUNC) &RGDAL_GDALVersionInfo, 1},
    {"RGDAL_GDALCheckVersion", (DL_FUNC) &RGDAL_GDALCheckVersion, 0},
    {"RGDAL_GDAL_DATA_Info", (DL_FUNC) &RGDAL_GDAL_DATA_Info, 0},
    {"RGDAL_GetDriverNames", (DL_FUNC) &RGDAL_GetDriverNames, 0},
    {"RGDAL_GetDriver", (DL_FUNC) &RGDAL_GetDriver, 1},
    {"RGDAL_GetDriverShortName", (DL_FUNC) &RGDAL_GetDriverShortName, 1},
    {"RGDAL_GetDriverLongName", (DL_FUNC) &RGDAL_GetDriverLongName, 1},
    {"RGDAL_OpenDataset", (DL_FUNC) &RGDAL_OpenDataset, 3},
    {"RGDAL_CloseHandle", (DL_FUNC) &RGDAL_CloseHandle, 1},
    {"RGDAL_CreateDataset", (DL_FUNC) &RGDAL_CreateDataset, 5},
    {"RGDAL_GetDatasetDriver", (DL_FUNC) &RGDAL_GetDatasetDriver, 1},
    {"RGDAL_CopyDataset", (DL_FUNC) &RGDAL_CopyDataset, 5},
    {"RGDAL_DeleteFile", (DL_FUNC) &RGDAL_DeleteFile, 2},
    {"RGDAL_CloseDataset", (DL_FUNC) &RGDAL_CloseDataset, 1},
    {"RGDAL_GetRasterYSize", (DL_FUNC) &RGDAL_GetRasterYSize, 1},
    {"RGDAL_GetRasterXSize", (DL_FUNC) &RGDAL_GetRasterXSize, 1},
    {"RGDAL_GetRasterCount", (DL_FUNC) &RGDAL_GetRasterCount, 1},
    {"RGDAL_GetProjectionRef", (DL_FUNC) &RGDAL_GetProjectionRef, 1},
    {"RGDAL_PutRasterData", (DL_FUNC) &RGDAL_PutRasterData, 3},
    {"RGDAL_GetRasterData", (DL_FUNC) &RGDAL_GetRasterData, 4},
    {"RGDAL_GetScale", (DL_FUNC) &RGDAL_GetScale, 1},
    {"RGDAL_GetOffset", (DL_FUNC) &RGDAL_GetOffset, 1},
    {"RGDAL_GetCategoryNames", (DL_FUNC) &RGDAL_GetCategoryNames, 1},
    {"RGDAL_GetColorTable", (DL_FUNC) &RGDAL_GetColorTable, 1},
    {"RGDAL_GetColorInterp", (DL_FUNC) &RGDAL_GetColorInterp, 1},
    {"RGDAL_GetPaletteInterp", (DL_FUNC) &RGDAL_GetPaletteInterp, 1},
    {"RGDAL_GenCMap", (DL_FUNC) &RGDAL_GenCMap, 6},
    {"RGDAL_GetRasterBand", (DL_FUNC) &RGDAL_GetRasterBand, 2},
    {"RGDAL_GetYSize", (DL_FUNC) &RGDAL_GetYSize, 1},
    {"RGDAL_GetXSize", (DL_FUNC) &RGDAL_GetXSize, 1},
    {"RGDAL_GetGeoTransform", (DL_FUNC) &RGDAL_GetGeoTransform, 1},
    {"RGDAL_GetRasterBlockSize", (DL_FUNC) &RGDAL_GetRasterBlockSize, 1},
    {"RGDAL_CPLGetConfigOption", (DL_FUNC) &RGDAL_CPLGetConfigOption, 1},
    {"RGDAL_CPLSetConfigOption", (DL_FUNC) &RGDAL_CPLSetConfigOption, 2},
    {"RGDAL_CPL_RECODE_ICONV", (DL_FUNC) &RGDAL_CPL_RECODE_ICONV, 0},
    {"PROJcopyEPSG", (DL_FUNC) &PROJcopyEPSG, 1},
    {"ogrInfo", (DL_FUNC) &ogrInfo, 2},
    {"R_OGR_types", (DL_FUNC) &R_OGR_types, 2},
    {"ogrFIDs", (DL_FUNC) &ogrFIDs, 2},
    {"ogr_GetDriverNames", (DL_FUNC) &ogr_GetDriverNames, 0},
    {"ogrP4S", (DL_FUNC) &ogrP4S, 2},
    {"ogrListLayers", (DL_FUNC) &ogrListLayers, 1},
    {"ogrDataFrame", (DL_FUNC) &ogrDataFrame, 4},
    {"R_OGR_CAPI_features", (DL_FUNC) &R_OGR_CAPI_features, 3},
    {"make_Polygonlist", (DL_FUNC) &make_Polygonlist, 2},
    {"p4s_to_wkt", (DL_FUNC) &p4s_to_wkt, 2},
    {"OGR_write", (DL_FUNC) &OGR_write, 1},
    {"ogrDeleteLayer", (DL_FUNC) &ogrDeleteLayer, 3},
    {"ogrDeleteDataSource", (DL_FUNC) &ogrDeleteDataSource, 2},
    {"ogrCheckExists", (DL_FUNC) &ogrCheckExists, 2},
    {"PROJ4VersionInfo", (DL_FUNC) &PROJ4VersionInfo, 0},
    {"PROJ4NADsInstalled", (DL_FUNC) &PROJ4NADsInstalled, 0},
    {"transform", (DL_FUNC) &transform, 6},
    {"projInfo", (DL_FUNC) &projInfo, 1},
    {"RGDAL_SetGeoTransform", (DL_FUNC) &RGDAL_SetGeoTransform, 2},
    {"RGDAL_GetNoDataValue", (DL_FUNC) &RGDAL_GetNoDataValue, 1},
    {"RGDAL_GetMetadata", (DL_FUNC) &RGDAL_GetMetadata, 2},
    {"RGDAL_GetBandType", (DL_FUNC) &RGDAL_GetBandType, 1},
    {"RGDAL_GetBandStatistics", (DL_FUNC) &RGDAL_GetBandStatistics, 2},
    {"RGDAL_GetBandMinimum", (DL_FUNC) &RGDAL_GetBandMinimum, 1},
    {"RGDAL_GetBandMaximum", (DL_FUNC) &RGDAL_GetBandMaximum, 1},
    {"RGDAL_GetRAT", (DL_FUNC) &RGDAL_GetRAT, 1},
    {"RGDAL_GetBandNoDataValue", (DL_FUNC) &RGDAL_GetBandNoDataValue, 1},
    {"RGDAL_SetProject", (DL_FUNC) &RGDAL_SetProject, 2},
    {"RGDAL_SetNoDataValue", (DL_FUNC) &RGDAL_SetNoDataValue, 2},
    {"RGDAL_SetStatistics", (DL_FUNC) &RGDAL_SetStatistics, 2},
    {"RGDAL_SetRasterColorTable", (DL_FUNC) &RGDAL_SetRasterColorTable, 4},
    {"RGDAL_SetCategoryNames", (DL_FUNC) &RGDAL_SetCategoryNames, 2},
    {"isGDALObjPtrNULL", (DL_FUNC) &isGDALObjPtrNULL, 1},
    {NULL, NULL, 0}
};


void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_rgdal(DllInfo *dll) {

    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL); 
    R_useDynamicSymbols(dll, FALSE);

}

