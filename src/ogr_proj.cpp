/* Copyright (c) 2006 Roger Bivand
* Function using C API and based on GPJ_grass_to_wkt from GRASS gproj
* library by Paul Kelly and Frank Warmerdam */


#include "ogrsf_frmts.h"
#include <ogr_spatialref.h>

// R headers moved outside extern "C" 070808 RSB re. note from BDR
// #ifdef __cplusplus
// extern "C" {
// #endif
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "rgdal.h"

#ifdef __cplusplus
extern "C" {
#endif

SEXP p4s_to_wkt(SEXP p4s, SEXP esri) {

    OGRSpatialReference hSRS = NULL;
    char *pszSRS_WKT = NULL;
    SEXP ans;

    installErrorHandler();
    if (hSRS.importFromProj4(CHAR(STRING_ELT(p4s, 0))) != OGRERR_NONE) {
        uninstallErrorHandlerAndTriggerError();
	error("Can't parse PROJ.4-style parameter string");
    }
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    if (INTEGER_POINTER(esri)[0] == 1) hSRS.morphToESRI();
    hSRS.exportToWkt(&pszSRS_WKT);
    uninstallErrorHandlerAndTriggerError();

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_WKT));

    UNPROTECT(1);

    return(ans);
}

SEXP ogrP4S(SEXP ogrsourcename, SEXP Layer) {

    OGRDataSource *poDS;
    OGRLayer *poLayer;
    OGRSFDriver *poDriver;

    OGRSpatialReference *hSRS = NULL;
    char *pszProj4 = NULL;
    SEXP ans;

    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrsourcename, 0)), 
	FALSE, &poDriver);
    uninstallErrorHandlerAndTriggerError();

    if(poDS==NULL){
      error("Cannot open file");
    }
    installErrorHandler();
    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));
    uninstallErrorHandlerAndTriggerError();

    if(poLayer == NULL){
      error("Cannot open layer");
    }

    PROTECT(ans=NEW_CHARACTER(1));

    installErrorHandler();
    hSRS = poLayer->GetSpatialRef();
    uninstallErrorHandlerAndTriggerError();

    if (hSRS != NULL) {
        installErrorHandler();
	hSRS->morphFromESRI();
        if (hSRS->exportToProj4(&pszProj4) != OGRERR_NONE) {
//	    SET_VECTOR_ELT(ans, 0, NA_STRING);
            SET_STRING_ELT(ans, 0, NA_STRING);
	} else {
//	    SET_VECTOR_ELT(ans, 0, COPY_TO_USER_STRING(pszProj4));
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszProj4));
	}
        uninstallErrorHandlerAndTriggerError();
//    } else SET_VECTOR_ELT(ans, 0, NA_STRING);
      } else SET_STRING_ELT(ans, 0, NA_STRING);

    installErrorHandler();
    delete poDS;
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(1);
    return(ans);
}
#ifdef __cplusplus
}
#endif


