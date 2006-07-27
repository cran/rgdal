/* Copyright (c) 2006 Roger Bivand
* Function using C API and based on GPJ_grass_to_wkt from GRASS gproj
* library by Paul Kelly and Frank Warmerdam */


#include "ogrsf_frmts.h"
#include <ogr_spatialref.h>

#ifdef __cplusplus
extern "C" {
#endif
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP p4s_to_wkt(SEXP p4s, SEXP esri) {

    OGRSpatialReference hSRS = NULL;
    char *pszSRS_WKT = NULL;
    SEXP ans;

    if (hSRS.importFromProj4(CHAR(STRING_ELT(p4s, 0))) != OGRERR_NONE)
	error("Can't parse PROJ.4-style parameter string");
    if (INTEGER_POINTER(esri)[0] == 1) hSRS.morphToESRI();
    hSRS.exportToWkt(&pszSRS_WKT);

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

    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrsourcename, 0)), 
	FALSE, &poDriver);

    if(poDS==NULL){
      error("Cannot open file");
    }
    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));

    if(poLayer == NULL){
      error("Cannot open layer");
    }

    PROTECT(ans=NEW_CHARACTER(1));

    hSRS = poLayer->GetSpatialRef();

    if (hSRS != NULL) {
	hSRS->morphFromESRI();
        if (hSRS->exportToProj4(&pszProj4) != OGRERR_NONE) {
//	    SET_VECTOR_ELT(ans, 0, NA_STRING);
            SET_STRING_ELT(ans, 0, NA_STRING);
	} else {
//	    SET_VECTOR_ELT(ans, 0, COPY_TO_USER_STRING(pszProj4));
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszProj4));
	}
//    } else SET_VECTOR_ELT(ans, 0, NA_STRING);
      } else SET_STRING_ELT(ans, 0, NA_STRING);

    delete poDS;
    UNPROTECT(1);
    return(ans);
}
#ifdef __cplusplus
}
#endif


