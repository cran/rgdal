/* Copyright (c) 2006 Roger Bivand
* Function using C API and based on v.in.ogr from GRASS by Radim Blazek
* to read OGR vector geometry features */

#include "ogr_api.h"

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

SEXP R_OGR_CAPI_features(SEXP dsn, SEXP layer, SEXP comments)
{

    OGRDataSourceH Ogr_ds;
    OGRLayerH Ogr_layer;
    OGRFeatureDefnH Ogr_featuredefn;
    OGRFeatureH Ogr_feature;
    OGRGeometryH Ogr_geometry;
    OGRwkbGeometryType eType;
    OGRGeometryH hRing, hRingM;

    int navailable_layers; 
    int i, j, k, km, jcnt;
/*    int iDriver;*/
    int dim, with_z;
    int np, nr, nm;
/*    char *pszProj4 = NULL;*/

    int pc=0;
    int nf, mp_count, mp_count_k0;
    int do_comments;
//    SEXP Hole;

    SEXP ans;
    SEXP ansnames;

    do_comments = INTEGER_POINTER(comments)[0];
//    PROTECT(Hole = NEW_INTEGER(1)); pc++;

    installErrorHandler();
    Ogr_ds = OGROpen(CHAR(STRING_ELT(dsn, 0)), FALSE, NULL);
    if (Ogr_ds == NULL) {
        uninstallErrorHandlerAndTriggerError();
        error("Cannot open data source");
    }
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    navailable_layers = OGR_DS_GetLayerCount(Ogr_ds);
    uninstallErrorHandlerAndTriggerError();

    j=-1;
    installErrorHandler();
    for (i = 0; i < navailable_layers; i++) {
	Ogr_layer =  OGR_DS_GetLayer( Ogr_ds, i );
	Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);
	if (strcmp((char *)OGR_FD_GetName(Ogr_featuredefn), 
	    CHAR(STRING_ELT(layer, 0))) == 0) j = i; 
    }
    uninstallErrorHandlerAndTriggerError();

    if (j < 0) error("Layer not found");

    PROTECT(ans = NEW_LIST(8)); pc++;
    PROTECT(ansnames = NEW_CHARACTER(8)); pc++;
    SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("dsn"));
    SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("layer"));
    SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("proj4string"));
    SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("geomTypes"));
    SET_STRING_ELT(ansnames, 4, COPY_TO_USER_STRING("crdlist"));
    SET_STRING_ELT(ansnames, 5, COPY_TO_USER_STRING("with_z"));
    SET_STRING_ELT(ansnames, 6, COPY_TO_USER_STRING("isNULL"));
    SET_STRING_ELT(ansnames, 7, COPY_TO_USER_STRING("polygonIntComments"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 0), 0,
    installErrorHandler();
    SET_STRING_ELT(VECTOR_ELT(ans, 0), 0, 
	COPY_TO_USER_STRING(OGR_DS_GetName(Ogr_ds)));
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    Ogr_layer =  OGR_DS_GetLayer(Ogr_ds, j);
    Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);
    uninstallErrorHandlerAndTriggerError();

    SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 1), 0, 
    installErrorHandler();
    SET_STRING_ELT(VECTOR_ELT(ans, 1), 0,
	COPY_TO_USER_STRING((char *)OGR_FD_GetName(Ogr_featuredefn)));
    uninstallErrorHandlerAndTriggerError();
    SET_VECTOR_ELT(ans, 2, NEW_INTEGER(1));

/* was projection */

    installErrorHandler();
#ifdef GDALV2
  GIntBig nFIDs64 = OGR_L_GetFeatureCount(Ogr_layer, 1);
  nf = (nFIDs64 > INT_MAX) ? INT_MAX : 
        (nFIDs64 < INT_MIN) ? INT_MIN : (int) nFIDs64;
  if ((GIntBig) nf != nFIDs64){
        uninstallErrorHandlerAndTriggerError();
        error("R_OGR_CAPI_features: feature count overflow");
  }
#else
    nf = OGR_L_GetFeatureCount(Ogr_layer, 1);
#endif
    uninstallErrorHandlerAndTriggerError();
    if (nf == -1) {
      i=0;
      installErrorHandler();
      while( ((Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL) && i <= INT_MAX){
        i++;
        OGR_F_Destroy(Ogr_feature);
//    delete poFeature;
      }
      uninstallErrorHandlerAndTriggerError();
      if (i == INT_MAX) {
        error("ogrFIDs: feature count overflow");
      } else {
        nf = i;
      }
      installErrorHandler();
      OGR_L_ResetReading(Ogr_layer);
      uninstallErrorHandlerAndTriggerError();
    }
//Rprintf("nf: %d\n", nf);
    SET_VECTOR_ELT(ans, 3, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 4, NEW_LIST(nf));
    SET_VECTOR_ELT(ans, 5, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 6, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 7, NEW_LIST(nf));

    i=0;
    installErrorHandler();
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
	    /* Geometry */
	Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	with_z = 0;
/* TODO fix NULL reading */
	if ( Ogr_geometry == NULL ||
          wkbFlatten(OGR_G_GetGeometryType(Ogr_geometry)) == 0) {
/*	    error("NULL geometry found"); */
	    INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
 	} else {
	    INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 0;
	    dim = OGR_G_GetCoordinateDimension(Ogr_geometry);
	    if (dim > 2) 
		with_z = 1;
        eType = wkbFlatten(OGR_G_GetGeometryType(Ogr_geometry));

	INTEGER_POINTER(VECTOR_ELT(ans, 3))[i] =  (int) eType;
	INTEGER_POINTER(VECTOR_ELT(ans, 5))[i] =  with_z;

	if (eType == wkbPoint) {
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(1));
	    if (with_z == 0) 
	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0, 
		NEW_LIST(2));
	    else SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0, 
		NEW_LIST(3));

	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0), 0,
		NEW_NUMERIC(1));
	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0), 1,
		NEW_NUMERIC(1));
	    NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 
		4), i), 0), 0))[0] = OGR_G_GetX(Ogr_geometry, 0);
	    NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 
		4), i), 0), 1))[0] = OGR_G_GetY(Ogr_geometry, 0);

	    if (with_z > 0) {
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 
		    4), i), 0), 2, NEW_NUMERIC(1));
	        NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		    ans, 4), i), 0), 2))[0] = OGR_G_GetZ(Ogr_geometry, 0);
	    }
	} else if (eType == wkbLineString) {
	  np = OGR_G_GetPointCount(Ogr_geometry);
          if (np > 0) {
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(1));
	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0, 
		NEW_LIST(2));

	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0), 0,
		NEW_NUMERIC(np));
	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0), 1,
		NEW_NUMERIC(np));
            for( j = 0; j < np; j++ ) {
	        NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		    ans, 4), i), 0), 0))[j] = OGR_G_GetX(Ogr_geometry, j);
	        NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		    ans, 4), i), 0), 1))[j] = OGR_G_GetY(Ogr_geometry, j);
	    }
          } else INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
	} else if (eType == wkbPolygon) {
	  nr = OGR_G_GetGeometryCount(Ogr_geometry);
          if (nr > 0) {
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(nr));
            jcnt = 0;
	    for (j=0; j < nr; j++) {
	      hRing = OGR_G_GetGeometryRef(Ogr_geometry, j);
	      np = OGR_G_GetPointCount(hRing);
              if (np > 0) {
                jcnt++;
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), j, 
		    NEW_LIST(2));
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 0, NEW_NUMERIC(np));
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 1, NEW_NUMERIC(np));
// added hole reporting since #0 is defined exterior, rest interior
//                if (j == 0) INTEGER_POINTER(Hole)[0] = 0;
//                else INTEGER_POINTER(Hole)[0] = 1;
//                if (!do_comments) INTEGER_POINTER(Hole)[0] = NA_INTEGER;
//                setAttrib(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), j),
//                   install("hole"), Hole);
//if (i == 1L) Rprintf("wkbPolygon i %d j %d Hole %d\n", i, j, INTEGER_POINTER(Hole)[0]);
//                Hole = getAttrib(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4),
//                    i), j), install("hole"));
// appear to need to retrieve Hole attribute 121016
                for(k = 0; k < np; k++) {
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 0))[k] = OGR_G_GetX(hRing, k);
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 1))[k] = OGR_G_GetY(hRing, k);
	        }
              } 
	    }
            if (jcnt == 0) INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
// added comment since #0 is defined exterior, rest interior
            if (jcnt > 0 && do_comments) {
              SET_VECTOR_ELT(VECTOR_ELT(ans, 7), i, NEW_INTEGER(nr));
              INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(ans, 7), i))[0] = 0;
              for (j=1; j<nr; j++)
                INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(ans, 7), i))[j] = ROFFSET;
            }
          } else INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
	} else if (eType == wkbMultiLineString) {
	  nm = OGR_G_GetGeometryCount(Ogr_geometry);
          if (nm > 0) {
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(nm));
            jcnt = 0;
	    for(j = 0; j < nm; j++) {
	      hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	      np = OGR_G_GetPointCount(hRingM);
              if (np > 0) {
                jcnt++;
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), j, 
		    NEW_LIST(2));
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 0, NEW_NUMERIC(np));
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 1, NEW_NUMERIC(np));
                for(k = 0; k < np; k++) {
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 0))[k] = OGR_G_GetX(hRingM, k);
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 1))[k] = OGR_G_GetY(hRingM, k);
	        }
              }
	    }
            if (jcnt == 0) INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
          } else INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
	} else if (eType == wkbMultiPolygon) {
	  nm = OGR_G_GetGeometryCount(Ogr_geometry);
          if (nm > 0) {
	    for(j = 0, mp_count = 0; j < nm; j++) {
		hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        nr = OGR_G_GetGeometryCount(hRingM);
		mp_count += nr;
	    }
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(mp_count));
// added comment since #0 is defined exterior, rest interior
            if (do_comments)
              SET_VECTOR_ELT(VECTOR_ELT(ans, 7), i, NEW_INTEGER(mp_count));
	    mp_count = 0;
            jcnt = 0;
	    for(j = 0; j < nm; j++) {
		hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        nr = OGR_G_GetGeometryCount(hRingM);
                if (nr > 0) {
	          for (k=0; k < nr; k++) {
	            hRing = OGR_G_GetGeometryRef(hRingM, k);
	            np = OGR_G_GetPointCount(hRing);
                    if (np > 0) {
                      jcnt++;
	              SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i),
                        mp_count, NEW_LIST(2));
	              SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), 
			i), mp_count), 0, NEW_NUMERIC(np));
	              SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), 
			i), mp_count), 1, NEW_NUMERIC(np));
                      for(km = 0; km < np; km++) {
	                  NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
			    VECTOR_ELT(ans, 4), i), mp_count), 0))[km] = 
			    OGR_G_GetX(hRing, km);
	                  NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
			    VECTOR_ELT(ans, 4), i), mp_count), 1))[km] = 
			    OGR_G_GetY(hRing, km);
	              }
// added hole reporting since #0 is defined exterior, rest interior
//                      if (k == 0) INTEGER_POINTER(Hole)[0] = 0;
//                      else INTEGER_POINTER(Hole)[0] = 1;
//                      if (!do_comments) INTEGER_POINTER(Hole)[0] = NA_INTEGER;
//                      setAttrib(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4),
//                        i), mp_count), install("hole"), Hole);
//                      Hole = getAttrib(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans,
//                        4), i), mp_count), install("hole"));
// appear to need to retrieve Hole attribute 121016
                      if (k == 0) mp_count_k0 = mp_count;
// added comment since #0 is defined exterior, rest interior
                      if (do_comments) {
                        if (k == 0) 
                          INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(ans,
                            7), i))[mp_count] = 0;
                        else INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(ans,
                            7), i))[mp_count] = mp_count_k0 + ROFFSET;
                      }
		      mp_count++;
                    }
                  }
	        }
	    } 
            if (jcnt == 0) INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
          } else INTEGER_POINTER(VECTOR_ELT(ans, 6))[i] = 1;
	} else warning("eType not chosen");
      }
      OGR_F_Destroy(Ogr_feature);
      i++;
    }
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    OGR_DS_Destroy(Ogr_ds);
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(pc);

    return(ans);
}

SEXP R_OGR_types(SEXP dsn, SEXP layer)
{

    OGRDataSourceH Ogr_ds;
    OGRLayerH Ogr_layer;
    OGRFeatureDefnH Ogr_featuredefn;
    OGRFeatureH Ogr_feature;
    OGRGeometryH Ogr_geometry;
    OGRwkbGeometryType eType;

    int navailable_layers; 
    int i, j;
/*    int iDriver;*/
    int dim, with_z, isNULL;
/*    char *pszProj4 = NULL;*/

    int pc=0;
    int nf;

    SEXP ans;
    SEXP ansnames;

    installErrorHandler();
    Ogr_ds = OGROpen(CHAR(STRING_ELT(dsn, 0)), FALSE, NULL);
    if (Ogr_ds == NULL) {
        uninstallErrorHandlerAndTriggerError();
        error("Cannot open data source");
    }
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    navailable_layers = OGR_DS_GetLayerCount(Ogr_ds);
    uninstallErrorHandlerAndTriggerError();

    j=-1;
    installErrorHandler();
    for (i = 0; i < navailable_layers; i++) {
	Ogr_layer =  OGR_DS_GetLayer( Ogr_ds, i );
	Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);
	if (strcmp((char *)OGR_FD_GetName(Ogr_featuredefn), 
	    CHAR(STRING_ELT(layer, 0))) == 0) j = i; 
    }
    uninstallErrorHandlerAndTriggerError();

    if (j < 0) error("Layer not found");

    PROTECT(ans = NEW_LIST(6)); pc++;
    PROTECT(ansnames = NEW_CHARACTER(6)); pc++;
    SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("dsn"));
    SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("layer"));
    SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("proj4string"));
    SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("geomTypes"));
    SET_STRING_ELT(ansnames, 4, COPY_TO_USER_STRING("with_z"));
    SET_STRING_ELT(ansnames, 5, COPY_TO_USER_STRING("isNULL"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 0), 0,
    installErrorHandler();
    SET_STRING_ELT(VECTOR_ELT(ans, 0), 0, 
	COPY_TO_USER_STRING(OGR_DS_GetName(Ogr_ds)));
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    Ogr_layer =  OGR_DS_GetLayer(Ogr_ds, j);
    Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);
    uninstallErrorHandlerAndTriggerError();

    SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 1), 0, 
    installErrorHandler();
    SET_STRING_ELT(VECTOR_ELT(ans, 1), 0,
	COPY_TO_USER_STRING((char *)OGR_FD_GetName(Ogr_featuredefn)));
    uninstallErrorHandlerAndTriggerError();
    SET_VECTOR_ELT(ans, 2, NEW_INTEGER(1));

/* was projection */

    installErrorHandler();
#ifdef GDALV2
  GIntBig nFIDs64 = OGR_L_GetFeatureCount(Ogr_layer, 1);
  nf = (nFIDs64 > INT_MAX) ? INT_MAX : 
        (nFIDs64 < INT_MIN) ? INT_MIN : (int) nFIDs64;
  if ((GIntBig) nf != nFIDs64){
        uninstallErrorHandlerAndTriggerError();
        error("R_OGR_types: feature count overflow");
  }
#else
    nf = OGR_L_GetFeatureCount(Ogr_layer, 1);
#endif
    uninstallErrorHandlerAndTriggerError();

    if (nf == -1) {
      i=0;
      installErrorHandler();
      while( ((Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL) && i <= INT_MAX){
        i++;
        OGR_F_Destroy(Ogr_feature);
//    delete poFeature;
      }
      uninstallErrorHandlerAndTriggerError();
      if (i == INT_MAX) {
        error("ogrFIDs: feature count overflow");
      } else {
        nf = i;
      }
      installErrorHandler();
      OGR_L_ResetReading(Ogr_layer);
      uninstallErrorHandlerAndTriggerError();
    }


    SET_VECTOR_ELT(ans, 3, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 4, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 5, NEW_INTEGER(nf));

    i=0;
    installErrorHandler();
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
	    /* Geometry */
	Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	with_z = 0;
        isNULL = 0;
        eType = wkbUnknown;
	if (Ogr_geometry == NULL) {
/*	    error("NULL geometry found"); */
	    isNULL = 1;
 	} else {
	    dim = OGR_G_GetCoordinateDimension(Ogr_geometry);
	    if (dim > 2) with_z = 1;
            eType = wkbFlatten(OGR_G_GetGeometryType(Ogr_geometry));
        }
/* ogrInfo("/home/rsb/tmp/bigshape", "parcel_mast_20050802") */
/* mast <- readOGR("/home/rsb/tmp/bigshape", "parcel_mast_20050802") */

	if (eType == wkbUnknown || eType == wkbNone) isNULL = 1;

	INTEGER_POINTER(VECTOR_ELT(ans, 3))[i] = (int) eType;
	INTEGER_POINTER(VECTOR_ELT(ans, 4))[i] = with_z;
	INTEGER_POINTER(VECTOR_ELT(ans, 5))[i] = isNULL;

	OGR_F_Destroy(Ogr_feature);
	i++;
      
    }
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    OGR_DS_Destroy(Ogr_ds);
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(pc);

    return(ans);
}

#ifdef __cplusplus
}
#endif


