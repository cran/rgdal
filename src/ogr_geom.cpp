/* Copyright (c) 2006 Roger Bivand
* Function using C API and based on v.in.ogr from GRASS by Radim Blazek
* to read OGR vector geometry features */

#include "ogr_api.h"

#ifdef __cplusplus
extern "C" {
#endif
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP R_OGR_CAPI_features(SEXP dsn, SEXP layer)
{

    OGRDataSourceH Ogr_ds;
    OGRLayerH Ogr_layer;
    OGRFeatureDefnH Ogr_featuredefn;
    OGRFeatureH Ogr_feature;
    OGRGeometryH Ogr_geometry;
    OGRwkbGeometryType eType;
    OGRGeometryH hRing, hRingM;

    int navailable_layers; 
    int i, j, k, km;
/*    int iDriver;*/
    int dim, with_z;
    int np, nr, nm;
/*    char *pszProj4 = NULL;*/

    int pc=0;
    int nf, mp_count;

    SEXP ans;
    SEXP ansnames;

    Ogr_ds = OGROpen(CHAR(STRING_ELT(dsn, 0)), FALSE, NULL);
    if (Ogr_ds == NULL) error("Cannot open data source");

    navailable_layers = OGR_DS_GetLayerCount(Ogr_ds);

    j=-1;
    for (i = 0; i < navailable_layers; i++) {
	Ogr_layer =  OGR_DS_GetLayer( Ogr_ds, i );
	Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);
	if (strcmp((char *)OGR_FD_GetName(Ogr_featuredefn), 
	    CHAR(STRING_ELT(layer, 0))) == 0) j = i; 
    }

    if (j < 0) error("Layer not found");

    PROTECT(ans = NEW_LIST(5)); pc++;
    PROTECT(ansnames = NEW_CHARACTER(5)); pc++;
    SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("dsn"));
    SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("layer"));
    SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("proj4string"));
    SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("geomTypes"));
    SET_STRING_ELT(ansnames, 4, COPY_TO_USER_STRING("crdlist"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 0), 0,
    SET_STRING_ELT(VECTOR_ELT(ans, 0), 0, 
	COPY_TO_USER_STRING(OGR_DS_GetName(Ogr_ds)));

    Ogr_layer =  OGR_DS_GetLayer(Ogr_ds, j);
    Ogr_featuredefn = OGR_L_GetLayerDefn(Ogr_layer);

    SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(1));
//    SET_VECTOR_ELT(VECTOR_ELT(ans, 1), 0, 
    SET_STRING_ELT(VECTOR_ELT(ans, 1), 0,
	COPY_TO_USER_STRING((char *)OGR_FD_GetName(Ogr_featuredefn)));
    SET_VECTOR_ELT(ans, 2, NEW_INTEGER(1));

/* was projection */

    nf = OGR_L_GetFeatureCount(Ogr_layer, 1);

    SET_VECTOR_ELT(ans, 3, NEW_INTEGER(nf));
    SET_VECTOR_ELT(ans, 4, NEW_LIST(nf));

    i=0;
    while( (Ogr_feature = OGR_L_GetNextFeature(Ogr_layer)) != NULL ) {
	    /* Geometry */
	Ogr_geometry = OGR_F_GetGeometryRef(Ogr_feature);
	with_z = 0;
	if ( Ogr_geometry == NULL ) {
	    warning("NULL geometry found");
	} else {
	    dim = OGR_G_GetCoordinateDimension(Ogr_geometry);
	    if (dim > 2) 
		with_z = 1;
	}
        eType = wkbFlatten(OGR_G_GetGeometryType(Ogr_geometry));

	INTEGER_POINTER(VECTOR_ELT(ans, 3))[i] =  eType;

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
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(1));
	    SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 0, 
		NEW_LIST(2));
	    np = OGR_G_GetPointCount(Ogr_geometry);

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
	} else if (eType == wkbPolygon) {
	    nr = OGR_G_GetGeometryCount(Ogr_geometry);
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(nr));
	    for (j=0; j < nr; j++) {
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), j, 
		    NEW_LIST(2));
	        hRing = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        np = OGR_G_GetPointCount(hRing);
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 0, NEW_NUMERIC(np));
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), 
		    j), 1, NEW_NUMERIC(np));

                for(k = 0; k < np; k++) {
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 0))[k] = OGR_G_GetX(hRing, k);
	            NUMERIC_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
		        ans, 4), i), j), 1))[k] = OGR_G_GetY(hRing, k);
	        }
	    }
	} else if (eType == wkbMultiLineString) {
	    nm = OGR_G_GetGeometryCount(Ogr_geometry);
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(nm));
	    for(j = 0; j < nm; j++) {
	        SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), j, 
		    NEW_LIST(2));
		hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        np = OGR_G_GetPointCount(hRingM);
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
	} else if (eType == wkbMultiPolygon) {
	    nm = OGR_G_GetGeometryCount(Ogr_geometry);
	    for(j = 0, mp_count = 0; j < nm; j++) {
		hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        nr = OGR_G_GetGeometryCount(hRingM);
		mp_count += nr;
	    }
	    SET_VECTOR_ELT(VECTOR_ELT(ans, 4), i, NEW_LIST(mp_count));
	    mp_count = 0;
	    for(j = 0; j < nm; j++) {
		hRingM = OGR_G_GetGeometryRef(Ogr_geometry, j);
	        nr = OGR_G_GetGeometryCount(hRingM);
	        for (k=0; k < nr; k++) {
	            SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 4), i), mp_count, 
		        NEW_LIST(2));
	            hRing = OGR_G_GetGeometryRef(hRingM, k);
	            np = OGR_G_GetPointCount(hRing);
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
		    mp_count++;
	        }
	    }
	} else warning("eType not chosen");
	OGR_F_Destroy(Ogr_feature);
	i++;
    }
    UNPROTECT(pc);

    return(ans);
}

#ifdef __cplusplus
}
#endif


