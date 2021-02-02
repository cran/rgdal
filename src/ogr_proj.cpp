/* Copyright (c) 2006-2019 Roger Bivand
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


SEXP P6_SRID_show(SEXP inSRID, SEXP format, SEXP multiline, SEXP in_format,
    SEXP epsg, SEXP out_format) {

#if GDAL_VERSION_MAJOR >= 3

    OGRSpatialReference *hSRS = new OGRSpatialReference;
    char *pszSRS = NULL;
    SEXP ans;
    char **papszOptions = NULL;
    SEXP Datum, ToWGS84, Ellps;
    int i, pc=0;
    int vis_order;
    const char *datum, *towgs84, *ellps;
    SEXP enforce_xy = getAttrib(in_format, install("enforce_xy"));

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;
//Rprintf("vis_order: %d\n", vis_order);
//srs.SetFromUserInput("ESRI:102008")
    if (INTEGER_POINTER(in_format)[0] == 1L) {
        installErrorHandler();
        if (hSRS->importFromProj4((const char *) CHAR(STRING_ELT(inSRID, 0))) != OGRERR_NONE) {
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't parse PROJ.4-style parameter string");
        }
        uninstallErrorHandlerAndTriggerError();
    } else if (INTEGER_POINTER(in_format)[0] == 2L) {
        installErrorHandler();
        if (hSRS->importFromURN((const char *) CHAR(STRING_ELT(inSRID, 0))) != OGRERR_NONE) {
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't parse URN-style parameter string");
        }
        uninstallErrorHandlerAndTriggerError();
    } else if (INTEGER_POINTER(in_format)[0] == 3L) {
        installErrorHandler();
        if (hSRS->importFromWkt((const char *) CHAR(STRING_ELT(inSRID, 0))) != OGRERR_NONE) {
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't parse WKT-style parameter string");
        }
        uninstallErrorHandlerAndTriggerError();
    } else if (INTEGER_POINTER(in_format)[0] == 4L) {
        installErrorHandler();
        if (hSRS->importFromEPSG(INTEGER_POINTER(epsg)[0]) != OGRERR_NONE) {
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't parse EPSG-style code");
        }
        uninstallErrorHandlerAndTriggerError();
    } else if (INTEGER_POINTER(in_format)[0] == 5L) {
        installErrorHandler();
        if (hSRS->SetFromUserInput((const char *) CHAR(STRING_ELT(inSRID, 0))) != OGRERR_NONE) {
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't parse user input string");
        }
        uninstallErrorHandlerAndTriggerError();
    }

//    Rprintf("MappingStrategy in: %d\n",  hSRS->GetAxisMappingStrategy());
    if (hSRS != NULL) {
        if (vis_order == 1) {
            hSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
        }
    }
//    Rprintf("MappingStrategy out: %d\n", hSRS->GetAxisMappingStrategy());


    PROTECT(ans=NEW_CHARACTER(1)); pc++;

    if (INTEGER_POINTER(out_format)[0] == 1L) {
        installErrorHandler();
        papszOptions = CSLAddString(papszOptions, CHAR(STRING_ELT(multiline, 0)));
        papszOptions = CSLAddString(papszOptions, CHAR(STRING_ELT(format, 0)));
        uninstallErrorHandlerAndTriggerError();

        installErrorHandler();
        if (hSRS->exportToWkt(&pszSRS, papszOptions) != OGRERR_NONE) {
            CPLFree(pszSRS);
            CSLDestroy(papszOptions);
            delete hSRS;
            uninstallErrorHandlerAndTriggerError();
	    error("Can't express as WKT");
        }
        uninstallErrorHandlerAndTriggerError();
        SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS));
    } else if (INTEGER_POINTER(out_format)[0] == 2L) {
        installErrorHandler();
        if (hSRS->exportToProj4(&pszSRS) != OGRERR_NONE) {
            SET_STRING_ELT(ans, 0, NA_STRING);
	} else {
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS));
        }
        uninstallErrorHandlerAndTriggerError();
    } else {
        CPLFree(pszSRS);
        CSLDestroy(papszOptions);
        delete hSRS;
        error("unknown output format");
    }

    if (hSRS != NULL) {
        installErrorHandler();
        datum = hSRS->GetAttrValue("DATUM");
        uninstallErrorHandlerAndTriggerError();
        PROTECT(Datum = NEW_CHARACTER(1)); pc++;
        if (datum != NULL) SET_STRING_ELT(Datum, 0, COPY_TO_USER_STRING(datum));

        installErrorHandler();
        ellps = hSRS->GetAttrValue("DATUM|SPHEROID");
        uninstallErrorHandlerAndTriggerError();
        PROTECT(Ellps = NEW_CHARACTER(1)); pc++;
        if (ellps != NULL) SET_STRING_ELT(Ellps, 0, COPY_TO_USER_STRING(ellps));

        PROTECT(ToWGS84 = NEW_CHARACTER(7)); pc++;
        installErrorHandler();
        for (i=0; i<7; i++) {
            towgs84 = hSRS->GetAttrValue("TOWGS84", i);
            if (towgs84 != NULL) SET_STRING_ELT(ToWGS84, i,
                COPY_TO_USER_STRING(towgs84));
        }
        uninstallErrorHandlerAndTriggerError();
        setAttrib(ans, install("towgs84"), ToWGS84);
        setAttrib(ans, install("datum"), Datum);
        setAttrib(ans, install("ellps"), Ellps);
    }

    CPLFree(pszSRS);
    CSLDestroy(papszOptions);
    delete hSRS;

    UNPROTECT(pc);

    return(ans);
#else
    return(R_NilValue);
#endif

}

SEXP OSR_is_projected(SEXP inSRID) {

    OGRSpatialReference *hSRS = new OGRSpatialReference;
    int is_proj;
    SEXP ans;

    installErrorHandler();
    if (hSRS->SetFromUserInput((const char *) CHAR(STRING_ELT(inSRID, 0))) != OGRERR_NONE) {
        delete hSRS;
        uninstallErrorHandlerAndTriggerError();
        error("Can't parse user input string");
    }
    uninstallErrorHandlerAndTriggerError();
    is_proj = hSRS->IsProjected();
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL_POINTER(ans)[0] = is_proj;
    delete hSRS;
    UNPROTECT(1);
    return(ans);
}


SEXP R_GDAL_OSR_PROJ() {

#if ((GDAL_VERSION_MAJOR == 3 && GDAL_VERSION_MINOR == 0 && GDAL_VERSION_REV >= 1) || (GDAL_VERSION_MAJOR == 3 && GDAL_VERSION_MINOR > 0) || (GDAL_VERSION_MAJOR > 3))
        SEXP OSRProjVersion;
        int pnMajor, pnMinor, pnPatch, pc=0;

        installErrorHandler();
        OSRGetPROJVersion(&pnMajor, &pnMinor, &pnPatch);
        uninstallErrorHandlerAndTriggerError();

        PROTECT(OSRProjVersion = NEW_INTEGER(3)); pc++;
        INTEGER_POINTER(OSRProjVersion)[0] = pnMajor;
        INTEGER_POINTER(OSRProjVersion)[1] = pnMinor;
        INTEGER_POINTER(OSRProjVersion)[2] = pnPatch;

        UNPROTECT(pc);
        return(OSRProjVersion);
#else
        return(R_NilValue);
#endif

}



SEXP p4s_to_wkt(SEXP p4s, SEXP esri) {

    OGRSpatialReference *hSRS = new OGRSpatialReference;
    char *pszSRS_WKT = NULL;
    SEXP ans;
#if GDAL_VERSION_MAJOR >= 3
    int vis_order;
    SEXP enforce_xy = getAttrib(esri, install("enforce_xy"));

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;
#endif

    installErrorHandler();
    if (hSRS->importFromProj4(CHAR(STRING_ELT(p4s, 0))) != OGRERR_NONE) {
        delete hSRS;
        uninstallErrorHandlerAndTriggerError();
	error("Can't parse PROJ.4-style parameter string");
    }
    uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
    installErrorHandler();
    if (vis_order == 1) 
        hSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
    uninstallErrorHandlerAndTriggerError();
#endif


    installErrorHandler();
#if GDAL_VERSION_MAJOR < 3
    if (LOGICAL_POINTER(esri)[0] == 1) hSRS->morphToESRI();
#endif
    hSRS->exportToWkt(&pszSRS_WKT);
    uninstallErrorHandlerAndTriggerError();

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_WKT));
    delete hSRS;
    CPLFree(pszSRS_WKT);

    UNPROTECT(1);

    return(ans);
}

SEXP wkt_to_p4s(SEXP wkt, SEXP esri) {

    OGRSpatialReference *hSRS = new OGRSpatialReference;
    char *pszSRS_P4 = NULL;
    SEXP ans;

#if GDAL_VERSION_MAJOR >= 3
    int vis_order;
    SEXP enforce_xy = getAttrib(esri, install("enforce_xy"));

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;
#endif

    installErrorHandler();
#if GDAL_VERSION_MAJOR == 1 || ( GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR <= 2 ) // thanks to Even Roualt https://github.com/OSGeo/gdal/issues/681
    char **ppszInput = NULL;
    ppszInput = CSLAddString(ppszInput, CHAR(STRING_ELT(wkt, 0)));
    if (hSRS->importFromWkt(ppszInput) != OGRERR_NONE) 
    {
        delete hSRS;
        CPLFree(ppszInput);
        uninstallErrorHandlerAndTriggerError();
	error("Can't parse WKT-style parameter string");
    }
    CPLFree(ppszInput);
#else
    if (hSRS->importFromWkt((const char *) CHAR(STRING_ELT(wkt, 0))) != OGRERR_NONE) 
    {
        delete hSRS;
        uninstallErrorHandlerAndTriggerError();
	error("Can't parse WKT-style parameter string");
    }
#endif 
    uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
    installErrorHandler();
    if (vis_order == 1) 
        hSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
    uninstallErrorHandlerAndTriggerError();
#endif


    installErrorHandler();
#if GDAL_VERSION_MAJOR < 3
    if (LOGICAL_POINTER(esri)[0] == 1) hSRS->morphFromESRI();
#endif
    hSRS->exportToProj4(&pszSRS_P4);
    uninstallErrorHandlerAndTriggerError();
    delete hSRS;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_P4));
    CPLFree(pszSRS_P4);

    UNPROTECT(1);

    return(ans);
}

SEXP ogrAutoIdentifyEPSG(SEXP p4s) {

    OGRSpatialReference *hSRS = new OGRSpatialReference;
    OGRErr thisOGRErr;
    SEXP ans;
#if GDAL_VERSION_MAJOR >= 3
    int vis_order;
    SEXP enforce_xy = getAttrib(p4s, install("enforce_xy"));

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;
#endif

    installErrorHandler();
    if (hSRS->importFromProj4(CHAR(STRING_ELT(p4s, 0))) != OGRERR_NONE) {
        delete hSRS;
        uninstallErrorHandlerAndTriggerError();
	error("Can't parse PROJ.4-style parameter string");
    }
    uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
    installErrorHandler();
    if (vis_order == 1)
        hSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
    uninstallErrorHandlerAndTriggerError();
#endif

    PROTECT(ans=NEW_CHARACTER(1));

    installErrorHandler();
    thisOGRErr = hSRS->AutoIdentifyEPSG();
    uninstallErrorHandlerAndTriggerError();

    if (thisOGRErr == OGRERR_NONE) {
        installErrorHandler();
        SET_STRING_ELT(ans, 0,
            COPY_TO_USER_STRING(hSRS->GetAuthorityCode(NULL)));
        uninstallErrorHandlerAndTriggerError();
    } else if (thisOGRErr == OGRERR_UNSUPPORTED_SRS) {
        SET_STRING_ELT(ans, 0,
            COPY_TO_USER_STRING("OGRERR_UNSUPPORTED_SRS"));
    }
    delete hSRS;
    UNPROTECT(1);

    return(ans);
}

SEXP ogrP4S(SEXP ogrsourcename, SEXP Layer, SEXP morphFromESRI, SEXP dumpSRS) {

#ifdef GDALV2
//    GDALDriver *poDriver;
    GDALDataset *poDS;
#else
    OGRSFDriver *poDriver;
    OGRDataSource *poDS;
#endif
    OGRLayer *poLayer;

// leak fixed
    OGRSpatialReference *hSRS; // = new OGRSpatialReference;
    char *pszProj4 = NULL;
    SEXP ans, Datum, ToWGS84, Ellps;
    int i, pc=0;
    const char *datum, *towgs84, *ellps;
#if GDAL_VERSION_MAJOR >= 3
    int vis_order;
    SEXP enforce_xy = getAttrib(dumpSRS, install("enforce_xy"));

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;
#endif

    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrsourcename, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
#else
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrsourcename, 0)), 
	FALSE, &poDriver);
#endif
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

    installErrorHandler();
    hSRS = poLayer->GetSpatialRef();
    uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
    if (hSRS != NULL) {
        installErrorHandler();
//Rprintf("ogrP4S input AxisMappingStrategy %d\n", hSRS->GetAxisMappingStrategy());
        if (vis_order == 1) 
            hSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
//Rprintf("ogrP4S output AxisMappingStrategy %d\n", hSRS->GetAxisMappingStrategy());
        uninstallErrorHandlerAndTriggerError();
    }
#endif


    PROTECT(ans=NEW_CHARACTER(1)); pc++;

    if (hSRS != NULL) {

        installErrorHandler();
        if (LOGICAL_POINTER(dumpSRS)[0]) {
            hSRS->dumpReadable();
        }
        uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
        SEXP WKT2_2018;
        char *wkt2=NULL;
        PROTECT(WKT2_2018 = NEW_CHARACTER(1)); pc++;

        installErrorHandler();
        const char* papszOptions[] = { "FORMAT=WKT2_2018", "MULTILINE=YES", nullptr };
        uninstallErrorHandlerAndTriggerError();

        installErrorHandler();
        if (hSRS->exportToWkt(&wkt2, papszOptions) != OGRERR_NONE) {
            SET_STRING_ELT(WKT2_2018, 0, NA_STRING);
        }
        SET_STRING_ELT(WKT2_2018, 0, COPY_TO_USER_STRING(wkt2));
        CPLFree( wkt2 ); // added
        uninstallErrorHandlerAndTriggerError();
        setAttrib(ans, install("WKT2_2018"), WKT2_2018);
#endif
    }


    if (hSRS != NULL) {
        installErrorHandler();
#if GDAL_VERSION_MAJOR < 3
        if (LOGICAL_POINTER(morphFromESRI)[0]) hSRS->morphFromESRI();
#endif
        if (hSRS->exportToProj4(&pszProj4) != OGRERR_NONE) {
            SET_STRING_ELT(ans, 0, NA_STRING);
            CPLFree(pszProj4);
	} else {
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszProj4));
            CPLFree(pszProj4);
	}
        uninstallErrorHandlerAndTriggerError();
    } else SET_STRING_ELT(ans, 0, NA_STRING);

    if (hSRS != NULL) {
        installErrorHandler();
        datum = hSRS->GetAttrValue("DATUM");
        uninstallErrorHandlerAndTriggerError();
        PROTECT(Datum = NEW_CHARACTER(1)); pc++;
        if (datum != NULL) SET_STRING_ELT(Datum, 0, COPY_TO_USER_STRING(datum));

        installErrorHandler();
        ellps = hSRS->GetAttrValue("DATUM|SPHEROID");
        uninstallErrorHandlerAndTriggerError();
        PROTECT(Ellps = NEW_CHARACTER(1)); pc++;
        if (ellps != NULL) SET_STRING_ELT(Ellps, 0, COPY_TO_USER_STRING(ellps));

        PROTECT(ToWGS84 = NEW_CHARACTER(7)); pc++;
        installErrorHandler();
        for (i=0; i<7; i++) {
            towgs84 = hSRS->GetAttrValue("TOWGS84", i);
            if (towgs84 != NULL) SET_STRING_ELT(ToWGS84, i,
                COPY_TO_USER_STRING(towgs84));
        }
        uninstallErrorHandlerAndTriggerError();
        setAttrib(ans, install("towgs84"), ToWGS84);
        setAttrib(ans, install("datum"), Datum);
        setAttrib(ans, install("ellps"), Ellps);
    }


    installErrorHandler();
    delete poDS;
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(pc);
    return(ans);
}
#ifdef __cplusplus
}
#endif


