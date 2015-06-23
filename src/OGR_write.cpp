#include <cpl_string.h>
#include "ogrsf_frmts.h"

// R headers moved outside extern "C" 070808 RSB re. note from BDR
// #ifdef __cplusplus
// extern "C" {
// #endif

#include <Rdefines.h> 
#include <R.h>  
#include "rgdal.h"

#ifdef __cplusplus
extern "C" {
#endif

// RSB 081009
void wrtDF(int, int, SEXP, SEXP, SEXP, OGRFeature*);

SEXP OGR_write(SEXP inp)
{

//  SEXP inp is an input list built in ogr_write() and documented
//  in code there

// poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i]) 130502

#ifdef GDALV2
    GDALDriver *poDriver;
    GDALDataset *poDS;
#else
    OGRSFDriver *poDriver;
    OGRDataSource *poDS;
#endif
    OGRLayer *poLayer;
    char **papszCreateOptions = NULL;
    char **papszCreateOptionsLayer = NULL;
    SEXP ans, oCard;
    int pc=0, i, j, k;

    PROTECT(ans = NEW_CHARACTER(1)); pc++;

    installErrorHandler();
#ifdef GDALV2
    poDriver = GetGDALDriverManager()->GetDriverByName(CHAR(STRING_ELT(VECTOR_ELT(inp, 3), 0)) );
#else
    poDriver = OGRSFDriverRegistrar::GetRegistrar()->GetDriverByName(
                CHAR(STRING_ELT(VECTOR_ELT(inp, 3), 0)) );
#endif
    uninstallErrorHandlerAndTriggerError();
    if( poDriver == NULL )
    {
        error("Driver not available");
    }
//  retrieve and set options:
//  papszCreateOptions: a StringList of name=value options.
//  Options are driver specific. 

    SEXP sOpts = VECTOR_ELT(inp, 9);

    installErrorHandler();
    for (i=0; i < length(sOpts); i++) papszCreateOptions = CSLAddString( 
        papszCreateOptions, CHAR(STRING_ELT(sOpts, i)) );
    uninstallErrorHandlerAndTriggerError();
#ifdef RGDALDEBUG
    installErrorHandler();
    for (i=0; i < CSLCount(papszCreateOptions); i++)
        Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptions, i));
    uninstallErrorHandlerAndTriggerError();
#endif

    installErrorHandler();
#ifdef GDALV2
    poDS = poDriver->Create( CHAR(STRING_ELT(VECTOR_ELT(inp,
        1), 0)), 0, 0, 0, GDT_Unknown, papszCreateOptions );
#else
    poDS = poDriver->CreateDataSource( CHAR(STRING_ELT(VECTOR_ELT(inp,
        1), 0)), papszCreateOptions );
#endif
    uninstallErrorHandlerAndTriggerError();
    if( poDS == NULL )
    {
        installErrorHandler();
#ifdef GDALV2
        GDALClose( poDS );
#else
        OGRDataSource::DestroyDataSource( poDS );
#endif
        CSLDestroy(papszCreateOptions);
        uninstallErrorHandlerAndTriggerError();
        error( "Creation of output file failed" );
    }
    installErrorHandler();
    CSLDestroy(papszCreateOptions);
    uninstallErrorHandlerAndTriggerError();

//  define layer characteristics

    SEXP obj = VECTOR_ELT(inp, 0);
    int nobs = INTEGER_POINTER(VECTOR_ELT(inp, 4))[0];
// const added 070604 RSB
    const char *cl = CHAR(asChar(getAttrib(obj, R_ClassSymbol)));
    OGRwkbGeometryType wkbtype = wkbUnknown;

    if (!strcmp(cl, "SpatialPointsDataFrame")) wkbtype = wkbPoint;
    else if (!strcmp(cl, "SpatialLinesDataFrame"))  wkbtype = wkbLineString;
    else if (!strcmp(cl, "SpatialPolygonsDataFrame")) wkbtype = wkbPolygon;

    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(cl));

//  check and if necessary set multiple geometries per data frame row
//  for line and polygon objects; multi-points not admitted

    if (wkbtype == wkbLineString) {

        SEXP lns = GET_SLOT(obj, install("lines"));
        if (length(lns) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        PROTECT(oCard = NEW_INTEGER(nobs)); pc++;
        int multi=0, Lns_l;
	for (i=0; i<nobs; i++) {
            Lns_l = length(GET_SLOT(VECTOR_ELT(lns, i), install("Lines")));
            INTEGER_POINTER(oCard)[i] = Lns_l;
            if (Lns_l > 1) multi=1;
	}
        if (multi > 0) wkbtype = wkbMultiLineString;
    }

    if (wkbtype == wkbPolygon) {

        SEXP pls = GET_SLOT(obj, install("polygons"));
        if (length(pls) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        PROTECT(oCard = NEW_INTEGER(nobs)); pc++;
        int multi=0, Pls_l;
	for (i=0; i<nobs; i++) {
            Pls_l = length(GET_SLOT(VECTOR_ELT(pls, i), install("Polygons")));
            INTEGER_POINTER(oCard)[i] = Pls_l;
            if (Pls_l > 1) multi=1;
	}
        if (multi > 0) wkbtype = wkbMultiPolygon;
    }

//  retrieve and set spatial reference system
//  retrieve and set options:
//  papszCreateOptions: a StringList of name=value options.
//  Options are driver specific. 

    SEXP sxpOpts = VECTOR_ELT(inp, 10);

    installErrorHandler();
    for (i=0; i < length(sxpOpts); i++) papszCreateOptionsLayer = CSLAddString( 
        papszCreateOptionsLayer, CHAR(STRING_ELT(sxpOpts, i)) );
    uninstallErrorHandlerAndTriggerError();
#ifdef RGDALDEBUG
    installErrorHandler();
    for (i=0; i < CSLCount(papszCreateOptionsLayer); i++)
        Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptionsLayer, i));
    uninstallErrorHandlerAndTriggerError();
#endif

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
// const added 070604 RSB
    const char *PROJ4 = CHAR(STRING_ELT(GET_SLOT(p4s, install("projargs")), 0));

    if (strcmp(PROJ4, "NA")) {
            OGRSpatialReference hSRS = NULL;
            installErrorHandler();
            if (hSRS.importFromProj4(PROJ4) != OGRERR_NONE) {
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
	        error("Can't parse PROJ.4-style parameter string");
            }
            uninstallErrorHandlerAndTriggerError();
            installErrorHandler();
            if (LOGICAL_POINTER(VECTOR_ELT(inp, 11))[0]) {
                    hSRS.morphToESRI();
            }
            uninstallErrorHandlerAndTriggerError();
            installErrorHandler();
            poLayer = poDS->CreateLayer( CHAR(STRING_ELT(VECTOR_ELT(inp, 2),
                0)), &hSRS, wkbtype, papszCreateOptionsLayer );
            uninstallErrorHandlerAndTriggerError();

    } else {
        installErrorHandler();
        poLayer = poDS->CreateLayer( CHAR(STRING_ELT(VECTOR_ELT(inp, 2),
            0)), NULL, wkbtype, papszCreateOptionsLayer );
        uninstallErrorHandlerAndTriggerError();
    }
    if( poLayer == NULL )
    {
        installErrorHandler();
#ifdef GDALV2
        GDALClose( poDS );
#else
        OGRDataSource::DestroyDataSource( poDS );
#endif
        uninstallErrorHandlerAndTriggerError();
        error( "Layer creation failed" );
    }

    installErrorHandler();
    CSLDestroy(papszCreateOptionsLayer);
    uninstallErrorHandlerAndTriggerError();

// create fields in layer

    int nf = INTEGER_POINTER(VECTOR_ELT(inp, 5))[0];
    SEXP fld_names = VECTOR_ELT(inp, 6);
    SEXP ogr_ftype = VECTOR_ELT(inp, 7);
    int OGR_type;

    for (i=0; i<nf; i++) {
        OGR_type = INTEGER_POINTER(ogr_ftype)[i];
        if (OGR_type != 0 && OGR_type != 2 && OGR_type != 4) {
            Rprintf("%s %d\n", CHAR(STRING_ELT(fld_names, i)), 
                (OGRFieldType) OGR_type);
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error( "Unknown field type" );
        }
        installErrorHandler();
        OGRFieldDefn oField( CHAR(STRING_ELT(fld_names, i)),
            (OGRFieldType)  OGR_type);
// RSB 081009 FIXME - not working yet, integer flips to real in shapefile
        if (OGR_type == 0) oField.SetPrecision(0);
        if( poLayer->CreateField( &oField ) != OGRERR_NONE ) {
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error( "Creating Name field failed" );
        }
        uninstallErrorHandlerAndTriggerError();
    }

    SEXP ldata = VECTOR_ELT(inp, 8);

// Point data

    if (wkbtype == wkbPoint) {
        SEXP crds, dim;
        crds = GET_SLOT(obj, install("coords"));
        dim = getAttrib(crds, install("dim"));
        int z=INTEGER_POINTER(dim)[1];
        if (INTEGER_POINTER(dim)[0] != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        installErrorHandler();
        for (i=0; i<nobs; i++) {
            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );

// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature);

            OGRPoint pt;
            pt.setX( NUMERIC_POINTER(crds)[i] );
            pt.setY( NUMERIC_POINTER(crds)[i+nobs] );
            if (z > 2) pt.setZ( NUMERIC_POINTER(crds)[i+(2*nobs)] );

            poFeature->SetGeometry( &pt ); 
#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
        uninstallErrorHandlerAndTriggerError();

// Line data

    } else if (wkbtype == wkbLineString) {

        SEXP lns = GET_SLOT(obj, install("lines"));
        if (length(lns) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        installErrorHandler();
	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature);

            SEXP crds, dim;
            crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(lns, i),
                install("Lines")), 0), install("coords"));
            dim = getAttrib(crds, install("dim"));
            int ncrds = INTEGER_POINTER(dim)[0];
            OGRLineString OGRln;
            for (j=0; j<ncrds; j++) 
                OGRln.setPoint( j, NUMERIC_POINTER(crds)[j],
                                   NUMERIC_POINTER(crds)[j+ncrds] );

            if( poFeature->SetGeometry( &OGRln ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set geometry" );
            } 

#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
        uninstallErrorHandlerAndTriggerError();

// Multi line data

    } else if (wkbtype == wkbMultiLineString) {

        SEXP lns = GET_SLOT(obj, install("lines"));
        if (length(lns) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        SEXP Lns;
        int Lns_l;
        installErrorHandler();
	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature);

            Lns = GET_SLOT(VECTOR_ELT(lns, i), install("Lines"));
            Lns_l = length(Lns);

            OGRMultiLineString OGRlns;

            for (k=0; k<Lns_l; k++) {
                SEXP crds, dim;
                crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(lns, i),
                    install("Lines")), k), install("coords"));
                dim = getAttrib(crds, install("dim"));
                int ncrds = INTEGER_POINTER(dim)[0];

                OGRLineString OGRln;

                for (j=0; j<ncrds; j++) 
                    OGRln.setPoint( j, NUMERIC_POINTER(crds)[j],
                                       NUMERIC_POINTER(crds)[j+ncrds] );

                if( OGRlns.addGeometry( &OGRln ) != OGRERR_NONE ) {
                   installErrorHandler();
#ifdef GDALV2
                    GDALClose( poDS );
#else
                    OGRDataSource::DestroyDataSource( poDS );
#endif
                    uninstallErrorHandlerAndTriggerError();
                    error( "Failed to add line" );
                } 
            }

            if( poFeature->SetGeometry( &OGRlns ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set geometry" );
            } 

#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
        uninstallErrorHandlerAndTriggerError();

// Polygon data

    } else if (wkbtype == wkbPolygon) {

        SEXP lns = GET_SLOT(obj, install("polygons"));
        if (length(lns) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        installErrorHandler();
	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature);

            SEXP crds, dim;
            crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(lns, i),
                install("Polygons")), 0), install("coords"));
            dim = getAttrib(crds, install("dim"));
            int ncrds = INTEGER_POINTER(dim)[0];

            OGRPolygon OGRply;
            OGRLinearRing OGRlr;

            for (j=0; j<ncrds; j++) 
                OGRlr.setPoint( j, NUMERIC_POINTER(crds)[j],
                                   NUMERIC_POINTER(crds)[j+ncrds] );
            OGRply.addRing( &OGRlr );

            if( poFeature->SetGeometry( &OGRply ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set geometry" );
            } 

#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                OGRDataSource::DestroyDataSource( poDS );
#endif
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
        uninstallErrorHandlerAndTriggerError();

// Multi polygon data

    } else if (wkbtype == wkbMultiPolygon) {
	// Rprintf("Yes, multipolygons...\n");

        SEXP lns = GET_SLOT(obj, install("polygons"));
        if (length(lns) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        SEXP Lns;
        int Lns_l;
        installErrorHandler();
	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature);

            Lns = GET_SLOT(VECTOR_ELT(lns, i), install("Polygons"));
            Lns_l = length(Lns);

            OGRPolygon OGRply;

            for (k=0; k<Lns_l; k++) {
                SEXP crds, dim;
                crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(lns, i),
                    install("Polygons")), k), install("coords"));
                dim = getAttrib(crds, install("dim"));
                int ncrds = INTEGER_POINTER(dim)[0];

                OGRLinearRing OGRlr;

                for (j=0; j<ncrds; j++) 
                    OGRlr.setPoint( j, NUMERIC_POINTER(crds)[j],
                                    NUMERIC_POINTER(crds)[j+ncrds] );

                OGRply.addRing( &OGRlr );

            } // k

             if( poFeature->SetGeometry( &OGRply ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to set geometry" );
            } 

// FIXME
#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
               installErrorHandler();
                OGRDataSource::DestroyDataSource( poDS );
#endif
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to set FID" );
            } 

	    // EJP:
	    poFeature->SetGeometryDirectly(
		OGRGeometryFactory::forceToMultiPolygon(
		poFeature->StealGeometry() ) );

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        } // i 
        uninstallErrorHandlerAndTriggerError();

    } // multiPolygon 

    installErrorHandler();
#ifdef GDALV2
    GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(pc);
    return(ans);
}

// RSB 081009
void wrtDF(int i, int nf, SEXP fld_names, SEXP ldata,
     SEXP ogr_ftype, OGRFeature* poFeature) {
     int j, OGR_type;
     for (j=0; j<nf; j++) {
         installErrorHandler();
         OGR_type = INTEGER_POINTER(ogr_ftype)[j];
         if (OGR_type == 2) {
             if (!ISNA(NUMERIC_POINTER(VECTOR_ELT(ldata, j))[i]))
                 poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                     NUMERIC_POINTER(VECTOR_ELT(ldata, j))[i] );
         } else if (OGR_type == 4) {
             if (STRING_ELT(VECTOR_ELT(ldata, j), i) != NA_STRING)
                 poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                     CHAR(STRING_ELT(VECTOR_ELT(ldata, j), i)) );
         } else if (OGR_type == 0) {
              if (INTEGER_POINTER(VECTOR_ELT(ldata, j))[i] != NA_INTEGER)
                  poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                      INTEGER_POINTER(VECTOR_ELT(ldata, j))[i] );
         }
         uninstallErrorHandlerAndTriggerError();
     }         
}


#ifdef __cplusplus
}
#endif

