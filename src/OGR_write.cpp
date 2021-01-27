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
void wrtDF(int, int, SEXP, SEXP, SEXP, OGRFeature*, int);

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
    OGRFeatureDefn *poDefn;
    char **papszCreateOptions = NULL;
    char **papszCreateOptionsLayer = NULL;
    SEXP ans, wkbtype_attr, comms, ofld_nms;
    int verbose = INTEGER_POINTER(getAttrib(VECTOR_ELT(inp, 5),
        install("verbose")))[0];
    int pc=0, i, j, k, is_shpfile, //shp_edge_case_fix,
        dumpSRS;
    OGRSpatialReference *poSRS = new OGRSpatialReference;//FIXME VG


    PROTECT(ans = NEW_CHARACTER(1)); pc++;
    PROTECT(wkbtype_attr = NEW_INTEGER(1)); pc++;
    is_shpfile = INTEGER_POINTER(getAttrib(VECTOR_ELT(inp, 3),
      install("is_shpfile")))[0]; 
//    shp_edge_case_fix = INTEGER_POINTER(getAttrib(VECTOR_ELT(inp, 3),
//      install("shp_edge_case_fix")))[0]; 
    dumpSRS = INTEGER_POINTER(getAttrib(VECTOR_ELT(inp, 11), 
      install("dumpSRS")))[0]; 

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

    if (!strcmp(cl, "SpatialPointsDataFrame")) {
        wkbtype = wkbPoint;
        if (verbose) Rprintf("Object initially classed as: wkbPoint\n");
    } else if (!strcmp(cl, "SpatialLinesDataFrame")) {
        wkbtype = wkbLineString;
        if (verbose) Rprintf("Object initially classed as: wkbLineString\n");
    } else if (!strcmp(cl, "SpatialPolygonsDataFrame")) {
        wkbtype = wkbPolygon;
        if (verbose) Rprintf("Object initially classed as: wkbPolygon\n");
    }

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
        int multi=0, Lns_l;
	for (i=0; i<nobs; i++) {
            Lns_l = length(GET_SLOT(VECTOR_ELT(lns, i), install("Lines")));
            if (Lns_l > 1) multi=1;
	}
        if (multi > 0) {
            wkbtype = wkbMultiLineString;
            if (verbose) Rprintf("Object reclassed as: wkbMultiLineString\n");
        }
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
        int multi=0, Pls_l, icomms=0;
	for (i=0; i<nobs; i++) {
            comms = SP_PREFIX(comment2comm)(VECTOR_ELT(pls, i));
            if (comms == R_NilValue) {
                Pls_l = length(GET_SLOT(VECTOR_ELT(pls, i),
                    install("Polygons")));
                if (Pls_l > 1) {
                    multi=1;
                    break;
                }
            } else {
                icomms = 1;
                if (length(comms) > 1) {
                    multi=1;
                    break;
                }
            }
	}
        if (verbose) {
            if (icomms == 0) Rprintf("No SFS comments in Polygons objects\n");
            else Rprintf("SFS comments in Polygons objects\n");
        }
        if (multi > 0) {
            wkbtype = wkbMultiPolygon;
            if (verbose) Rprintf("Object reclassed as: wkbMultiPolygon\n");
        }
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
    SEXP comment = getAttrib(p4s, install("comment"));

    if (!isNull(comment)) {
//        Rprintf("CRS comment: %s\n", CHAR(STRING_ELT(comment, 0)));
        installErrorHandler();
#if GDAL_VERSION_MAJOR == 1 || ( GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR <= 2 ) // thanks to Even Roualt https://github.com/OSGeo/gdal/issues/681
//#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
        if (poSRS->importFromWkt((char **) CHAR(STRING_ELT(comment, 0))) != OGRERR_NONE) {
#else
        if (poSRS->importFromWkt((const char *) CHAR(STRING_ELT(comment, 0))) != OGRERR_NONE) {
#endif
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
            poSRS->Release();
            uninstallErrorHandlerAndTriggerError();
            error("Can't parse WKT2-style parameter string");
        }
        uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
        installErrorHandler();
//Rprintf("OGR_write input AxisMappingStrategy %d\n", poSRS->GetAxisMappingStrategy());
        poSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
        uninstallErrorHandlerAndTriggerError();
//Rprintf("OGR_write output AxisMappingStrategy %d\n", poSRS->GetAxisMappingStrategy());
#endif

        installErrorHandler();
        if (dumpSRS) poSRS->dumpReadable();
        uninstallErrorHandlerAndTriggerError();

        installErrorHandler();
        poLayer = poDS->CreateLayer( CHAR(STRING_ELT(VECTOR_ELT(inp, 2),
            0)), poSRS, wkbtype, papszCreateOptionsLayer );
        uninstallErrorHandlerAndTriggerError();

    } else {
// const added 070604 RSB
        const char *PROJ4 = CHAR(STRING_ELT(GET_SLOT(p4s, install("projargs")), 0));

        if (strcmp(PROJ4, "NA")) {
            OGRSpatialReference* poSRS = new OGRSpatialReference;//FIXME VG
            installErrorHandler();

            if (poSRS->importFromProj4(PROJ4) != OGRERR_NONE) { //FIXME VG
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                poSRS->Release();
                uninstallErrorHandlerAndTriggerError();
	        error("Can't parse PROJ.4-style parameter string");
            }
            uninstallErrorHandlerAndTriggerError();
/*            installErrorHandler();
            if (LOGICAL_POINTER(VECTOR_ELT(inp, 11))[0]) {
                    poSRS->morphToESRI();
            }
            uninstallErrorHandlerAndTriggerError();*/

            installErrorHandler();
            if (dumpSRS) poSRS->dumpReadable();
            uninstallErrorHandlerAndTriggerError();

            installErrorHandler();
            poLayer = poDS->CreateLayer( CHAR(STRING_ELT(VECTOR_ELT(inp, 2),
                0)), poSRS, wkbtype, papszCreateOptionsLayer );
            uninstallErrorHandlerAndTriggerError();

        } else {
            installErrorHandler();
            poLayer = poDS->CreateLayer( CHAR(STRING_ELT(VECTOR_ELT(inp, 2),
                0)), NULL, wkbtype, papszCreateOptionsLayer );
            uninstallErrorHandlerAndTriggerError();
        }
    }
    if( poLayer == NULL )
    {
        installErrorHandler();
        delete poSRS;
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

    INTEGER_POINTER(wkbtype_attr)[0] = wkbtype;
    setAttrib(ans, install("wkbtype_attr"), wkbtype_attr);
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
            delete poSRS;
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
    
    installErrorHandler();
    poDefn = poLayer->GetLayerDefn();
    int nFields =  poDefn->GetFieldCount();
    uninstallErrorHandlerAndTriggerError();

    PROTECT(ofld_nms = NEW_CHARACTER(nFields)); pc++;

    installErrorHandler();
    for (i=0; i<nFields; i++) {
      OGRFieldDefn *poField = poDefn->GetFieldDefn(i);
      SET_STRING_ELT(ofld_nms, i, COPY_TO_USER_STRING(poField->GetNameRef()));
    }
    uninstallErrorHandlerAndTriggerError();
        
    setAttrib(ans, install("ofld_nms"), ofld_nms);

    SEXP ldata = VECTOR_ELT(inp, 8);

// Point data

#ifdef GDALV2
    int transaction_ds = poDS->TestCapability(ODsCTransactions);
//Rprintf("transaction_ds: %s, %d\n", CHAR(STRING_ELT(VECTOR_ELT(inp, 3), 0)) , transaction_ds);
#endif

    if (wkbtype == wkbPoint) {
        SEXP crds, dim;
        crds = GET_SLOT(obj, install("coords"));
        dim = getAttrib(crds, install("dim"));
        int z=INTEGER_POINTER(dim)[1];
        if (INTEGER_POINTER(dim)[0] != nobs) {
            installErrorHandler();
            delete poSRS;
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        if (verbose) Rprintf("Writing %d wkbPoint objects\n", nobs);
        installErrorHandler();

#ifdef GDALV2
        int start_trans = transaction_ds &&
          (poDS->StartTransaction() == OGRERR_NONE);
#endif

        for (i=0; i<nobs; i++) {
            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );

// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature, is_shpfile);

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
                delete poSRS;
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                delete poSRS;
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }

#ifdef GDALV2
        if (start_trans && poDS->CommitTransaction() != OGRERR_NONE) {
          GDALClose( poDS );
          delete poSRS;
          uninstallErrorHandlerAndTriggerError();
          error("commit transaction failure");
        }
#endif

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
            delete poSRS;
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        if (verbose) Rprintf("Writing %d wkbLineString objects\n", nobs);
        installErrorHandler();
#ifdef GDALV2
        int start_trans = transaction_ds &&
          (poDS->StartTransaction() == OGRERR_NONE);
#endif

	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature, is_shpfile);

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
                delete poSRS;
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
                delete poSRS;
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
                delete poSRS;
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
#ifdef GDALV2
        if (start_trans && poDS->CommitTransaction() != OGRERR_NONE) {
          GDALClose( poDS );
          delete poSRS;
          uninstallErrorHandlerAndTriggerError();
          error("commit transaction failure");
        }
#endif

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
            delete poSRS;
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        SEXP Lns;
        int Lns_l;
        if (verbose) Rprintf("Writing %d wkbMultiLineString objects\n", nobs);
        installErrorHandler();
#ifdef GDALV2
        int start_trans = transaction_ds &&
          (poDS->StartTransaction() == OGRERR_NONE);
#endif

	for (i=0; i<nobs; i++) {

            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature, is_shpfile);

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
                    delete poSRS;
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
                delete poSRS;
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                delete poSRS;
                uninstallErrorHandlerAndTriggerError();
                error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
#ifdef GDALV2
        if (start_trans && poDS->CommitTransaction() != OGRERR_NONE) {
          GDALClose( poDS );
          delete poSRS;
          uninstallErrorHandlerAndTriggerError();
          error("commit transaction failure");
        }
#endif

        uninstallErrorHandlerAndTriggerError();

// Polygon data

    } else if (wkbtype == wkbPolygon) {

        SEXP pls = GET_SLOT(obj, install("polygons"));
        if (length(pls) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            delete poSRS;
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }
        if (verbose) Rprintf("Writing %d wkbPolygon objects\n", nobs);
        installErrorHandler();
#ifdef GDALV2
        int start_trans = transaction_ds &&
          (poDS->StartTransaction() == OGRERR_NONE);
#endif

	for (i=0; i<nobs; i++) {

             
            OGRFeature *poFeature;
            poFeature = new OGRFeature( poLayer->GetLayerDefn() );
// RSB 081009
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature, is_shpfile);

            SEXP crds, dim;

            OGRPolygon OGRply;

            PROTECT(comms = SP_PREFIX(comment2comm)(VECTOR_ELT(pls, i)));
            if (comms == R_NilValue) {
                crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(pls, i),
                    install("Polygons")), 0), install("coords"));
                dim = getAttrib(crds, install("dim"));
                int ncrds = INTEGER_POINTER(dim)[0];
                OGRLinearRing OGRlr;
                for (j=0; j<ncrds; j++) 
                    OGRlr.setPoint( j, NUMERIC_POINTER(crds)[j],
                                   NUMERIC_POINTER(crds)[j+ncrds] );
                OGRply.addRing( &OGRlr ); 
            } else {
                for (k=0; k<length(VECTOR_ELT(comms, 0)); k++) {
                    crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(pls, i),
                        install("Polygons")), INTEGER_POINTER(VECTOR_ELT(comms,
                        0))[k]-R_OFFSET), install("coords"));
                    dim = getAttrib(crds, install("dim"));
                    int ncrds = INTEGER_POINTER(dim)[0];
                    OGRLinearRing OGRlr;
                    for (j=0; j<ncrds; j++) 
                        OGRlr.setPoint( j, NUMERIC_POINTER(crds)[j],
                                   NUMERIC_POINTER(crds)[j+ncrds] );
                    OGRply.addRing( &OGRlr ); // first is Ering, others Iring(s)
                }
            }
            UNPROTECT(1);
            if( poFeature->SetGeometry( &OGRply ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
                delete poSRS;
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
                delete poSRS;
                error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
               installErrorHandler();
#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
               delete poSRS;
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        }
#ifdef GDALV2
        if (start_trans && poDS->CommitTransaction() != OGRERR_NONE) {
          GDALClose( poDS );
          delete poSRS;
          uninstallErrorHandlerAndTriggerError();
          error("commit transaction failure");
        }
#endif

        uninstallErrorHandlerAndTriggerError();

// Multi polygon data

    } else if (wkbtype == wkbMultiPolygon) {

        SEXP pls = GET_SLOT(obj, install("polygons"));
        if (length(pls) != nobs) {
            installErrorHandler();
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            delete poSRS;
            uninstallErrorHandlerAndTriggerError();
            error("number of objects mismatch");
        }

        if (verbose) Rprintf("Writing %d wkbMultiPolygon objects\n", nobs);
        installErrorHandler();
#ifdef GDALV2
        int start_trans = transaction_ds &&
          (poDS->StartTransaction() == OGRERR_NONE);
#endif

	for (i=0; i<nobs; i++) {
            OGRFeature *poFeature =  new OGRFeature( poLayer->GetLayerDefn() );
            PROTECT(comms = SP_PREFIX(comment2comm)(VECTOR_ELT(pls, i)));
            if (comms == R_NilValue) {
                error("Polygons object without ordering comment");
            } else {
                int nExtRings = length(comms);
                OGRMultiPolygon poRet = OGRMultiPolygon();
                SEXP PLSi = GET_SLOT(VECTOR_ELT(pls, i), install("Polygons"));
                for (int iER=0; iER<nExtRings; iER++) {
                    OGRPolygon OGRply = OGRPolygon();
                    int nthisiER = length(VECTOR_ELT(comms, iER));

                    for (k=0; k<nthisiER; k++) {
                        int thisk = INTEGER_POINTER(VECTOR_ELT(comms,
                            iER))[k]-R_OFFSET;
                        SEXP crds, dim;
                        crds = GET_SLOT(VECTOR_ELT(PLSi, thisk),
                            install("coords"));
                        dim = getAttrib(crds, install("dim"));
                        int ncrds = INTEGER_POINTER(dim)[0];
                        OGRLinearRing OGRlr;
                        for (j=0; j<ncrds; j++) 
                           OGRlr.setPoint( j, NUMERIC_POINTER(crds)[j],
                                   NUMERIC_POINTER(crds)[j+ncrds] );
                        OGRply.addRing( &OGRlr ); 
                    }
                    poRet.addGeometry(&OGRply);
                    OGRply.empty();
                }
                if( poFeature->SetGeometry( &poRet ) != OGRERR_NONE ) {
#ifdef GDALV2
                    GDALClose( poDS );
#else
                    OGRDataSource::DestroyDataSource( poDS );
#endif
                    delete poSRS;
                    uninstallErrorHandlerAndTriggerError();
                    error( "Failed to set geometry" );
                } 
            }
            UNPROTECT(1);
            uninstallErrorHandlerAndTriggerError();
            wrtDF(i, nf, fld_names, ldata, ogr_ftype, poFeature, is_shpfile);

// FIXME
#ifdef GDALV2
            if(poFeature->SetFID((GIntBig) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
                GDALClose( poDS );
#else
            if(poFeature->SetFID((long) INTEGER_POINTER(VECTOR_ELT(inp, 12))[i])  != OGRERR_NONE ) {
                OGRDataSource::DestroyDataSource( poDS );
#endif
               delete poSRS;
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to set FID" );
            } 

            if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {

#ifdef GDALV2
                GDALClose( poDS );
#else
                OGRDataSource::DestroyDataSource( poDS );
#endif
               delete poSRS;
               uninstallErrorHandlerAndTriggerError();
               error( "Failed to create feature" );
            } 

             OGRFeature::DestroyFeature( poFeature );
        } // i 

#ifdef GDALV2
        if (start_trans && poDS->CommitTransaction() != OGRERR_NONE) {
          GDALClose( poDS );
          delete poSRS;
          uninstallErrorHandlerAndTriggerError();
          error("commit transaction failure");
        }
#endif
    uninstallErrorHandlerAndTriggerError();
    } // multiPolygon 


    installErrorHandler();
#ifdef GDALV2
        GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
    delete poSRS;
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(pc);
    return(ans);
}

// RSB 081009
void wrtDF(int i, int nf, SEXP fld_names, SEXP ldata,
     SEXP ogr_ftype, OGRFeature* poFeature, int is_shpfile) {
     int j, OGR_type;
     for (j=0; j<nf; j++) {
         installErrorHandler();
         OGR_type = INTEGER_POINTER(ogr_ftype)[j];
         if (OGR_type == 2) {
             if (!ISNA(NUMERIC_POINTER(VECTOR_ELT(ldata, j))[i]))
               if (is_shpfile) {
                 poFeature->SetField(j,
                     NUMERIC_POINTER(VECTOR_ELT(ldata, j))[i] );
               } else {
                 poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                     NUMERIC_POINTER(VECTOR_ELT(ldata, j))[i] );
               }
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
             else poFeature->SetFieldNull(j);
#else
             else poFeature->UnsetField(j);
#endif
         } else if (OGR_type == 4) {
             if (STRING_ELT(VECTOR_ELT(ldata, j), i) != NA_STRING)
               if (is_shpfile) {
                 poFeature->SetField(j,
                     CHAR(STRING_ELT(VECTOR_ELT(ldata, j), i)) );
               } else {
                 poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                     CHAR(STRING_ELT(VECTOR_ELT(ldata, j), i)) );
               }
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
             else poFeature->SetFieldNull(j);
#else
             else poFeature->UnsetField(j);
#endif
         } else if (OGR_type == 0) {
              if (INTEGER_POINTER(VECTOR_ELT(ldata, j))[i] != NA_INTEGER)
               if (is_shpfile) {
                  poFeature->SetField(j,
                      INTEGER_POINTER(VECTOR_ELT(ldata, j))[i] );
               } else {
                  poFeature->SetField( CHAR(STRING_ELT(fld_names, j)),
                      INTEGER_POINTER(VECTOR_ELT(ldata, j))[i] );
               }
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
             else poFeature->SetFieldNull(j);
#else
             else poFeature->UnsetField(j);
#endif
         }
         uninstallErrorHandlerAndTriggerError();
     }         
}

/* FIXME return to this later
 * SEXP ogrOrganizeSpatialPolygons(SEXP obj) {
        SEXP ans;
        SEXP pls = GET_SLOT(obj, install("polygons"));
        SEXP Lns, Pls;
        int Lns_l, pls_l, i, k, pc=0;
        pls_l <- length(pls);
        PROTECT(ans = NEW_LIST(pls_l)); pc++;
        installErrorHandler();
	for (i=0; i<; i++) {
            Pls = VECTOR_ELT(pls, i);
            Lns = GET_SLOT(Pls, install("Polygons"));
            Lns_l = (int) length(Lns);
            if (Lns_l == 1) {
                
                SET_VECTOR_ELT(ans, i, Pls);
            } else {
                OGRGeometry* poRet = NULL;
                OGRPolygon** papoPolygons = new OGRPolygon*[ Lns_l ];
                for (k=0; k<Lns_l; k++) {
                    papoPolygons[k] = new OGRPolygon();
                    SEXP crds, dim;
                    crds = GET_SLOT(VECTOR_ELT(GET_SLOT(VECTOR_ELT(pls, i),
                        install("Polygons")), k), install("coords"));
                    dim = getAttrib(crds, install("dim"));
                    int ncrds = INTEGER_POINTER(dim)[0];

                    OGRLinearRing *OGRlr = new OGRLinearRing;

                    for (j=0; j<ncrds; j++) 
                        OGRlr->setPoint( j, NUMERIC_POINTER(crds)[j],
                                    NUMERIC_POINTER(crds)[j+ncrds] );

                    papoPolygons[k]->addRingDirectly(OGRlr);

                } // k
                int isValidGeometry;
                poRet = OGRGeometryFactory::organizePolygons(
                    (OGRGeometry**)papoPolygons, Lns_l, &isValidGeometry );
                if (!isValidGeometry) {
                    warning("OGR_write: uncommented multiring Polygons object %d conversion to SFS invalid", i+R_OFFSET);
                }

                delete[] papoPolygons;
            }
         }
         uninstallErrorHandlerAndTriggerError();

                

}*/

#ifdef __cplusplus
}
#endif

