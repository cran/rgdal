// Copyright 2003 (c) Barry Rowlingson
// Modified 2006 Roger Bivand
// OGR data source interface
//
//  note: the following objects must be deleted or memory leaks:
// OGRDataSource 
// OGRFeature
//
//  the following objects are owned by their parents, and so 
//  shouldnt be deleted (they get cleaned up when their parent is deleted)
// OGRLayer
// OGRGeometry
// OGRFeatureDefn
// OGRSpatialReference
// 
// see ogrsf_frmts.dox for authoritative notes!

#include "ogrsf_frmts.h"

// R headers moved outside extern "C" 070808 RSB re. note from BDR
//extern "C" {
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
//}
#include "rgdal.h"

#ifdef __cplusplus
extern "C" {
#endif

//#define EJP

// extern "C" {
  SEXP ogrInfo(SEXP ogrsourcename, SEXP Layer){
    // return FIDs, nFields, fieldInfo

    SEXP ans,vec,/*mat,*/drv;
    SEXP itemlist, itemnames, itemwidth, itemtype, itemTypeNames;
    /*SEXP geotype;*/

    int nFIDs, nFields, iField, pc=0;

    OGRDataSource *poDS;
    OGRLayer *poLayer;
    OGRFeatureDefn *poDefn;
  /*  OGRGeometry *poGeom;*/
    OGRSFDriver *poDriver;

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

    installErrorHandler();
    nFIDs   = poLayer->GetFeatureCount();
    uninstallErrorHandlerAndTriggerError();

    // allocate a list for return values   
    PROTECT(ans=allocVector(VECSXP,4)); pc++;

    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    installErrorHandler();
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
    uninstallErrorHandlerAndTriggerError();
    SET_VECTOR_ELT(ans,3,drv);

    // store number of FIDs
    PROTECT(vec=allocVector(INTSXP,1)); pc++;
    INTEGER(vec)[0]=nFIDs;
    SET_VECTOR_ELT(ans,0,vec);


    // store other stuff....
    installErrorHandler();
    poDefn = poLayer->GetLayerDefn();
    nFields =  poDefn->GetFieldCount();
    uninstallErrorHandlerAndTriggerError();

    // store number of fields
    PROTECT(vec=allocVector(INTSXP,1)); pc++;
    INTEGER(vec)[0]=nFields;
    SET_VECTOR_ELT(ans,1,vec);
    
    PROTECT(itemnames=allocVector(STRSXP,nFields)); pc++;
    PROTECT(itemtype=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemwidth=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemTypeNames=allocVector(STRSXP,nFields)); pc++;

    installErrorHandler();
    for(iField=0;iField<nFields;iField++){
      OGRFieldDefn *poField = poDefn->GetFieldDefn(iField);
      SET_STRING_ELT(itemnames,iField,mkChar(poField->GetNameRef()));
      INTEGER(itemtype)[iField]=poField->GetType();
      INTEGER(itemwidth)[iField]=poField->GetWidth();
      SET_STRING_ELT(itemTypeNames,iField,mkChar(poField->GetFieldTypeName(
        poField->GetType())));
    }
    uninstallErrorHandlerAndTriggerError();
    PROTECT(itemlist=allocVector(VECSXP,4)); pc++;
    SET_VECTOR_ELT(itemlist,0,itemnames);
    SET_VECTOR_ELT(itemlist,1,itemtype);
    SET_VECTOR_ELT(itemlist,2,itemwidth);
    SET_VECTOR_ELT(itemlist,3,itemTypeNames);
    SET_VECTOR_ELT(ans,2,itemlist);

    UNPROTECT(pc);

    OGRDataSource::DestroyDataSource( poDS );
//    delete poDS;
    return(ans);

  }
// }
#ifdef __cplusplus
}
#endif


#ifdef __cplusplus
extern "C" {
#endif
//extern "C" {
  SEXP ogrFIDs(SEXP filename, SEXP layer){
  SEXP fids, nf, ii;
  int /*layerNum,*/i;
  int nFeatures, pc=0;
  OGRDataSource *poDS;
  OGRLayer *poLayer;
  OGRFeature *poFeature;
  OGRSFDriver *poDriver;

  installErrorHandler();
  poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(filename, 0)), 
	FALSE, &poDriver);
  uninstallErrorHandlerAndTriggerError();

  if(poDS==NULL){
    error("Cannot open file");
  }

  installErrorHandler();
  poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(layer, 0)));
  uninstallErrorHandlerAndTriggerError();

  if(poLayer == NULL){
    error("Cannot open layer");
  }
  installErrorHandler();
  nFeatures=poLayer->GetFeatureCount();
  uninstallErrorHandlerAndTriggerError();

  PROTECT(fids=allocVector(INTSXP,nFeatures)); pc++;
  PROTECT(nf = NEW_INTEGER(1)); pc++;
  INTEGER_POINTER(nf)[0] = nFeatures;
  PROTECT(ii = NEW_INTEGER(1)); pc++;

  installErrorHandler();
  poLayer->ResetReading();
  uninstallErrorHandlerAndTriggerError();


  i=0;
  installErrorHandler();
  while( (poFeature = poLayer->GetNextFeature()) != NULL ){
    INTEGER(fids)[i]=poFeature->GetFID();
    i++;
    OGRFeature::DestroyFeature( poFeature );
//    delete poFeature;
  }
  uninstallErrorHandlerAndTriggerError();

  INTEGER_POINTER(ii)[0] = i;
  setAttrib(fids, install("nf"), nf);
  setAttrib(fids, install("i"), ii);

  OGRDataSource::DestroyDataSource( poDS );
//  delete poDS;

  UNPROTECT(pc);
  return(fids);

  }
// }
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
// extern "C" {

  SEXP ogrReadColumn(OGRLayer *poLayer, SEXP FIDs, int iField){
    // read feature data and return something according to the type
    OGRFeatureDefn *poDefn;
    OGRFieldDefn *poField;
    OGRFeature *poFeature;
    int iRow,nRows;
    SEXP ans = R_NilValue;

    nRows=length(FIDs);
    // get field data from layer
    installErrorHandler();
    poDefn = poLayer->GetLayerDefn();
    poField = poDefn->GetFieldDefn(iField);
    uninstallErrorHandlerAndTriggerError();
    if(poField == NULL){
      error("Error getting field %d ",iField);
    }
    // allocate an object for the result depending on the feature type:
    installErrorHandler();
    switch(poField->GetType()){
    case OFTInteger:
      PROTECT(ans=allocVector(INTSXP,nRows));
      break;
    case OFTReal:
      PROTECT(ans=allocVector(REALSXP,nRows));
      break;
    case OFTString:
      PROTECT(ans=allocVector(STRSXP,nRows));
      break;
      case OFTDate:
	PROTECT(ans=allocVector(STRSXP,nRows));
	break;
      case OFTDateTime:
	PROTECT(ans=allocVector(STRSXP,nRows));
	break;
      case OFTTime:
	PROTECT(ans=allocVector(STRSXP,nRows));
	break;
    default:
        const char *desc = poField->GetFieldTypeName(poField->GetType());
        uninstallErrorHandlerAndTriggerError();
        error("unsupported field type: %s", desc);
	break;
    }
    uninstallErrorHandlerAndTriggerError();

    // now go over each row and retrieve data. iRow is an index in a 
    // vector of FIDs
/*#ifndef EJP
    installErrorHandler();
    for(iRow=0;iRow<nRows;iRow++){
      poFeature=poLayer->GetFeature(INTEGER(FIDs)[iRow]);
      if(poFeature == NULL){
	error("Error getting feature FID: %d",(INTEGER(FIDs)[iRow]));
      }
    }
    uninstallErrorHandlerAndTriggerError();
#else*/
    // EJP, changed into:
    installErrorHandler();
    poLayer->ResetReading();
    iRow = 0;
    while((poFeature = poLayer->GetNextFeature()) != NULL) {
//#endif
      // now get the value using the right type:
      switch(poField->GetType()){
      case OFTInteger:
	if (poFeature->IsFieldSet(iField)) 
          INTEGER(ans)[iRow]=poFeature->GetFieldAsInteger(iField);
	else INTEGER(ans)[iRow]=NA_INTEGER;
	break;
      case OFTReal:
	if (poFeature->IsFieldSet(iField)) 
          REAL(ans)[iRow]=poFeature->GetFieldAsDouble(iField);
	else REAL(ans)[iRow]=NA_REAL;
	break;
      case OFTString:
	if (poFeature->IsFieldSet(iField)) 
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTDate:
	if (poFeature->IsFieldSet(iField)) 
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTDateTime:
	if (poFeature->IsFieldSet(iField)) 
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTTime:
	if (poFeature->IsFieldSet(iField)) 
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;

      default:
        OGRFeature::DestroyFeature( poFeature );
//        delete poFeature;
        uninstallErrorHandlerAndTriggerError();
	error("Unsupported field type. should have been caught before");
      }
      OGRFeature::DestroyFeature( poFeature );
//      delete poFeature;
//#ifdef EJP
      // according to tutorial: OGRFeature::DestroyFeature(poFeature);
      // see comment FW in OGR tutorial: We could just "delete" it, 
      // but this can cause problems in windows builds where the GDAL DLL 
      // has a different "heap" from the main program. To be on the safe 
      // side we use a GDAL function to delete the feature.
      iRow++;
//#endif
    }
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(1);
    return(ans);
  }
// }
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
// extern "C" {
  SEXP ogrDataFrame(SEXP ogrSource, SEXP Layer, SEXP FIDs, SEXP iFields){
    // query an OGR data source and return a list
    SEXP ans;
    OGRLayer *poLayer;
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
    int iField;
    // open the data source layer or error
    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource,0)), 
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

    // reserve a list for the result
    PROTECT(ans=allocVector(VECSXP,length(iFields)));
    // now set each element of the list
    installErrorHandler();
    for(iField=0;iField<length(iFields);iField++){
      SET_VECTOR_ELT(ans,iField,ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField]));
    }
    uninstallErrorHandlerAndTriggerError();
    // clean up and return
    installErrorHandler();
    OGRDataSource::DestroyDataSource( poDS );
//    delete poDS;
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(1);
    return(ans);
  }

// }
#ifdef __cplusplus
}
#endif


#ifdef __cplusplus
extern "C" {
#endif
// extern "C" {
  SEXP OGRFeatureInfo (SEXP ogrSource, SEXP Layer){
    SEXP ans;
    OGRLayer *poLayer;
    OGRDataSource *poDS;
    OGRFeatureDefn *poDefn;
    OGRSFDriver *poDriver;

    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource,0)), 
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
    installErrorHandler();
    poDefn = poLayer->GetLayerDefn();
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    for( int iAttr = 0; iAttr < poDefn->GetFieldCount(); iAttr++ )
      {
	OGRFieldDefn    *poField = poDefn->GetFieldDefn( iAttr );
	
	Rprintf( "%s: %s (%d.%d)\n",
		poField->GetNameRef(),
		poField->GetFieldTypeName( poField->GetType() ),
		poField->GetWidth(),
		poField->GetPrecision() );
      }
    uninstallErrorHandlerAndTriggerError();
    
    
//    PROTECT(ans=allocVector(INTSXP,1));
//    INTEGER(ans)[0]=999;
    
    installErrorHandler();
    OGRDataSource::DestroyDataSource( poDS );
//    delete poDS;
    uninstallErrorHandlerAndTriggerError();
//    UNPROTECT(1);
    return(R_NilValue);
  }

SEXP ogrDeleteLayer (SEXP ogrSource, SEXP Layer, SEXP ogrDriver) {
    OGRLayer *poLayer;
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
    int iLayer = -1;
    int flag = 0;

    installErrorHandler();
    poDriver = OGRSFDriverRegistrar::GetRegistrar()->GetDriverByName(
                CHAR(STRING_ELT(ogrDriver, 0)) );
    uninstallErrorHandlerAndTriggerError();
    if (poDriver == NULL) {
        error("Driver not available");
    }

    installErrorHandler();
    poDS = poDriver->Open(CHAR(STRING_ELT(ogrSource, 0)), 
	TRUE);
    uninstallErrorHandlerAndTriggerError();

    if (poDS==NULL)
        error("Cannot open data source for update");

    installErrorHandler();
    for(iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
        poLayer = poDS->GetLayer(iLayer);
/* poLayer->GetLayerDefn()->GetName() is poLayer->GetName() from 1.8 */
        if (poLayer != NULL && EQUAL(poLayer->GetLayerDefn()->GetName(),
            CHAR(STRING_ELT(Layer, 0)))) {
            flag = 1;
            break;
        }
    }
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    if (flag != 0) {
        if (poDS->DeleteLayer(iLayer) != OGRERR_NONE) {
            OGRDataSource::DestroyDataSource(poDS);
            uninstallErrorHandlerAndTriggerError();
            error("ogrDeleteLayer: failed to delete layer");
        }
    } else {
        warning("ogrDeleteLayer: no such layer");
    }
    OGRDataSource::DestroyDataSource(poDS);
    uninstallErrorHandlerAndTriggerError();
    return(R_NilValue);
}

SEXP ogrDeleteDataSource (SEXP ogrSource, SEXP ogrDriver) {
    OGRSFDriver *poDriver;

    installErrorHandler();
    poDriver = OGRSFDriverRegistrar::GetRegistrar()->GetDriverByName(
                CHAR(STRING_ELT(ogrDriver, 0)) );
    uninstallErrorHandlerAndTriggerError();
    if (poDriver == NULL) {
        error("Driver not available");
    }
    installErrorHandler();
    if (poDriver->TestCapability(ODrCDeleteDataSource)) {
        if (poDriver->DeleteDataSource(CHAR(STRING_ELT(ogrSource, 0)))
            != OGRERR_NONE) {
            uninstallErrorHandlerAndTriggerError();
            error("Data source could not be deleted");
        }
    } else {
        error("This driver not capable of data source deletion");
    }
    uninstallErrorHandlerAndTriggerError();
    return(R_NilValue);
}


SEXP ogrListLayers (SEXP ogrSource) {
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
    OGRLayer *poLayer;
    int i, nlayers;
    SEXP ans;
    int pc=0;

    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource, 0)), 
	FALSE, &poDriver);
    uninstallErrorHandlerAndTriggerError();

    if(poDS==NULL){
      error("Cannot open data source");
    }

    installErrorHandler();
    nlayers = poDS->GetLayerCount();
    uninstallErrorHandlerAndTriggerError();

    PROTECT(ans=NEW_CHARACTER(nlayers+1)); pc++;

    for (i=0; i<nlayers; i++) {
        installErrorHandler();
        poLayer = poDS->GetLayer(i);

        if(poLayer == NULL){
            uninstallErrorHandlerAndTriggerError();
            error("Cannot open layer");
        }
        SET_STRING_ELT(ans, i, mkChar(poLayer->GetLayerDefn()->GetName()));
        uninstallErrorHandlerAndTriggerError();
    }

    installErrorHandler();
    SET_STRING_ELT(ans, nlayers, mkChar(poDriver->GetName()));
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
    OGRDataSource::DestroyDataSource( poDS );
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(pc);
    return(ans);

}

//}


#ifdef __cplusplus
}
#endif

