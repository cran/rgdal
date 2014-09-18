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

    SEXP ans, vec1, vec2, vec3,/*mat,*/drv, dvec;
    SEXP itemlist, itemnames, itemwidth, itemtype, itemTypeNames;
    SEXP itemlistmaxcount;
    /*SEXP geotype;*/

    int nFIDs, nFields, iField, *nCount, pc=0;

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
      installErrorHandler();
      OGRDataSource::DestroyDataSource( poDS );
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      error("Cannot open file");
    }

    installErrorHandler();
    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));
    uninstallErrorHandlerAndTriggerError();

    if(poLayer == NULL){
      installErrorHandler();
      OGRDataSource::DestroyDataSource( poDS );
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      error("Cannot open layer");
    }

    installErrorHandler();
    nFIDs   = poLayer->GetFeatureCount();
    uninstallErrorHandlerAndTriggerError();

    // allocate a list for return values   
    PROTECT(ans=allocVector(VECSXP,6)); pc++;

    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    installErrorHandler();
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
    uninstallErrorHandlerAndTriggerError();
    SET_VECTOR_ELT(ans,3,drv);

    // store number of FIDs
    PROTECT(vec1=allocVector(INTSXP,1)); pc++;
    INTEGER(vec1)[0]=nFIDs;
    SET_VECTOR_ELT(ans,0,vec1);


    // store other stuff....
    installErrorHandler();
    poDefn = poLayer->GetLayerDefn();
    nFields =  poDefn->GetFieldCount();
    uninstallErrorHandlerAndTriggerError();

    // store number of fields
    PROTECT(vec2=allocVector(INTSXP,1)); pc++;
    INTEGER(vec2)[0]=nFields;
    SET_VECTOR_ELT(ans,1,vec2);
    installErrorHandler();
    OGREnvelope oExt;
    if (poLayer->GetExtent(&oExt, TRUE) == OGRERR_NONE) {
        PROTECT(dvec=allocVector(REALSXP,4)); pc++;
        REAL(dvec)[0] = oExt.MinX;
        REAL(dvec)[1] = oExt.MinY;
        REAL(dvec)[2] = oExt.MaxX;
        REAL(dvec)[3] = oExt.MaxY;
        SET_VECTOR_ELT(ans,4,dvec);
    }
    uninstallErrorHandlerAndTriggerError();
    
    PROTECT(itemnames=allocVector(STRSXP,nFields)); pc++;
    PROTECT(itemtype=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemwidth=allocVector(INTSXP,nFields)); pc++;
// try List types
    PROTECT(itemlistmaxcount=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemTypeNames=allocVector(STRSXP,nFields)); pc++;
    int listFieldCount=0;

    installErrorHandler();
    for(iField=0;iField<nFields;iField++){
      OGRFieldDefn *poField = poDefn->GetFieldDefn(iField);
      SET_STRING_ELT(itemnames,iField,mkChar(poField->GetNameRef()));
      INTEGER(itemtype)[iField]=poField->GetType();
      if (INTEGER(itemtype)[iField] == OFTIntegerList ||
         INTEGER(itemtype)[iField] == OFTRealList ||
         INTEGER(itemtype)[iField] == OFTStringList) listFieldCount++;
      INTEGER(itemwidth)[iField]=poField->GetWidth();
      SET_STRING_ELT(itemTypeNames,iField,mkChar(poField->GetFieldTypeName(
        poField->GetType())));
      INTEGER(itemlistmaxcount)[iField] = 0;
    }
    uninstallErrorHandlerAndTriggerError();

    PROTECT(vec3=allocVector(INTSXP,1)); pc++;
    INTEGER(vec3)[0]=listFieldCount;
    SET_VECTOR_ELT(ans,5,vec3);
    PROTECT(itemlist=allocVector(VECSXP,5)); pc++;
    SET_VECTOR_ELT(itemlist,0,itemnames);
    SET_VECTOR_ELT(itemlist,1,itemtype);
    SET_VECTOR_ELT(itemlist,2,itemwidth);
    SET_VECTOR_ELT(itemlist,3,itemTypeNames);
// try List types
    if (listFieldCount > 0) {

        poLayer->ResetReading();
        OGRFeature* poFeature;

        nCount = (int *) R_alloc((size_t) nFields, sizeof(int));
        for (iField=0; iField<nFields; iField++) nCount[iField] = 0;
        installErrorHandler();
        OGRField* psField;
        while( (poFeature = poLayer->GetNextFeature()) != NULL ){
            for(iField=0; iField<nFields; iField++){
                psField = poFeature->GetRawFieldRef(iField);
                if (INTEGER(itemtype)[iField] == OFTIntegerList) {
                    nCount[iField] = psField->IntegerList.nCount;
                    if (nCount[iField] > INTEGER(itemlistmaxcount)[iField])
                        INTEGER(itemlistmaxcount)[iField] = nCount[iField];
                } else if (INTEGER(itemtype)[iField] == OFTRealList) {
                    nCount[iField] = psField->RealList.nCount;
                    if (nCount[iField] > INTEGER(itemlistmaxcount)[iField])
                        INTEGER(itemlistmaxcount)[iField] = nCount[iField];
                } else if (INTEGER(itemtype)[iField] == OFTStringList) {
                    nCount[iField] = psField->StringList.nCount;
                    if (nCount[iField] > INTEGER(itemlistmaxcount)[iField])
                        INTEGER(itemlistmaxcount)[iField] = nCount[iField];
                }
            }
            OGRFeature::DestroyFeature( poFeature );
//    delete poFeature;
        }
        uninstallErrorHandlerAndTriggerError();

    }
    SET_VECTOR_ELT(itemlist,4,itemlistmaxcount);
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
    INTEGER(fids)[i]= (int) poFeature->GetFID();
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

  SEXP ogrReadListColumn(OGRLayer *poLayer, SEXP FIDs, int iField, int k){
    // read feature data and return something according to the type
    OGRFeatureDefn *poDefn;
    OGRFieldDefn *poField;
    OGRFeature *poFeature;
    int iRow,nRows,nlist;
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
    case OFTIntegerList:
      PROTECT(ans=allocVector(INTSXP,nRows));
      break;
    case OFTRealList:
      PROTECT(ans=allocVector(REALSXP,nRows));
      break;
    case OFTStringList:
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
    installErrorHandler();
    poLayer->ResetReading();
    OGRField* psField;
    iRow = 0;

    while((poFeature = poLayer->GetNextFeature()) != NULL) {
      if (poFeature->IsFieldSet(iField)) {

      // now get the value using the right type:
        psField = poFeature->GetRawFieldRef(iField);

        switch(poField->GetType()){
        case OFTIntegerList:
          nlist = psField->IntegerList.nCount;
	  if (k < nlist) 
            INTEGER(ans)[iRow] = psField->IntegerList.paList[k];
	  else INTEGER(ans)[iRow]=NA_INTEGER;
	  break;

        case OFTRealList:
          nlist = psField->RealList.nCount;
	  if (k < nlist) 
            REAL(ans)[iRow] = psField->RealList.paList[k];
	  else REAL(ans)[iRow]=NA_REAL;
	  break;

        case OFTStringList:
          nlist = psField->StringList.nCount;
	  if (k < nlist) 
            SET_STRING_ELT(ans,iRow,mkChar(psField->StringList.paList[k]));
	  else SET_STRING_ELT(ans, iRow, NA_STRING);
	  break;

        default:
          OGRFeature::DestroyFeature( poFeature );
//        delete poFeature;
          uninstallErrorHandlerAndTriggerError();
	  error("Unsupported field type. should have been caught before");
        }
      }
      OGRFeature::DestroyFeature( poFeature );
//      delete poFeature;
      iRow++;
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
    SEXP nListFields, ListFields;
    OGRLayer *poLayer;
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
    int iField, nflds=length(iFields), j=0, k;
    int pc=0;

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

    nListFields = getAttrib(iFields, mkString("nListFields"));

    // reserve a list for the result
    if (INTEGER_POINTER(nListFields)[0] == 0) {
        PROTECT(ans=allocVector(VECSXP,length(iFields))); pc++;
    } else {
        nflds = INTEGER_POINTER(getAttrib(iFields, mkString("nflds")))[0];
        PROTECT(ans=allocVector(VECSXP,nflds)); pc++;
        ListFields = getAttrib(iFields, mkString("ListFields"));
    }
    // now set each element of the list
    installErrorHandler();
    if (INTEGER_POINTER(nListFields)[0] == 0) {
      for(iField=0;iField<length(iFields);iField++){
        SET_VECTOR_ELT(ans,iField,ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField]));
      }
    } else {
        j=0;
        for(iField=0;iField<length(iFields);iField++){
            if (INTEGER_POINTER(ListFields)[iField] == 0) {
                SET_VECTOR_ELT(ans, j, 
                    ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField]));
                j++;
            } else {
                for (k=0; k < INTEGER_POINTER(ListFields)[iField]; k++) {
                    SET_VECTOR_ELT(ans, j, 
                        ogrReadListColumn(poLayer, FIDs,
                        INTEGER(iFields)[iField], k));
                    j++;
                }
            }
        }
    }
    uninstallErrorHandlerAndTriggerError();
    // clean up and return
    installErrorHandler();
    OGRDataSource::DestroyDataSource( poDS );
//    delete poDS;
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(pc);
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
/*  SEXP OGRFeatureInfo (SEXP ogrSource, SEXP Layer){ unused function
    SEXP ans=NULL;
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
  }*/


SEXP ogrCheckExists (SEXP ogrSource, SEXP Layer) {
    OGRLayer *poLayer;
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
    SEXP ans, drv;
    int pc=0;
    PROTECT(ans=NEW_LOGICAL(1)); pc++;

    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource, 0)), 
	FALSE, &poDriver);
    uninstallErrorHandlerAndTriggerError();

    if (poDS==NULL){
      installErrorHandler();
      OGRDataSource::DestroyDataSource( poDS );
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      LOGICAL_POINTER(ans)[0] = FALSE;
      UNPROTECT(pc);
      return(ans);
    }

    installErrorHandler();
    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));
    uninstallErrorHandlerAndTriggerError();

    if (poLayer == NULL){
      installErrorHandler();
      OGRDataSource::DestroyDataSource( poDS );
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      LOGICAL_POINTER(ans)[0] = FALSE;
      UNPROTECT(pc);
      return(ans);
    }

    LOGICAL_POINTER(ans)[0] = TRUE;
    
    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    installErrorHandler();
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
    uninstallErrorHandlerAndTriggerError();
    setAttrib(ans, install("driver"), drv);

    installErrorHandler();
    OGRDataSource::DestroyDataSource( poDS );
    uninstallErrorHandlerAndTriggerError();
//    delete poDS;
    UNPROTECT(pc);
    return(ans);
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
    SEXP ans, debug;
    int pc=0;

    installErrorHandler();
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource, 0)), 
	FALSE, &poDriver);
    uninstallErrorHandlerAndTriggerError();

    if(poDS==NULL){
      error("Cannot open data source");
    }

    debug = getAttrib(ogrSource, mkString("debug"));
    installErrorHandler();
    nlayers = poDS->GetLayerCount();
    uninstallErrorHandlerAndTriggerError();
    if (LOGICAL_POINTER(debug)[0] == TRUE)
        Rprintf("ogrListLayers: nlayers %d\n", nlayers);

    PROTECT(ans=NEW_CHARACTER(nlayers+1)); pc++;

    for (i=0; i<nlayers; i++) {
        installErrorHandler();
        poLayer = poDS->GetLayer(i);

        if(poLayer == NULL){
            if (LOGICAL_POINTER(debug)[0] == TRUE) {
                SET_STRING_ELT(ans, i, mkChar(""));
                Rprintf("ogrListLayers: NULL layer %d\n", i);
            } else {
                uninstallErrorHandlerAndTriggerError();
                error("Cannot open layer");
            }
        } else {
            SET_STRING_ELT(ans, i, mkChar(poLayer->GetLayerDefn()->GetName()));
        }
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

