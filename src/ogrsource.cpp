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

#ifdef __cplusplus
extern "C" {
#endif

#define EJP

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

    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrsourcename, 0)), 
	FALSE, &poDriver);

    if(poDS==NULL){
      error("Cannot open file");
    }

    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));

    if(poLayer == NULL){
      error("Cannot open layer");
    }

    nFIDs   = poLayer->GetFeatureCount();

    // allocate a list for return values   
    PROTECT(ans=allocVector(VECSXP,4)); pc++;

    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
    SET_VECTOR_ELT(ans,3,drv);

    // store number of FIDs
    PROTECT(vec=allocVector(INTSXP,1)); pc++;
    INTEGER(vec)[0]=nFIDs;
    SET_VECTOR_ELT(ans,0,vec);


    // store other stuff....
    poDefn = poLayer->GetLayerDefn();
    nFields =  poDefn->GetFieldCount();

    // store number of fields
    PROTECT(vec=allocVector(INTSXP,1)); pc++;
    INTEGER(vec)[0]=nFields;
    SET_VECTOR_ELT(ans,1,vec);
    
    PROTECT(itemnames=allocVector(STRSXP,nFields)); pc++;
    PROTECT(itemtype=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemwidth=allocVector(INTSXP,nFields)); pc++;
    PROTECT(itemTypeNames=allocVector(STRSXP,nFields)); pc++;

    for(iField=0;iField<nFields;iField++){
      OGRFieldDefn *poField = poDefn->GetFieldDefn(iField);
      SET_STRING_ELT(itemnames,iField,mkChar(poField->GetNameRef()));
      INTEGER(itemtype)[iField]=poField->GetType();
      INTEGER(itemwidth)[iField]=poField->GetWidth();
      SET_STRING_ELT(itemTypeNames,iField,mkChar(poField->GetFieldTypeName(
        poField->GetType())));
    }
    PROTECT(itemlist=allocVector(VECSXP,4)); pc++;
    SET_VECTOR_ELT(itemlist,0,itemnames);
    SET_VECTOR_ELT(itemlist,1,itemtype);
    SET_VECTOR_ELT(itemlist,2,itemwidth);
    SET_VECTOR_ELT(itemlist,3,itemTypeNames);
    SET_VECTOR_ELT(ans,2,itemlist);

    UNPROTECT(pc);

    delete poDS;
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
  SEXP fids;
  int /*layerNum,*/i;
  int nFeatures;
  OGRDataSource *poDS;
  OGRLayer *poLayer;
  OGRFeature *poFeature;
  OGRSFDriver *poDriver;

  poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(filename, 0)), 
	FALSE, &poDriver);

  if(poDS==NULL){
    error("Cannot open file");
  }

  poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(layer, 0)));

  if(poLayer == NULL){
    error("Cannot open layer");
  }
  nFeatures=poLayer->GetFeatureCount();

  PROTECT(fids=allocVector(INTSXP,nFeatures));

  poLayer->ResetReading();

  i=0;
  while( (poFeature = poLayer->GetNextFeature()) != NULL ){
    INTEGER(fids)[i]=poFeature->GetFID();
    i++;
    delete poFeature;
  }

  delete poDS;

  UNPROTECT(1);
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
    poDefn = poLayer->GetLayerDefn();
    poField = poDefn->GetFieldDefn(iField);
    if(poField == NULL){
      error("Error getting field %d ",iField);
    }
    // allocate an object for the result depending on the feature type:
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
    default:
      error("unsupported field type: %s",poField->GetFieldTypeName(
        poField->GetType()));
    }

    // now go over each row and retrieve data. iRow is an index in a 
    // vector of FIDs
#ifndef EJP
    for(iRow=0;iRow<nRows;iRow++){
      poFeature=poLayer->GetFeature(INTEGER(FIDs)[iRow]);
      if(poFeature == NULL){
	error("Error getting feature FID: %d",(INTEGER(FIDs)[iRow]));
      }
    }
#else
    // EJP, changed into:
    poLayer->ResetReading();
    iRow = 0;
    while((poFeature = poLayer->GetNextFeature()) != NULL) {
#endif
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
      default:
        delete poFeature;
	error("Unsupported field type. should have been caught before");
      }
      delete poFeature;
#ifdef EJP
      // according to tutorial: OGRFeature::DestroyFeature(poFeature);
      // see comment FW in OGR tutorial: We could just "delete" it, 
      // but this can cause problems in windows builds where the GDAL DLL 
      // has a different "heap" from the main program. To be on the safe 
      // side we use a GDAL function to delete the feature.
      iRow++;
#endif
    }
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
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource,0)), 
      FALSE, &poDriver);

    if(poDS==NULL){
      error("Cannot open file");
    }

    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));

    if(poLayer == NULL){
      error("Cannot open layer");
    }

    // reserve a list for the result
    PROTECT(ans=allocVector(VECSXP,length(iFields)));
    // now set each element of the list
    for(iField=0;iField<length(iFields);iField++){
      SET_VECTOR_ELT(ans,iField,ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField]));
    }
    // clean up and return
    delete poDS;
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

    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource,0)), 
	FALSE, &poDriver);

    if(poDS==NULL){
      error("Cannot open file");
    }

    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));

    if(poLayer == NULL){
      error("Cannot open layer");
    }
    poDefn = poLayer->GetLayerDefn();
    for( int iAttr = 0; iAttr < poDefn->GetFieldCount(); iAttr++ )
      {
	OGRFieldDefn    *poField = poDefn->GetFieldDefn( iAttr );
	
	printf( "%s: %s (%d.%d)\n",
		poField->GetNameRef(),
		poField->GetFieldTypeName( poField->GetType() ),
		poField->GetWidth(),
		poField->GetPrecision() );
      }
    
    
    PROTECT(ans=allocVector(INTSXP,1));
    INTEGER(ans)[0]=999;
    
    delete poDS;
    UNPROTECT(1);
    return(ans);
  }

//}


#ifdef __cplusplus
}
#endif

