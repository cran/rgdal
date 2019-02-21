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
  SEXP RGDAL_ogrInfo(SEXP ogrsourcename, SEXP Layer){
    // return FIDs, nFields, fieldInfo

    SEXP ans, vec1, vec2, vec3,/*mat,*/drv, dvec;
    SEXP itemlist, itemnames, itemwidth, itemtype, itemTypeNames;
    SEXP itemlistmaxcount;
#ifdef GDALV2
    SEXP dFIDs;
#endif
    /*SEXP geotype;*/

    int nFIDs, nFields, iField, *nCount, pc=0;

#ifdef GDALV2
    GDALDriver *poDriver;
    GDALDataset *poDS;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif
    OGRLayer *poLayer;
    OGRFeature *poFeature;
    OGRFeatureDefn *poDefn;
  /*  OGRGeometry *poGeom;*/

    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrsourcename, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
    if(poDS==NULL){
      uninstallErrorHandlerAndTriggerError();
      error("Cannot open data source");
    }
    poDriver = poDS->GetDriver();
#else
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrsourcename, 0)), 
	FALSE, &poDriver);
#endif
    uninstallErrorHandlerAndTriggerError();

    if(poDS==NULL){
      installErrorHandler();
#ifdef GDALV2
      GDALClose( poDS );
#else
      OGRDataSource::DestroyDataSource( poDS );
#endif
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      error("Cannot open file");
    }

    installErrorHandler();
    poLayer = poDS->GetLayerByName(CHAR(STRING_ELT(Layer, 0)));
    uninstallErrorHandlerAndTriggerError();

    if(poLayer == NULL){
      installErrorHandler();
#ifdef GDALV2
      GDALClose( poDS );
#else
      OGRDataSource::DestroyDataSource( poDS );
#endif
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      error("Cannot open layer");
    }

    // allocate a list for return values   
    PROTECT(ans=allocVector(VECSXP,6)); pc++;

    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    installErrorHandler();
#ifdef GDALV2
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetDescription()));
#else
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
#endif
    uninstallErrorHandlerAndTriggerError();
    SET_VECTOR_ELT(ans,3,drv);

    PROTECT(vec1=allocVector(INTSXP,1)); pc++;
    installErrorHandler();
#ifdef GDALV2
    GIntBig nFIDs64 = poLayer->GetFeatureCount();
    nFIDs = (nFIDs64 > INT_MAX) ? INT_MAX : 
        (nFIDs64 < INT_MIN) ? INT_MIN : (int) nFIDs64;
    if ((GIntBig) nFIDs != nFIDs64){
        warning("ogrInfo: feature count overflow");
        INTEGER(vec1)[0]=NA_INTEGER;      
        PROTECT(dFIDs=NEW_NUMERIC(1)); pc++;
        NUMERIC_POINTER(dFIDs)[0] = (double) nFIDs64;
        setAttrib(vec1, install("dFIDs"), dFIDs);
    } else {
    // store number of FIDs
        INTEGER(vec1)[0]=nFIDs;
    }
#else
    nFIDs   = poLayer->GetFeatureCount();
    // store number of FIDs
    INTEGER(vec1)[0]=nFIDs;
#endif
    uninstallErrorHandlerAndTriggerError();

    if (nFIDs == -1) {
      int i=0;
      installErrorHandler();
      while( ((poFeature = poLayer->GetNextFeature()) != NULL) && i <= INT_MAX){
        i++;
        OGRFeature::DestroyFeature( poFeature );
//    delete poFeature;
      }
      uninstallErrorHandlerAndTriggerError();
      if (i == INT_MAX) {
        error("ogrInfo: undeclared feature count overflow");
      } else {
        nFIDs = i;
        warning("ogrInfo: feature count not given; %d counted", nFIDs);
      }
      installErrorHandler();
      poLayer->ResetReading();
      uninstallErrorHandlerAndTriggerError();

      INTEGER(vec1)[0]=nFIDs;
    }

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
#ifdef GDALV2
         INTEGER(itemtype)[iField] == OFTInteger64List ||
#endif
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
#ifdef GDALV2
                } else if (INTEGER(itemtype)[iField] == OFTInteger64List) {
                    nCount[iField] = psField->StringList.nCount;
                    if (nCount[iField] > INTEGER(itemlistmaxcount)[iField])
                        INTEGER(itemlistmaxcount)[iField] = nCount[iField];
#endif
                }
            }
            OGRFeature::DestroyFeature( poFeature );
//    delete poFeature;
        }
        uninstallErrorHandlerAndTriggerError();

    }
    SET_VECTOR_ELT(itemlist,4,itemlistmaxcount);
    SET_VECTOR_ELT(ans,2,itemlist);


    installErrorHandler();
#ifdef GDALV2
    GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
    uninstallErrorHandlerAndTriggerError();
//    delete poDS;
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
//extern "C" {
  SEXP RGDAL_ogrFIDs(SEXP filename, SEXP layer){
  SEXP fids, nf, ii;
  int /*layerNum,*/i;
  int nFeatures, pc=0;
  OGRLayer *poLayer;
  OGRFeature *poFeature;
#ifdef GDALV2
    GDALDataset *poDS;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif

  installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(filename, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
#else
  poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(filename, 0)), 
	FALSE, &poDriver);
#endif
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
#ifdef GDALV2
  GIntBig nFIDs64 = poLayer->GetFeatureCount();
  nFeatures = (nFIDs64 > INT_MAX) ? INT_MAX : 
        (nFIDs64 < INT_MIN) ? INT_MIN : (int) nFIDs64;
  if ((GIntBig) nFeatures != nFIDs64){
        uninstallErrorHandlerAndTriggerError();
        error("ogrFIDs: feature count overflow");
  }
#else
  nFeatures=poLayer->GetFeatureCount();
#endif
  uninstallErrorHandlerAndTriggerError();

  if (nFeatures == -1) {
    i=0;
    installErrorHandler();
    while( ((poFeature = poLayer->GetNextFeature()) != NULL) && i <= INT_MAX){
      i++;
      OGRFeature::DestroyFeature( poFeature );
//    delete poFeature;
    }
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    poLayer->ResetReading();
    uninstallErrorHandlerAndTriggerError();
    if (i == INT_MAX) {
      error("ogrFIDs: feature count overflow");
    } else {
      nFeatures = i;
    }
  }

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

  installErrorHandler();
#ifdef GDALV2
  GDALClose( poDS );
#else
  OGRDataSource::DestroyDataSource( poDS );
#endif
  uninstallErrorHandlerAndTriggerError();
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

  SEXP ogrReadColumn(OGRLayer *poLayer, SEXP FIDs, int iField, int int64){
    // read feature data and return something according to the type
    OGRFeatureDefn *poDefn;
    OGRFieldDefn *poField;
    OGRFeature *poFeature;
    int iRow, nRows, warn_int64=0;
    double dbl_max_int64 = pow(2.0, 53);
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
#ifdef GDALV2
    case OFTInteger64:
      if (int64 == 4) {
          PROTECT(ans=allocVector(REALSXP,nRows));
      } else if (int64 ==3) {
          PROTECT(ans=allocVector(STRSXP,nRows));
      } else {
         PROTECT(ans=allocVector(INTSXP,nRows));
      }
      break;
#endif
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
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
          INTEGER(ans)[iRow]=poFeature->GetFieldAsInteger(iField);
	else INTEGER(ans)[iRow]=NA_INTEGER;
	break;
#ifdef GDALV2
      case OFTInteger64:
#if (GDAL_VERSION_MINOR >= 2 || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField)) {
#else
	if (poFeature->IsFieldSet(iField)) {
#endif
            if (int64 == 4) {
                    REAL(ans)[iRow] = poFeature->GetFieldAsDouble(iField);
                    if (REAL(ans)[iRow] > dbl_max_int64) warn_int64 = 1;
            } else if (int64 == 3) {
                SET_STRING_ELT(ans, iRow, 
                    mkChar(poFeature->GetFieldAsString(iField)));
            } else {
                GIntBig nVal64 = poFeature->GetFieldAsInteger64(iField);
                int nVal = (nVal64 > INT_MAX) ? INT_MAX : 
                    (nVal64 < INT_MIN) ? INT_MIN : (int) nVal64;
                INTEGER(ans)[iRow]=nVal;
                if (((GIntBig)nVal != nVal64) && int64 == 2) {
                    warning("Integer64 value clamped: feature %d", iRow);
                }
            }
        } else {
            if (int64 == 3) {
                SET_STRING_ELT(ans, iRow, NA_STRING);
            } else {
                INTEGER(ans)[iRow]=NA_INTEGER;
            }
        }
	break;
#endif
      case OFTReal:
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
          REAL(ans)[iRow]=poFeature->GetFieldAsDouble(iField);
	else REAL(ans)[iRow]=NA_REAL;
	break;
      case OFTString:
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTDate:
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTDateTime:
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
          SET_STRING_ELT(ans,iRow,mkChar(poFeature->GetFieldAsString(iField)));
	else SET_STRING_ELT(ans, iRow, NA_STRING);
	break;
      case OFTTime:
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField))
#else
	if (poFeature->IsFieldSet(iField)) 
#endif
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
    if (warn_int64 == 1) {
        warning("Integer64 values larger than %g lost significance after conversion to double", dbl_max_int64);
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

  SEXP ogrReadListColumn(OGRLayer *poLayer, SEXP FIDs, int iField, int k, int int64){
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
#ifdef GDALV2
    case OFTInteger64List:
      if (int64 == 4) {
          PROTECT(ans=allocVector(REALSXP,nRows));
      } else if (int64 == 3) {
          PROTECT(ans=allocVector(STRSXP,nRows));
      } else {
          PROTECT(ans=allocVector(INTSXP,nRows));
      }
      break;
#endif
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
    double DINT_MAX = 2251799813685248.0;
    double DINT_MIN = -2251799813685248.0;

    while((poFeature = poLayer->GetNextFeature()) != NULL) {
#if ((GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 2) || GDAL_VERSION_MAJOR > 2)
        if (poFeature->IsFieldSetAndNotNull(iField)) {
#else
	if (poFeature->IsFieldSet(iField)) {
#endif

      // now get the value using the right type:
        psField = poFeature->GetRawFieldRef(iField);

        switch(poField->GetType()){
        case OFTIntegerList:
          nlist = psField->IntegerList.nCount;
	  if (k < nlist) 
            INTEGER(ans)[iRow] = psField->IntegerList.paList[k];
	  else INTEGER(ans)[iRow]=NA_INTEGER;
	  break;
#ifdef GDALV2
        case OFTInteger64List:
          nlist = psField->Integer64List.nCount;
	  if (k < nlist) {
            GIntBig nVal64 = psField->Integer64List.paList[k];
            if (int64 == 4) {
                    REAL(ans)[iRow] = (double) nVal64;
            } else if (int64 == 3) {
                double dnval = (double) nVal64;
                if (dnval > DINT_MAX || dnval < DINT_MIN)
                    warning("Integer64 value clamped: feature %d", iRow);
                char szItem[32];
// CPL FORMAT assumes > ISO C++98
                snprintf(szItem, sizeof(szItem), "%.0f", dnval);
                SET_STRING_ELT(ans, iRow, mkChar(szItem));
            } else {
                int nVal = (nVal64 > INT_MAX) ? INT_MAX : 
                    (nVal64 < INT_MIN) ? INT_MIN : (int) nVal64;
                if (((GIntBig)nVal != nVal64) && int64 == 2) {
                    warning("Integer64 value clamped: feature %d", iRow);
                }
                INTEGER(ans)[iRow]=nVal;
            }
          } else {
            if (int64 == 3) {
                SET_STRING_ELT(ans, iRow, NA_STRING);
            } else {
                INTEGER(ans)[iRow]=NA_INTEGER;
            }
          }
	  break;
#endif

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
    SEXP nListFields, ListFields=R_NilValue, int64;
    OGRLayer *poLayer;
#ifdef GDALV2
    GDALDataset *poDS;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif

    int iField, nflds=length(iFields), j=0, k;
    int pc=0;

    // open the data source layer or error
    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrSource, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
#else
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource,0)), 
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

    PROTECT(int64 = getAttrib(iFields, install("int64"))); pc++;
    PROTECT(nListFields = getAttrib(iFields, install("nListFields"))); pc++;

    // reserve a list for the result
    if (INTEGER_POINTER(nListFields)[0] == 0) {
        PROTECT(ans=allocVector(VECSXP,length(iFields))); pc++;
    } else {
        nflds = INTEGER_POINTER(getAttrib(iFields, install("nflds")))[0];
        PROTECT(ans=allocVector(VECSXP,nflds)); pc++;
        PROTECT(ListFields = getAttrib(iFields, install("ListFields"))); pc++;
    }
    // now set each element of the list
    installErrorHandler();
    if (INTEGER_POINTER(nListFields)[0] == 0) {
      for(iField=0;iField<length(iFields);iField++){
        SET_VECTOR_ELT(ans,iField,ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField], INTEGER(int64)[0]));
      }
    } else {
        j=0;
        for(iField=0;iField<length(iFields);iField++){
            if (INTEGER_POINTER(ListFields)[iField] == 0) {
                SET_VECTOR_ELT(ans, j, 
                    ogrReadColumn(poLayer, FIDs, INTEGER(iFields)[iField], INTEGER(int64)[0]));
                j++;
            } else {
                for (k=0; k < INTEGER_POINTER(ListFields)[iField]; k++) {
                    SET_VECTOR_ELT(ans, j, 
                        ogrReadListColumn(poLayer, FIDs,
                        INTEGER(iFields)[iField], k, INTEGER(int64)[0]));
                    j++;
                }
            }
        }
    }
    uninstallErrorHandlerAndTriggerError();
    // clean up and return
    installErrorHandler();
#ifdef GDALV2
    GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
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
#ifdef GDALV2
    GDALDataset *poDS;
    GDALDriver *poDriver;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif
    SEXP ans, drv;
    int pc=0;
    PROTECT(ans=NEW_LOGICAL(1)); pc++;

    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrSource, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
    if (poDS != NULL) poDriver = poDS->GetDriver();
#else
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource, 0)), 
	FALSE, &poDriver);
#endif
    uninstallErrorHandlerAndTriggerError();

    if (poDS==NULL){
//      installErrorHandler();
//      OGRDataSource::DestroyDataSource( poDS );
//      uninstallErrorHandlerAndTriggerError();
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
#ifdef GDALV2
      GDALClose( poDS );
#else
      OGRDataSource::DestroyDataSource( poDS );
#endif
      uninstallErrorHandlerAndTriggerError();
//    delete poDS;
      LOGICAL_POINTER(ans)[0] = FALSE;
      UNPROTECT(pc);
      return(ans);
    }

    LOGICAL_POINTER(ans)[0] = TRUE;
    
    PROTECT(drv=allocVector(STRSXP,1)); pc++;
    installErrorHandler();
#ifdef GDALV2
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetDescription()));
#else
    SET_STRING_ELT(drv, 0, mkChar(poDriver->GetName()));
#endif
    uninstallErrorHandlerAndTriggerError();
    setAttrib(ans, install("driver"), drv);

    installErrorHandler();
#ifdef GDALV2
    GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
    uninstallErrorHandlerAndTriggerError();
//    delete poDS;
    UNPROTECT(pc);
    return(ans);
}


SEXP RGDAL_ogrDeleteLayer (SEXP ogrSource, SEXP Layer, SEXP ogrDriver) {
    OGRLayer *poLayer;
#ifdef GDALV2
    GDALDataset *poDS;
    GDALDriver *poDriver;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif
    int iLayer = -1;
    int flag = 0;

    installErrorHandler();
#ifdef GDALV2
    poDriver = GetGDALDriverManager()->GetDriverByName(CHAR(STRING_ELT(ogrDriver, 0)));
#else
    poDriver = OGRSFDriverRegistrar::GetRegistrar()->GetDriverByName(
                CHAR(STRING_ELT(ogrDriver, 0)) );
#endif
    uninstallErrorHandlerAndTriggerError();
    if (poDriver == NULL) {
        error("Driver not available");
    }

    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrSource, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
    if(poDS==NULL){
      error("Cannot open data source");
    }
    if (!EQUAL(CHAR(STRING_ELT(ogrDriver, 0)),
        poDS->GetDriver()->GetDescription())) {
        GDALClose( poDS );
        poDS = NULL;
    }
#else
    poDS = poDriver->Open(CHAR(STRING_ELT(ogrSource, 0)), 
	TRUE);
#endif
    uninstallErrorHandlerAndTriggerError();

    if (poDS==NULL)
        error("Cannot open data source for update");

    installErrorHandler();
    for(iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
        poLayer = poDS->GetLayer(iLayer);
#ifdef GDALV2
        if (poLayer != NULL && EQUAL(poLayer->GetName(),
            CHAR(STRING_ELT(Layer, 0)))) {
            flag = 1;
            break;
        }
#else
        if (poLayer != NULL && EQUAL(poLayer->GetLayerDefn()->GetName(),
            CHAR(STRING_ELT(Layer, 0)))) {
            flag = 1;
            break;
        }
#endif
    }
    uninstallErrorHandlerAndTriggerError();
    installErrorHandler();
    if (flag != 0) {
        int res = poDS->DeleteLayer(iLayer);
        if (res != OGRERR_NONE) {
#ifdef GDALV2
            GDALClose( poDS );
#else
            OGRDataSource::DestroyDataSource( poDS );
#endif
            uninstallErrorHandlerAndTriggerError();
            error("ogrDeleteLayer: failed to delete layer");
        }
    } else {
        warning("ogrDeleteLayer: no such layer");
    }
#ifdef GDALV2
    GDALClose( poDS );
#else
    OGRDataSource::DestroyDataSource( poDS );
#endif
    uninstallErrorHandlerAndTriggerError();
    return(R_NilValue);
}

SEXP RGDAL_ogrDeleteDataSource (SEXP ogrSource, SEXP ogrDriver) {
#ifdef GDALV2
    GDALDriver *poDriver;
#else
    OGRSFDriver *poDriver;
#endif

    installErrorHandler();
#ifdef GDALV2
    poDriver = GetGDALDriverManager()->GetDriverByName(CHAR(STRING_ELT(ogrDriver, 0)));
#else
    poDriver = OGRSFDriverRegistrar::GetRegistrar()->GetDriverByName(
                CHAR(STRING_ELT(ogrDriver, 0)) );
#endif
    uninstallErrorHandlerAndTriggerError();
    if (poDriver == NULL) {
        error("Driver not available");
    }
    installErrorHandler();
#ifdef GDALV2
    poDriver->Delete(CHAR(STRING_ELT(ogrSource, 0)));
#else
    if (poDriver->TestCapability(ODrCDeleteDataSource)) {
        if (poDriver->DeleteDataSource(CHAR(STRING_ELT(ogrSource, 0)))
            != OGRERR_NONE) {
            uninstallErrorHandlerAndTriggerError();
            error("Data source could not be deleted");
        }
    } else {
        uninstallErrorHandlerAndTriggerError();
        error("This driver is not capable of data source deletion");
    }
#endif
    uninstallErrorHandlerAndTriggerError();
    return(R_NilValue);
}


SEXP RGDAL_ogrListLayers (SEXP ogrSource) {
#ifdef GDALV2
    GDALDataset *poDS;
    GDALDriver *poDriver;
#else
    OGRDataSource *poDS;
    OGRSFDriver *poDriver;
#endif
    OGRLayer *poLayer;
    int i, nlayers;
    SEXP ans, debug;
    int pc=0;

    installErrorHandler();
#ifdef GDALV2
    poDS=(GDALDataset*) GDALOpenEx(CHAR(STRING_ELT(ogrSource, 0)), GDAL_OF_VECTOR, NULL, NULL, NULL);
    if(poDS==NULL){
      uninstallErrorHandlerAndTriggerError();
      error("Cannot open data source");
    }
    poDriver = poDS->GetDriver();
#else
    poDS=OGRSFDriverRegistrar::Open(CHAR(STRING_ELT(ogrSource, 0)), 
	FALSE, &poDriver);
#endif
    uninstallErrorHandlerAndTriggerError();

    if(poDS==NULL){
      error("Cannot open data source");
    }

    PROTECT(debug = getAttrib(ogrSource, install("debug"))); pc++;
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
#ifdef GDALV2
    SET_STRING_ELT(ans, nlayers, mkChar(poDriver->GetDescription()));
#else
    SET_STRING_ELT(ans, nlayers, mkChar(poDriver->GetName()));
#endif
    uninstallErrorHandlerAndTriggerError();

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

//}


#ifdef __cplusplus
}
#endif

