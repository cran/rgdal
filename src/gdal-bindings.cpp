#include <gdal_priv.h>
#include <gdal_alg.h>
#include <gdal_rat.h>
#include <cpl_string.h>
#include <cpl_csv.h>
#include <ogr_spatialref.h>
#include <ogrsf_frmts.h>
#include <cpl_error.h>

#include <gdal_version.h>

// R headers moved outside extern "C" 070808 RSB re. note from BDR
// #ifdef __cplusplus
// extern "C" {
// #endif

/*#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>*/
#include "rgdal.h"

#ifdef __cplusplus
extern "C" {
#endif

#if R_XDR_INTEGER_SIZE == 1
#define GDAL_INTEGER_TYPE GDT_Byte
#elif R_XDR_INTEGER_SIZE == 2
#define GDAL_INTEGER_TYPE GDT_Int16
#elif R_XDR_INTEGER_SIZE == 4
#define GDAL_INTEGER_TYPE GDT_Int32
#endif

#if R_XDR_DOUBLE_SIZE == 4
#define GDAL_FLOAT_TYPE GDT_Float32
#define GDAL_COMPLEX_TYPE GDT_CFloat32
#elif R_XDR_DOUBLE_SIZE == 8
#define GDAL_FLOAT_TYPE GDT_Float64
#define GDAL_COMPLEX_TYPE GDT_CFloat64
#endif

static CPLErr saved_eErrClass = CE_None;
static int saved_err_no = 0;
static char saved_error_msg[2048];

static SEXP
mkString_safe(const char *string) {

  if (string == NULL) return(R_NilValue);

  return(mkString(string));

}

//static char* RSB 070604
static const char*
asString(SEXP sxpString, const int i = 0) {

  if (isNull(sxpString)) return NULL;

  return(CHAR(STRING_ELT(sxpString, i)));

}

static SEXP
getObjHandle(SEXP sxpObj) {

  SEXP sxpHandle;
  PROTECT(sxpHandle = getAttrib(sxpObj, install("handle")));

  if (isNull(sxpHandle)) error("Null object handle\n");
  UNPROTECT(1);
  return(sxpHandle);

}

static void*
getGDALObjPtr(SEXP sxpObj) {

  SEXP sxpHandle;
  PROTECT(sxpHandle = getObjHandle(sxpObj));

  void *extPtr = R_ExternalPtrAddr(sxpHandle);

  if (extPtr == NULL) error("Null external pointer\n");
  UNPROTECT(1);
  return(extPtr);

}

SEXP isGDALObjPtrNULL(SEXP sxpObj) {

  SEXP sxpHandle, res;
  PROTECT(sxpHandle = getObjHandle(sxpObj));
  PROTECT(res = NEW_LOGICAL(1));
  LOGICAL_POINTER(res)[0] = FALSE;

  void *extPtr = R_ExternalPtrAddr(sxpHandle);

  if (extPtr == NULL) LOGICAL_POINTER(res)[0] = TRUE;

  UNPROTECT(2);

  return(res);

}

static GDALDriver*
getGDALDriverPtr(SEXP sxpDriver) {

  GDALDriver *pDriver = (GDALDriver *) getGDALObjPtr(sxpDriver);
  
  if (pDriver == NULL) error("Invalid GDAL driver\n");

  return (pDriver);

}

static GDALDataset*
getGDALDatasetPtr(SEXP sxpDataset) {

  GDALDataset *pDataset = (GDALDataset *) getGDALObjPtr(sxpDataset);
  
  if (pDataset == NULL) error("Invalid GDAL dataset handle\n");

  return(pDataset);

}

static GDALRasterBand*
getGDALRasterPtr(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand =
    (GDALRasterBand *) getGDALObjPtr(sxpRasterBand);

  if (pRasterBand == NULL) error("Invalid raster band\n");

  return(pRasterBand);

}

/*static void
__errorHandler(CPLErr eErrClass, int err_no, const char *msg) {

  if (eErrClass == CE_Warning) {

    warning("\n\tNon-fatal GDAL Error %d: %s\n", err_no, msg);

  } else {

    error("\n\tGDAL Error %d: %s\n", err_no, msg);

  }

  return;

}*/

/************************************************************************/
/*                       CPLDefaultErrorHandler()                       */
/* quoted from GDAL, pointed at REprintf()                              */
/************************************************************************/

/*void CPL_STDCALL R_CPLDefaultErrorHandler( CPLErr eErrClass, int nError, 
                             const char * pszErrorMsg )

{
    static int       nCount = 0;
    static int       nMaxErrors = -1;

    if (eErrClass != CE_Debug)
    {
        if( nMaxErrors == -1 )
        {
            nMaxErrors = 
                atoi(CPLGetConfigOption( "CPL_MAX_ERROR_REPORTS", "1000" ));
        }

        nCount++;
        if (nCount > nMaxErrors && nMaxErrors > 0 )
            return;
    }


    if( eErrClass == CE_Debug )
        REprintf("%s\n", pszErrorMsg );
    else if( eErrClass == CE_Warning )
        REprintf("CPL Warning %d: %s\n", nError, pszErrorMsg );
    else
        REprintf("CPL ERROR %d: %s\n", nError, pszErrorMsg );

    if (eErrClass != CE_Debug 
        && nMaxErrors > 0 
        && nCount == nMaxErrors )
    {
        REprintf( "More than %d errors or warnings have been reported. "
                 "No more will be reported from now.\n", 
                 nMaxErrors );
    }

}*/

static void
__errorHandler(CPLErr eErrClass, int err_no, const char *msg) {
        saved_eErrClass = eErrClass;
        saved_err_no = err_no;
/* a mutex could be usefull here to avoid a race condition if 2 threads 
trigger the error handler at the same time */
        strncpy(saved_error_msg, msg, sizeof(saved_error_msg));
        saved_error_msg[sizeof(saved_error_msg)-1] = 0;
}

void installErrorHandler()
{
   CPLPushErrorHandler(__errorHandler);
   saved_err_no = 0;
}

void uninstallErrorHandlerAndTriggerError()
{
    CPLPopErrorHandler();
    if (saved_err_no == CE_Warning) {

    warning("\n\tNon-fatal GDAL Error %d: %s\n", saved_err_no, 
saved_error_msg);

  } else if (saved_err_no == CE_Failure) {

    error("\n\tGDAL Error %d: %s\n", saved_err_no, saved_error_msg);

  }
}



SEXP
RGDAL_Init(void) {

//  CPLSetErrorHandler((CPLErrorHandler)__errorHandler);
//  CPLPushErrorHandler((CPLErrorHandler)__errorHandler);
#ifdef GDALV2

    installErrorHandler();
  GDALAllRegister();
    uninstallErrorHandlerAndTriggerError();

#else

    installErrorHandler();
  GDALAllRegister();
    uninstallErrorHandlerAndTriggerError();

    installErrorHandler();
  OGRRegisterAll();
    uninstallErrorHandlerAndTriggerError();

#endif
 
  return(R_NilValue);

}

SEXP
RGDAL_Exit(void) {

//  CPLPopErrorHandler();
// from sf CPL_gdal_cleanup_all()
  OGRCleanupAll();
  OSRCleanup();
  GDALDestroyDriverManager();

  return(R_NilValue);

}

SEXP
RGDAL_GDALVersionInfo(SEXP str) {
    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));

    installErrorHandler();
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GDALVersionInfo(asString(str))));
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(1);

    return(ans);
}

SEXP
RGDAL_GDALCheckVersion(void) {
    SEXP ans;

    PROTECT(ans=NEW_LOGICAL(1));

    installErrorHandler();
    LOGICAL_POINTER(ans)[0] = GDALCheckVersion(GDAL_VERSION_MAJOR,
        GDAL_VERSION_MINOR, NULL);
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(1);

    return(ans);
}


SEXP
RGDAL_GDAL_DATA_Info(void) {
    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    installErrorHandler();
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(CSVFilename( "stateplane.csv" )));
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(1);

    return(ans);
}

SEXP
RGDAL_GDALwithGEOS(void) {
    SEXP ans;

//    int withGEOS;

#if GDAL_VERSION_MAJOR >= 3 // New GDAL

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GDALVersionInfo("BUILD_INFO")));

#else // Old GDAL

    PROTECT(ans=NEW_LOGICAL(1));

    CPLPushErrorHandler(CPLQuietErrorHandler);
    saved_err_no = 0;

    OGRGeometry *poGeometry1, *poGeometry2;
    char* pszWKT;
    pszWKT = (char*) "POINT (10 20)";
#if GDAL_VERSION_MAJOR == 1 || ( GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR <= 2 ) // thanks to Even Roualt https://github.com/OSGeo/gdal/issues/681
//#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
    OGRGeometryFactory::createFromWkt( &pszWKT, NULL, &poGeometry1 );
#else // l 339
    OGRGeometryFactory::createFromWkt( (const char*) pszWKT, NULL, &poGeometry1 );
#endif // l 339
    pszWKT = (char*) "POINT (30 20)";
#if GDAL_VERSION_MAJOR == 1 || ( GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR <= 2 ) // thanks to Even Roualt https://github.com/OSGeo/gdal/issues/681
//#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
    OGRGeometryFactory::createFromWkt( &pszWKT, NULL, &poGeometry2 );
#else // l 346
    OGRGeometryFactory::createFromWkt( (const char*) pszWKT, NULL, &poGeometry2 );
#endif // l 346
    int withGEOS = 1;
    if (poGeometry1->Union(poGeometry2) == NULL) withGEOS = 0;//FIXME VG
    OGRGeometryFactory::destroyGeometry(poGeometry1);
    OGRGeometryFactory::destroyGeometry(poGeometry2);

    CPLPopErrorHandler();
    saved_err_no = 0;
    LOGICAL_POINTER(ans)[0] = withGEOS;

#endif // New GDAL

    UNPROTECT(1);

    return(ans);
}

SEXP
RGDAL_NullHandle(void) {

  return(R_MakeExternalPtr(NULL,
			   install("Null handle"),
			   R_NilValue));
  
}

SEXP
RGDAL_GetDescription(SEXP sxpObj) {

  void *pGDALObj = getGDALObjPtr(sxpObj);

  installErrorHandler();
  const char *desc = ((GDALMajorObject *)pGDALObj)->GetDescription();
  uninstallErrorHandlerAndTriggerError();

  return(mkString_safe(desc));

}


SEXP
RGDAL_GetDriverNames(void) {

#ifdef GDALV2
  SEXP ans, ansnames, vattr, rattr;
#else
  SEXP ans, ansnames;
#endif
  int pc=0;
  installErrorHandler();
  int nDr=GDALGetDriverCount();
  uninstallErrorHandlerAndTriggerError();

  PROTECT(ans = NEW_LIST(4)); pc++;
  PROTECT(ansnames = NEW_CHARACTER(4)); pc++;
  SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
  SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("long_name"));
  SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("create"));
  SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("copy"));
  setAttrib(ans, R_NamesSymbol, ansnames);
//  PROTECT(sxpDriverList = allocVector(STRSXP, GDALGetDriverCount()));
  SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(nDr));
  SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(nDr));
  SET_VECTOR_ELT(ans, 2, NEW_LOGICAL(nDr));
  SET_VECTOR_ELT(ans, 3, NEW_LOGICAL(nDr));
#ifdef GDALV2
  PROTECT(vattr = NEW_LOGICAL(nDr)); pc++;
  PROTECT(rattr = NEW_LOGICAL(nDr)); pc++;
#endif


  int i, flag;
  installErrorHandler();
  for (i = 0; i < nDr; ++i) {
#ifdef GDALV2
    LOGICAL_POINTER(vattr)[i] = FALSE;
    LOGICAL_POINTER(rattr)[i] = FALSE;
#endif

    GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
#ifdef GDALV2
    if(pDriver->GetMetadataItem(GDAL_DCAP_VECTOR) != NULL)
      LOGICAL_POINTER(vattr)[i] = TRUE;
    if(pDriver->GetMetadataItem(GDAL_DCAP_RASTER) != NULL)
      LOGICAL_POINTER(rattr)[i] = TRUE;
#endif
    
    SET_STRING_ELT(VECTOR_ELT(ans, 0), i, 
      mkChar(GDALGetDriverShortName( pDriver )));
    SET_STRING_ELT(VECTOR_ELT(ans, 1), i, 
      mkChar(GDALGetDriverLongName( pDriver )));
    flag=0;
    if (GDALGetMetadataItem( pDriver, GDAL_DCAP_CREATE, NULL )) flag=1;
    LOGICAL_POINTER(VECTOR_ELT(ans, 2))[i] = flag;
    flag=0;
    if (GDALGetMetadataItem( pDriver, GDAL_DCAP_CREATECOPY, NULL )) flag=1;
    LOGICAL_POINTER(VECTOR_ELT(ans, 3))[i] = flag;
  }
  uninstallErrorHandlerAndTriggerError();
#ifdef GDALV2
  setAttrib(ans, install("isVector"), vattr);
  setAttrib(ans, install("isRaster"), rattr);
#endif

  UNPROTECT(pc);

  return(ans);

}

SEXP
RGDAL_GetDriver(SEXP sxpDriverName) {

  const char *pDriverName = asString(sxpDriverName);

  installErrorHandler();
  GDALDriver *pDriver = (GDALDriver *) GDALGetDriverByName(pDriverName);
  uninstallErrorHandlerAndTriggerError();

  if (pDriver == NULL)
    error("No driver registered with name: %s\n", pDriverName);
  
  SEXP sxpHandle = R_MakeExternalPtr((void *) pDriver,
				     install("GDAL Driver"),
				     R_NilValue);

  return(sxpHandle);

}

static void
deleteFile(GDALDriver *pDriver, const char *filename) {


#ifdef RGDALDEBUG
  Rprintf("Deleting temp file: %s... ", filename);
//  fflush(stderr);
#endif

  installErrorHandler();
  if (strcmp(GDALGetDriverLongName( pDriver ), "In Memory Raster") != 0) {
//      CPLErr eErr = pDriver->Delete(filename);
    GDALDeleteDataset((GDALDriverH) pDriver, filename);
/*    if (eErr == CE_Failure)
      warning("Failed to delete dataset: %s\n", filename);*/
  }
  uninstallErrorHandlerAndTriggerError();

#ifdef RGDALDEBUG
  Rprintf("done.\n", filename);
//  fflush(stderr);
#endif

  return;

}

SEXP
RGDAL_DeleteFile(SEXP sxpDriver, SEXP sxpFileName) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  const char *filename = asString(sxpFileName);

//  GDALDeleteDataset((GDALDriverH) pDriver, filename);

  deleteFile(pDriver, filename);

  return(R_NilValue);

}

SEXP
RGDAL_CloseHandle(SEXP sxpHandle) {

  GDALDataset *pDataset =
    (GDALDataset *) R_ExternalPtrAddr(sxpHandle);

#ifdef RGDALDEBUG
  Rprintf("Closing GDAL dataset handle %p... ", (void *) pDataset);
#endif

  installErrorHandler();
  if (pDataset != NULL) {

// Even Roualt 120816
      GDALClose((GDALDatasetH)pDataset);
//    pDataset->~GDALDataset();

    R_ClearExternalPtr(sxpHandle);

#ifdef RGDALDEBUG
    Rprintf(" destroyed ... ");
#endif

  }
  uninstallErrorHandlerAndTriggerError();

#ifdef RGDALDEBUG
  Rprintf("done.\n");
#endif

  return(R_NilValue);

}

SEXP
RGDAL_DeleteHandle(SEXP sxpHandle) {

  GDALDataset *pDataset =
    (GDALDataset *) R_ExternalPtrAddr(sxpHandle);

  if (pDataset == NULL) return(R_NilValue);

  installErrorHandler();

  GDALDriver *pDriver = pDataset->GetDriver();
// 131202 ASAN fix
  const char *desc = GDALGetDriverShortName( pDriver );
//Rprintf("Driver short name %s\n", desc);
  GDALDriver *pDriver1 = (GDALDriver *) GDALGetDriverByName(desc);

  char *filename = strdup(pDataset->GetDescription());
//Rprintf("file: %s\n", filename);

// 131105 Even Roualt idea
  GDALClose((GDALDatasetH) pDataset);
//Rprintf("after GDALClose\n");
  GDALDeleteDataset((GDALDriverH) pDriver1, filename);
//Rprintf("after GDALDeleteDataset\n");
  free(filename);

/* #ifndef OSGEO4W
  deleteFile(pDriver, filename);
#endif */

  R_ClearExternalPtr(sxpHandle);
//  RGDAL_CloseHandle(sxpHandle);

  uninstallErrorHandlerAndTriggerError();
  return(R_NilValue);

}

SEXP
RGDAL_CloseDataset(SEXP sxpDataset) {


  SEXP sxpHandle;
  PROTECT(sxpHandle = getObjHandle(sxpDataset));

  if (sxpHandle == NULL) {
    UNPROTECT(1);
    return(R_NilValue);
  }

  const char *classname = asString(getAttrib(sxpDataset, R_ClassSymbol));

  if (strcmp(classname, "GDALTransientDataset") == 0) {
    
    RGDAL_DeleteHandle(sxpHandle);

  } else {

    RGDAL_CloseHandle(sxpHandle);
  }
  UNPROTECT(1);

  return(R_NilValue);

}

// FIXME rchk Function RGDAL_CloseDataset
//  [PB] has possible protection stack imbalance rgdal/src/gdal-bindings.cpp:608
SEXP
RGDAL_CreateDataset(SEXP sxpDriver, SEXP sDim, SEXP sType,
		    SEXP sOpts, SEXP sFile) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);
  GDALDataset *pDataset;
  const char *filename = asString(sFile);
  int i/*, n*/;
  char **papszCreateOptions = NULL;

#ifdef RGDALDEBUG
  Rprintf("Opening dataset: %s\n", filename);
//  fflush(stderr);
#endif

  if (filename == NULL) error("Invalid file name\n");

  GDALDataType eGDALType = (GDALDataType) asInteger(sType);

  installErrorHandler();
  for (i=0; i < length(sOpts); i++) papszCreateOptions = CSLAddString( 
    papszCreateOptions, CHAR(STRING_ELT(sOpts, i)) );
#ifdef RGDALDEBUG
  for (i=0; i < CSLCount(papszCreateOptions); i++)
    Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptions, i));
#endif
  uninstallErrorHandlerAndTriggerError();
  installErrorHandler();
  pDataset = pDriver->Create(filename,
			  INTEGER(sDim)[0],
			  INTEGER(sDim)[1],
			  INTEGER(sDim)[2],
			  eGDALType, papszCreateOptions);
  uninstallErrorHandlerAndTriggerError();
  installErrorHandler();
  CSLDestroy(papszCreateOptions);
  uninstallErrorHandlerAndTriggerError();

  if (pDataset == NULL) error("Unable to create dataset\n");

  installErrorHandler();
  pDataset->SetDescription(filename);
  uninstallErrorHandlerAndTriggerError();

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDataset,
				     install("GDAL Dataset"),
				     R_NilValue);

  return(sxpHandle);

}

SEXP
RGDAL_OpenDataset(SEXP filename, SEXP read_only, SEXP silent, SEXP allowedDr, SEXP sOpts) {

  const char *fn = asString(filename);

#ifdef GDALV2
  int i;
  char **papszOpenOptions = NULL;
  char **papszAllowedDrivers = NULL;
  installErrorHandler();
  for (i=0; i < length(sOpts); i++) papszOpenOptions = CSLAddString( 
    papszOpenOptions, CHAR(STRING_ELT(sOpts, i)) );
  for (i=0; i < CSLCount(papszOpenOptions); i++)
    Rprintf("option %d: %s\n", i, CSLGetField(papszOpenOptions, i));
  uninstallErrorHandlerAndTriggerError();
  installErrorHandler();
  for (i=0; i < length(allowedDr); i++) papszAllowedDrivers = CSLAddString( 
    papszAllowedDrivers, CHAR(STRING_ELT(allowedDr, i)) );
  for (i=0; i < CSLCount(papszAllowedDrivers); i++)
    Rprintf("driver %d: %s\n", i, CSLGetField(papszAllowedDrivers, i));
  uninstallErrorHandlerAndTriggerError();
#endif

#ifdef GDALV2
  unsigned int RWFlag;
  if (asLogical(read_only))
    RWFlag = GDAL_OF_RASTER | GDAL_OF_READONLY;
  else
    RWFlag = GDAL_OF_RASTER | GDAL_OF_UPDATE;
#else
  GDALAccess RWFlag;
  if (asLogical(read_only))
    RWFlag = GA_ReadOnly;
  else
    RWFlag = GA_Update;
#endif

/* Modification suggested by Even Rouault, 2009-08-08: */

  CPLErrorReset();
  if (asLogical(silent))
    CPLPushErrorHandler(CPLQuietErrorHandler);
  else
     installErrorHandler();

#ifdef GDALV2
  GDALDataset *pDataset = (GDALDataset *) GDALOpenEx(fn, RWFlag,
    papszAllowedDrivers, papszOpenOptions, NULL);
#else
  GDALDataset *pDataset = (GDALDataset *) GDALOpen(fn, RWFlag);
#endif


  if (pDataset == NULL)
    error("%s\n", CPLGetLastErrorMsg());

  if (asLogical(silent))
    CPLPopErrorHandler();
  else
    uninstallErrorHandlerAndTriggerError();

#ifdef GDALV2
  installErrorHandler();
  CSLDestroy(papszOpenOptions);
  CSLDestroy(papszAllowedDrivers);
  uninstallErrorHandlerAndTriggerError();
#endif

/* Similarly to SWIG bindings, the following lines will cause
RGDAL_OpenDataset() to fail on - uncleared - errors even if pDataset is not
NULL. They could also be just removed. While pDataset != NULL, there's some
hope ;-) */

/*  CPLErr eclass = CPLGetLastErrorType();

  if (pDataset != NULL && eclass == CE_Failure) {
    GDALClose(pDataset);
    pDataset = NULL;
    __errorHandler(eclass, CPLGetLastErrorNo(), CPLGetLastErrorMsg());
  }*/


  SEXP sxpHandle = R_MakeExternalPtr((void *) pDataset,
				     install("GDAL Dataset"),
				     R_NilValue);

  return(sxpHandle);

}

SEXP
RGDAL_CopyDataset(SEXP sxpDataset, SEXP sxpDriver,
		  SEXP sxpStrict,  SEXP sxpOpts,
		  SEXP sxpFile) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);
  char **papszCreateOptions = NULL;
  int i;

  const char *filename = asString(sxpFile);

  if (filename == NULL) error("Invalid filename\n");

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  installErrorHandler();
  for (i=0; i < length(sxpOpts); i++) papszCreateOptions = CSLAddString( 
    papszCreateOptions, CHAR(STRING_ELT(sxpOpts, i)) );
  uninstallErrorHandlerAndTriggerError();
#ifdef RGDALDEBUG
  installErrorHandler();
  for (i=0; i < CSLCount(papszCreateOptions); i++)
    Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptions, i));
  uninstallErrorHandlerAndTriggerError();
#endif
  installErrorHandler();
  GDALDataset *pDatasetCopy = pDriver->CreateCopy(filename,
		pDataset, asInteger(sxpStrict),
		papszCreateOptions, NULL, NULL);
  uninstallErrorHandlerAndTriggerError();

  if (pDatasetCopy == NULL) error("Dataset copy failed\n");

  installErrorHandler();
  CSLDestroy(papszCreateOptions);
  uninstallErrorHandlerAndTriggerError();

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDatasetCopy,
				     install("GDAL Dataset"),
				     R_NilValue);


  return(sxpHandle);

}

SEXP
RGDAL_GetRasterXSize(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  int res;

  installErrorHandler();
  res = pDataset->GetRasterXSize();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarInteger(res));

}

SEXP
RGDAL_GetRasterYSize(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  int res;

  installErrorHandler();
  res = pDataset->GetRasterYSize();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarInteger(res));

}

SEXP
RGDAL_GetRasterCount(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  int res;

  installErrorHandler();
  res = pDataset->GetRasterCount();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarInteger(res));

}

SEXP
RGDAL_GetProjectionRef3(SEXP sDataset, SEXP enforce_xy);

SEXP
RGDAL_GetProjectionRef3(SEXP sDataset, SEXP enforce_xy) {

#if GDAL_VERSION_MAJOR > 2

  SEXP ans, Datum, ToWGS84, Ellps;
  int i, pc=0;
  const char *datum, *towgs84, *ellps;
  OGRSpatialReference *oSRS;
  int vis_order;

  if (enforce_xy == R_NilValue) vis_order = 0;
  else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
  else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
  else vis_order = 0;

  installErrorHandler();
  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);
  oSRS = (OGRSpatialReference*) pDataset->GetSpatialRef();
  uninstallErrorHandlerAndTriggerError();

  if (oSRS != NULL) {
    installErrorHandler();
    if (vis_order == 1) 
    oSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
    uninstallErrorHandlerAndTriggerError();
  }

  PROTECT(ans = NEW_CHARACTER(1)); pc++;

  if (oSRS != NULL) {

    installErrorHandler();
    datum = oSRS->GetAttrValue("DATUM");
    uninstallErrorHandlerAndTriggerError();
    PROTECT(Datum = NEW_CHARACTER(1)); pc++;
    if (datum != NULL) {
      SET_STRING_ELT(Datum, 0, COPY_TO_USER_STRING(datum));
      setAttrib(ans, install("datum"), Datum);
    }

    installErrorHandler();
    ellps = oSRS->GetAttrValue("DATUM|SPHEROID");
    uninstallErrorHandlerAndTriggerError();
    PROTECT(Ellps = NEW_CHARACTER(1)); pc++;
    if (ellps != NULL) {
      SET_STRING_ELT(Ellps, 0, COPY_TO_USER_STRING(ellps));
      setAttrib(ans, install("ellps"), Ellps);
    }

    PROTECT(ToWGS84 = NEW_CHARACTER(7)); pc++;
    installErrorHandler();
    for (i=0; i<7; i++) {
      towgs84 = oSRS->GetAttrValue("TOWGS84", i);
      if (towgs84 != NULL) SET_STRING_ELT(ToWGS84, i,
        COPY_TO_USER_STRING(towgs84));
    }
    setAttrib(ans, install("towgs84"), ToWGS84);
    uninstallErrorHandlerAndTriggerError();

    SEXP WKT2_2018;
    char *wkt2=NULL;
    PROTECT(WKT2_2018 = NEW_CHARACTER(1)); pc++;
    const char* papszOptions[] = { "FORMAT=WKT2_2018", "MULTILINE=YES", nullptr };
    installErrorHandler();
    if (oSRS->exportToWkt(&wkt2, papszOptions) != OGRERR_NONE) {
      SET_STRING_ELT(WKT2_2018, 0, NA_STRING);
    } else {
      SET_STRING_ELT(WKT2_2018, 0, COPY_TO_USER_STRING(wkt2));
      CPLFree( wkt2 );
    }
    uninstallErrorHandlerAndTriggerError();
    setAttrib(ans, install("WKT2_2018"), WKT2_2018);

    installErrorHandler();
    char *pszSRS_P4 = NULL;
    if (oSRS->exportToProj4( &pszSRS_P4 ) != OGRERR_NONE) {
      SET_STRING_ELT(ans, 0, NA_STRING);
    } else {
      SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_P4));
      CPLFree( pszSRS_P4 );
    }
    uninstallErrorHandlerAndTriggerError();
  } else SET_STRING_ELT(ans, 0, NA_STRING);


  UNPROTECT(pc);
  return(ans);

#else

  return(R_NilValue);

#endif

}

/* changed to return proj4 string 20060212 RSB */
SEXP
RGDAL_GetProjectionRef(SEXP sDataset, SEXP enforce_xy) {

  if (GDAL_VERSION_MAJOR >= 3) {
    return(RGDAL_GetProjectionRef3(sDataset, enforce_xy));
  }

  OGRSpatialReference *oSRS = new OGRSpatialReference;
  char *pszSRS_WKT = NULL;
  SEXP ans;
  int pc=0;
  
  installErrorHandler();

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  pszSRS_WKT = (char*) pDataset->GetProjectionRef();
  uninstallErrorHandlerAndTriggerError();

  PROTECT(ans = NEW_CHARACTER(1)); pc++;

  if (strlen(pszSRS_WKT) == 0) {
    SET_STRING_ELT(ans, 0, NA_STRING);
    UNPROTECT(pc); 
    return(ans);
  }

  installErrorHandler();
#if GDAL_VERSION_MAJOR == 1 || ( GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR <= 2 ) // https://github.com/OSGeo/gdal/issues/681
//#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2 
  oSRS->importFromWkt( &pszSRS_WKT );
#else
  oSRS->importFromWkt( (const char*) pszSRS_WKT );
#endif
  uninstallErrorHandlerAndTriggerError();

  if (oSRS != NULL) {
    installErrorHandler();
    if (oSRS->exportToProj4( &pszSRS_WKT ) != OGRERR_NONE) {
      SET_STRING_ELT(ans, 0, NA_STRING);
    } else {
      SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_WKT));
      CPLFree( pszSRS_WKT );
    }
    uninstallErrorHandlerAndTriggerError();
  } else SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(""));


//  delete oSRS;

  UNPROTECT(pc);
  return(ans);

}

SEXP
RGDAL_GetMetadata(SEXP sDataset, SEXP tag) {

    char **papszMetadata;
    SEXP ans;
    int i, n, pc=0;

    GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  installErrorHandler();
    if (tag == R_NilValue) {
        papszMetadata = pDataset->GetMetadata( NULL );
    } else {
        papszMetadata = pDataset->GetMetadata(CHAR(STRING_ELT(tag, 0)));
    }
  uninstallErrorHandlerAndTriggerError();

    if (CSLCount(papszMetadata) == 0) return(R_NilValue);

    for (n=0; papszMetadata[n] != NULL; n++);
    PROTECT(ans = NEW_CHARACTER(n)); pc++;
    for (i=0; i<n; i++)
        SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(papszMetadata[i]));

    UNPROTECT(pc);
    return(ans);
}





SEXP
RGDAL_GetDatasetDriver(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  installErrorHandler();
  GDALDriver *pDriver = pDataset->GetDriver();
  uninstallErrorHandlerAndTriggerError();

  SEXP sxpDriver = R_MakeExternalPtr((void *) pDriver,
				     install("GDAL Dataset"),
				     R_NilValue);

  return(sxpDriver);

}

SEXP
RGDAL_GetDriverShortName(SEXP sxpDriver) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  installErrorHandler();
  const char *desc = GDALGetDriverShortName( pDriver );
  uninstallErrorHandlerAndTriggerError();
  return(mkString_safe(desc));

}

SEXP
RGDAL_GetDriverLongName(SEXP sxpDriver) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);
  installErrorHandler();
  const char *desc = GDALGetDriverLongName( pDriver );
  uninstallErrorHandlerAndTriggerError();
  return(mkString_safe(desc));
}

SEXP
RGDAL_GetRasterBand(SEXP sDataset, SEXP sBand) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  int band = asInteger(sBand);

  installErrorHandler();
  GDALRasterBand *pRasterBand = pDataset->GetRasterBand(band);
  uninstallErrorHandlerAndTriggerError();

  SEXP rpRasterBand = R_MakeExternalPtr((void *) pRasterBand,
					install("GDAL Raster Band"),
					R_NilValue);
  return(rpRasterBand);

}

SEXP
RGDAL_GetXSize(SEXP sRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sRasterBand);

  int res;

  installErrorHandler();
  res = pRasterBand->GetXSize();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarInteger(res));

}

SEXP
RGDAL_GetYSize(SEXP sRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sRasterBand);

  int res;

  installErrorHandler();
  res = pRasterBand->GetYSize();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarInteger(res));

}

SEXP
RGDAL_GetRasterBlockSize(SEXP rasterObj) {
	
	 GDALRasterBand *raster = getGDALRasterPtr(rasterObj);
	 
	 SEXP blockSize;
         PROTECT(blockSize = allocVector(INTSXP, 2));
	 
  installErrorHandler();
	 raster->GetBlockSize(INTEGER(blockSize) + 1, INTEGER(blockSize));
  uninstallErrorHandlerAndTriggerError();
	 UNPROTECT(1);
	 return(blockSize);
	 
}

SEXP
RGDAL_GetAccess(SEXP sxpDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);
  int res;

  installErrorHandler();
  res = pDataset->GetAccess() == GA_ReadOnly;
  uninstallErrorHandlerAndTriggerError();
  return(ScalarLogical(res));

}

SEXP
RGDAL_GetRasterAccess(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  int res;

  installErrorHandler();
  res = pRasterBand->GetAccess() == GA_ReadOnly;
  uninstallErrorHandlerAndTriggerError();
  return(ScalarLogical(res));

}

SEXP
RGDAL_GetNoDataValue(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  int hasNoDataValue;

  installErrorHandler();
  double noDataValue = pRasterBand->GetNoDataValue(&hasNoDataValue);
  uninstallErrorHandlerAndTriggerError();

  return(hasNoDataValue ? ScalarReal(noDataValue) : R_NilValue);

}

SEXP
RGDAL_GetOffset(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  double res;

  installErrorHandler();
  res = pRasterBand->GetOffset();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarReal(res));

}

SEXP
RGDAL_GetScale(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  double res;

  installErrorHandler();
  res = pRasterBand->GetScale();
  uninstallErrorHandlerAndTriggerError();
  return(ScalarReal(res));

}

/* SEXP
RGDAL_GetBandMetadataItem(SEXP sxpRasterBand, SEXP sxpItem, SEXP sxpDomain) {
 item: PIXELTYPE, domain: IMAGE_STRUCTURE
 http://trac.osgeo.org/gdal/wiki/rfc14_imagestructure

  SEXP ans;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  PROTECT(ans = NEW_CHARACTER(1));

  SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pRasterBand->GetMetadataItem(
    CHAR(STRING_ELT(sxpItem, 0)), CHAR(STRING_ELT(sxpDomain, 0)))));

  UNPROTECT(1);
  return(ans);
} */


SEXP
RGDAL_GetRAT(SEXP sxpRasterBand) {

  SEXP ans, GFT_type, GFT_usage, nc_names;

  int nc, nr, i, j, ival, np=0;
  double val;
  GDALRATFieldType *nc_types;
  GDALRATFieldUsage *nc_usages;
  const char *GFU_type_string[] = {"GFT_Integer",
                                   "GFT_Real",
                                   "GFT_String"};
  const char *GFU_usage_string[] = {"GFU_Generic",
                                    "GFU_PixelCount",
                                    "GFU_Name",
                                    "GFU_Min",
                                    "GFU_Max",
                                    "GFU_MinMax",
                                    "GFU_Red",
                                    "GFU_Green",
                                    "GFU_Blue",
                                    "GFU_Alpha",
                                    "GFU_RedMin",
                                    "GFU_GreenMin",
                                    "GFU_BlueMin",
                                    "GFU_AlphaMin",
                                    "GFU_RedMax",
                                    "GFU_GreenMax",
                                    "GFU_BlueMax",
                                    "GFU_AlphaMax",
                                    "GFU_MaxCount"};

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  const GDALRasterAttributeTable *pRAT = pRasterBand->GetDefaultRAT();
  uninstallErrorHandlerAndTriggerError();

  if (pRAT == NULL) return(R_NilValue);

  installErrorHandler();
  nc = (int) pRAT->GetColumnCount();
  uninstallErrorHandlerAndTriggerError();
  PROTECT(ans = NEW_LIST(nc));np++;
  PROTECT(nc_names = NEW_CHARACTER(nc));np++;
  nc_types = (GDALRATFieldType *) R_alloc((size_t) nc,
    sizeof(GDALRATFieldType));
  nc_usages = (GDALRATFieldUsage *) R_alloc((size_t) nc,
    sizeof(GDALRATFieldUsage));
  installErrorHandler();
  nr = (int) pRAT->GetRowCount();
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  for (i=0; i<nc; i++) {
    nc_types[i] = pRAT->GetTypeOfCol(i);
    nc_usages[i] = pRAT->GetUsageOfCol(i);
    SET_STRING_ELT(nc_names, i, COPY_TO_USER_STRING(pRAT->GetNameOfCol(i)));
    if (nc_types[i] == GFT_Integer) {
      SET_VECTOR_ELT(ans, i, NEW_INTEGER(nr));
    } else if (nc_types[i] == GFT_Real) {
      SET_VECTOR_ELT(ans, i, NEW_NUMERIC(nr));
    } else if (nc_types[i] == GFT_String) {
      SET_VECTOR_ELT(ans, i, NEW_CHARACTER(nr));
    } else {
      error("unknown column type");
    }
  }
  uninstallErrorHandlerAndTriggerError();
  installErrorHandler();
  for (i=0; i<nc; i++) {

    if (nc_types[i] == GFT_Integer) {

      for (j=0; j<nr; j++) {
        ival = (int) pRAT->GetValueAsInt(j, i);
        INTEGER_POINTER(VECTOR_ELT(ans, i))[j] = ival;
      }
      
    } else if (nc_types[i] == GFT_Real) {

      for (j=0; j<nr; j++) {
        val = (double) pRAT->GetValueAsDouble(j, i);
        NUMERIC_POINTER(VECTOR_ELT(ans, i))[j] = val;
      }
      
    } else if (nc_types[i] == GFT_String) {

      for (j=0; j<nr; j++) {
        SET_STRING_ELT(VECTOR_ELT(ans, i), j,
          COPY_TO_USER_STRING(pRAT->GetValueAsString(j, i)));
      }
      
    }     

  }
  uninstallErrorHandlerAndTriggerError();
  PROTECT(GFT_type = NEW_CHARACTER(nc));np++;
  PROTECT(GFT_usage = NEW_CHARACTER(nc));np++;

  for (i=0; i<nc; i++) {
    SET_STRING_ELT(GFT_type, i,
      COPY_TO_USER_STRING(GFU_type_string[nc_types[i]]));
    SET_STRING_ELT(GFT_usage, i,
      COPY_TO_USER_STRING(GFU_usage_string[nc_usages[i]]));
  }

  setAttrib(ans, install("GFT_type"), GFT_type);
  setAttrib(ans, install("GFT_usage"), GFT_usage);
  setAttrib(ans, R_NamesSymbol, nc_names);
  
  UNPROTECT(np);
  return(ans);
}

SEXP
RGDAL_GetBandMinimum(SEXP sxpRasterBand) {

  SEXP ans;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  PROTECT(ans = NEW_NUMERIC(1));

  installErrorHandler();
  NUMERIC_POINTER(ans)[0] = (double) pRasterBand->GetMinimum();
  uninstallErrorHandlerAndTriggerError();

  UNPROTECT(1);
  return(ans);
}

SEXP
RGDAL_GetBandMaximum(SEXP sxpRasterBand) {

  SEXP ans;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  PROTECT(ans = NEW_NUMERIC(1));

  installErrorHandler();
  NUMERIC_POINTER(ans)[0] = (double) pRasterBand->GetMaximum();
  uninstallErrorHandlerAndTriggerError();

  UNPROTECT(1);
  return(ans);
}

SEXP
RGDAL_GetBandStatistics(SEXP sxpRasterBand, SEXP silent) {

  CPLErr err;

  SEXP ans;

  double min, max, mean, sd;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  err = pRasterBand->GetStatistics(FALSE, FALSE, &min, &max, &mean, &sd);

  if (err == CE_Failure) {
	if (!LOGICAL_POINTER(silent)[0])
            warning("statistics not supported by this driver");
  uninstallErrorHandlerAndTriggerError();
        return(R_NilValue);
  }

  if (err == CE_Warning) {
	if (!LOGICAL_POINTER(silent)[0])
    	    warning("statistics not supported by this driver");
  uninstallErrorHandlerAndTriggerError();
        return(R_NilValue);
  }
  uninstallErrorHandlerAndTriggerError();

  PROTECT(ans = NEW_NUMERIC(4));
  NUMERIC_POINTER(ans)[0] = min;
  NUMERIC_POINTER(ans)[1] = max;
  NUMERIC_POINTER(ans)[2] = mean;
  NUMERIC_POINTER(ans)[3] = sd;

  UNPROTECT(1);
  return(ans);
}



SEXP
RGDAL_GetBandType(SEXP sxpRasterBand) {

  SEXP ans;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  PROTECT(ans = NEW_INTEGER(1));

  installErrorHandler();
  INTEGER_POINTER(ans)[0] = (int) pRasterBand->GetRasterDataType();
  uninstallErrorHandlerAndTriggerError();

  UNPROTECT(1);
  return(ans);
}


SEXP
RGDAL_PutRasterData(SEXP sxpRasterBand, SEXP sxpData, SEXP sxpOffset) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  int rowsIn = nrows(sxpData);
  int colsIn = ncols(sxpData);

  GDALDataType eGDALType = GDT_Int32;

  switch(pRasterBand->GetRasterDataType()) {

  case GDT_Byte:
  case GDT_UInt16:
  case GDT_Int16:
  case GDT_UInt32:
  case GDT_Int32:

    eGDALType = GDAL_INTEGER_TYPE;
    PROTECT(sxpData = coerceVector(sxpData, INTSXP));
  // Transpose data
// replication for 2.4.0 RSB 20060726
    installErrorHandler();
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)INTEGER(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure) {
      uninstallErrorHandlerAndTriggerError();
      error("Failure during raster IO\n");
    }
    uninstallErrorHandlerAndTriggerError();

    break;

  case GDT_Float32:
  case GDT_Float64:

    eGDALType = GDAL_FLOAT_TYPE;
    PROTECT(sxpData = coerceVector(sxpData, REALSXP));
  // Transpose data
    installErrorHandler();
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)REAL(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure) {
      uninstallErrorHandlerAndTriggerError();
      error("Failure during raster IO\n");
    }
    uninstallErrorHandlerAndTriggerError();

    break;

  case GDT_CInt16:
  case GDT_CInt32:
  case GDT_CFloat32:
  case GDT_CFloat64:

    eGDALType = GDAL_COMPLEX_TYPE;
    PROTECT(sxpData = coerceVector(sxpData, CPLXSXP));
  // Transpose data
    installErrorHandler();
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)COMPLEX(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure) {
      uninstallErrorHandlerAndTriggerError();
      error("Failure during raster IO\n");
    }
    uninstallErrorHandlerAndTriggerError();

    break;
    
  default:

    error("Raster data type unknown\n");
    
    break;

  }

  UNPROTECT(1);

  return(sxpRasterBand);

}

SEXP
RGDAL_GetBandNoDataValue(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  SEXP res;
  int hasNoDataValue;
  installErrorHandler();
  double noDataValue = pRasterBand->GetNoDataValue(&hasNoDataValue);
  uninstallErrorHandlerAndTriggerError();

  if (hasNoDataValue) {
    PROTECT(res = NEW_NUMERIC(1));
    NUMERIC_POINTER(res)[0] = noDataValue;
  } else {
    return(R_NilValue);
  }

  UNPROTECT(1);
  return(res);

}

SEXP
RGDAL_GetRasterData(SEXP sxpRasterBand,
		    SEXP sxpRegion,
		    SEXP sxpDimOut,
		    SEXP sxpInterleave) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  GDALDataType eGDALType = GDT_Int32;
  SEXPTYPE uRType = INTSXP;

  installErrorHandler();
  int RDT = pRasterBand->GetRasterDataType();
  uninstallErrorHandlerAndTriggerError();

  switch(RDT) {

  case GDT_Byte:
  case GDT_UInt16:
  case GDT_Int16:
  case GDT_UInt32:
  case GDT_Int32:

    uRType = INTSXP;
    eGDALType = GDAL_INTEGER_TYPE;

    break;

  case GDT_Float32:
  case GDT_Float64:

    uRType = REALSXP;
    eGDALType = GDAL_FLOAT_TYPE;

    break;

  case GDT_CInt16:
  case GDT_CInt32:
  case GDT_CFloat32:
  case GDT_CFloat64:

    uRType = CPLXSXP;
    eGDALType = GDAL_COMPLEX_TYPE;

    break;
    
  default:

    error("Raster data type unknown\n");
    
    break;

  }

  // Create matrix transposed
  int pc=0;
  SEXP sRStorage;
// Mathias Moser https://stat.ethz.ch/pipermail/r-sig-geo/2020-April/028072.html
  PROTECT(sRStorage = allocVector(uRType,
			       ((R_xlen_t) INTEGER(sxpDimOut)[1]) *
			       INTEGER(sxpDimOut)[0])); pc++;

// replication for 2.4.0 RSB 20060726
  switch(uRType) {

    case INTSXP:
      installErrorHandler();
      if(pRasterBand->RasterIO(GF_Read,
			   INTEGER(sxpRegion)[1],
			   INTEGER(sxpRegion)[0],
			   INTEGER(sxpRegion)[3],
			   INTEGER(sxpRegion)[2],
			   (void *)INTEGER(sRStorage),
			   INTEGER(sxpDimOut)[1],
			   INTEGER(sxpDimOut)[0],
			   eGDALType,
			   INTEGER(sxpInterleave)[0],
			   INTEGER(sxpInterleave)[1])
         == CE_Failure) {
           uninstallErrorHandlerAndTriggerError();
           error("Failure during raster IO\n");
      }
      uninstallErrorHandlerAndTriggerError();
      break;

    case REALSXP:

      installErrorHandler();
      if(pRasterBand->RasterIO(GF_Read,
			   INTEGER(sxpRegion)[1],
			   INTEGER(sxpRegion)[0],
			   INTEGER(sxpRegion)[3],
			   INTEGER(sxpRegion)[2],
			   (void *)REAL(sRStorage),
			   INTEGER(sxpDimOut)[1],
			   INTEGER(sxpDimOut)[0],
			   eGDALType,
			   INTEGER(sxpInterleave)[0],
			   INTEGER(sxpInterleave)[1])
         == CE_Failure) {
           uninstallErrorHandlerAndTriggerError();
           error("Failure during raster IO\n");
      }
      uninstallErrorHandlerAndTriggerError();
      break;

    case CPLXSXP:

      installErrorHandler();
      if(pRasterBand->RasterIO(GF_Read,
			   INTEGER(sxpRegion)[1],
			   INTEGER(sxpRegion)[0],
			   INTEGER(sxpRegion)[3],
			   INTEGER(sxpRegion)[2],
			   (void *)COMPLEX(sRStorage),
			   INTEGER(sxpDimOut)[1],
			   INTEGER(sxpDimOut)[0],
			   eGDALType,
			   INTEGER(sxpInterleave)[0],
			   INTEGER(sxpInterleave)[1])
         == CE_Failure) {
           uninstallErrorHandlerAndTriggerError();
           error("Failure during raster IO\n");
      }
      uninstallErrorHandlerAndTriggerError();
      break;

    default:

          error("Raster data type unknown\n");
    
      break;

  }

  int hasNoDataValue;

  double noDataValue;

  SEXP NDV = RGDAL_GetBandNoDataValue(sxpRasterBand);

  if (NDV == R_NilValue) {
    hasNoDataValue = FALSE;
  } else {
    hasNoDataValue = TRUE;
    noDataValue = NUMERIC_POINTER(NDV)[0];
  }

  /*int*/ R_xlen_t i;
        R_xlen_t sz = XLENGTH(sRStorage);

  if (hasNoDataValue) {

    switch(uRType) {

    case INTSXP:
    {
      int* pVals = INTEGER(sRStorage);
        for (i = 0; i < sz; ++i)
	if (pVals[i] == (int) noDataValue) {
	  pVals[i] = NA_INTEGER;
	}
    }

      break;

    case REALSXP:

      switch(pRasterBand->GetRasterDataType()) {

        case GDT_Float32:
        {
          double* pVals = REAL(sRStorage);

        for (i = 0; i < sz; ++i)
	  if (pVals[i] == (double) ((float) noDataValue)) {
	    pVals[i] = NA_REAL;
	  }
        }
	break;

        case GDT_Float64:
        {
          double* pVals = REAL(sRStorage);

        for (i = 0; i < sz; ++i)
	  if (pVals[i] == (double) (noDataValue)) {
	    pVals[i] = NA_REAL;
	  }
        }
	break;

        default:

          error("Raster data type unknown\n");
    
        break;

      }

      break;

    default:

      warning("Output data values = %f are invalid\n", noDataValue);

      break;

    }

  } else {
    installErrorHandler();
    if (uRType == REALSXP && pRasterBand->GetRasterDataType() == GDT_Float32) {
      double* pVals = REAL(sRStorage);
        for (i = 0; i < sz; ++i)
	  if (ISNAN(pVals[i])) {
	    pVals[i] = NA_REAL;
	  }
      
    }
    uninstallErrorHandlerAndTriggerError();
  }

  UNPROTECT(pc);
  return(sRStorage);

}

SEXP
RGDAL_GetPaletteInterp(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  GDALPaletteInterp ePI = 
    pRasterBand->GetColorTable()->GetPaletteInterpretation();
  uninstallErrorHandlerAndTriggerError();
  
  installErrorHandler();
  const char *desc = GDALGetPaletteInterpretationName(ePI);
  uninstallErrorHandlerAndTriggerError();
  return(mkString_safe(desc));

}

SEXP
RGDAL_GetColorInterp(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  GDALColorInterp eCI = pRasterBand->GetColorInterpretation();
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  const char *desc = GDALGetColorInterpretationName(eCI);
  uninstallErrorHandlerAndTriggerError();
  return(mkString_safe(desc));

}

static SEXP
GDALColorTable2Matrix(GDALColorTableH ctab) {

        installErrorHandler();
	int ncol = GDALGetColorEntryCount(ctab);
        uninstallErrorHandlerAndTriggerError();

	SEXP cmat;
        PROTECT(cmat = allocMatrix(INTSXP, ncol, 4));

        installErrorHandler();
	for (int i = 0; i < ncol; ++i) {

    	const GDALColorEntry* ce = GDALGetColorEntry(ctab, i);

    	INTEGER(cmat)[i] = static_cast<int>(ce->c1);
    	INTEGER(cmat)[i + ncol] = static_cast<int>(ce->c2);
    	INTEGER(cmat)[i + 2 * ncol] = static_cast<int>(ce->c3);
    	INTEGER(cmat)[i + 3 * ncol] = static_cast<int>(ce->c4);

  	}
        uninstallErrorHandlerAndTriggerError();
        UNPROTECT(1);

  	return(cmat);
	
}

SEXP
RGDAL_GetColorTable(SEXP rasterObj) {

	GDALRasterBandH rasterBand = getGDALRasterPtr(rasterObj);

        installErrorHandler();
	GDALColorTableH ctab = GDALGetRasterColorTable(rasterBand);
        uninstallErrorHandlerAndTriggerError();

	if (ctab == NULL) return(R_NilValue);

	return(GDALColorTable2Matrix(ctab));

}


SEXP
RGDAL_SetCategoryNames(SEXP sxpRasterBand, SEXP sxpNames) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  char **nameList = NULL;

  int i;
  installErrorHandler();
  for (i = 0; i < length(sxpNames); ++i)
    nameList = CSLAddString(nameList, asString(sxpNames, i));
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  CPLErr err = pRasterBand->SetCategoryNames(nameList);

  if (err == CE_Failure) warning("Failed to set category names");
  CSLDestroy(nameList);
  uninstallErrorHandlerAndTriggerError();

  return(sxpRasterBand);

}

SEXP
RGDAL_GetCategoryNames(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  char **pcCNames = pRasterBand->GetCategoryNames();
  uninstallErrorHandlerAndTriggerError();

  if (pcCNames == NULL) return(R_NilValue);

  installErrorHandler();
  pcCNames = CSLDuplicate(pcCNames);
  uninstallErrorHandlerAndTriggerError();

  SEXP sxpCNames;

  installErrorHandler();
  int ii = CSLCount(pcCNames);
  uninstallErrorHandlerAndTriggerError();
  PROTECT(sxpCNames = allocVector(STRSXP, ii));

  int i;
  installErrorHandler();
  for (i = 0; i < ii; ++i) {

    const char *field = CSLGetField(pcCNames, i);

    SET_STRING_ELT(sxpCNames, i, mkChar(field));

  }
  CSLDestroy(pcCNames);
  uninstallErrorHandlerAndTriggerError();

  UNPROTECT(1);
  
  return(sxpCNames);

}

SEXP
RGDAL_GetGeoTransform(SEXP sxpDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  SEXP sxpGeoTrans, ceFail;
  PROTECT(sxpGeoTrans = allocVector(REALSXP, 6));
  PROTECT(ceFail = NEW_LOGICAL(1));
  LOGICAL_POINTER(ceFail)[0] = FALSE;

  installErrorHandler();
  CPLErr err = pDataset->GetGeoTransform(REAL(sxpGeoTrans));

  if (err == CE_Failure) {

    REAL(sxpGeoTrans)[0] = 0; // x-origin ul
    REAL(sxpGeoTrans)[1] = 1; // x-resolution (pixel width)
    REAL(sxpGeoTrans)[2] = 0; // x-oblique
    REAL(sxpGeoTrans)[3] = (double) pDataset->GetRasterYSize();
 // y-origin ul; 091028
    REAL(sxpGeoTrans)[4] = 0; // y-oblique
    REAL(sxpGeoTrans)[5] = -1; // y-resolution (pixel height); 091028 added sign
    LOGICAL_POINTER(ceFail)[0] = TRUE;

  }
  setAttrib(sxpGeoTrans, install("CE_Failure"), ceFail);
  uninstallErrorHandlerAndTriggerError();
  UNPROTECT(2);

  return(sxpGeoTrans);

}

SEXP
RGDAL_SetNoDataValue(SEXP sxpRasterBand, SEXP NoDataValue) {
  CPLErr err;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  err = pRasterBand->SetNoDataValue(NUMERIC_POINTER(NoDataValue)[0]);

  if (err == CE_Failure)
	warning("setting of missing value not supported by this driver");
  uninstallErrorHandlerAndTriggerError();

  return(sxpRasterBand);

}

SEXP RGDAL_SetStatistics(SEXP sxpRasterBand, SEXP statistics) {

  CPLErr err;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  installErrorHandler();
  err = pRasterBand->SetStatistics(NUMERIC_POINTER(statistics)[0],
    NUMERIC_POINTER(statistics)[1], NUMERIC_POINTER(statistics)[2],
    NUMERIC_POINTER(statistics)[3]);

  if (err == CE_Failure)
	warning("setting of statistics not supported by this driver");
  uninstallErrorHandlerAndTriggerError();

  return(sxpRasterBand);

}

SEXP
RGDAL_SetGeoTransform(SEXP sxpDataset, SEXP GeoTransform) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  if (LENGTH(GeoTransform) != 6)
	error("GeoTransform argument should have length 6");

  installErrorHandler();
  CPLErr err = pDataset->SetGeoTransform(NUMERIC_POINTER(GeoTransform));

  if (err == CE_Failure) 
	warning("Failed to set GeoTransform\n");
  uninstallErrorHandlerAndTriggerError();

  return(sxpDataset);
}
/* added RSB 20060212 */
SEXP
RGDAL_SetProject(SEXP sxpDataset, SEXP proj4string) {

  OGRSpatialReference *oSRS = new OGRSpatialReference;
  char *pszSRS_WKT = NULL;

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  installErrorHandler();
  oSRS->importFromProj4(CHAR(STRING_ELT(proj4string, 0)));
  uninstallErrorHandlerAndTriggerError();

#if GDAL_VERSION_MAJOR >= 3
  installErrorHandler();
    oSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
  uninstallErrorHandlerAndTriggerError();
#endif
  
  installErrorHandler();
  oSRS->exportToWkt( &pszSRS_WKT );
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  OGRErr err = pDataset->SetProjection(pszSRS_WKT);
  CPLFree( pszSRS_WKT );

  if (err == CE_Failure) 
	warning("Failed to set projection\n");
  delete oSRS;
  uninstallErrorHandlerAndTriggerError();

  return(sxpDataset);
}
/* added RSB 20191103 */
SEXP
RGDAL_SetProject_WKT2(SEXP sxpDataset, SEXP WKT2string, SEXP enforce_xy) {

#if GDAL_VERSION_MAJOR >= 3

  OGRSpatialReference *oSRS = new OGRSpatialReference;
  int vis_order;

  if (enforce_xy == R_NilValue) vis_order = 0;
  else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
  else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
  else vis_order = 0;

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

//Rprintf("%s\n", CHAR(STRING_ELT(WKT2string, 0)));

  installErrorHandler();
  oSRS->importFromWkt(CHAR(STRING_ELT(WKT2string, 0)));
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  if (vis_order == 1)
    oSRS->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
  uninstallErrorHandlerAndTriggerError();

  installErrorHandler();
  OGRErr err = pDataset->SetSpatialRef(oSRS);

  if (err == CE_Failure) {
	warning("Failed to set projection\n");
        delete oSRS;
  }
  delete oSRS;
  uninstallErrorHandlerAndTriggerError();

  return(sxpDataset);
#else
  return(R_NilValue);
#endif
}


SEXP RGDAL_SetRasterColorTable(SEXP raster, SEXP icT, SEXP ricT, SEXP cicT) {

    int i, nr=INTEGER_POINTER(ricT)[0], nc=INTEGER_POINTER(cicT)[0];
    GDALRasterBand* target = getGDALRasterPtr(raster);
		
    installErrorHandler();
    GDALColorTableH ctab = GDALCreateColorTable(GPI_RGB);//FIXME VG
    uninstallErrorHandlerAndTriggerError();

    for (i=0; i<nr; i++) {

        GDALColorEntry ce;

        ce.c1 = (GByte) INTEGER_POINTER(icT)[i];
        ce.c2 = (GByte) INTEGER_POINTER(icT)[i+nr];
        ce.c3 = (GByte) INTEGER_POINTER(icT)[i+(nr*2)];
        if (nc == 3) ce.c4 = 255;
        else ce.c4 = (GByte) INTEGER_POINTER(icT)[i+(nr*3)];

        installErrorHandler();
        GDALSetColorEntry (ctab, i, &ce);
        uninstallErrorHandlerAndTriggerError();
    }

    installErrorHandler();
    int err = GDALSetRasterColorTable(target, ctab);
	
    if (err == CE_Failure) {
        uninstallErrorHandlerAndTriggerError();
        warning("Unable to set color table");
    }
    GDALDestroyColorTable(ctab);
    uninstallErrorHandlerAndTriggerError();

    return(raster);

}

SEXP
RGDAL_GenCMap(SEXP input1, SEXP input2, SEXP input3, SEXP output, SEXP nColors, SEXP setCMap) {
	
	GDALRasterBand* band1 = getGDALRasterPtr(input1);
	GDALRasterBand* band2 = getGDALRasterPtr(input2);
	GDALRasterBand* band3 = getGDALRasterPtr(input3);
		
	GDALColorTable ctab;
	
	int ncol = asInteger(nColors);
	
	if (ncol < 2 || ncol > 256)
		error("Number of colors should range from 2 to 256");
	
        installErrorHandler();
	int err = GDALComputeMedianCutPCT(band1, band2, band3, NULL,
	                                  ncol, &ctab, NULL, NULL); 
	
	if (err == CE_Failure) {
          uninstallErrorHandlerAndTriggerError();
          error("Error generating color table");
	}
        uninstallErrorHandlerAndTriggerError();
	if (output != R_NilValue) {
		
		GDALRasterBand* target = getGDALRasterPtr(output);
	
                installErrorHandler();
		err = GDALDitherRGB2PCT(band1, band2, band3, target, &ctab, NULL, NULL);
	
		if (err == CE_Failure) {
                  uninstallErrorHandlerAndTriggerError();
                  error("Image dithering failed");
                }
                uninstallErrorHandlerAndTriggerError();
	
		if (asLogical(setCMap)) {
			
                        installErrorHandler();
			err = GDALSetRasterColorTable(target, &ctab);
	
			if (err == CE_Failure) {
                          uninstallErrorHandlerAndTriggerError();
                          warning("Unable to set color table");
                        }
                        uninstallErrorHandlerAndTriggerError();
			
		}
		
	}   	
	
	return(GDALColorTable2Matrix(&ctab));
	
}

// CPLGetConfigOption( const char *pszKey, const char *pszDefault )

SEXP RGDAL_CPLGetConfigOption(SEXP inOption) {
    installErrorHandler();
    if (CPLGetConfigOption(asString(inOption), NULL) == NULL) {
        uninstallErrorHandlerAndTriggerError();
        return(R_NilValue);
    }
    SEXP res;
    PROTECT(res=NEW_CHARACTER(1));
    installErrorHandler();
    SET_STRING_ELT(res, 0,
        COPY_TO_USER_STRING(CPLGetConfigOption(asString(inOption), NULL)));
    uninstallErrorHandlerAndTriggerError();
    UNPROTECT(1);
    return(res);
}

// CPLSetConfigOption( const char *pszKey, const char *pszValue )

SEXP RGDAL_CPLSetConfigOption(SEXP inOption, SEXP value) {
    installErrorHandler();
    if (value == R_NilValue)
        CPLSetConfigOption(asString(inOption), NULL);
    else
        CPLSetConfigOption(asString(inOption), asString(value));
    uninstallErrorHandlerAndTriggerError();
    return(R_NilValue);
}

SEXP RGDAL_CPL_RECODE_ICONV(void) {
    SEXP ans;
    PROTECT(ans=NEW_LOGICAL(1));
#ifdef CPL_RECODE_ICONV
    LOGICAL_POINTER(ans)[0] = TRUE;
#else /* CPL_RECODE_ICONV */
    LOGICAL_POINTER(ans)[0] = FALSE;
#endif /* CPL_RECODE_ICONV */
    UNPROTECT(1);
    return(ans);
}


#ifdef __cplusplus
}
#endif

