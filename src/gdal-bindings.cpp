#include <gdal_priv.h>
#include <gdal_alg.h>
#include <gdal_rat.h>
#include <cpl_string.h>
#include <cpl_csv.h>
#include <ogr_spatialref.h>
#include <ogrsf_frmts.h>
#include <cpl_error.h>

// R headers moved outside extern "C" 070808 RSB re. note from BDR
// #ifdef __cplusplus
// extern "C" {
// #endif

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

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

  SEXP sxpHandle = getAttrib(sxpObj, mkString("handle"));

  if (isNull(sxpHandle)) error("Null object handle\n");

  return(sxpHandle);

}

static void*
getGDALObjPtr(SEXP sxpObj) {

  SEXP sxpHandle = getObjHandle(sxpObj);

  void *extPtr = R_ExternalPtrAddr(sxpHandle);

  if (extPtr == NULL) error("Null external pointer\n");

  return(extPtr);

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

static void
__errorHandler(CPLErr eErrClass, int err_no, const char *msg) {

  if (eErrClass == CE_Warning) {

    warning("\n\tNon-fatal GDAL Error %d: %s\n", err_no, msg);

  } else {

    error("\n\tGDAL Error %d: %s\n", err_no, msg);

  }

  return;

}

/************************************************************************/
/*                       CPLDefaultErrorHandler()                       */
/* quoted from GDAL, pointed at REprintf()                              */
/************************************************************************/

void CPL_STDCALL R_CPLDefaultErrorHandler( CPLErr eErrClass, int nError, 
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

}


SEXP
RGDAL_Init(void) {

  CPLSetErrorHandler((CPLErrorHandler)__errorHandler);

  GDALAllRegister();

  OGRRegisterAll();
 
  return(R_NilValue);

}

SEXP
RGDAL_GDALVersionInfo(void) {
    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GDALVersionInfo( "--version" )));

    UNPROTECT(1);

    return(ans);
}

SEXP
RGDAL_GDAL_DATA_Info(void) {
    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(CSVFilename( "prime_meridian.csv" )));

    UNPROTECT(1);

    return(ans);
}


SEXP
RGDAL_NullHandle(void) {

  return(R_MakeExternalPtr(NULL,
			   mkString("Null handle"),
			   R_NilValue));
  
}

SEXP
RGDAL_GetDescription(SEXP sxpObj) {

  void *pGDALObj = getGDALObjPtr(sxpObj);

  const char *desc = ((GDALMajorObject *)pGDALObj)->GetDescription();

  return(mkString_safe(desc));

}


SEXP
RGDAL_GetDriverNames(void) {

  SEXP ans, ansnames;
  int pc=0;
  int nDr=GDALGetDriverCount();

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

  int i, flag;
  for (i = 0; i < nDr; ++i) {

    GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
    
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

  UNPROTECT(pc);

  return(ans);

}

SEXP
RGDAL_GetDriver(SEXP sxpDriverName) {

  const char *pDriverName = asString(sxpDriverName);

  GDALDriver *pDriver = (GDALDriver *) GDALGetDriverByName(pDriverName);

  if (pDriver == NULL)
    error("No driver registered with name: %s\n", pDriverName);
  
  SEXP sxpHandle = R_MakeExternalPtr((void *) pDriver,
				     mkChar("GDAL Driver"),
				     R_NilValue);

  return(sxpHandle);

}

static void
deleteFile(GDALDriver *pDriver, const char *filename) {


#ifdef RGDALDEBUG
  Rprintf("Deleting temp file: %s... ", filename);
//  fflush(stderr);
#endif

  if (strcmp(GDALGetDriverLongName( pDriver ), "In Memory Raster") != 0) {
      CPLErr eErr = pDriver->Delete(filename);

    if (eErr == CE_Failure)
      warning("Failed to delete dataset: %s\n", filename);
  }

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

  if (pDataset != NULL) {

    pDataset->~GDALDataset();

    R_ClearExternalPtr(sxpHandle);

#ifdef RGDALDEBUG
    Rprintf(" destroyed ... ");
#endif

  }

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

  GDALDriver *pDriver = pDataset->GetDriver();

  const char *filename = pDataset->GetDescription();

  RGDAL_CloseHandle(sxpHandle);

/* FIXME path name problem to temporary file */
#ifndef OSGEO4W
  deleteFile(pDriver, filename);
#endif
  return(R_NilValue);

}

SEXP
RGDAL_CloseDataset(SEXP sxpDataset) {


  SEXP sxpHandle = getObjHandle(sxpDataset);

  if (sxpHandle == NULL) return(R_NilValue);

  const char *classname = asString(getAttrib(sxpDataset, R_ClassSymbol));

  if (strcmp(classname, "GDALTransientDataset") == 0) {
    
    RGDAL_DeleteHandle(sxpHandle);

  } else {

    RGDAL_CloseHandle(sxpHandle);
  }

  return(R_NilValue);

}

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

  for (i=0; i < length(sOpts); i++) papszCreateOptions = CSLAddString( 
    papszCreateOptions, CHAR(STRING_ELT(sOpts, i)) );
#ifdef RGDALDEBUG
  for (i=0; i < CSLCount(papszCreateOptions); i++)
    Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptions, i));
#endif
  pDataset = pDriver->Create(filename,
			  INTEGER(sDim)[0],
			  INTEGER(sDim)[1],
			  INTEGER(sDim)[2],
			  eGDALType, papszCreateOptions);
  CSLDestroy(papszCreateOptions);

  if (pDataset == NULL) error("Unable to create dataset\n");

  pDataset->SetDescription(filename);

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDataset,
				     mkChar("GDAL Dataset"),
				     R_NilValue);

  return(sxpHandle);

}

SEXP
RGDAL_OpenDataset(SEXP filename, SEXP read_only, SEXP silent) {

  const char *fn = asString(filename);

  GDALAccess RWFlag;

  if (asLogical(read_only))
    RWFlag = GA_ReadOnly;
  else
    RWFlag = GA_Update;

/* Modification suggested by Even Rouault, 2009-08-08: */

  CPLErrorReset();
  if (asLogical(silent))
    CPLPushErrorHandler(CPLQuietErrorHandler);
  else
    CPLPushErrorHandler(R_CPLDefaultErrorHandler); 

  GDALDataset *pDataset = (GDALDataset *) GDALOpen(fn, RWFlag);

  CPLPopErrorHandler();

/* Similarly to SWIG bindings, the following lines will cause
RGDAL_OpenDataset() to fail on - uncleared - errors even if pDataset is not
NULL. They could also be just removed. While pDataset != NULL, there's some
hope ;-) */

  CPLErr eclass = CPLGetLastErrorType();

  if (pDataset != NULL && eclass == CE_Failure) {
    GDALClose(pDataset);
    pDataset = NULL;
    __errorHandler(eclass, CPLGetLastErrorNo(), CPLGetLastErrorMsg());
  }

  if (pDataset == NULL)
    error("%s\n", CPLGetLastErrorMsg());

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDataset,
				     mkChar("GDAL Dataset"),
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

  for (i=0; i < length(sxpOpts); i++) papszCreateOptions = CSLAddString( 
    papszCreateOptions, CHAR(STRING_ELT(sxpOpts, i)) );
#ifdef RGDALDEBUG
  for (i=0; i < CSLCount(papszCreateOptions); i++)
    Rprintf("option %d: %s\n", i, CSLGetField(papszCreateOptions, i));
#endif
  GDALDataset *pDatasetCopy = pDriver->CreateCopy(filename,
		pDataset, asInteger(sxpStrict),
		papszCreateOptions, NULL, NULL);

  if (pDatasetCopy == NULL) error("Dataset copy failed\n");
  CSLDestroy(papszCreateOptions);

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDatasetCopy,
				     mkChar("GDAL Dataset"),
				     R_NilValue);


  return(sxpHandle);

}

SEXP
RGDAL_GetRasterXSize(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  return(ScalarInteger(pDataset->GetRasterXSize()));

}

SEXP
RGDAL_GetRasterYSize(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  return(ScalarInteger(pDataset->GetRasterYSize()));

}

SEXP
RGDAL_GetRasterCount(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  return(ScalarInteger(pDataset->GetRasterCount()));

}

/* changed to return proj4 string 20060212 RSB */
SEXP
RGDAL_GetProjectionRef(SEXP sDataset) {

  OGRSpatialReference oSRS;
  char *pszSRS_WKT = NULL;
  SEXP ans;

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);
  
  pszSRS_WKT = (char*) pDataset->GetProjectionRef();

  oSRS.importFromWkt( &pszSRS_WKT );
  oSRS.exportToProj4( &pszSRS_WKT );
  PROTECT(ans = NEW_CHARACTER(1));
  SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS_WKT));

  CPLFree( pszSRS_WKT );
  UNPROTECT(1);
  return(ans);

}

SEXP
RGDAL_GetMetadata(SEXP sDataset, SEXP tag) {

    char **papszMetadata;
    SEXP ans;
    int i, n, pc=0;

    GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

    if (tag == R_NilValue) {
        papszMetadata = pDataset->GetMetadata( NULL );
    } else {
        papszMetadata = pDataset->GetMetadata(CHAR(STRING_ELT(tag, 0)));
    }

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

  GDALDriver *pDriver = pDataset->GetDriver();

  SEXP sxpDriver = R_MakeExternalPtr((void *) pDriver,
				     mkChar("GDAL Dataset"),
				     R_NilValue);

  return(sxpDriver);

}

SEXP
RGDAL_GetDriverShortName(SEXP sxpDriver) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  return(mkString_safe(GDALGetDriverShortName( pDriver )));

}

SEXP
RGDAL_GetDriverLongName(SEXP sxpDriver) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  return(mkString_safe(GDALGetDriverLongName( pDriver )));
}

SEXP
RGDAL_GetRasterBand(SEXP sDataset, SEXP sBand) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  int band = asInteger(sBand);

  GDALRasterBand *pRasterBand = pDataset->GetRasterBand(band);

  SEXP rpRasterBand = R_MakeExternalPtr((void *) pRasterBand,
					mkChar("GDAL Raster Band"),
					R_NilValue);
  return(rpRasterBand);

}

SEXP
RGDAL_GetXSize(SEXP sRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sRasterBand);

  return(ScalarInteger(pRasterBand->GetXSize()));

}

SEXP
RGDAL_GetYSize(SEXP sRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sRasterBand);

  return(ScalarInteger(pRasterBand->GetYSize()));

}

SEXP
RGDAL_GetRasterBlockSize(SEXP rasterObj) {
	
	 GDALRasterBand *raster = getGDALRasterPtr(rasterObj);
	 
	 SEXP blockSize = allocVector(INTSXP, 2);
	 
	 raster->GetBlockSize(INTEGER(blockSize) + 1, INTEGER(blockSize));
	 
	 return(blockSize);
	 
}

SEXP
RGDAL_GetAccess(SEXP sxpDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  return(ScalarLogical(pDataset->GetAccess() == GA_ReadOnly));

}

SEXP
RGDAL_GetRasterAccess(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  return(ScalarLogical(pRasterBand->GetAccess() == GA_ReadOnly));

}

SEXP
RGDAL_GetNoDataValue(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  int hasNoDataValue;

  double noDataValue = pRasterBand->GetNoDataValue(&hasNoDataValue);

  return(hasNoDataValue ? ScalarReal(noDataValue) : R_NilValue);

}

SEXP
RGDAL_GetOffset(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  return(ScalarReal(pRasterBand->GetOffset()));

}

SEXP
RGDAL_GetScale(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  return(ScalarReal(pRasterBand->GetScale()));

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

  const GDALRasterAttributeTable *pRAT = pRasterBand->GetDefaultRAT();

  if (pRAT == NULL) return(R_NilValue);

  nc = (int) pRAT->GetColumnCount();
  PROTECT(ans = NEW_LIST(nc));np++;
  PROTECT(nc_names = NEW_CHARACTER(nc));np++;
  nc_types = (GDALRATFieldType *) R_alloc((size_t) nc,
    sizeof(GDALRATFieldType));
  nc_usages = (GDALRATFieldUsage *) R_alloc((size_t) nc,
    sizeof(GDALRATFieldUsage));
  nr = (int) pRAT->GetRowCount();

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

  NUMERIC_POINTER(ans)[0] = (double) pRasterBand->GetMinimum();

  UNPROTECT(1);
  return(ans);
}

SEXP
RGDAL_GetBandMaximum(SEXP sxpRasterBand) {

  SEXP ans;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);
  PROTECT(ans = NEW_NUMERIC(1));

  NUMERIC_POINTER(ans)[0] = (double) pRasterBand->GetMaximum();

  UNPROTECT(1);
  return(ans);
}

SEXP
RGDAL_GetBandStatistics(SEXP sxpRasterBand, SEXP silent) {

  CPLErr err;

  SEXP ans;

  double min, max, mean, sd;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  err = pRasterBand->GetStatistics(FALSE, FALSE, &min, &max, &mean, &sd);

  if (err == CE_Failure) {
	if (!LOGICAL_POINTER(silent)[0])
            warning("statistics not supported by this driver");
        return(R_NilValue);
  }

  if (err == CE_Warning) {
	if (!LOGICAL_POINTER(silent)[0])
    	    warning("statistics not supported by this driver");
        return(R_NilValue);
  }

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

  INTEGER_POINTER(ans)[0] = (int) pRasterBand->GetRasterDataType();

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
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)INTEGER(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure)
      error("Failure during raster IO\n");

    break;

  case GDT_Float32:
  case GDT_Float64:

    eGDALType = GDAL_FLOAT_TYPE;
    PROTECT(sxpData = coerceVector(sxpData, REALSXP));
  // Transpose data
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)REAL(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure)
      error("Failure during raster IO\n");

    break;

  case GDT_CInt16:
  case GDT_CInt32:
  case GDT_CFloat32:
  case GDT_CFloat64:

    eGDALType = GDAL_COMPLEX_TYPE;
    PROTECT(sxpData = coerceVector(sxpData, CPLXSXP));
  // Transpose data
    if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)COMPLEX(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
       == CE_Failure)
      error("Failure during raster IO\n");

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
  double noDataValue = pRasterBand->GetNoDataValue(&hasNoDataValue);

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

  int RDT = pRasterBand->GetRasterDataType();

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
  PROTECT(sRStorage = allocMatrix(uRType,
			       INTEGER(sxpDimOut)[1],
			       INTEGER(sxpDimOut)[0])); pc++;

// replication for 2.4.0 RSB 20060726
  switch(uRType) {

    case INTSXP:
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
         == CE_Failure)
           error("Failure during raster IO\n");
      break;

    case REALSXP:

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
         == CE_Failure)
           error("Failure during raster IO\n");
      break;

    case CPLXSXP:

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
         == CE_Failure)
           error("Failure during raster IO\n");
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

  int i;

  if (hasNoDataValue) {

    switch(uRType) {

    case INTSXP:

      for (i = 0; i < LENGTH(sRStorage); ++i)
	if (INTEGER(sRStorage)[i] == (int) noDataValue) {
	  INTEGER(sRStorage)[i] = NA_INTEGER;
	}

      break;

    case REALSXP:

      switch(pRasterBand->GetRasterDataType()) {

        case GDT_Float32:

        for (i = 0; i < LENGTH(sRStorage); ++i)
	  if (REAL(sRStorage)[i] == (double) ((float) noDataValue)) {
	    REAL(sRStorage)[i] = NA_REAL;
	  }
	break;

        case GDT_Float64:

        for (i = 0; i < LENGTH(sRStorage); ++i)
	  if (REAL(sRStorage)[i] == (double) (noDataValue)) {
	    REAL(sRStorage)[i] = NA_REAL;
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
    if (uRType == REALSXP && pRasterBand->GetRasterDataType() == GDT_Float32) {
        for (i = 0; i < LENGTH(sRStorage); ++i)
	  if (ISNAN(REAL(sRStorage)[i])) {
	    REAL(sRStorage)[i] = NA_REAL;
	  }
      
    }
  }

  UNPROTECT(pc);
  return(sRStorage);

}

SEXP
RGDAL_GetPaletteInterp(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  GDALPaletteInterp ePI = 
    pRasterBand->GetColorTable()->GetPaletteInterpretation();

  return(mkString_safe(GDALGetPaletteInterpretationName(ePI)));

}

SEXP
RGDAL_GetColorInterp(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  GDALColorInterp eCI = pRasterBand->GetColorInterpretation();

  return(mkString_safe(GDALGetColorInterpretationName(eCI)));

}

static SEXP
GDALColorTable2Matrix(GDALColorTableH ctab) {

	int ncol = GDALGetColorEntryCount(ctab);

	SEXP cmat = allocMatrix(INTSXP, ncol, 4);

	for (int i = 0; i < ncol; ++i) {

    	const GDALColorEntry* ce = GDALGetColorEntry(ctab, i);

    	INTEGER(cmat)[i] = static_cast<int>(ce->c1);
    	INTEGER(cmat)[i + ncol] = static_cast<int>(ce->c2);
    	INTEGER(cmat)[i + 2 * ncol] = static_cast<int>(ce->c3);
    	INTEGER(cmat)[i + 3 * ncol] = static_cast<int>(ce->c4);

  	}

  	return(cmat);
	
}

SEXP
RGDAL_GetColorTable(SEXP rasterObj) {

	GDALRasterBandH rasterBand = getGDALRasterPtr(rasterObj);

	GDALColorTableH ctab = GDALGetRasterColorTable(rasterBand);

	if (ctab == NULL) return(R_NilValue);

	return(GDALColorTable2Matrix(ctab));

}


SEXP
RGDAL_SetCategoryNames(SEXP sxpRasterBand, SEXP sxpNames) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  char **nameList = NULL;

  int i;
  for (i = 0; i < length(sxpNames); ++i)
    nameList = CSLAddString(nameList, asString(sxpNames, i));

  CPLErr err = pRasterBand->SetCategoryNames(nameList);

  if (err == CE_Failure) warning("Failed to set category names");

  return(sxpRasterBand);

}

SEXP
RGDAL_GetCategoryNames(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  char **pcCNames = pRasterBand->GetCategoryNames();

  if (pcCNames == NULL) return(R_NilValue);

  pcCNames = CSLDuplicate(pcCNames);

  SEXP sxpCNames;

  PROTECT(sxpCNames = allocVector(STRSXP, CSLCount(pcCNames)));

  int i;
  for (i = 0; i < CSLCount(pcCNames); ++i) {

    const char *field = CSLGetField(pcCNames, i);

    SET_VECTOR_ELT(sxpCNames, i, mkChar(field));

  }

  UNPROTECT(1);
  
  return(sxpCNames);

}

SEXP
RGDAL_GetGeoTransform(SEXP sxpDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  SEXP sxpGeoTrans = allocVector(REALSXP, 6);
  SEXP ceFail = NEW_LOGICAL(1);
  LOGICAL_POINTER(ceFail)[0] = FALSE;

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
  return(sxpGeoTrans);

}

SEXP
RGDAL_SetNoDataValue(SEXP sxpRasterBand, SEXP NoDataValue) {
  CPLErr err;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  err = pRasterBand->SetNoDataValue(NUMERIC_POINTER(NoDataValue)[0]);

  if (err == CE_Failure)
	warning("setting of missing value not supported by this driver");

  return(sxpRasterBand);

}

SEXP RGDAL_SetStatistics(SEXP sxpRasterBand, SEXP statistics) {

  CPLErr err;

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  err = pRasterBand->SetStatistics(NUMERIC_POINTER(statistics)[0],
    NUMERIC_POINTER(statistics)[1], NUMERIC_POINTER(statistics)[2],
    NUMERIC_POINTER(statistics)[3]);

  if (err == CE_Failure)
	warning("setting of statistics not supported by this driver");

  return(sxpRasterBand);

}

SEXP
RGDAL_SetGeoTransform(SEXP sxpDataset, SEXP GeoTransform) {

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  if (LENGTH(GeoTransform) != 6)
	error("GeoTransform argument should have length 6");

  CPLErr err = pDataset->SetGeoTransform(NUMERIC_POINTER(GeoTransform));

  if (err == CE_Failure) 
	warning("Failed to set GeoTransform\n");

  return(sxpDataset);
}
/* added RSB 20060212 */
SEXP
RGDAL_SetProject(SEXP sxpDataset, SEXP proj4string) {

  OGRSpatialReference oSRS;
  char *pszSRS_WKT = NULL;

  GDALDataset *pDataset = getGDALDatasetPtr(sxpDataset);

  oSRS.importFromProj4(CHAR(STRING_ELT(proj4string, 0)));
  oSRS.exportToWkt( &pszSRS_WKT );

  OGRErr err = pDataset->SetProjection(pszSRS_WKT);
  CPLFree( pszSRS_WKT );

  if (err == CE_Failure) 
	warning("Failed to set projection\n");

  return(sxpDataset);
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
	
	int err = GDALComputeMedianCutPCT(band1, band2, band3, NULL,
	                                  ncol, &ctab, NULL, NULL); 
	
	if (err == CE_Failure) error("Error generating color table");
	
	if (output != R_NilValue) {
		
		GDALRasterBand* target = getGDALRasterPtr(output);
	
		err = GDALDitherRGB2PCT(band1, band2, band3, target, &ctab, NULL, NULL);
	
		if (err == CE_Failure) error("Image dithering failed");
	
		if (asLogical(setCMap)) {
			
			err = GDALSetRasterColorTable(target, &ctab);
	
			if (err == CE_Failure) warning("Unable to set color table");
			
		}
		
	}   	
	
	return(GDALColorTable2Matrix(&ctab));
	
}

#ifdef __cplusplus
}
#endif

