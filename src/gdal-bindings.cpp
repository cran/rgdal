#include <gdal_priv.h>
#include <cpl_string.h>

#ifdef __cplusplus
extern "C" {
#endif

#include <R.h>
#include <Rinternals.h>

static SEXP
mkString_safe(const char *string) {

  if (string == NULL) return(R_NilValue);

  return(mkString(string));

}

static char*
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

    warning("\n\tGDAL Error %d: %s\n", err_no, msg);

  } else {

    error("\n\tGDAL Error %d: %s\n", err_no, msg);

  }

  return;

}

SEXP
RGDAL_Init(void) {

  CPLSetErrorHandler((CPLErrorHandler)__errorHandler);

  GDALAllRegister();

  return(R_NilValue);

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
RGDAL_GetMetadata(SEXP sxpObj, SEXP sxpDomain) {

  void *pGDALObj = getGDALObjPtr(sxpObj);

  const char *domain = asString(sxpDomain);

  char **metadata = ((GDALMajorObject *)pGDALObj)->GetMetadata(domain);

  if (metadata == NULL) return(R_NilValue);

  metadata = CSLDuplicate(metadata);

  SEXP sxpMetadata, sxpNames;

  PROTECT(sxpMetadata = allocVector(VECSXP, CSLCount(metadata)));

  PROTECT(sxpNames = allocVector(STRSXP, CSLCount(metadata)));

  for (int i = 0; i < CSLCount(metadata); ++i) {

    const char *field = CSLGetField(metadata, i);

    char *tag = NULL;

    const char *value = CPLParseNameValue(field, &tag);

    SET_VECTOR_ELT(sxpMetadata, i, value ? mkChar(value) : mkChar(""));

    SET_VECTOR_ELT(sxpNames, i, tag ? mkChar(tag) : mkChar(""));

  }

  setAttrib(sxpMetadata, R_NamesSymbol, sxpNames);

  UNPROTECT(2);

  return(sxpMetadata);

}

SEXP
RGDAL_SetMetadata(SEXP sxpObj, SEXP sxpMetadataList) {

  void *pGDALObj = getGDALObjPtr(sxpObj);

  SEXP sxpNames = getAttrib(sxpMetadataList, R_NamesSymbol);

  char **metadata = (char **) CPLCalloc(1, sizeof(char *));

  const char *name, *value;

  if (isNull(sxpNames)) {

    for (int i = 0; i < length(sxpMetadataList); ++i) {
     
      value = asString(VECTOR_ELT(sxpMetadataList, i));
      CSLAddString(metadata, value);
  
    }

  } else {

    for (int i = 0; i < length(sxpMetadataList); ++i) {

      name = asString(sxpNames, i);
      value = asString(VECTOR_ELT(sxpMetadataList, i));
      CSLAddNameValue(metadata, name, value);
		      
    }

  }

  CPLErr err = ((GDALMajorObject *)pGDALObj)->SetMetadata(metadata, NULL);

  if (err == CE_Failure) warning("Failed to set metadata\n");

  return(sxpObj);

}

SEXP
RGDAL_GetDriverNames(void) {

  SEXP sxpDriverList;

  PROTECT(sxpDriverList = allocVector(STRSXP, GDALGetDriverCount()));

  for (int i = 0; i < GDALGetDriverCount(); ++i) {

    GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
    
  SET_VECTOR_ELT(sxpDriverList, i, mkChar(GDALGetDriverShortName( pDriver )));

  }

  UNPROTECT(1);

  return(sxpDriverList);

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
  fprintf(stderr, "Deleting temp file: %s... ", filename);
  fflush(stderr);
#endif

  CPLErr eErr = pDriver->Delete(filename);

  if (eErr == CE_Failure)
    warning("Failed to delete dataset: %s\n", filename);

#ifdef RGDALDEBUG
  fprintf(stderr, "done.\n", filename);
  fflush(stderr);
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

  Rprintf("Closing GDAL dataset handle %p... ", (void *) pDataset);

  if (pDataset != NULL) {

    pDataset->~GDALDataset();

    R_ClearExternalPtr(sxpHandle);

    Rprintf(" destroyed ... ");

  }

  Rprintf("done.\n");

  return(R_NilValue);

}

SEXP
RGDAL_DeleteHandle(SEXP sxpHandle) {

  GDALDataset *pDataset =
    (GDALDataset *) R_ExternalPtrAddr(sxpHandle);

  if (pDataset == NULL) return(R_NilValue);

  GDALDriver *pDriver = pDataset->GetDriver();

  const char *filename = pDataset->GetDescription();

  deleteFile(pDriver, filename);

  RGDAL_CloseHandle(sxpHandle);

  return(R_NilValue);

}

SEXP
RGDAL_CloseDataset(SEXP sxpDataset) {

  SEXP sxpHandle = getObjHandle(sxpDataset);

  if (sxpHandle == NULL) return(R_NilValue);

  RGDAL_CloseHandle(sxpHandle);

  return(R_NilValue);

}

SEXP
RGDAL_CreateDataset(SEXP sxpDriver, SEXP sDim, SEXP sType,
		    SEXP sOpts, SEXP sFile) {

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  const char *filename = asString(sFile);

#ifdef RGDALDEBUG
  fprintf(stderr, "Opening dataset: %s\n", filename);
  fflush(stderr);
#endif

  if (filename == NULL) error("Invalid file name\n");

  GDALDataType eGDALType = (GDALDataType) asInteger(sType);

  GDALDataset *pDataset = pDriver->Create(filename,
					  INTEGER(sDim)[0],
					  INTEGER(sDim)[1],
					  INTEGER(sDim)[2],
					  eGDALType, NULL);

  if (pDataset == NULL) error("Unable to create dataset\n");

  pDataset->SetDescription(filename);

  SEXP sxpHandle = R_MakeExternalPtr((void *) pDataset,
				     mkChar("GDAL Dataset"),
				     R_NilValue);

  return(sxpHandle);

}

SEXP
RGDAL_OpenDataset(SEXP filename, SEXP read_only) {

  const char *fn = asString(filename);

  GDALAccess RWFlag;

  if (asLogical(read_only))
    RWFlag = GA_ReadOnly;
  else
    RWFlag = GA_Update;

  GDALDataset *pDataset = (GDALDataset *) GDALOpen(fn, RWFlag);

  if (pDataset == NULL)
    error("Could not open file: %s\n", filename);

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

  const char *filename = asString(sxpFile);

  if (filename == NULL) error("Invalid filename\n");

  GDALDriver *pDriver = getGDALDriverPtr(sxpDriver);

  GDALDataset *pDatasetCopy = pDriver->CreateCopy(filename,
						  pDataset,
						  asInteger(sxpStrict),
						  NULL, NULL, NULL);

  if (pDatasetCopy == NULL) error("Dataset copy failed\n");

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

SEXP
RGDAL_GetProjectionRef(SEXP sDataset) {

  GDALDataset *pDataset = getGDALDatasetPtr(sDataset);

  return(mkString_safe(pDataset->GetProjectionRef()));

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

SEXP
RGDAL_PutRasterData(SEXP sxpRasterBand, SEXP sxpData, SEXP sxpOffset) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  int rowsIn = nrows(sxpData);
  int colsIn = ncols(sxpData);

  GDALDataType eGDALType;

  switch(pRasterBand->GetRasterDataType()) {

  case GDT_Byte:
  case GDT_UInt16:
  case GDT_Int16:
  case GDT_UInt32:
  case GDT_Int32:

    eGDALType = GDT_Int32;
    PROTECT(sxpData = coerceVector(sxpData, INTSXP));

    break;

  case GDT_Float32:
  case GDT_Float64:

    eGDALType = GDT_Float64;
    PROTECT(sxpData = coerceVector(sxpData, REALSXP));

    break;

  case GDT_CInt16:
  case GDT_CInt32:
  case GDT_CFloat32:
  case GDT_CFloat64:

    eGDALType = GDT_CFloat64;
    PROTECT(sxpData = coerceVector(sxpData, CPLXSXP));

    break;
    
  default:

    error("Raster data type unknown\n");
    
    break;

  }

  // Transpose data
  if(pRasterBand->RasterIO(GF_Write,
			   INTEGER(sxpOffset)[1],
			   INTEGER(sxpOffset)[0],
			   rowsIn, colsIn,
			   (void *)CHAR(sxpData),
			   rowsIn, colsIn,
			   eGDALType,
			   0, 0)
     == CE_Failure)
    error("Failure during raster IO\n");

  UNPROTECT(1);

  return(sxpRasterBand);

}

SEXP
RGDAL_GetRasterData(SEXP sxpRasterBand,
		    SEXP sxpRegion,
		    SEXP sxpDimOut,
		    SEXP sxpInterleave) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  GDALDataType eGDALType;
  SEXPTYPE uRType;

  switch(pRasterBand->GetRasterDataType()) {

  case GDT_Byte:
  case GDT_UInt16:
  case GDT_Int16:
  case GDT_UInt32:
  case GDT_Int32:

    // Fix me!
    uRType = INTSXP;
    eGDALType = GDT_Int32;

    break;

  case GDT_Float32:
  case GDT_Float64:

    // Fix me!
    uRType = REALSXP;
    eGDALType = GDT_Float64;

    break;

  case GDT_CInt16:
  case GDT_CInt32:
  case GDT_CFloat32:
  case GDT_CFloat64:

    // Fix me!
    uRType = CPLXSXP;
    eGDALType = GDT_CFloat64;

    break;
    
  default:

    error("Raster data type unknown\n");
    
    break;

  }

  // Create matrix transposed
  SEXP sRStorage = allocMatrix(uRType,
			       INTEGER(sxpDimOut)[1],
			       INTEGER(sxpDimOut)[0]);

  // Data is read in transposed order
  if(pRasterBand->RasterIO(GF_Read,
			   INTEGER(sxpRegion)[1],
			   INTEGER(sxpRegion)[0],
			   INTEGER(sxpRegion)[3],
			   INTEGER(sxpRegion)[2],
			   (void *)CHAR(sRStorage),
			   INTEGER(sxpDimOut)[1],
			   INTEGER(sxpDimOut)[0],
			   eGDALType,
			   INTEGER(sxpInterleave)[0],
			   INTEGER(sxpInterleave)[1])
     == CE_Failure)
    error("Failure during raster IO\n");

  int hasNoDataValue;

  double noDataValue = pRasterBand->GetNoDataValue(&hasNoDataValue);

  if (hasNoDataValue) {

    switch(uRType) {

    case INTSXP:

      for (int i = 0; i < LENGTH(sRStorage); ++i)
	if (INTEGER(sRStorage)[i] == (int)noDataValue)
	  INTEGER(sRStorage)[i] = NA_INTEGER;

      break;

    case REALSXP:

      for (int i = 0; i < LENGTH(sRStorage); ++i)
	if (REAL(sRStorage)[i] == noDataValue)
	  REAL(sRStorage)[i] = NA_REAL;

      break;

    default:

      warning("Output data values = %f are invalid\n", noDataValue);

      break;

    }

  }

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

SEXP
RGDAL_GetColorTable(SEXP sxpRasterBand) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  GDALColorTable *pColorTable = pRasterBand->GetColorTable();

  if (pColorTable == NULL) return(R_NilValue);

  int nColorEntries = pColorTable->GetColorEntryCount();

  SEXP sxpColorMatrix = allocMatrix(INTSXP, nColorEntries, 4);

  for (int i = 0; i < nColorEntries; ++i) {

    const GDALColorEntry *pColorEntry = pColorTable->GetColorEntry(i);

    INTEGER(sxpColorMatrix)[i] = (int) pColorEntry->c1;
    INTEGER(sxpColorMatrix)[i + nColorEntries] = (int) pColorEntry->c2;
    INTEGER(sxpColorMatrix)[i + 2 * nColorEntries] = (int) pColorEntry->c3;
    INTEGER(sxpColorMatrix)[i + 3 * nColorEntries] = (int) pColorEntry->c4;

  }

  return(sxpColorMatrix);

}

SEXP
RGDAL_SetCategoryNames(SEXP sxpRasterBand, SEXP sxpNames) {

  GDALRasterBand *pRasterBand = getGDALRasterPtr(sxpRasterBand);

  char **nameList = NULL;

  for (int i = 0; i < length(sxpNames); ++i)
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

  for (int i = 0; i < CSLCount(pcCNames); ++i) {

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

  CPLErr err = pDataset->GetGeoTransform(REAL(sxpGeoTrans));

  if (err == CE_Failure) {

    REAL(sxpGeoTrans)[0] = 0;
    REAL(sxpGeoTrans)[1] = 1;
    REAL(sxpGeoTrans)[2] = 0;
    REAL(sxpGeoTrans)[3] = 0;
    REAL(sxpGeoTrans)[4] = 0;
    REAL(sxpGeoTrans)[5] = 1;

  }

  return(sxpGeoTrans);

}

#ifdef __cplusplus
}
#endif

