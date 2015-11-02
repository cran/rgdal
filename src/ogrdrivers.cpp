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
//extern "C" {
  SEXP ogr_GetDriverNames(void){
    SEXP ans, ansnames;
    int i, n, pc=0;
#ifdef GDALV2
    int j, vsum=0, create=0;
    int *vector;
#endif

    PROTECT(ans = NEW_LIST(2)); pc++;
    PROTECT(ansnames = NEW_CHARACTER(2)); pc++;
    SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
    SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("write"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    installErrorHandler();
#ifdef GDALV2
    n = GetGDALDriverManager()->GetDriverCount();
    vector = (int *) R_alloc((size_t) n, sizeof(int));
    for (i=0; i < n; i++) {
        vector[i] = 0;
        if( GetGDALDriverManager()->GetDriver(i)->GetMetadataItem(GDAL_DCAP_VECTOR) != NULL) vector[i] = 1;
        vsum+=vector[i];
    }
#else
    OGRSFDriverRegistrar *poR = OGRSFDriverRegistrar::GetRegistrar();
    n = poR->GetDriverCount();
#endif
    uninstallErrorHandlerAndTriggerError();
#ifdef GDALV2
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(vsum));
    SET_VECTOR_ELT(ans, 1, NEW_LOGICAL(vsum));
#else
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
    SET_VECTOR_ELT(ans, 1, NEW_LOGICAL(n));
#endif

    installErrorHandler();
#ifdef GDALV2
    j = 0;
#endif
    for (i=0; i < n; i++) {
#ifdef GDALV2
      if (vector[i] == 1) {
        GDALDriver *poDriver = GetGDALDriverManager()->GetDriver(i);
        if( poDriver->GetMetadataItem(GDAL_DCAP_CREATE) != NULL) create = 1;
        SET_STRING_ELT(VECTOR_ELT(ans, 0), j,
          COPY_TO_USER_STRING(poDriver->GetDescription()));
        LOGICAL_POINTER(VECTOR_ELT(ans, 1))[j] = create;
        j++;
      }
#else
      OGRSFDriver *poDriver = poR->GetDriver(i);
//      SET_VECTOR_ELT(sxpDriverList, i,
      SET_STRING_ELT(VECTOR_ELT(ans, 0), i,
        COPY_TO_USER_STRING(poDriver->GetName()));
      LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i] = 
        poDriver->TestCapability(ODrCCreateDataSource);
#endif
    }
    uninstallErrorHandlerAndTriggerError();

    UNPROTECT(pc);
    return(ans);
  }

#ifdef __cplusplus
}
#endif
//}

