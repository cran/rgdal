#include "ogrsf_frmts.h"

extern "C" {
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
}



extern "C" {
  SEXP ogr_GetDriverNames(void){
    SEXP ans, ansnames;
    int i, n, pc=0;

    PROTECT(ans = NEW_LIST(2)); pc++;
    PROTECT(ansnames = NEW_CHARACTER(2)); pc++;
    SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
    SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("write"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    OGRSFDriverRegistrar *poR = OGRSFDriverRegistrar::GetRegistrar();
    n = poR->GetDriverCount();
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
    SET_VECTOR_ELT(ans, 1, NEW_LOGICAL(n));

    for (i=0; i < n; i++) {
      OGRSFDriver *poDriver = poR->GetDriver(i);
//      SET_VECTOR_ELT(sxpDriverList, i,
      SET_STRING_ELT(VECTOR_ELT(ans, 0), i,
        COPY_TO_USER_STRING(poDriver->GetName()));
      LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i] = 
        poDriver->TestCapability(ODrCCreateDataSource);
    }

    UNPROTECT(pc);
    return(ans);
  }
}

