#include "ogrsf_frmts.h"

extern "C" {
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
}



extern "C" {
  SEXP ogr_GetDriverNames(void){
    SEXP sxpDriverList;
    int i, n, pc=0;

    OGRSFDriverRegistrar *poR = OGRSFDriverRegistrar::GetRegistrar();
    n = poR->GetDriverCount();
    PROTECT(sxpDriverList = NEW_CHARACTER(n)); pc++;

    for (i=0; i < n; i++) {
      OGRSFDriver *poDriver = poR->GetDriver(i);
//      SET_VECTOR_ELT(sxpDriverList, i,
      SET_STRING_ELT(sxpDriverList, i,
        COPY_TO_USER_STRING(poDriver->GetName()));
    }

    UNPROTECT(pc);
    return(sxpDriverList);
  }
}

