/* Copyright (c) 2020 Roger Bivand */


#include <R.h>
#include <Rdefines.h>
#include "rgdal.h"
//#ifdef ACCEPT_USE_OF_DEPRECATED_PROJ_API_H // kludge for 6 only
#ifdef PROJ_H_API
#include <proj.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP proj_network_enabled() {
#if PROJ_VERSION_MAJOR > 6
    SEXP res;
    PJ_CONTEXT *ctx = proj_context_create();
    int rs;
    rs = proj_context_is_network_enabled(ctx);
    PROTECT(res = NEW_LOGICAL(1));
    LOGICAL_POINTER(res)[0] = rs;
    UNPROTECT(1);
    proj_context_destroy(ctx);
    return(res);
#else
//    Rprintf("Not available for PROJ version < 7");
    return(R_NilValue);
#endif
}

#ifdef __cplusplus
}
#endif
#endif


