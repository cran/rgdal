# include "rgdal.h"

SEXP make_Polygonlist(SEXP iG, SEXP iGc) {

    SEXP res, jG, jGc;
    int n, i, pc=0;

    n = length(iG);

    PROTECT(res = NEW_LIST(n)); pc++;
    PROTECT(jGc = NEW_INTEGER(1)); pc++;

    for (i=0; i<n; i++) {

        jG = VECTOR_ELT(iG, i);
// hole setting based on comments by default (OGC SFS order)
// but by ring order if comment NULL 121019
        if (iGc == R_NilValue) {
            SET_VECTOR_ELT(res, i, make_Polygon(jG, R_NilValue));
        } else {
            INTEGER_POINTER(jGc)[0] = INTEGER_POINTER(iGc)[i];
            SET_VECTOR_ELT(res, i, make_Polygon(jG, jGc));
        }
    }

    UNPROTECT(pc);
    return(res);

}

SEXP make_Polygon(SEXP jG, SEXP jGc) {

    SEXP res, coords, dim, dimnames, n, ihole;
    int i, nn, pc=0, copy1=FALSE;

    nn = length(VECTOR_ELT(jG, 0));
    if ((NUMERIC_POINTER(VECTOR_ELT(jG, 0))[0] != 
         NUMERIC_POINTER(VECTOR_ELT(jG, 0))[(nn-1)]) && 
        (NUMERIC_POINTER(VECTOR_ELT(jG, 1))[0] != 
         NUMERIC_POINTER(VECTOR_ELT(jG, 1))[(nn-1)])) copy1 = TRUE;
    if (copy1) {

        PROTECT(coords = NEW_NUMERIC((nn+1)*2)); pc++;

        for (i=0; i<nn; i++) {
            NUMERIC_POINTER(coords)[i] = NUMERIC_POINTER(VECTOR_ELT(jG, 0))[i];
            NUMERIC_POINTER(coords)[i+(nn+1)] = 
                NUMERIC_POINTER(VECTOR_ELT(jG, 1))[i];
        }
        NUMERIC_POINTER(coords)[nn] = 
            NUMERIC_POINTER(VECTOR_ELT(jG, 0))[0];
        NUMERIC_POINTER(coords)[nn+(nn+1)] = 
            NUMERIC_POINTER(VECTOR_ELT(jG, 1))[0];

    } else {

        PROTECT(coords = NEW_NUMERIC(nn*2)); pc++;

        for (i=0; i<nn; i++) {
            NUMERIC_POINTER(coords)[i] = NUMERIC_POINTER(VECTOR_ELT(jG, 0))[i];
            NUMERIC_POINTER(coords)[i+nn] = 
                NUMERIC_POINTER(VECTOR_ELT(jG, 1))[i];
        }
    }

// hole setting based on comments by default (OGC SFS order)
// but by ring order if comment NULL 121019
    PROTECT(ihole = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ihole)[0] = 0L;
    if (jGc == R_NilValue) INTEGER_POINTER(ihole)[0] = NA_INTEGER;
    else if (INTEGER_POINTER(jGc)[0] != 0L) INTEGER_POINTER(ihole)[0] = 1L;
    PROTECT(n = NEW_INTEGER(1)); pc++;
    if (copy1) INTEGER_POINTER(n)[0] = nn+1;
    else INTEGER_POINTER(n)[0] = nn;

    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = (int) INTEGER_POINTER(n)[0];
    INTEGER_POINTER(dim)[1] = (int) 2;
    setAttrib(coords, R_DimSymbol, dim);
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("y"));

    PROTECT(res = SP_PREFIX(Polygon_c)(coords, n, ihole)); pc++;

    UNPROTECT(pc);
    return(res);

}
    

SEXP rgdal_sp_linkingTo_version(void) {
    return(SP_PREFIX(sp_linkingTo_version)());
}

