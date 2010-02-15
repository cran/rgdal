# include "rgdal.h"

SEXP make_Polygonlist(SEXP iG) {

    SEXP res, jG;
    int n, i, pc=0;

    n = length(iG);

    PROTECT(res = NEW_LIST(n)); pc++;

    for (i=0; i<n; i++) {

        jG = VECTOR_ELT(iG, i);
        SET_VECTOR_ELT(res, i, make_Polygon(jG));
    }

    UNPROTECT(pc);
    return(res);

}

SEXP make_Polygon(SEXP jG) {

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

    PROTECT(ihole = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ihole)[0] = NA_INTEGER;
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
    
