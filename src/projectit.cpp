/* Copyright (c) 2003 Barry Rowlingson and Roger Bivand */

#ifdef __cplusplus
extern "C" {
#endif

#include <R.h>
#include <Rdefines.h>
/*#include <Rinternals.h>*/
#include <proj_api.h>

void project(int *n, double *xlon, double *ylat, double *x, double *y, char **projarg){

  /* call the _forward_ projection specified by the string projarg,
  * using longitude and lat from xlon and ylat vectors, return
  * answers in x and y vectors (all vectors of length n) */

  int i;

  projUV p;
  projPJ pj;
  
  if (!(pj = pj_init_plus(*projarg))) 
    error(pj_strerrno(*pj_get_errno_ref()));
/*  Rprintf("%s\n", pj_get_def(pj, 0));*/

  for(i=0;i<*n;i++){
    /* preserve NAs and NaNs. Allow Infs, since maybe proj can handle them. */
    if(ISNAN(xlon[i]) || ISNAN(ylat[i])){
      x[i]=xlon[i];
      y[i]=ylat[i];
    } else {
      p.u=xlon[i];
      p.v=ylat[i];
      p.u *= DEG_TO_RAD;
      p.v *= DEG_TO_RAD;
      p = pj_fwd(p, pj);
      if (p.u == HUGE_VAL) {
	      pj_free(pj);
	      error(pj_strerrno(*pj_get_errno_ref()));
      }
      x[i]=p.u;
      y[i]=p.v;
    }
  }

  pj_free(pj);
}

void project_inv(int *n, double *x, double *y, double *xlon, double *ylat, char **projarg){

  /* call the _inverse_ projection specified by the string projarg,
  * returning longitude and lat in xlon and ylat vectors, given the
  * numbers in x and y vectors (all vectors of length n) */

  int i;

  projUV p;
  projPJ pj;
  
  if (!(pj = pj_init_plus(*projarg)))
    error(pj_strerrno(*pj_get_errno_ref()));
/*  Rprintf("%s\n", pj_get_def(pj, 0));*/

  for(i=0;i<*n;i++){
    if(ISNAN(x[i]) || ISNAN(y[i])){
      xlon[i]=x[i];
      ylat[i]=y[i];
    } else {
      p.u=x[i];
      p.v=y[i];
      p = pj_inv(p, pj);
      if (p.u == HUGE_VAL) {
            pj_free(pj);
      	    error(pj_strerrno(*pj_get_errno_ref()));
      }
      xlon[i]=p.u * RAD_TO_DEG;
      ylat[i]=p.v * RAD_TO_DEG;
    }
  }

  pj_free(pj);
}

SEXP transform(SEXP fromargs, SEXP toargs, SEXP npts, SEXP x, SEXP y) {

	/* interface to pj_transform() to be able to use longlat proj
	 * and datum transformation in an SEXP format */

	int i, n;
	double z;
	projUV p;
	projPJ fromPJ, toPJ;
	SEXP res;
	
	if (!(fromPJ = pj_init_plus(CHAR(STRING_ELT(fromargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	if (!(toPJ = pj_init_plus(CHAR(STRING_ELT(toargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	n = INTEGER_POINTER(npts)[0];
	
	PROTECT(res = NEW_LIST(4));
	SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
	SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
	SET_VECTOR_ELT(res, 2, NEW_CHARACTER(1));
	SET_STRING_ELT(VECTOR_ELT(res, 2), 0, 
		COPY_TO_USER_STRING(pj_get_def(fromPJ, 0)));
	SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
	SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(pj_get_def(toPJ, 0)));
	for(i=0; i < n; i++){
		if(ISNAN(NUMERIC_POINTER(x)[i]) || 
			ISNAN(NUMERIC_POINTER(y)[i])){
			NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = NA_REAL;
			NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = NA_REAL;
		} else {
			p.u = NUMERIC_POINTER(x)[i];
			p.v = NUMERIC_POINTER(y)[i];
			z = (double) 0;
			if (p.v == HUGE_VAL) p.u = HUGE_VAL;
			if (p.u != HUGE_VAL) {
				if ( pj_is_latlong(fromPJ) ) {
                			p.v *= DEG_TO_RAD;
                			p.u *= DEG_TO_RAD;
            			}

            			if( pj_transform( fromPJ, toPJ, 1, 0, 
                              		&(p.u), &(p.v), &z ) != 0 ) {
                			p.u = HUGE_VAL;
                			p.v = HUGE_VAL;
            			}
			}

        		if (p.u == HUGE_VAL)  {
            			pj_free(fromPJ); pj_free(toPJ);
      	    			error(pj_strerrno(*pj_get_errno_ref()));
      			}
			if ( pj_is_latlong(toPJ) ) {
                		p.v *= RAD_TO_DEG;
                		p.u *= RAD_TO_DEG;
            		}

			NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = p.u;
			NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = p.v;
		}
	}

        pj_free(fromPJ); pj_free(toPJ);
	UNPROTECT(1);
	return(res);
}

SEXP checkCRSArgs(SEXP args) {
	SEXP res;
	projPJ pj;
	PROTECT(res = NEW_LIST(2));
	SET_VECTOR_ELT(res, 0, NEW_LOGICAL(1));
	SET_VECTOR_ELT(res, 1, NEW_CHARACTER(1));
	LOGICAL_POINTER(VECTOR_ELT(res, 0))[0] = FALSE;
	
	if (!(pj = pj_init_plus(CHAR(STRING_ELT(args, 0))))) {

		SET_STRING_ELT(VECTOR_ELT(res, 1), 0, 
			COPY_TO_USER_STRING(pj_strerrno(*pj_get_errno_ref())));
		
		UNPROTECT(1);
		return(res);
	}

	SET_STRING_ELT(VECTOR_ELT(res, 1), 0, 
		COPY_TO_USER_STRING(pj_get_def(pj, 0)));
	
	LOGICAL_POINTER(VECTOR_ELT(res, 0))[0] = TRUE;
	
	UNPROTECT(1);
	return(res);
}

#ifdef __cplusplus
}
#endif

