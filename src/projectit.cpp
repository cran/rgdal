/* Copyright (c) 2003-8 Barry Rowlingson and Roger Bivand */

// R headers moved outside extern "C" 070808 RSB re. note from BDR
// #ifdef __cplusplus
// extern "C" {
// #endif

#include <R.h>
#include <Rdefines.h>
/*#include <Rinternals.h>*/
#include "rgdal.h"

#ifdef __cplusplus
extern "C" {
#endif
/* #include <projects.h> */
#include <proj_api.h>
#ifdef P4CTX
FILE *pj_open_lib(projCtx, const char *, const char *);
#else
FILE *pj_open_lib(const char *, const char *);
#endif

SEXP
PROJ4VersionInfo(void) {
    SEXP ans;

    PROTECT(ans=NEW_LIST(2));
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(1));
    SET_VECTOR_ELT(ans, 1, NEW_INTEGER(1));
    SET_STRING_ELT(VECTOR_ELT(ans, 0), 0,
        COPY_TO_USER_STRING(pj_get_release()));
    INTEGER_POINTER(VECTOR_ELT(ans, 1))[0] = PJ_VERSION;

    UNPROTECT(1);

    return(ans);
}

SEXP
PROJ4NADsInstalled(void) {
    SEXP ans;
#ifdef OSGEO4W
    PROTECT(ans=NEW_LOGICAL(1));
    LOGICAL_POINTER(ans)[0] = TRUE;
#else
    FILE *fp;

    PROTECT(ans=NEW_LOGICAL(1));

#ifdef P4CTX
    fp = pj_open_lib(pj_get_default_ctx(), "conus", "rb");
#else
    fp = pj_open_lib("conus", "rb");
#endif
    if (fp == NULL) LOGICAL_POINTER(ans)[0] = FALSE;
    else {
        LOGICAL_POINTER(ans)[0] = TRUE;
        fclose(fp);
    }
#endif /* OSGEO4W */
    UNPROTECT(1);

    return(ans);
}

#define MAX_LINE_LEN 512	/* maximal line length supported.     */

SEXP
PROJcopyEPSG(SEXP tf) {
    SEXP ans;

    FILE *fp, *fptf;
    char buf[MAX_LINE_LEN+1];   /* input buffer */
    int i=0;

    PROTECT(ans=NEW_INTEGER(1));
    INTEGER_POINTER(ans)[0] = 0;

#ifdef OSGEO4W
    fp = fopen("C:\\OSGeo4W\\share\\proj\\epsg", "rb");
#endif /* OSGEO4W */

#ifndef OSGEO4W
#ifdef P4CTX
    fp = pj_open_lib(pj_get_default_ctx(), "epsg", "rb");
#else
    fp = pj_open_lib("epsg", "rb");
#endif
#endif /* OSGEO4W */
    if (fp == NULL) INTEGER_POINTER(ans)[0] = 0;
    else {
        fptf = fopen(CHAR(STRING_ELT(tf, 0)), "wb");
        if (fptf == NULL) {
            INTEGER_POINTER(ans)[0] = 0;
            fclose(fp);
            UNPROTECT(1);
            return(ans);
        }
        /* copy from fp to fptf */
        /* copy source file to target file, line by line. */
        while (fgets(buf, MAX_LINE_LEN+1, fp)) {
	    if (fputs(buf, fptf) == EOF) {  /* error writing data */
                INTEGER_POINTER(ans)[0] = 0;
                fclose(fp);
                fclose(fptf);
                UNPROTECT(1);
                return(ans);
	    }
            i++;
        }
        if (!feof(fp)) { /* fgets failed _not_ due to encountering EOF */
            INTEGER_POINTER(ans)[0] = 0;
            fclose(fp);
            fclose(fptf);
            UNPROTECT(1);
            return(ans);
        }
        INTEGER_POINTER(ans)[0] = i;
        fclose(fp);
        fclose(fptf);
    }
    
    UNPROTECT(1);

    return(ans);
}

void project(int *n, double *xlon, double *ylat, double *x, double *y, char **projarg, int *ob_tran){

  /* call the _forward_ projection specified by the string projarg,
  * using longitude and lat from xlon and ylat vectors, return
  * answers in x and y vectors (all vectors of length n) */

  int i, nwarn=0;

  projUV p;
  projPJ pj;
  
  if (!(pj = pj_init_plus(*projarg))) 
    error(pj_strerrno(*pj_get_errno_ref()));
/*  Rprintf("%s\n", pj_get_def(pj, 0));*/

  for (i=0; i<*n; i++) {
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
      if (p.u == HUGE_VAL || ISNAN(p.u)) {
              nwarn++;
/*	      Rprintf("projected point not finite\n");*/
      }
      if (*ob_tran) {
        p.u *= RAD_TO_DEG;
        p.v *= RAD_TO_DEG;
      }
      x[i]=p.u;
      y[i]=p.v;
    }
  }
  if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);

  pj_free(pj);
}

void project_inv(int *n, double *x, double *y, double *xlon, double *ylat, char **projarg, int *ob_tran){

  /* call the _inverse_ projection specified by the string projarg,
  * returning longitude and lat in xlon and ylat vectors, given the
  * numbers in x and y vectors (all vectors of length n) */

  int i, nwarn=0;

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
      if (*ob_tran) {
        p.u *= DEG_TO_RAD;
        p.v *= DEG_TO_RAD;
      }
      p = pj_inv(p, pj);
      if (p.u == HUGE_VAL || ISNAN(p.u)) {
            nwarn++;
/*	    Rprintf("inverse projected point not finite\n");*/
      }
      xlon[i]=p.u * RAD_TO_DEG;
      ylat[i]=p.v * RAD_TO_DEG;
    }
  }
  if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);

  pj_free(pj);
}

SEXP transform(SEXP fromargs, SEXP toargs, SEXP npts, SEXP x, SEXP y) {

	/* interface to pj_transform() to be able to use longlat proj
	 * and datum transformation in an SEXP format */

	int i, n, nwarn=0, ob_tran;
	double *xx, *yy, *zz;
	projPJ fromPJ, toPJ;
        SEXP use_ob_tran = getAttrib(npts, install("ob_tran"));
	SEXP res;

        if (use_ob_tran == R_NilValue) ob_tran = 0;
        else if (INTEGER_POINTER(use_ob_tran)[0] == 1) ob_tran = 1;
        else if (INTEGER_POINTER(use_ob_tran)[0] == -1) ob_tran = -1;
        else ob_tran = 0;
	
	if (!(fromPJ = pj_init_plus(CHAR(STRING_ELT(fromargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	if (!(toPJ = pj_init_plus(CHAR(STRING_ELT(toargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	n = INTEGER_POINTER(npts)[0];
	xx = (double *) R_alloc((long) n, sizeof(double));
	yy = (double *) R_alloc((long) n, sizeof(double));
	zz = (double *) R_alloc((long) n, sizeof(double));

	for (i=0; i < n; i++) {
		xx[i] = NUMERIC_POINTER(x)[i];
		yy[i] = NUMERIC_POINTER(y)[i];
		zz[i] = (double) 0;
	}
	if ( pj_is_latlong(fromPJ) || ob_tran == 1) {
		for (i=0; i < n; i++) {
       			 xx[i] *= DEG_TO_RAD;
       			 yy[i] *= DEG_TO_RAD;
		}
	}

	PROTECT(res = NEW_LIST(4));
	SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
	SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
	SET_VECTOR_ELT(res, 2, NEW_CHARACTER(1));
	SET_STRING_ELT(VECTOR_ELT(res, 2), 0, 
		COPY_TO_USER_STRING(pj_get_def(fromPJ, 0)));
	SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
	SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(pj_get_def(toPJ, 0)));

        if (ob_tran != 0) {
	    if( pj_transform( toPJ, fromPJ, (long) n, 0, xx, yy, zz ) != 0 ) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	    }
        } else {
	    if( pj_transform( fromPJ, toPJ, (long) n, 0, xx, yy, zz ) != 0 ) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	    }
        }

        pj_free(fromPJ); pj_free(toPJ);
	if ( pj_is_latlong(toPJ) || ob_tran == -1) {
		for (i=0; i < n; i++) {
               		xx[i] *= RAD_TO_DEG;
               		yy[i] *= RAD_TO_DEG;
            	}
	}
	for (i=0; i < n; i++) {
		if (xx[i] == HUGE_VAL || yy[i] == HUGE_VAL 
		    || ISNAN(xx[i]) || ISNAN(yy[i])) {
                    nwarn++;
/*		    Rprintf("transformed point not finite\n");*/
		}
		NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = xx[i];
		NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = yy[i];
	}

        if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);
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

/* #include <projects.h> */
struct PJconsts;
    
struct PJ_LIST {
	char	*id;		/* projection keyword */
	struct PJconsts	*(*proj)(struct PJconsts*);/* projection entry point */
	char 	* const *descr;	/* description text */
};
struct PJ_LIST  *pj_get_list_ref( void );
struct PJ_ELLPS {
	char	*id;	/* ellipse keyword name */
	char	*major;	/* a= value */
	char	*ell;	/* elliptical parameter */
	char	*name;	/* comments */
};
struct PJ_ELLPS *pj_get_ellps_ref( void );
struct PJ_DATUMS {
    char    *id;     /* datum keyword */
    char    *defn;   /* ie. "to_wgs84=..." */
    char    *ellipse_id; /* ie from ellipse table */
    char    *comments; /* EPSG code, etc */
};
struct PJ_DATUMS *pj_get_datums_ref( void ); 

SEXP projInfo(SEXP type) {
    SEXP ans;
    SEXP ansnames;
    int n=0, pc=0;


    if (INTEGER_POINTER(type)[0] == 0) {
        PROTECT(ans = NEW_LIST(2)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(2)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("description"));
        setAttrib(ans, R_NamesSymbol, ansnames);

        struct PJ_LIST *lp;
        for (lp = pj_get_list_ref() ; lp->id ; ++lp) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        n=0;
        for (lp = pj_get_list_ref() ; lp->id ; ++lp) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(lp->id));

            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(*lp->descr));
            n++;
        }
    } else if (INTEGER_POINTER(type)[0] == 1) {
        PROTECT(ans = NEW_LIST(4)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(4)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("major"));
        SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("ell"));
        SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("description"));
        setAttrib(ans, R_NamesSymbol, ansnames);

        struct PJ_ELLPS *le;
        for (le = pj_get_ellps_ref(); le->id ; ++le) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 3, NEW_CHARACTER(n));
        n=0;
        for (le = pj_get_ellps_ref(); le->id ; ++le) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(le->id));
            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(le->major));
            SET_STRING_ELT(VECTOR_ELT(ans, 2), n, 
		COPY_TO_USER_STRING(le->ell));
            SET_STRING_ELT(VECTOR_ELT(ans, 3), n, 
		COPY_TO_USER_STRING(le->name));
            n++;
        }
    } else if (INTEGER_POINTER(type)[0] == 2) {
        PROTECT(ans = NEW_LIST(4)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(4)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("name"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("ellipse"));
        SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("definition"));
        SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("description"));
        setAttrib(ans, R_NamesSymbol, ansnames);

        struct PJ_DATUMS *ld;
        for (ld = pj_get_datums_ref(); ld->id ; ++ld) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 3, NEW_CHARACTER(n));
        n=0;
        for (ld = pj_get_datums_ref(); ld->id ; ++ld) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(ld->id));
            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(ld->ellipse_id));
            SET_STRING_ELT(VECTOR_ELT(ans, 2), n, 
		COPY_TO_USER_STRING(ld->defn));
            SET_STRING_ELT(VECTOR_ELT(ans, 3), n, 
		COPY_TO_USER_STRING(ld->comments));
            n++;
        }

    } else error("no such type");
    
    UNPROTECT(pc);
    return(ans);
}


#ifdef __cplusplus
}
#endif

