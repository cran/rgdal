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
#ifndef PROJ_H_API // PROJ_H_API
#include <proj_api.h>
/* BDR 131123 */
#ifndef P4CTX
#if PJ_VERSION >= 480
# define P4CTX 1
#endif
#endif

#if PJ_VERSION == 480
FILE *pj_open_lib(projCtx, const char *, const char *);
#else
#if PJ_VERSION < 480
FILE *pj_open_lib(const char *, const char *);
#endif
#endif

#if PJ_VERSION < 493
int inversetest(void *);
#endif

#if PJ_VERSION >= 600
# define PROJ6 1
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
#ifdef P4CTX
    projCtx ctx;
#endif
#ifdef OSGEO4W
    PROTECT(ans=NEW_LOGICAL(1));
    LOGICAL_POINTER(ans)[0] = TRUE;
#else
#if PJ_VERSION <= 480
    FILE *fp;
#else
    PAFile fp;
#endif

    PROTECT(ans=NEW_LOGICAL(1));

#ifdef P4CTX
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "conus", "rb");
#else
    fp = pj_open_lib("conus", "rb");
#endif
    if (fp == NULL) LOGICAL_POINTER(ans)[0] = FALSE;
    else {
        LOGICAL_POINTER(ans)[0] = TRUE;
#if PJ_VERSION <= 480
        fclose(fp);
#else
        pj_ctx_fclose(ctx, fp);
#endif
    }
#endif /* OSGEO4W */
    UNPROTECT(1);

    return(ans);
}

SEXP
PROJ4_proj_def_dat_Installed(void) {
    SEXP ans;
#ifdef P4CTX
    projCtx ctx;
#endif
#if PJ_VERSION <= 480
    FILE *fp;
#else
    PAFile fp;
#endif

    PROTECT(ans=NEW_LOGICAL(1));

#ifdef P4CTX
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "proj_def.dat", "r");
#else
    fp = pj_open_lib("proj_def.dat", "r");
#endif
    if (fp == NULL) LOGICAL_POINTER(ans)[0] = FALSE;
    else {
        LOGICAL_POINTER(ans)[0] = TRUE;
#if PJ_VERSION <= 480
        fclose(fp);
#else
        pj_ctx_fclose(ctx, fp);
#endif
    }
    UNPROTECT(1);

    return(ans);
}

#define MAX_LINE_LEN 512	/* maximal line length supported.     */

SEXP get_proj_search_path(void) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP set_proj_paths(SEXP path) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP get_proj_user_writable_dir() {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}


SEXP list_coordinate_ops(SEXP source, SEXP target, SEXP area_of_interest, SEXP strict_containment, SEXP viz_order) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP CRS_compare(SEXP fromargs, SEXP toargs, SEXP type1, SEXP type2) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP transform_ng(SEXP fromargs, SEXP toargs, SEXP coordOp, SEXP npts, SEXP x, SEXP y, SEXP z, SEXP aoi) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP project_ng(SEXP n, SEXP xlon, SEXP ylat, SEXP zz, SEXP coordOp) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP project_ng_coordOp(SEXP proj, SEXP inv, SEXP aoi, SEXP ob_tran) {
    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP proj_network_enabled() {
//    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP enable_proj_network() {
//    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP disable_proj_network() {
//    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}

SEXP get_source_crs(SEXP source) {
//    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}
SEXP proj_vis_order(SEXP wkt2) {
//    Rprintf("Not available for deprecated API");
    return(R_NilValue);
}


SEXP P6_SRID_proj(SEXP inSRID, SEXP format, SEXP multiline, SEXP in_format,
    SEXP epsg, SEXP out_format) {

    return(R_NilValue);

}

#ifndef PROJ6

SEXP
PROJcopyEPSG(SEXP tf) {

    SEXP ans;

#if PJ_VERSION <= 480
    FILE *fp;
#else // PJ_VERSION <= 480
    PAFile fp;
#endif // PJ_VERSION <= 480
    FILE *fptf;
    char buf[MAX_LINE_LEN+1];   /* input buffer */
    int i=0;
#ifdef P4CTX
    projCtx ctx;
#endif // P4CTX

    PROTECT(ans=NEW_INTEGER(1));
    INTEGER_POINTER(ans)[0] = 0;

#ifdef OSGEO4W
    fp = fopen("C:\\OSGeo4W\\share\\proj\\epsg", "rb");
#endif /* OSGEO4W */

#ifndef OSGEO4W
#ifdef P4CTX
    ctx = pj_get_default_ctx();
    fp = pj_open_lib(ctx, "epsg", "rb");
#else // P4CTX
    fp = pj_open_lib("epsg", "rb");
#endif //  P4CTX
#endif /* OSGEO4W */
    if (fp == NULL) INTEGER_POINTER(ans)[0] = 0;
    else {
        fptf = fopen(CHAR(STRING_ELT(tf, 0)), "wb");
        if (fptf == NULL) {
            INTEGER_POINTER(ans)[0] = 0;
#if PJ_VERSION <= 480
            fclose(fp);
#else // PJ_VERSION <= 480
            pj_ctx_fclose(ctx, fp);
#endif // PJ_VERSION <= 480
            UNPROTECT(1);
            return(ans);
        }
        /* copy from fp to fptf */
        /* copy source file to target file, line by line. */
// pj_ctx_fgets()
        while (
#if PJ_VERSION <= 480
            fgets(buf, MAX_LINE_LEN+1, fp)
#else // PJ_VERSION <= 480
            pj_ctx_fgets(ctx, buf, MAX_LINE_LEN+1, fp)
#endif // PJ_VERSION <= 480
            != NULL) {
	    if (fputs(buf, fptf) == EOF) {  /* error writing data */
                INTEGER_POINTER(ans)[0] = 0;
#if PJ_VERSION <= 480
                fclose(fp);
#else // PJ_VERSION <= 480
                pj_ctx_fclose(ctx, fp);
#endif // PJ_VERSION <= 480
                fclose(fptf);
                UNPROTECT(1);
                return(ans);
	    }
            i++;
        }
        INTEGER_POINTER(ans)[0] = i;
#if PJ_VERSION <= 480
        fclose(fp);
#else // PJ_VERSION <= 480
        pj_ctx_fclose(ctx, fp);
#endif // PJ_VERSION <= 480
        fclose(fptf);
    }
    
    UNPROTECT(1);

    return(ans);
}
#endif // PROJ6


SEXP RGDAL_project(SEXP n, SEXP xlon, SEXP ylat, SEXP projarg, SEXP ob_tran) {
//void project(int *n, double *xlon, double *ylat, double *x, double *y, char **projarg, int *ob_tran){

  /* call the _forward_ projection specified by the string projarg,
  * using longitude and lat from xlon and ylat vectors, return
  * answers in x and y vectors (all vectors of length n) */

  int i, nwarn=0, is_ob_tran=LOGICAL_POINTER(ob_tran)[0], 
      nn=INTEGER_POINTER(n)[0];

  projUV p;
  projPJ pj;
  SEXP res;
  double ixlon, iylat;
  
  if (!(pj = pj_init_plus(CHAR(STRING_ELT(projarg, 0))))) //FIXME VG poss
    error(pj_strerrno(*pj_get_errno_ref()));
//Rprintf("pj_fwd: %s\n", pj_get_def(pj, 0));
  PROTECT(res = NEW_LIST(2));
  SET_VECTOR_ELT(res, 0, NEW_NUMERIC(nn));
  SET_VECTOR_ELT(res, 1, NEW_NUMERIC(nn));

//Rprintf("n: %d, is_ob_tran: %d\n", nn, is_ob_tran);

  for (i=0; i<nn; i++) {
//Rprintf("i: %d ", i);
    ixlon = NUMERIC_POINTER(xlon)[i];
//Rprintf("xlon: %f ", ixlon);
    iylat = NUMERIC_POINTER(ylat)[i];
//Rprintf("ylat: %f\n", iylat);
    /* preserve NAs and NaNs. Allow Infs, since maybe proj can handle them. */
    if(ISNAN(ixlon) || ISNAN(iylat)){
      NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=ixlon;
      NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=iylat;
    } else {
      p.u=ixlon;
      p.v=iylat;
      p.u *= DEG_TO_RAD;
      p.v *= DEG_TO_RAD;
      p = pj_fwd(p, pj);
      if (p.u == HUGE_VAL || ISNAN(p.u) || p.v == HUGE_VAL || ISNAN(p.v)) {
              nwarn++;
/*	      Rprintf("projected point not finite\n");*/
      }
       if (is_ob_tran) {
        p.u *= RAD_TO_DEG;
        p.v *= RAD_TO_DEG;
      }
//Rprintf("i: %d x: %f y: %f\n", i, p.u, p.v);
      NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=p.u;
      NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=p.v;
    }
  }
  if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);

  pj_free(pj);
  UNPROTECT(1);
  return(res);
}

SEXP project_inv(SEXP n, SEXP x, SEXP y, SEXP projarg, SEXP ob_tran) {
//void project_inv(int *n, double *x, double *y, double *xlon, double *ylat, char **projarg, int *ob_tran){

  /* call the _inverse_ projection specified by the string projarg,
  * returning longitude and lat in xlon and ylat vectors, given the
  * numbers in x and y vectors (all vectors of length n) */

  int i, nwarn=0, is_ob_tran=LOGICAL_POINTER(ob_tran)[0], 
      nn=INTEGER_POINTER(n)[0];

  projUV p;
  projPJ pj;
  SEXP res;
  double ix, iy;

  pj = pj_init_plus((const char*) CHAR(STRING_ELT(projarg, 0)));//FIXME VG poss
  if (!(pj)) {
    pj_free(pj);
    error(pj_strerrno(*pj_get_errno_ref()));
  }
/*Rprintf("pj_inv: %s\n", pj_get_def(pj, 0));*/
// check
#if PJ_VERSION < 493
  if ( inversetest(pj) == 0) {
    pj_free(pj);
    error("No inverse for this projection");
  };
#endif // PJ_VERSION < 493
  PROTECT(res = NEW_LIST(2));
  SET_VECTOR_ELT(res, 0, NEW_NUMERIC(nn));
  SET_VECTOR_ELT(res, 1, NEW_NUMERIC(nn));

  for(i=0;i<nn;i++){
    ix = NUMERIC_POINTER(x)[i];
    iy = NUMERIC_POINTER(y)[i];
    if(ISNAN(ix) || ISNAN(iy)){
      NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=ix;
      NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=iy;
    } else {
      p.u=ix;
      p.v=iy;
      if (is_ob_tran) {
        p.u *= DEG_TO_RAD;
        p.v *= DEG_TO_RAD;
      }
      p = pj_inv(p, pj);
      if (p.u == HUGE_VAL || ISNAN(p.u) || p.v == HUGE_VAL || ISNAN(p.v)) {
            nwarn++;
/*	    Rprintf("inverse projected point not finite\n");*/
      }
      NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=p.u * RAD_TO_DEG;
      NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=p.v * RAD_TO_DEG;
    }
  }
  if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);

  pj_free(pj);
  UNPROTECT(1);
  return(res);
}

SEXP transform(SEXP fromargs, SEXP toargs, SEXP npts, SEXP x, SEXP y, SEXP z) {

	/* interface to pj_transform() to be able to use longlat proj
	 * and datum transformation in an SEXP format */

	int i, n, nwarn=0, ob_tran, have_z;
	double *xx, *yy, *zz=NULL;
	projPJ fromPJ, toPJ;
        SEXP use_ob_tran = getAttrib(npts, install("ob_tran"));
	SEXP res;

        if (z == R_NilValue) have_z = 0;
        else have_z = 1;
        if (use_ob_tran == R_NilValue) ob_tran = 0;
        else if (INTEGER_POINTER(use_ob_tran)[0] == 1) ob_tran = 1;
        else if (INTEGER_POINTER(use_ob_tran)[0] == -1) ob_tran = -1;
        else ob_tran = 0;
	
	if (!(fromPJ = pj_init_plus(CHAR(STRING_ELT(fromargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	if (!(toPJ = pj_init_plus(CHAR(STRING_ELT(toargs, 0))))) 
		error(pj_strerrno(*pj_get_errno_ref()));
	
	n = INTEGER_POINTER(npts)[0];
	xx = (double *) R_alloc((size_t) n, sizeof(double));
	yy = (double *) R_alloc((size_t) n, sizeof(double));
	if (have_z) zz = (double *) R_alloc((size_t) n, sizeof(double));

	for (i=0; i < n; i++) {
		xx[i] = NUMERIC_POINTER(x)[i];
		yy[i] = NUMERIC_POINTER(y)[i];
		if (have_z) zz[i] = NUMERIC_POINTER(z)[i];
	}
	if ( pj_is_latlong(fromPJ) || ob_tran == 1) {
		for (i=0; i < n; i++) {
       			 xx[i] *= DEG_TO_RAD;
       			 yy[i] *= DEG_TO_RAD;
		}
	}

	if (have_z) PROTECT(res = NEW_LIST(5));
        else PROTECT(res = NEW_LIST(4));
	SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
	SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
	if (have_z) {
            SET_VECTOR_ELT(res, 2, NEW_NUMERIC(n));
            SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
	    SET_VECTOR_ELT(res, 4, NEW_CHARACTER(1));
        } else {
            SET_VECTOR_ELT(res, 2, NEW_CHARACTER(1));
	    SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
        }

        if (ob_tran != 0) {
	    if (have_z) {
              if(pj_transform(toPJ, fromPJ, (long) n, 0, xx, yy, zz) != 0) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	      }
            } else {
              if(pj_transform(toPJ, fromPJ, (long) n, 0, xx, yy, NULL) != 0) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	      }
            }
        } else {
	    if (have_z) {
	      if(pj_transform(fromPJ, toPJ, (long) n, 0, xx, yy, zz) != 0) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	      }
            } else {
	      if(pj_transform(fromPJ, toPJ, (long) n, 0, xx, yy, NULL) != 0) {
		pj_free(fromPJ); pj_free(toPJ);
		error("error in pj_transform: %s",
                    pj_strerrno(*pj_get_errno_ref()));
	      }
            }
        }
        char *from_def = pj_get_def(fromPJ, 0);
        char *to_def = pj_get_def(toPJ, 0);
	if (have_z) {
	    SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(from_def));
	    SET_STRING_ELT(VECTOR_ELT(res, 4), 0, 
		COPY_TO_USER_STRING(to_def));
        } else {
	    SET_STRING_ELT(VECTOR_ELT(res, 2), 0, 
		COPY_TO_USER_STRING(from_def));
	    SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(to_def));
        }

        pj_free(fromPJ);
        pj_dalloc(from_def);
        pj_dalloc(to_def);
	if ( pj_is_latlong(toPJ) || ob_tran == -1) {
		for (i=0; i < n; i++) {
               		xx[i] *= RAD_TO_DEG;
               		yy[i] *= RAD_TO_DEG;
            	}
	}
        pj_free(toPJ);
        if (have_z) {
	    for (i=0; i < n; i++) {
		if (xx[i] == HUGE_VAL || yy[i] == HUGE_VAL || zz[i] == HUGE_VAL
		    || ISNAN(xx[i]) || ISNAN(yy[i]) || ISNAN(zz[i])) {
                    nwarn++;
/*		    Rprintf("transformed point not finite\n");*/
		}
		NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = xx[i];
		NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = yy[i];
		NUMERIC_POINTER(VECTOR_ELT(res, 2))[i] = zz[i];
	    }
        } else {
	    for (i=0; i < n; i++) {
		if (xx[i] == HUGE_VAL || yy[i] == HUGE_VAL 
		    || ISNAN(xx[i]) || ISNAN(yy[i])) {
                    nwarn++;
/*		    Rprintf("transformed point not finite\n");*/
		}
		NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = xx[i];
		NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = yy[i];
	    }
        }

        if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);
	UNPROTECT(1);
	return(res);
}

SEXP RGDAL_checkCRSArgs(SEXP args) {
	SEXP res;
	projPJ pj;
        char *cp=NULL;
	PROTECT(res = NEW_LIST(2));
	SET_VECTOR_ELT(res, 0, NEW_LOGICAL(1));
	SET_VECTOR_ELT(res, 1, NEW_CHARACTER(1));
	LOGICAL_POINTER(VECTOR_ELT(res, 0))[0] = FALSE;
        pj = pj_init_plus(CHAR(STRING_ELT(args, 0)));
	if (!(pj)) {

		SET_STRING_ELT(VECTOR_ELT(res, 1), 0, 
			COPY_TO_USER_STRING(pj_strerrno(*pj_get_errno_ref()))
                );
		pj_free(pj);
		UNPROTECT(1);
		return(res);
	}

        cp = pj_get_def(pj, 0); 
// Thanks to Brian Ripley for resolving poor coding issue found in GCC 8
        if (isspace(cp[0])) {
	    SET_STRING_ELT(VECTOR_ELT(res, 1), 0, 
		COPY_TO_USER_STRING(cp+1));
        } else {
	    SET_STRING_ELT(VECTOR_ELT(res, 1), 0, 
		COPY_TO_USER_STRING(cp));
        }
        pj_dalloc(cp);
	
	LOGICAL_POINTER(VECTOR_ELT(res, 0))[0] = TRUE;
	pj_free(pj);
	UNPROTECT(1);
	return(res);
}

#ifndef PROJ6

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
struct PJ_UNITS {
	char	*id;	/* units keyword */
	char	*to_meter;	/* multiply by value to get meters */
	char	*name;	/* comments */
#if PJ_VERSION >= 500
	double   factor;       /* to_meter factor in actual numbers */
#endif // PJ_VERSION >= 500 
};
struct PJ_UNITS *pj_get_units_ref( void );


SEXP RGDAL_projInfo(SEXP type) {
    SEXP ans=NULL;
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

    } else if (INTEGER_POINTER(type)[0] == 3) {
#if PJ_VERSION < 500 
        PROTECT(ans = NEW_LIST(3)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(3)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("id"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("to_meter"));
        SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("name"));
        setAttrib(ans, R_NamesSymbol, ansnames);

        struct PJ_UNITS *lu;
        for (lu = pj_get_units_ref(); lu->id ; ++lu) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        n=0;
        for (lu = pj_get_units_ref(); lu->id ; ++lu) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(lu->id));
            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(lu->to_meter));
            SET_STRING_ELT(VECTOR_ELT(ans, 2), n, 
		COPY_TO_USER_STRING(lu->name));
            n++;
        }
#else // PJ_VERSION < 500
        PROTECT(ans = NEW_LIST(4)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(4)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("id"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("to_meter"));
        SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("name"));
        SET_STRING_ELT(ansnames, 3, COPY_TO_USER_STRING("factor"));
        setAttrib(ans, R_NamesSymbol, ansnames);

        struct PJ_UNITS *lu;
        for (lu = pj_get_units_ref(); lu->id ; ++lu) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 3, NEW_NUMERIC(n));
        n=0;
        for (lu = pj_get_units_ref(); lu->id ; ++lu) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(lu->id));
            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(lu->to_meter));
            SET_STRING_ELT(VECTOR_ELT(ans, 2), n, 
		COPY_TO_USER_STRING(lu->name));
            NUMERIC_POINTER(VECTOR_ELT(ans, 3))[n] = lu->factor;
            n++;
        }
#endif // PJ_VERSION < 500
    } else error("no such type");
    
    UNPROTECT(pc);
    return(ans);
}
#endif // not PROJ_H_API

#endif //  

#ifdef __cplusplus
}
#endif

