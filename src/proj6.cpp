/* Copyright (c) 2019-20 Roger Bivand */


#include <R.h>
#include <Rdefines.h>
#include "rgdal.h"
//#ifdef ACCEPT_USE_OF_DEPRECATED_PROJ_API_H // kludge for 6 only
#ifdef PROJ_H_API // PROJ_H_API
#include <proj.h>

#ifdef __cplusplus
extern "C" {
#endif

static void silent_logger(void *, int, const char *) {}


SEXP RGDAL_checkCRSArgs(SEXP args) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}

SEXP PROJ4NADsInstalled(void) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}

SEXP PROJ4_proj_def_dat_Installed(void) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}

SEXP transform(SEXP fromargs, SEXP toargs, SEXP npts, SEXP x, SEXP y, SEXP z) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}


SEXP RGDAL_project(SEXP n, SEXP xlon, SEXP ylat, SEXP projarg, SEXP ob_tran) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}


SEXP project_inv(SEXP n, SEXP x, SEXP y, SEXP projarg, SEXP ob_tran) {
//    Rprintf("Not available for PROJ version >= 6");
    return(R_NilValue);
}



SEXP PROJ4VersionInfo(void) {
    SEXP ans;
    PJ_INFO pji= proj_info();

    PROTECT(ans=NEW_LIST(2));
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(1));
    SET_VECTOR_ELT(ans, 1, NEW_INTEGER(1));
    SET_STRING_ELT(VECTOR_ELT(ans, 0), 0,
        COPY_TO_USER_STRING(pji.release));
    INTEGER_POINTER(VECTOR_ELT(ans, 1))[0] = (pji.major*100)+(pji.minor*10)+pji.patch;

    UNPROTECT(1);

    return(ans);
}

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

        const struct PJ_LIST *lp;
        for (lp = proj_list_operations() ; lp->id ; ++lp) {
            if( strcmp(lp->id,"latlong") == 0
                || strcmp(lp->id,"longlat") == 0
                || strcmp(lp->id,"geocent") == 0 )
            continue;
            n++;
        }
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        n=0;
        for (lp = proj_list_operations() ; lp->id ; ++lp) {
            if( strcmp(lp->id,"latlong") == 0
                || strcmp(lp->id,"longlat") == 0
                || strcmp(lp->id,"geocent") == 0 )
            continue;
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

        const struct PJ_ELLPS *le;
        for (le = proj_list_ellps(); le->id ; ++le) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 3, NEW_CHARACTER(n));
        n=0;
        for (le = proj_list_ellps(); le->id ; ++le) {
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
        return(R_NilValue);
    } else if (INTEGER_POINTER(type)[0] == 3) {
        PROTECT(ans = NEW_LIST(3)); pc++;
        PROTECT(ansnames = NEW_CHARACTER(3)); pc++;
        SET_STRING_ELT(ansnames, 0, COPY_TO_USER_STRING("id"));
        SET_STRING_ELT(ansnames, 1, COPY_TO_USER_STRING("to_meter"));
        SET_STRING_ELT(ansnames, 2, COPY_TO_USER_STRING("name"));
        setAttrib(ans, R_NamesSymbol, ansnames);
#if ((PROJ_VERSION_MAJOR == 7 && PROJ_VERSION_MINOR >= 1) || PROJ_VERSION_MAJOR > 7)
        PROJ_UNIT_INFO** units;
        units = proj_get_units_from_database(nullptr, nullptr, "linear", false, nullptr);
        n = 0;
//Rprintf("n: %d\n", n);
        for (int i = 0; units && units[i]; i++) {
            if (units[i]->proj_short_name) n++;
//Rprintf("%s\n", units[i]->proj_short_name);
        }
//Rprintf("n: %d\n", n);
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_NUMERIC(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        int n1=0;
        for (int i = 0; units && units[i]; i++) {
            if (units[i]->proj_short_name) {
                SET_STRING_ELT(VECTOR_ELT(ans, 0), n1, 
		    COPY_TO_USER_STRING(units[i]->proj_short_name));
                NUMERIC_POINTER(VECTOR_ELT(ans, 1))[n1] = units[i]->conv_factor;
                SET_STRING_ELT(VECTOR_ELT(ans, 2), n1, 
		    COPY_TO_USER_STRING(units[i]->name));
                n1++;
//Rprintf("%s %f %s\n", units[i]->proj_short_name, units[i]->conv_factor, units[i]->name);
            }
            if (n1 >= n) break; //EPJ correction
        }
        proj_unit_list_destroy(units);
// >= 710 proj_get_units_from_database()
#else
        const struct PJ_UNITS *lu;
        for (lu = proj_list_units(); lu->id ; ++lu) n++;
        SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(n));
        SET_VECTOR_ELT(ans, 2, NEW_CHARACTER(n));
        n=0;
        for (lu = proj_list_units(); lu->id ; ++lu) {
            SET_STRING_ELT(VECTOR_ELT(ans, 0), n, 
		COPY_TO_USER_STRING(lu->id));
            SET_STRING_ELT(VECTOR_ELT(ans, 1), n, 
		COPY_TO_USER_STRING(lu->to_meter));
            SET_STRING_ELT(VECTOR_ELT(ans, 2), n, 
		COPY_TO_USER_STRING(lu->name));
            n++;
        }
#endif
    } else error("no such type");
    
    UNPROTECT(pc);
    return(ans);
}

SEXP get_proj_search_path(void) {

    SEXP res;
    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0, COPY_TO_USER_STRING(proj_info().searchpath));
    UNPROTECT(1);
    return(res);

}

SEXP get_proj_user_writable_dir() {

#if ((PROJ_VERSION_MAJOR == 7 && PROJ_VERSION_MINOR >= 1) || PROJ_VERSION_MAJOR > 7)
    SEXP res;
    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0, COPY_TO_USER_STRING(proj_context_get_user_writable_directory(PJ_DEFAULT_CTX, false)));
    UNPROTECT(1);
    return(res);
#else
    return(R_NilValue);
#endif

}


SEXP set_proj_paths(SEXP paths) {
    //PJ_CONTEXT *ctx = proj_context_create();
    SEXP res;
    int i, n = length(paths);
    char **paths_loc = (char **) R_alloc((size_t) n, sizeof(char*));
    for (i=0; i<n; i++) paths_loc[i] = (char *) CHAR(STRING_ELT(paths, i));
//    const char *paths = CHAR(STRING_ELT(path, 0));
    proj_context_set_search_paths(PJ_DEFAULT_CTX, n, (const char* const*) paths_loc);
    if (int this_errno = proj_context_errno(PJ_DEFAULT_CTX) != 0) {
        const char *errstr = proj_errno_string(this_errno);
        //proj_context_destroy(ctx);
	error("setting search path failed: %s", errstr);
    }
    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0, COPY_TO_USER_STRING(proj_info().searchpath));
    UNPROTECT(1);
    //proj_context_destroy(ctx);
    return(res);
}

SEXP get_source_crs(SEXP source) {

    PJ_CONTEXT *ctx = proj_context_create();
    PJ *source_crs, *target_crs;
    SEXP res;

    source_crs = proj_create(ctx, CHAR(STRING_ELT(source, 0)));

    if (source_crs == NULL) {
        proj_context_destroy(ctx);
        error("source crs not created");
    }

    target_crs = proj_get_source_crs(ctx, source_crs);

    if (target_crs == NULL) {
        proj_context_destroy(ctx);
        error("target crs not created");
    }

    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0,
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3
        COPY_TO_USER_STRING(proj_as_wkt(ctx, target_crs, PJ_WKT2_2018, NULL))
#else
        COPY_TO_USER_STRING(proj_as_wkt(ctx, target_crs, PJ_WKT2_2019, NULL))
#endif
    );
    UNPROTECT(1);
    proj_destroy(target_crs);
    proj_destroy(source_crs);
    proj_context_destroy(ctx);

    return(res);


}

SEXP proj_vis_order(SEXP wkt2) {

#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3
    warning("no CRS normalization available before PROJ 6.3");
    return(wkt2);
#else

    PJ_CONTEXT *ctx = proj_context_create();
    PJ *source_crs, *target_crs;
    SEXP res;

    source_crs = proj_create(ctx, CHAR(STRING_ELT(wkt2, 0)));

    if (source_crs == NULL) {
        proj_context_destroy(ctx);
        error("proj_vis_order: source crs not created");
    }
    target_crs = proj_normalize_for_visualization(ctx, source_crs);
    if (target_crs == NULL) {
        proj_context_destroy(ctx);
        error("proj_vis_order: target crs not created");
    }

    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0,
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3
        COPY_TO_USER_STRING(proj_as_wkt(ctx, target_crs, PJ_WKT2_2018, NULL))
#else
        COPY_TO_USER_STRING(proj_as_wkt(ctx, target_crs, PJ_WKT2_2019, NULL))
#endif
    );
    UNPROTECT(1);
    proj_destroy(target_crs);
    proj_destroy(source_crs);
    proj_context_destroy(ctx);

    return(res);
#endif
}

// unname(sapply(o[[2]], function(x) gsub(" ", " +", paste0("+", x))))

SEXP list_coordinate_ops(SEXP source, SEXP target, SEXP area_of_interest, SEXP strict_containment, SEXP vis_order) {

    PJ_CONTEXT *ctx = proj_context_create();
    PJ_OPERATION_FACTORY_CONTEXT* operation_factory_context = NULL;
    PJ_OBJ_LIST *pj_operations = NULL;
    PJ *source_crs, *target_crs;
//    PJ* pj_transform = NULL;
    SEXP ans, input;
    int num_operations, i, j, is_instantiable, is_ballpark, grid_count;
    int pc=0;
    double accuracy;
    int grid_OK, out_direct_download, out_open_license, out_available;
    const char *out_short_name, *out_full_name, *out_package_name, *out_url;
    PJ_PROJ_INFO pjinfo;

// valgrind 210120
    source_crs = proj_create(ctx, CHAR(STRING_ELT(source, 0)));

    if (source_crs == NULL) {
        proj_context_destroy(ctx);
        error("source crs not created");
    }
    
// valgrind 210120
    target_crs = proj_create(ctx, CHAR(STRING_ELT(target, 0)));

    if (target_crs == NULL) {
        proj_destroy(source_crs);
        proj_context_destroy(ctx);
        error("target crs not created");
    }

    operation_factory_context = proj_create_operation_factory_context(ctx,
        NULL);
    if (operation_factory_context == NULL) {
        proj_destroy(source_crs); proj_destroy(target_crs);
        proj_context_destroy(ctx);
        error("operation factory context not created");
    }

    if (!ISNA(NUMERIC_POINTER(area_of_interest)[0])) {
        proj_operation_factory_context_set_area_of_interest(
            ctx, operation_factory_context,
            NUMERIC_POINTER(area_of_interest)[0],
            NUMERIC_POINTER(area_of_interest)[1],
            NUMERIC_POINTER(area_of_interest)[2],
            NUMERIC_POINTER(area_of_interest)[3]);
    }

    if (LOGICAL_POINTER(strict_containment)[0])
        proj_operation_factory_context_set_spatial_criterion(
            ctx, operation_factory_context,
            PROJ_SPATIAL_CRITERION_STRICT_CONTAINMENT);
    else proj_operation_factory_context_set_spatial_criterion(
            ctx, operation_factory_context,
            PROJ_SPATIAL_CRITERION_PARTIAL_INTERSECTION);

    proj_operation_factory_context_set_grid_availability_use(
            ctx, operation_factory_context,
            PROJ_GRID_AVAILABILITY_USED_FOR_SORTING);

// valgrind 210120
    pj_operations = proj_create_operations(ctx,
                source_crs, target_crs, operation_factory_context);
    
    if (pj_operations == NULL) {
        proj_operation_factory_context_destroy(operation_factory_context);
        proj_destroy(source_crs); proj_destroy(target_crs);
        proj_context_destroy(ctx);
        error("operations list not created");
    }

    num_operations = proj_list_get_count(pj_operations);

    if (num_operations < 1L) {
        proj_list_destroy(pj_operations);
        proj_operation_factory_context_destroy(operation_factory_context);
        proj_destroy(source_crs); proj_destroy(target_crs);
        proj_context_destroy(ctx);
        return(R_NilValue);
    }

    PROTECT(ans=NEW_LIST(7)); pc++;
    SET_VECTOR_ELT(ans, 0, NEW_CHARACTER(num_operations));
    SET_VECTOR_ELT(ans, 1, NEW_CHARACTER(num_operations));
    SET_VECTOR_ELT(ans, 2, NEW_NUMERIC(num_operations));
    SET_VECTOR_ELT(ans, 3, NEW_LOGICAL(num_operations));
    SET_VECTOR_ELT(ans, 4, NEW_LOGICAL(num_operations));
    SET_VECTOR_ELT(ans, 5, NEW_INTEGER(num_operations));
    SET_VECTOR_ELT(ans, 6, NEW_LIST(num_operations));

    PROTECT(input=NEW_LIST(5)); pc++;
    SET_VECTOR_ELT(input, 0, source);
    SET_VECTOR_ELT(input, 1, target);
    SET_VECTOR_ELT(input, 2, area_of_interest);
    SET_VECTOR_ELT(input, 3, strict_containment);
    SET_VECTOR_ELT(input, 4, vis_order);
    setAttrib(ans, install("input"), input);


    for (i=0; i<num_operations; i++) {
        PJ * pj_transform = proj_list_get(ctx, pj_operations, i);
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 1
        warning("no Coordinate Operation normalization available before PROJ 6.1");
#else
        if (LOGICAL_POINTER(vis_order)[0]) {
            PJ* pj_trans_orig = pj_transform;
            pj_transform = proj_normalize_for_visualization(ctx, pj_trans_orig);
            proj_destroy(pj_trans_orig);
        }
#endif
        is_instantiable = proj_coordoperation_is_instantiable(ctx,
            pj_transform);
        is_ballpark = proj_coordoperation_has_ballpark_transformation(ctx,
            pj_transform);
        accuracy = proj_coordoperation_get_accuracy(ctx,
            pj_transform);
        grid_count = proj_coordoperation_get_grid_used_count(ctx,
            pj_transform);
        pjinfo = proj_pj_info(pj_transform);

        SET_STRING_ELT(VECTOR_ELT(ans, 0), i,
            COPY_TO_USER_STRING(pjinfo.description));
        SET_STRING_ELT(VECTOR_ELT(ans, 1), i,
            COPY_TO_USER_STRING(pjinfo.definition));

        NUMERIC_POINTER(VECTOR_ELT(ans, 2))[i] = accuracy;
        LOGICAL_POINTER(VECTOR_ELT(ans, 3))[i] = is_instantiable;
        LOGICAL_POINTER(VECTOR_ELT(ans, 4))[i] = is_ballpark;
        INTEGER_POINTER(VECTOR_ELT(ans, 5))[i] = grid_count;

        if (grid_count > 0L) {
            SET_VECTOR_ELT(VECTOR_ELT(ans, 6), i, NEW_LIST(grid_count));
            for (j=0; j<grid_count; j++) {
                grid_OK = proj_coordoperation_get_grid_used(ctx, pj_transform,
                    j, &out_short_name, &out_full_name, &out_package_name,
                    &out_url, &out_direct_download, &out_open_license,
                    &out_available);
                 if (grid_OK) {
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(ans, 6), i), j,
                         NEW_LIST(7));
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 0, NEW_CHARACTER(1));
                     SET_STRING_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 0), 0,
                         COPY_TO_USER_STRING(out_short_name));
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 1, NEW_CHARACTER(1));
                     SET_STRING_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 1), 0, 
                         COPY_TO_USER_STRING(out_full_name));
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 2, NEW_CHARACTER(1));
                     SET_STRING_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 2), 0, 
                         COPY_TO_USER_STRING(out_package_name));
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 3, NEW_CHARACTER(1));
                     SET_STRING_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 3), 0, 
                         COPY_TO_USER_STRING(out_url));
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 4, NEW_LOGICAL(1));
                     LOGICAL_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 4))[0] = 
                         out_direct_download;
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 5, NEW_LOGICAL(1));
                     LOGICAL_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 5))[0] = 
                         out_open_license;
                     SET_VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT( ans, 6),
                         i), j), 6, NEW_LOGICAL(1));
                     LOGICAL_POINTER(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(
                         VECTOR_ELT(ans, 6), i), j), 6))[0] = 
                         out_available;

                 } 
            }
        }
        
        proj_destroy(pj_transform);

    }

//    proj_destroy(pj_transform);
    proj_list_destroy(pj_operations);
    proj_operation_factory_context_destroy(operation_factory_context);
    proj_destroy(source_crs); proj_destroy(target_crs);
    proj_context_destroy(ctx);

    UNPROTECT(pc);
    return(ans);

}

SEXP CRS_compare(SEXP fromargs, SEXP toargs, SEXP type1, SEXP type2) {

    //PJ_CONTEXT *ctx = proj_context_create();
    PJ *source_crs, *target_crs;
    SEXP res;
    int ires_strict, ires_equiv, ires_equiv_ao;
//Rprintf("source crs input: %s\n", CHAR(STRING_ELT(fromargs, 0)));
    if ((source_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(fromargs, 0)))) == NULL) {
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
        //proj_context_destroy(ctx);
	error("source crs creation failed: %s", errstr);
    }
	
//Rprintf("source crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, source_crs, PJ_PROJ_5, NULL)); not filled for WKT
//Rprintf("target crs input:  %s\n", CHAR(STRING_ELT(toargs, 0)));
    if ((target_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(toargs, 0)))) == NULL) {
        proj_destroy(source_crs);
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
        //proj_context_destroy(ctx);
        error("target crs creation failed: %s", errstr);
    }
//Rprintf("target crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, target_crs, PJ_PROJ_5, NULL)); not filled for WKT
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3
    ires_strict = proj_is_equivalent_to(source_crs, target_crs,
        PJ_COMP_STRICT);
    ires_equiv = proj_is_equivalent_to(source_crs, target_crs,
        PJ_COMP_EQUIVALENT);
    ires_equiv_ao = proj_is_equivalent_to(source_crs, target_crs,
        PJ_COMP_EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS);
#else
    ires_strict = proj_is_equivalent_to_with_ctx(PJ_DEFAULT_CTX, source_crs, target_crs,
        PJ_COMP_STRICT);
    ires_equiv = proj_is_equivalent_to_with_ctx(PJ_DEFAULT_CTX, source_crs, target_crs,
        PJ_COMP_EQUIVALENT);
    ires_equiv_ao = proj_is_equivalent_to_with_ctx(PJ_DEFAULT_CTX, source_crs, target_crs,
        PJ_COMP_EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS);
#endif
//Rprintf("ires: %d, iresao\n", ires);
    PROTECT(res = NEW_INTEGER(3));
    INTEGER_POINTER(res)[0] = ires_strict;
    INTEGER_POINTER(res)[1] = ires_equiv;
    INTEGER_POINTER(res)[2] = ires_equiv_ao;
    proj_destroy(target_crs);
    proj_destroy(source_crs);
    //proj_context_destroy(ctx);
    UNPROTECT(1);
    return(res);
}

SEXP transform_ng(SEXP fromargs, SEXP toargs, SEXP coordOp, SEXP npts, SEXP x, SEXP y, SEXP z, SEXP aoi) {

    //PJ_CONTEXT *ctx = proj_context_create();
    PJ *source_crs = NULL, *target_crs = NULL;
    PJ* pj_transform = NULL;
    PJ_AREA *area_of_interest = 0;

    int i, n, nwarn=0, have_z, pc=0, have_CO, vis_order, use_aoi=1;
    double *xx, *yy, *zz=NULL;
    SEXP enforce_xy = getAttrib(npts, install("enforce_xy"));
    SEXP res;

    if (coordOp == R_NilValue) have_CO = 0;
    else have_CO = 1;

    if (z == R_NilValue) have_z = 0;
    else have_z = 1;


    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;

    if (aoi == R_NilValue) use_aoi = 0;

    if (!have_CO && use_aoi) {
        area_of_interest = proj_area_create();
        proj_area_set_bbox(area_of_interest, NUMERIC_POINTER(aoi)[0],
            NUMERIC_POINTER(aoi)[1], NUMERIC_POINTER(aoi)[2],
            NUMERIC_POINTER(aoi)[3]);
    }
//        proj_area_destroy(area_of_interest);


	
//Rprintf("have_CO: %d, have_z: %d: %d\n", have_CO, have_z);

    if (have_CO) {
//Rprintf("coordinate operation input: %s\n", CHAR(STRING_ELT(coordOp, 0)));
        if ((pj_transform = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(coordOp, 0)))) == 0) {
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
	    error("coordinate operation creation failed: %s", errstr);
        }
    } else {
//Rprintf("source crs input: %s\n", CHAR(STRING_ELT(fromargs, 0)));
    	if ((source_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(fromargs, 0)))) == NULL) {
            proj_area_destroy(area_of_interest);
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
	    error("source crs creation failed: %s", errstr);
        }
	
//Rprintf("source crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, source_crs, PJ_PROJ_5, NULL)); 
//Rprintf("target crs input:  %s\n", CHAR(STRING_ELT(toargs, 0)));
	if ((target_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(toargs, 0)))) == NULL) {
            proj_area_destroy(area_of_interest);
            proj_destroy(source_crs);
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
	    error("target crs creation failed: %s", errstr);
        }
//Rprintf("target crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, target_crs, PJ_PROJ_5, NULL)); 
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 2
        if ((pj_transform = proj_create_crs_to_crs(PJ_DEFAULT_CTX,
            CHAR(STRING_ELT(fromargs, 0)),
            CHAR(STRING_ELT(toargs, 0)), area_of_interest)) == 0) {
// FIXME >= 6.2.0
            proj_area_destroy(area_of_interest);
            proj_destroy(target_crs);
            proj_destroy(source_crs);
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
	    error("coordinate operation creation from WKT failed: %s", errstr);
        }

#else
        if ((pj_transform = proj_create_crs_to_crs_from_pj(PJ_DEFAULT_CTX, 
            source_crs, target_crs, area_of_interest, NULL)) == 0) {
// FIXME >= 6.2.0
            proj_area_destroy(area_of_interest);
            proj_destroy(target_crs);
            proj_destroy(source_crs);
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
	    error("coordinate operation creation from WKT failed: %s", errstr);
        }
#endif
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 1
    warning("no Coordinate Operation normalization available before PROJ 6.1");
#else
        if (vis_order == 1) {
            PJ* pj_trans_orig = pj_transform;
            pj_transform = proj_normalize_for_visualization(PJ_DEFAULT_CTX, pj_trans_orig);
            proj_destroy(pj_trans_orig);
        }
#endif
    }
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);
//	if (proj_normalize_for_visualization(ctx, pj_transform) != NULL) // EJP
//		pj_transform = proj_normalize_for_visualization(ctx, pj_transform); // EJP
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);

    n = INTEGER_POINTER(npts)[0];
    xx = (double *) R_alloc((size_t) n, sizeof(double));
    yy = (double *) R_alloc((size_t) n, sizeof(double));
    if (have_z) zz = (double *) R_alloc((size_t) n, sizeof(double));

    for (i=0; i < n; i++) {
        xx[i] = NUMERIC_POINTER(x)[i];
	yy[i] = NUMERIC_POINTER(y)[i];
	if (have_z) zz[i] = NUMERIC_POINTER(z)[i];
    }
/*	if (ob_tran == 1) {
		for (i=0; i < n; i++) {
       			 xx[i] = proj_torad(xx[i]);
       			 yy[i] = proj_torad(yy[i]);
		}
	}*/
    size_t stride = sizeof(double), n1;
// FIXME handle out-of-domain output https://github.com/r-spatial/sf/issues/1223
    if (have_z) {
        if((n1 = proj_trans_generic(pj_transform, PJ_FWD, xx, stride,
            (size_t) n, yy, stride, (size_t) n, zz, stride, (size_t) n,
            NULL, stride, 0)) != (size_t) n) {
            proj_destroy(pj_transform);
            if (!have_CO) {
                proj_area_destroy(area_of_interest);
                proj_destroy(target_crs);
                proj_destroy(source_crs);
            }
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            //proj_context_destroy(ctx);
            error("error in proj_transform_generic: %n of %n coordinates succeeded\n  %s", n1, n, errstr);
	    }
    } else {
      if((n1 = proj_trans_generic(pj_transform, PJ_FWD, xx, stride,
          (size_t) n, yy, stride, (size_t) n, NULL, stride, 0,
          NULL, stride, 0)) != (size_t) n) {
          proj_destroy(pj_transform);
          if (!have_CO) {
              proj_area_destroy(area_of_interest);
              proj_destroy(target_crs);
              proj_destroy(source_crs);
          }
          const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
          //proj_context_destroy(ctx);
          error("error in proj_transform_generic: %n of %n coordinates succeeded\n  %s", n1, n, errstr);
        }
    }
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);
    if (have_z) {PROTECT(res = NEW_LIST(6)); pc++;}
    else {PROTECT(res = NEW_LIST(5)); pc++;}
    SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
    SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
    if (have_z) {
        SET_VECTOR_ELT(res, 2, NEW_NUMERIC(n));
        SET_VECTOR_ELT(res, 5, NEW_CHARACTER(1));
    } else {
        SET_VECTOR_ELT(res, 4, NEW_CHARACTER(1));
    }

    if (have_z) {
        if (have_CO) {
            SET_VECTOR_ELT(res, 3, fromargs);
            SET_VECTOR_ELT(res, 4, toargs);
        } else {
            SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
            SET_VECTOR_ELT(res, 4, NEW_CHARACTER(1));
            SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(proj_pj_info(source_crs).definition));
	    SET_STRING_ELT(VECTOR_ELT(res, 4), 0, 
		COPY_TO_USER_STRING(proj_pj_info(target_crs).definition));
        }
	SET_STRING_ELT(VECTOR_ELT(res, 5), 0, 
	    COPY_TO_USER_STRING(proj_pj_info(pj_transform).definition));
    } else {
        if (have_CO) {
            SET_VECTOR_ELT(res, 2, fromargs);
            SET_VECTOR_ELT(res, 3, toargs);
        } else {
            SET_VECTOR_ELT(res, 2, NEW_CHARACTER(1));
            SET_VECTOR_ELT(res, 3, NEW_CHARACTER(1));
	    SET_STRING_ELT(VECTOR_ELT(res, 2), 0, 
		COPY_TO_USER_STRING(proj_pj_info(source_crs).definition));
	    SET_STRING_ELT(VECTOR_ELT(res, 3), 0, 
		COPY_TO_USER_STRING(proj_pj_info(target_crs).definition));
	}
        SET_STRING_ELT(VECTOR_ELT(res, 4), 0, 
	    COPY_TO_USER_STRING(proj_pj_info(pj_transform).definition));
    }

/*	if ( ob_tran == -1) {
		for (i=0; i < n; i++) {
               		xx[i] = proj_todeg(xx[i]);
               		yy[i] = proj_todeg(yy[i]);
            	}
	}*/

    proj_destroy(pj_transform);
    if (!have_CO) {
        proj_area_destroy(area_of_interest);
        proj_destroy(target_crs);
        proj_destroy(source_crs);
    }
    //proj_context_destroy(ctx);

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
    UNPROTECT(pc);
    return(res);
}

SEXP project_ng_coordOp(SEXP proj, SEXP inv, SEXP aoi, SEXP ob_tran) {

    //PJ_CONTEXT *ctx = proj_context_create();
    PJ *source_crs, *target_crs;
    PJ* pj_transform = NULL;
    PJ_AREA *area_of_interest = 0;
    int use_ob_tran = LOGICAL_POINTER(ob_tran)[0], 
        use_inv, use_aoi=1;

    proj_log_func(PJ_DEFAULT_CTX, NULL, silent_logger);

/*    if (ob_tran == R_NilValue) use_ob_tran = 0;
    else if (LOGICAL_POINTER(ob_tran)[0] == 1) use_ob_tran = 1;
    else if (LOGICAL_POINTER(ob_tran)[0] == 0) use_ob_tran = 0;
    else use_ob_tran = 0;*/

    if (inv == R_NilValue) use_inv = 0;
    else if (LOGICAL_POINTER(inv)[0] == 1) use_inv = 1;
    else if (LOGICAL_POINTER(inv)[0] == 0) use_inv = 0;
    else use_inv = 0;

    if (aoi == R_NilValue) use_aoi = 0;
    

//Rprintf("target crs input: %s\n", CHAR(STRING_ELT(proj, 0)));
    if ((target_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(proj, 0)))) == 0) {
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
        //proj_context_destroy(ctx);
	error("target crs creation failed: %s", errstr);
    }
	
//Rprintf("target crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, target_crs, PJ_PROJ_5, NULL)); 
    if (proj_get_type(target_crs) == PJ_TYPE_GEOGRAPHIC_2D_CRS && use_ob_tran) {
        if ((source_crs = proj_get_source_crs(PJ_DEFAULT_CTX, target_crs)) == 0) {
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            proj_destroy(target_crs);
            //proj_context_destroy(ctx);
            error("source crs creation failed: %s", errstr);
        }
    } else {
        if ((source_crs = proj_crs_get_geodetic_crs(PJ_DEFAULT_CTX, target_crs)) == 0) {
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            proj_destroy(target_crs);
            //proj_context_destroy(ctx);
            error("source crs creation failed: %s", errstr);
        }
    }
//Rprintf("source crs: %s\n", proj_as_proj_string(PJ_DEFAULT_CTX, source_crs, PJ_PROJ_5, NULL)); 

    if (use_aoi) {
        area_of_interest = proj_area_create();
        proj_area_set_bbox(area_of_interest, NUMERIC_POINTER(aoi)[0],
            NUMERIC_POINTER(aoi)[1], NUMERIC_POINTER(aoi)[2],
            NUMERIC_POINTER(aoi)[3]);
    }
//Rprintf("use_inv: %d\n", use_inv);
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 2
    if (use_inv) pj_transform = proj_create_crs_to_crs(PJ_DEFAULT_CTX, 
        proj_as_wkt(PJ_DEFAULT_CTX, target_crs, PJ_WKT2_2018, NULL),
        proj_as_wkt(PJ_DEFAULT_CTX, source_crs, PJ_WKT2_2018, NULL),
        area_of_interest);
    else pj_transform = proj_create_crs_to_crs(PJ_DEFAULT_CTX,
        proj_as_wkt(PJ_DEFAULT_CTX, source_crs, PJ_WKT2_2018, NULL),
        proj_as_wkt(PJ_DEFAULT_CTX, target_crs, PJ_WKT2_2018, NULL),
        area_of_interest);
#else
    if (use_inv) pj_transform = proj_create_crs_to_crs_from_pj(PJ_DEFAULT_CTX, target_crs,
        source_crs, area_of_interest, NULL);
    else pj_transform = proj_create_crs_to_crs_from_pj(PJ_DEFAULT_CTX, source_crs,
        target_crs, area_of_interest, NULL);
// FIXME >= 6.2.0
#endif
    if (pj_transform == 0) {
        proj_area_destroy(area_of_interest);
        proj_destroy(target_crs);
        proj_destroy(source_crs);
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
        //proj_context_destroy(ctx);
        error("coordinate operation creation failed: %s", errstr);
    }
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 1
    warning("no Coordinate Operation normalization available before PROJ 6.1");
#else
    PJ* pj_trans_orig = pj_transform;
    pj_transform = proj_normalize_for_visualization(PJ_DEFAULT_CTX, pj_trans_orig);
    proj_destroy(pj_trans_orig);
#endif
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);

    SEXP res;
    PROTECT(res = NEW_CHARACTER(1));
    SET_STRING_ELT(res, 0,
        COPY_TO_USER_STRING(proj_pj_info(pj_transform).definition));
    UNPROTECT(1);
    proj_destroy(pj_transform);
    proj_area_destroy(area_of_interest);
    proj_destroy(target_crs);
    proj_destroy(source_crs);
    //proj_context_destroy(ctx);

    return(res);
}

SEXP project_ng(SEXP n, SEXP xlon, SEXP ylat, SEXP zz, SEXP coordOp) {
    int i, nwarn=0, nn=INTEGER_POINTER(n)[0];
    SEXP res;
    //PJ_CONTEXT *ctx = proj_context_create();
    PJ* pj_transform = NULL;
    PJ_COORD a, b;
    double ixlon, iylat, iz=0.0;

    proj_log_func(PJ_DEFAULT_CTX, NULL, silent_logger);

    if ((pj_transform = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(coordOp, 0)))) == 0) {
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
        //proj_context_destroy(ctx);
        error("coordinate operation creation failed: %s", errstr);
    }
//Rprintf("%s\n", proj_pj_info(pj_transform).definition);
    
    if (zz  != R_NilValue) {
        PROTECT(res = NEW_LIST(3));
        SET_VECTOR_ELT(res, 2, NEW_NUMERIC(nn));
    } else {
        PROTECT(res = NEW_LIST(2));
    }
    SET_VECTOR_ELT(res, 0, NEW_NUMERIC(nn));
    SET_VECTOR_ELT(res, 1, NEW_NUMERIC(nn));

    for (i=0; i<nn; i++) {
        ixlon = NUMERIC_POINTER(xlon)[i];
        iylat = NUMERIC_POINTER(ylat)[i];
        if (zz  != R_NilValue) iz = NUMERIC_POINTER(zz)[i];
// preserve NAs and NaNs. Allow Infs, since maybe proj can handle them. 
        if (ISNAN(ixlon) || ISNAN(iylat)) {
            NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=ixlon;
            NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=iylat;
        } else {
            a = proj_coord(ixlon, iylat, iz, 0);
            b = proj_trans(pj_transform, PJ_FWD, a);
            if (b.uv.u == HUGE_VAL || ISNAN(b.uv.u) || b.uv.v == HUGE_VAL || 
                ISNAN(b.uv.v)) {
                nwarn++;
            }
            NUMERIC_POINTER(VECTOR_ELT(res, 0))[i]=b.uv.u;
            NUMERIC_POINTER(VECTOR_ELT(res, 1))[i]=b.uv.v;
            if (zz  != R_NilValue)
                NUMERIC_POINTER(VECTOR_ELT(res, 2))[i]=b.uvw.w;
        }
    }
    if (nwarn > 0) warning("%d projected point(s) not finite", nwarn);

    proj_destroy(pj_transform);
    //proj_context_destroy(ctx);

    UNPROTECT(1);
    return(res);
}

SEXP P6_SRID_proj(SEXP inSRID, SEXP format, SEXP multiline, SEXP in_format,
    SEXP epsg, SEXP out_format) {

    SEXP ans;
    SEXP Datum, Ellps;
    int pc=0;
    int vis_order;
    SEXP enforce_xy = getAttrib(in_format, install("enforce_xy"));
    const char *pszSRS = NULL;

    if (enforce_xy == R_NilValue) vis_order = 0;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 1) vis_order = 1;
    else if (LOGICAL_POINTER(enforce_xy)[0] == 0) vis_order = 0;
    else vis_order = 0;

    PJ *source_crs = proj_create(PJ_DEFAULT_CTX, CHAR(STRING_ELT(inSRID, 0)));
    // valgrind resolved by copying EJP 
    // https://github.com/r-spatial/gstat/issues/82#issuecomment-762970800
    if (source_crs == NULL) {
        const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
//        proj_context_destroy(ctx);
	error("source crs creation failed: %s", errstr);
    }

#if (PROJ_VERSION_MAJOR > 6 || (PROJ_VERSION_MAJOR == 6 && PROJ_VERSION_MINOR >= 1))
    PJ_TYPE type;
    type = proj_get_type(source_crs);

    if (type == PJ_TYPE_BOUND_CRS) {
        PJ *orig_crs = source_crs;
        source_crs = proj_get_source_crs(PJ_DEFAULT_CTX, orig_crs);
        proj_destroy(orig_crs);
        if (source_crs == NULL) {
//            proj_context_destroy(ctx);
            error("crs not converted to source only");
        }
    }
#endif

#if (PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3)
    warning("no CRS normalization available before PROJ 6.3");
#else
    if (vis_order) {
        PJ *orig_crs = source_crs;
        source_crs = proj_normalize_for_visualization(PJ_DEFAULT_CTX, orig_crs);
        proj_destroy(orig_crs);
        if (source_crs == NULL) {
//            proj_context_destroy(ctx);
            error("crs not converted to visualization order");
        }
    }
#endif
// FIXME PROJ 8.0 datum ensemble vulnerability
    PJ* dtm = proj_crs_get_datum(PJ_DEFAULT_CTX, source_crs);
    if (dtm != NULL) {
        PROTECT(Datum = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(Datum, 0, COPY_TO_USER_STRING(proj_get_name(dtm)));
        proj_destroy(dtm);
    } else {
        Datum = R_NilValue;
    }

    PJ* ellps = proj_get_ellipsoid(PJ_DEFAULT_CTX, source_crs);
    if (ellps != NULL) {
        PROTECT(Ellps = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(Ellps, 0, COPY_TO_USER_STRING(proj_get_name(ellps)));
        proj_destroy(ellps);
    } else {
        Ellps = R_NilValue;
    }



    PROTECT(ans=NEW_CHARACTER(1)); pc++;

    if (INTEGER_POINTER(out_format)[0] == 1L) {
        
#if PROJ_VERSION_MAJOR == 6  && PROJ_VERSION_MINOR < 3
        if ((pszSRS = proj_as_wkt(PJ_DEFAULT_CTX, source_crs, PJ_WKT2_2018, NULL))
            == NULL) {
#else
        if ((pszSRS = proj_as_wkt(PJ_DEFAULT_CTX, source_crs, PJ_WKT2_2019, NULL))
            == NULL) {
#endif
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            warning("export to WKT2 failed: %s", errstr);
            SET_STRING_ELT(ans, 0, NA_STRING);
	} else {
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS));
        }
    } else if (INTEGER_POINTER(out_format)[0] == 2L) {
        
        if ((pszSRS = proj_as_proj_string(PJ_DEFAULT_CTX, source_crs, PJ_PROJ_4, NULL)) 
            == NULL) {
            const char *errstr = proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX));
            warning("export to PROJ failed: %s", errstr);
            SET_STRING_ELT(ans, 0, NA_STRING);
	} else {
            SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(pszSRS));
        }
    } else {
        proj_destroy(source_crs);
//        proj_context_destroy(ctx);
        UNPROTECT(pc);
        error("unknown output format");
    }
    setAttrib(ans, install("datum"), Datum);
    setAttrib(ans, install("ellps"), Ellps);


    proj_destroy(source_crs);
//    proj_context_destroy(ctx);
    UNPROTECT(pc);

    return(ans);

}



// blocks error messages in this context
// https://lists.osgeo.org/pipermail/proj/2019-March/008310.html
static void proj_logger(void * user_data, int level, const char * message) {}

// code borrowed from GRASS g.proj main.c adapted for PROJ6 by Markus Metz

SEXP
PROJcopyEPSG(SEXP tf) {

    SEXP ans;
    PROTECT(ans=NEW_INTEGER(1));
    INTEGER_POINTER(ans)[0] = 0;
    int i, crs_cnt;
    PROJ_CRS_INFO **proj_crs_info;
    PJ_CONTEXT *ctx = proj_context_create();
    FILE *fptf;

	
    crs_cnt = 0;
    proj_crs_info = proj_get_crs_info_list_from_database(ctx, "EPSG", NULL,
        &crs_cnt);
    if (crs_cnt < 1) {
        UNPROTECT(1);
        return(ans);
    }
    fptf = fopen(CHAR(STRING_ELT(tf, 0)), "wb");
    if (fptf == NULL) {
        UNPROTECT(1);
        return(ans);
    }
    fprintf(fptf, "\"code\",\"note\",\"prj4\",\"prj_method\"\n");

//    PJ *pj = NULL;
// blocks error messages in this context
    proj_log_func(ctx, NULL, proj_logger);
    for (i = 0; i < crs_cnt; i++) {
        const char *proj_definition;

        PJ* pj = proj_create_from_database(ctx,
            proj_crs_info[i]->auth_name, proj_crs_info[i]->code,
            PJ_CATEGORY_CRS, 0, NULL);
        proj_definition = proj_as_proj_string(ctx, pj, // valgrind
            PJ_PROJ_5, NULL);
//        proj_destroy(pj); // valgrind

        fprintf(fptf, "%s,\"%s\",\"%s\",\"%s\"\n", proj_crs_info[i]->code, //ASAN
  	    proj_crs_info[i]->name, proj_definition, 
            proj_crs_info[i]->projection_method_name);
        proj_destroy(pj);
    }

    fclose(fptf);
    proj_crs_info_list_destroy(proj_crs_info);
    proj_context_destroy(ctx);
    INTEGER_POINTER(ans)[0] = crs_cnt;
    UNPROTECT(1);

    return(ans);

}

#ifdef __cplusplus
}
#endif
#endif // PROJ_H_API


