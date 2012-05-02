#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <cpl_string.h>
#include <cpl_csv.h>
#include <cpl_error.h>

#ifdef __cplusplus
extern "C" {
#endif
#define ROFFSET 1
#define SP_XPORT(x) RGDAL_ ## x
#include "sp.h"

SEXP make_Polygonlist(SEXP iG);

SEXP make_Polygon(SEXP jG);

static CPLErr saved_eErrClass = CE_None;
static int saved_err_no = 0;
static char saved_error_msg[2048];

void installErrorHandler();
void uninstallErrorHandlerAndTriggerError();
#ifdef __cplusplus
}
#endif

