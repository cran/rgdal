#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#define SP_XPORT(x) RGDAL_ ## x
#include "sp.h"

SEXP make_Polygonlist(SEXP iG);

SEXP make_Polygon(SEXP jG);

