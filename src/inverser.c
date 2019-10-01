/* Copyright (c) 2016 Barry Rowlingson */
typedef int make_iso_compilers_happy;

#ifndef ACCEPT_USE_OF_DEPRECATED_PROJ_API_H
#include <projects.h>
#include <proj_api.h>
#if PJ_VERSION < 493

int inversetest(PJ *P){
  return (P->inv ? 1: 0);

}
#endif
#endif

//xy <- try(project(cbind(0, 0), "+proj=airy", legacy=TRUE))
//try(project(xy, "+proj=airy", inv=TRUE, legacy=TRUE))
