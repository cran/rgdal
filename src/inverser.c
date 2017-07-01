/* Copyright (c) 2016 Barry Rowlingson */

#if PJ_VERSION < 493
#include <projects.h>

int inversetest(PJ *P){
  return (P->inv ? 1: 0);

}
#endif

//xy <- try(project(cbind(0, 0), "+proj=airy", legacy=TRUE))
//try(project(xy, "+proj=airy", inv=TRUE, legacy=TRUE))
