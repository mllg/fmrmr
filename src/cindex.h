#ifndef _fmrmr_cindex_h
#define _fmrmr_cindex_h

#include <RcppEigen.h>
typedef unsigned int size;

RcppExport SEXP cindex(SEXP R_time, SEXP R_status, SEXP R_x);

#endif
