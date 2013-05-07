#ifndef _fmrmr_correlation_h
#define _fmrmr_correlation_h

#include <RcppEigen.h>
typedef unsigned int size;

RcppExport SEXP maxcor(SEXP R_x);
RcppExport SEXP meancor(SEXP R_x);

#endif
