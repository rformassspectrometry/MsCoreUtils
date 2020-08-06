#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>

SEXP C_joinOuter(SEXP x, SEXP y, SEXP tolerance, SEXP ppm) {
  int lx, ly, idx, i, j, nj, iprev, *presx, *presy, *ptresx, *ptresy;
  double *px, *py, ppmv, tol, diff;
  SEXP resx, resy, output, names, tresx, tresy;
  lx = LENGTH(x);
  ly = LENGTH(y);
  px = REAL(x);
  py = REAL(y);
  ppmv = REAL(ppm)[0] / 1000000;
  tol = REAL(tolerance)[0];
  PROTECT(resx = allocVector(INTSXP, lx + ly));
  PROTECT(resy = allocVector(INTSXP, lx + ly));
  presx = INTEGER(resx);
  presy = INTEGER(resy);
  idx = -1;
  i = 0;
  j = 0;
  iprev = -1;

  while (i < lx || j < ly) {
    idx++;
    // if one of the two is outside just add the other
    if (i >= lx) {
      j++;
      presx[idx] = NA_INTEGER;
      presy[idx] = j;
      continue;
    }
    if (j >= ly) {
      i++;
      presx[idx] = i;
      presy[idx] = NA_INTEGER;
      continue;
    }

    // only calculate diff if i changed.
    if (iprev != i) {
      diff = ppmv * px[i] + tol;
      iprev = i;
    }
    if (fabs(px[i] - py[j]) <= diff) {
      i++;
      j++;
      presx[idx] = i;
      presy[idx] = j;
    } else {
      if (px[i] < py[j]) {
  	i++;
  	presx[idx] = i;
  	presy[idx] = NA_INTEGER;
      } else {
  	j++;
  	presx[idx] = NA_INTEGER;
  	presy[idx] = j;
      }
    }
  }

  PROTECT(tresx = allocVector(INTSXP, idx + 1));
  PROTECT(tresy = allocVector(INTSXP, idx + 1));
  ptresx = INTEGER(tresx);
  ptresy = INTEGER(tresy);
  for (i = 0; i <= idx; i++) {
    ptresx[i] = presx[i];
    ptresy[i] = presy[i];
  }

  PROTECT(output = allocVector(VECSXP, 2));
  PROTECT(names = allocVector(STRSXP, 2));
  SET_VECTOR_ELT(output, 0, tresx);
  SET_VECTOR_ELT(output, 1, tresy);
  SET_STRING_ELT(names, 0, mkChar("x"));
  SET_STRING_ELT(names, 1, mkChar("y"));
  setAttrib(output, R_NamesSymbol, names);

  UNPROTECT(6);
  return output;
}
