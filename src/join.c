#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/*
@param x: sorted `numeric`.
@param y: sorted `numeric`.
@param tolerance: `numeric` of length equal `length(y)` with accepted difference
*/
SEXP C_join_outer(SEXP x, SEXP y, SEXP tolerance) {
  int lx, ly, idx, i, j, nj, *ptresx, *ptresy;
  double *px, *py, *tol, maxdiff, lastdiff, idiff;
  SEXP resx, resy, output, names, tresx, tresy;
  lx = LENGTH(x);
  ly = LENGTH(y);
  px = REAL(x);
  py = REAL(y);
  tol = REAL(tolerance);
  if (ly != LENGTH(tolerance))
    error("'tolerance' has to be of length 1 or length equal to 'length(y)'");
  PROTECT(resx = allocVector(INTSXP, lx + ly));
  PROTECT(resy = allocVector(INTSXP, lx + ly));
  int *presx = INTEGER(resx);
  int *presy = INTEGER(resy);
  double xdiff;
  double ydiff;
  idx = -1;
  i = 0;
  j = 0;

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
    // compare elements
    lastdiff = fabs(px[i] - py[j]);
    if (lastdiff <= tol[j]) {
      // If we just add the map here we would simply take the first match.
      // Finding the closest elements is more tricky as we have to perform a
      // *look ahead* search for potentially better matching values - in
      // either x or y direction.
      if (i + 1 < lx) xdiff = fabs(px[(i + 1)] - py[j]);
      else xdiff = lastdiff + 1;
      if (j + 1 < ly) ydiff = fabs(px[i] - py[(j + 1)]);
      else ydiff = lastdiff + 1;
      if (xdiff < lastdiff || ydiff < lastdiff) {
	// Now go into the direction of the smallest difference.
	if (xdiff < ydiff) {
	  i++;
	  // same for i.
	  while (i < lx && (idiff = fabs(px[i] - py[j])) < lastdiff) {
	    lastdiff = idiff;
	    presx[idx] = i;
	    presy[idx] = NA_INTEGER;
	    i++;
	    idx++;
	  }
	  j++;
	} else {
	  j++;
	  // check if distance to next j might be smaller...
	  while (j < ly && (idiff = fabs(px[i] - py[j])) < lastdiff) {
	    lastdiff = idiff;
	    presx[idx] = NA_INTEGER;
	    presy[idx] = j;
	    j++;
	    idx++;
	  }
	  i++;
	}
      } else {
	i++;
	j++;
      }
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

// Alternative:
// all pairwise differences.
// order them.
// from smalles first:
// check if smaller tolerance.
// check if already used (second boolean).
// add them to resx and resy at position idx. add also the sum of both to a
// second array that we might use to bring them in the right order.