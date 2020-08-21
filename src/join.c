#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <Rmath.h>

/*
@param x: sorted `numeric`.
@param y: sorted `numeric`.
@param tolerance: `numeric` of length equal `length(y)` with accepted difference
*/
SEXP C_join_outer(SEXP x, SEXP y, SEXP tolerance) {
  int lx, ly, idx, i, j, nj, *ptresx, *ptresy;
  double *px, *py, *tol, idiff;
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
    idiff = fabs(px[i] - py[j]);
    if (idiff <= tol[j]) {
      // If we just add the map here we would simply take the first match.
      // Finding the closest elements is more tricky as we have to perform a
      // *look ahead* search for potentially better matching values - in
      // either x or y direction. The decision to look ahead into x or y depends
      // on the difference to the next elements: we choose the road in which
      // the elements have the smallest difference < idiff.
      if (i + 1 < lx) xdiff = fabs(px[(i + 1)] - py[j]);
      else xdiff = R_PosInf;
      if (j + 1 < ly) ydiff = fabs(px[i] - py[(j + 1)]);
      else ydiff = R_PosInf;
      if (xdiff < idiff || ydiff < idiff) {
	// Now go into the direction of the smallest difference.
	if (xdiff < ydiff) {
	  i++;
	  while (i < lx && (xdiff = fabs(px[i] - py[j])) < idiff) {
	    idiff = xdiff;
	    presx[idx] = i;
	    presy[idx] = NA_INTEGER;
	    i++;
	    idx++;
	  }
	  j++;
	} else {
	  j++;
	  while (j < ly && (ydiff = fabs(px[i] - py[j])) < idiff) {
	    idiff = ydiff;
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
      // Decide whether to increment i or j: in the outer join matches are
      // expected to be ordered by value, thus, if x[i] < y[j] we add i to the
      // result and increment it.
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

SEXP C_join_left(SEXP x, SEXP y, SEXP tolerance) {
  int lx, ly;
  double *px, *py, *tol;
  SEXP resx, resy, output, names;
  lx = LENGTH(x);
  ly = LENGTH(y);
  px = REAL(x);
  py = REAL(y);
  tol = REAL(tolerance);
  if (ly != LENGTH(tolerance))
    error("'tolerance' has to be of length 1 or length equal to 'length(y)'");
  PROTECT(resx = allocVector(INTSXP, lx));
  PROTECT(resy = allocVector(INTSXP, lx));
  int *presx = INTEGER(resx);
  int *presy = INTEGER(resy);

  double xdiff;
  double ydiff;
  double idiff;

  int xi = 0;
  int yi = 0;
  int xi_next = 0;
  int yi_next = 0;
  int yi_last_used = -1;
  int xi_last_used = R_PosInf;

  while (xi < lx) {
    xi_next = xi + 1;
    if (yi < ly) {
      yi_next = yi + 1;
      // difference for current pair.
      idiff = fabs(px[xi] - py[yi]);
      // difference for next pairs.
      if (xi_next < lx) xdiff = fabs(px[xi_next] - py[yi]);
      else xdiff = R_PosInf;
      if (yi_next < ly) ydiff = fabs(px[xi] - py[yi_next]);
      else ydiff = R_PosInf;
      // do we have an acceptable match?
      if (idiff <= tol[yi]) {
	presx[xi] = xi_next;
	presy[xi] = yi_next;
	// Remove last hit with same yi if we incremented xi and will NOT
	// increment yi next.
	if (yi == yi_last_used && xi > xi_last_used) {
	  // we're not incrementing yi next round.
	  if (ydiff > idiff || ydiff > xdiff)
	    presy[xi_last_used] = NA_INTEGER;
	}
	yi_last_used = yi;
	xi_last_used = xi;
      } else {
	presx[xi] = xi_next;
	presy[xi] = NA_INTEGER;
      }
      // Decide which index to increment.
      if (xdiff < idiff || ydiff < idiff) {
	// increment the index with the smaller distance
	if (xdiff < ydiff) xi = xi_next;
	else yi = yi_next;
      } else {
	// neither xdiff nor ydiff better than idiff, increment both
	yi = yi_next;
	xi = xi_next;
      }
    } else {
      // just fill-up the result.
      presy[xi] = NA_INTEGER;
      presx[xi] = xi_next;
      xi = xi_next;
    }
  }

  PROTECT(output = allocVector(VECSXP, 2));
  PROTECT(names = allocVector(STRSXP, 2));
  SET_VECTOR_ELT(output, 0, resx);
  SET_VECTOR_ELT(output, 1, resy);
  SET_STRING_ELT(names, 0, mkChar("x"));
  SET_STRING_ELT(names, 1, mkChar("y"));
  setAttrib(output, R_NamesSymbol, names);

  UNPROTECT(4);
  return output;
}
