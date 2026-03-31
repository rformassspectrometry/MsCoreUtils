/* Sebastian Gibb <mail@sebastiangibb.de>
 *
 * This file is taken from MALDIquant for R and related languages.
 * License: Artistic-2.0
 *
 */

#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

R_xlen_t windowMaxIdx(double* x, R_xlen_t start, R_xlen_t end) {
  R_xlen_t i, m=start;
  for (i=start+1; i<=end; ++i) {
    if (x[m] < x[i]) {
      m=i;
    }
  }
  return(m);
}

/* y = array of double values
 * s = half window size
 */
SEXP C_localMaxima(SEXP y, SEXP s) {
  SEXP output;
  R_xlen_t n, q, m, windowSize, i, l, mid;

  PROTECT(y=Rf_coerceVector(y, REALSXP));
  n=XLENGTH(y);

  PROTECT(output=Rf_allocVector(LGLSXP, n));

  double* xy=REAL(y);
  int* xo=LOGICAL(output);
  memset(xo, 0, n*sizeof(int));

  q=Rf_asInteger(s);
  windowSize=q*2;
  m=windowMaxIdx(xy, 0, windowSize);

  xo[m]=m==q;

  /* i == rhs; l == lhs; mid == middle pos */
  for (i=windowSize+1, l=i-windowSize, mid=(l+i)/2; i<n; ++i, ++mid, ++l) {
    /* maximum out of window, calculate new maximum in current window */
    if (m < l) {
      m=windowMaxIdx(xy, l, i);
    } else if (xy[i] > xy[m]) {
      m=i;
    }

    if (m == mid) {
      xo[m]=1;
    }
  }

  UNPROTECT(2);
  return(output);
}
