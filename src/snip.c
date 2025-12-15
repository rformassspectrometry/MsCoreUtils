/* Sebastian Gibb <mail@sebastiangibb.de>
 *
 * This file is taken from MALDIquant for R and related languages.
 * License: Artistic-2.0
 *
 */

#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

/* y = array of double values
 * iterations = max iteration steps
 * decreasing = use a decreasing clipping window?
 */
SEXP C_snip(SEXP y, SEXP iterations, SEXP decreasing) {
  SEXP dup, output;
  R_xlen_t n, i, j, k;
  int d;
  double a, b;

  PROTECT(dup=Rf_duplicate(y));
  PROTECT(y=Rf_coerceVector(dup, REALSXP));
  n=XLENGTH(y);
  d=Rf_asInteger(decreasing);

  PROTECT(output=Rf_allocVector(REALSXP, n));

  double* xo=REAL(output);
  double* xy=REAL(y);

  k=Rf_asInteger(iterations);

  /* code duplication to use faster ++i/--i instead of i+=step */
  if (d) {
    for (i=k; i>0; --i) {
      for (j=i; j<n-i; ++j) {
        a=xy[j];
        b=(xy[j-i]+xy[j+i])/2;
        if (b < a) {
          a=b;
        }
        xo[j]=a;
      }

      for(j=i; j<n-i; ++j) {
        xy[j]=xo[j];
      }
    }
  } else {
    for (i=1; i<=k; ++i) {
      for (j=i; j<n-i; ++j) {
        a=xy[j];
        b=(xy[j-i]+xy[j+i])/2;
        if (b < a) {
          a=b;
        }
        xo[j]=a;
      }

      for(j=i; j<n-i; ++j) {
        xy[j]=xo[j];
      }
    }
  }

  memcpy(xo, xy, n*sizeof(double));

  UNPROTECT(3);

  return(output);
}
