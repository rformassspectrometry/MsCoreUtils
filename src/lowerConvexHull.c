/* Sebastian Gibb <mail@sebastiangibb.de>
 *
 * This file is taken from MALDIquant for R and related languages.
 *
 */

#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>


/* build cross product of 2 vectors and compare to zero
 * returns true for P2(x2, y2) left of P0->P1
 */
double left(double x0, double y0, double x1, double y1, double x2, double y2) {
    return(((x1-x0)*(y2-y0) - (x2-x0)*(y1-y0)) > 0);
}

/* Lower Convex Hull Algorithm based on:
 * A. M. Andrew, "Another Efficient Algorithm for Convex Hulls in Two
 * Dimensions", Info. Proc. Letters 9, 216-219 (1979).
 * only calculate lower hull (we don't need the upper one)
 */

/* x = array of double values
 * y = array of double values
 * length = length of y
 * output = array of double values (new y values)
 */
SEXP C_lowerConvexHull(SEXP x, SEXP y) {
  SEXP output;
  /* TODO: replace by R_xlen_t in R 3.0.0 */
  R_xlen_t n, i, j, k=0;
  int* nodes;
  double m, c;

  PROTECT(x=coerceVector(x, REALSXP));
  PROTECT(y=coerceVector(y, REALSXP));
  n=XLENGTH(x);

  PROTECT(output=allocVector(REALSXP, n));
  /* TODO: replace by R_xlen_t in R 3.0.0 */
  /* allocate vector - error handling is done by R */
  nodes=(int*) Calloc((size_t) n, int);

  double* xx=REAL(x);
  double* xy=REAL(y);
  double* xo=REAL(output);

  /* find lower convex hull */
  for (i=0; i<n; ++i) {
    while (k > 1 && !left(xx[nodes[k-2]], xy[nodes[k-2]],
                          xx[nodes[k-1]], xy[nodes[k-1]], xx[i], xy[i])) {
            k-=1;
    }
    nodes[k]=i;
    k+=1;
  }

  /* build linear function y=mx+c to calculate values between nodes */
  for (i=0; i<k; ++i) {
    m=(xy[nodes[i+1]]-xy[nodes[i]])/(xx[nodes[i+1]]-xx[nodes[i]]);
    c=xy[nodes[i]]-m*xx[nodes[i]];

    for (j=nodes[i]; j<nodes[i+1]; ++j) {
      xo[j]=m*xx[j]+c;
    }
  }

  xo[n-1]=xy[n-1];

  Free(nodes);
  UNPROTECT(3);

  return(output);
}
