#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>


/**
 * Sum values skipping NA values. Returns NA if all values are NA.
 */
SEXP C_sumi(SEXP x) {
    SEXP r;
    const R_len_t n = XLENGTH(x);
    unsigned int calc = 0;
    PROTECT(r=allocVector(REALSXP, 1));
    double* rp = REAL(r);
    double  result = 0.0, val = 0.0;
    double* xp = REAL(x);
    double* ep = xp + n;

    while( xp++!=ep ) {
        val = *xp;
        if( !ISNA(val) ) {
            result += val;
            calc = 1;
        }
    }

    *rp = calc ? result : (n==0 ? 0 : NA_REAL);
    UNPROTECT(1);
    return(r);
}
