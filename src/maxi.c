#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>


/**
 * Determine the maximal non-missing value returning NA if length(x) is
 * 0 or if all values are missing.
 */
SEXP C_maxi(SEXP x) {
    if (!Rf_isReal(x))
        x = Rf_coerceVector(x, REALSXP);
    double* xp = REAL(x);
    const R_len_t n = XLENGTH(x);

    SEXP r = PROTECT(Rf_allocVector(REALSXP, 1));
    double* rp = REAL(r);
    *rp = R_NegInf;

    for (R_xlen_t i = 0; i < n; i++)
        if (xp[i] > *rp)
            *rp = xp[i];

    if (!(R_FINITE(*rp)))
        *rp = NA_REAL;

    UNPROTECT(1);
    return(r);
}
