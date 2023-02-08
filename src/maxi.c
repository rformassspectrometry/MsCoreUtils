#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>


/**
 * Determine the maximal non-missing value returning NA if length(x) is
 * 0 or if all values are missing.
 */
SEXP C_maxi(SEXP x) {
    SEXP r;
    const R_len_t n = XLENGTH(x);
    PROTECT(r=allocVector(REALSXP, 1));
    double* rp = REAL(r);
    double* xp = REAL(x);
    *rp = NA_REAL;

    for (int i = 0; i < n; i++) {
        if (!ISNA(xp[i])) {
            if (ISNA(*rp) || (!ISNA(*rp) && xp[i] > *rp))
                *rp = xp[i];
        }
    }

    UNPROTECT(1);
    return(r);
}
