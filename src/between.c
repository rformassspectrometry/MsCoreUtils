#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

/**
 * Determine whether a vector element is between a given range of values.
 *
 * \param x numeric
 * \param range numeric(2)
 * \author Sebastian Gibb
 */
SEXP C_between(SEXP x, SEXP range) {
    if (!Rf_isReal(x))
        x = Rf_coerceVector(x, REALSXP);

    if (!Rf_isReal(range))
        range = Rf_coerceVector(range, REALSXP);

    if (XLENGTH(range) != 2L)
        Rf_error("'range' has to be a numeric of length 2.");

    double l = REAL(range)[0], r = REAL(range)[1];

    if (l > r) {
        double tmp = r;
        r = l;
        l = tmp;
    }

    R_xlen_t n = XLENGTH(x);
    SEXP between = PROTECT(Rf_allocVector(LGLSXP, n));
    int* pbetween = LOGICAL(between);

    if (R_IsNA(l) || R_IsNA(r))
        for (R_xlen_t i = 0; i < n; ++i, ++pbetween)
            *pbetween = NA_LOGICAL;
    else {
        double* px = REAL(x);
        for (R_xlen_t i = 0; i < n; ++i, ++pbetween, ++px)
            *pbetween = R_IsNA(*px) ? NA_LOGICAL : l <= *px && *px <= r;
    }

    UNPROTECT(1);

    return between;
}
