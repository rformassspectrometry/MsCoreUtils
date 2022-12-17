#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

/**
 * Determine whether a vector element is between a given range of values.
 *
 * \param x numeric
 * \param left numeric(1)
 * \param right numeric(1)
 * \author Sebastian Gibb
 */
SEXP C_between(SEXP x, SEXP left, SEXP right) {
    double l = REAL(left)[0], r = REAL(right)[0];

    R_xlen_t n = XLENGTH(x);
    SEXP between = PROTECT(allocVector(LGLSXP, n));
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
