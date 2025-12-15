#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

/**
 * The `reduce()` function *reduces* the provided numeric ranges to
 * non-overlapping (disjoint) ranges. This is similar to the
 * `IRanges::reduce()` function, but works with `numeric` vectors instead of
 * integer ranges (`IRanges`).
 *
 * \param start numeric
 * \param end numeric
 * \param check logical
 * \author Johannes Rainer and Sebastian Gibb
 */
SEXP C_reduce(SEXP start, SEXP end, SEXP check) {
    double* ps = REAL(start);
    double* pe = REAL(end);

    const unsigned int n = LENGTH(start);

    if (!n) {
        SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(out, 0, PROTECT(Rf_allocVector(REALSXP, 0)));
        SET_VECTOR_ELT(out, 1, PROTECT(Rf_allocVector(REALSXP, 0)));
        UNPROTECT(3);
        return out;
    }
    if (*LOGICAL(check)) {
        if (n != LENGTH(end))
            Rf_error("'start' and 'end' need to have the same length.");

        for (unsigned int i = 0; i < n; ++i)
            if (ps[i] > pe[i])
                Rf_error("Values in 'start' have to be smaller or equal \
                        to the respective values in 'end'.");
    }

    SEXP rs = PROTECT(Rf_allocVector(REALSXP, n));
    double* prs = REAL(rs);
    SEXP re = PROTECT(Rf_allocVector(REALSXP, n));
    double* pre = REAL(re);

    SEXP ord = PROTECT(Rf_allocVector(INTSXP, n));
    int* pord = INTEGER(ord);
    Rboolean nalast = TRUE;
    Rboolean decreasing = FALSE;
    R_orderVector1(pord, n, start, nalast, decreasing);

    prs[0] = ps[pord[0]];
    pre[0] = pe[pord[0]];

    unsigned int pos = 0;
    unsigned int i = 1;

    for (; i < n; ++i) {
        if (ps[pord[i]] <= pre[pos]) {
            if (pre[pos] < pe[pord[i]]) {
                pre[pos] = pe[pord[i]];
            }
        } else {
            ++pos;
            prs[pos] = ps[pord[i]];
            pre[pos] = pe[pord[i]];
        }
    }

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, Rf_lengthgets(rs, pos + 1));
    SET_VECTOR_ELT(out, 1, Rf_lengthgets(re, pos + 1));

    UNPROTECT(4);

    return out;
}
