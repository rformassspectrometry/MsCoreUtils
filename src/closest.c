/* Sebastian Gibb <mail@sebastiangibb.de>
 */

#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/**
 * Find closest value to table, keep duplicates.
 *
 * \param x key value to look for.
 * \param table table/haystack where to look for.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_keep(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    double *px = REAL(x);
    const unsigned int nx = LENGTH(x);

    double *ptable = REAL(table);
    const unsigned int ntable1 = LENGTH(table) - 1;

    double *ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    unsigned int j = 1;

    double prevdiff = R_PosInf, nextdiff = R_PosInf;

    for (unsigned int i = 0; i < nx; ++i) {
        while(j < ntable1 && ptable[j] < px[i])
            ++j;

        /* fabs should be just needed for the first element */
        prevdiff = fabs(px[i] - ptable[j - 1]);
        nextdiff = fabs(ptable[j] - px[i]);

        if (prevdiff <= ptolerance[i] || nextdiff <= ptolerance[i]) {
            if (prevdiff <= nextdiff)
                pout[i] = j;
            else
                pout[i] = ++j;
        } else
            pout[i] = asInteger(nomatch);
    }

    UNPROTECT(1);
    return out;
}

/**
 * Find closest value to table, keep just closest duplicates.
 *
 * \param x key value to look for.
 * \param table table/haystack where to look for.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_closest(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    double *px = REAL(x);
    const unsigned int nx = LENGTH(x);

    double *ptable = REAL(table);
    const unsigned int ntable1 = LENGTH(table) - 1;

    double *ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    unsigned int j = 1, lastj = 0;
    double prevdiff = R_PosInf, nextdiff = R_PosInf, lastdiff = R_PosInf;

    for (int i = 0; i < nx; ++i) {
        while(j < ntable1 && ptable[j] < px[i])
            ++j;

        /* fabs should be just needed for the first element */
        prevdiff = fabs(px[i] - ptable[j - 1]);
        nextdiff = fabs(ptable[j] - px[i]);

        if (prevdiff <= ptolerance[i] || nextdiff <= ptolerance[i]) {
            if (prevdiff < nextdiff) {
                /* match on the left */
                if (lastj == j) {
                    if (prevdiff < lastdiff) {
                        /* same match as before but with smaller difference */
                        pout[i] = j;
                        pout[i - 1] = asInteger(nomatch);
                        lastdiff = prevdiff;
                    } else
                        pout[i] = asInteger(nomatch);
                } else {
                    pout[i] = j;
                    lastdiff = prevdiff;
                }
            } else if (prevdiff > nextdiff) {
                /* match on the right */
                if (lastj == j + 1) {
                    if (nextdiff < lastdiff) {
                        /* same match as before but with smaller difference */
                        pout[i] = ++j;
                        pout[i - 1] = asInteger(nomatch);
                        lastdiff = nextdiff;
                    } else
                        pout[i] = asInteger(nomatch);
                } else {
                    pout[i] = ++j;
                    lastdiff = nextdiff;
                }
            } else {
                /* match on the left or right */
                if (lastj == j && prevdiff > lastdiff) {
                    pout[i] = ++j;
                    lastdiff = nextdiff;
                } else {
                    pout[i] = j;
                    lastdiff = prevdiff;
                }
            }
            lastj = j;
        } else {
            pout[i] = asInteger(nomatch);
            lastdiff = R_PosInf;
        }
    }

    UNPROTECT(1);
    return out;
}

/**
 * Find closest value to table, remove duplicates.
 *
 * \param x key value to look for.
 * \param table table/haystack where to look for.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_remove(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    double *px = REAL(x);
    const unsigned int nx = LENGTH(x);

    double *ptable = REAL(table);
    const unsigned int ntable1 = LENGTH(table) - 1;

    double *ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    unsigned int j = 1, lastj = 0;
    double prevdiff = R_PosInf, nextdiff = R_PosInf;

    for (unsigned int i = 0; i < nx; ++i) {
        while(j < ntable1 && ptable[j] < px[i])
            ++j;

        /* fabs should be just needed for the first element */
        prevdiff = fabs(px[i] - ptable[j - 1]);
        nextdiff = fabs(ptable[j] - px[i]);

        if (prevdiff <= ptolerance[i] || nextdiff <= ptolerance[i]) {
            if (prevdiff <= nextdiff) {
                /* match on the left */
                if (lastj == j) {
                    pout[i] = asInteger(nomatch);
                    pout[i - 1] = asInteger(nomatch);
                } else {
                    pout[i] = j;
                }
            } else {
                /* match on the right */
                if (lastj == j + 1) {
                    pout[i] = asInteger(nomatch);
                    pout[i - 1] = asInteger(nomatch);
                } else {
                    pout[i] = ++j;
                }
            }
        } else
            pout[i] = asInteger(nomatch);
        lastj = j;
    }

    UNPROTECT(1);
    return out;
}
