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

    SEXP out = PROTECT(Rf_allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    const unsigned int inomatch = Rf_asInteger(nomatch);
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
            pout[i] = inomatch;
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
 * \author Sebastian Gibb and Johannes Rainer
 */
SEXP C_closest_dup_closest(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    double *px = REAL(x);
    const unsigned int nx = LENGTH(x);

    double *ptable = REAL(table);
    const unsigned int ntable = LENGTH(table);

    double *ptolerance = REAL(tolerance);

    SEXP out = PROTECT(Rf_allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    const unsigned int inomatch = Rf_asInteger(nomatch);

    unsigned int ix = 0, ixlastused = 1;
    unsigned int itbl = 0, itbllastused = 1;
    double diff = R_PosInf, diffnxtx = R_PosInf, diffnxttbl = R_PosInf;

    while (ix < nx) {
        if (itbl < ntable) {
            /* difference for current pair */
            diff = fabs(px[ix] - ptable[itbl]);
            /* difference for next pairs */
            diffnxtx =
                ix + 1 < nx ? fabs(px[ix + 1] - ptable[itbl]) : R_PosInf;
            diffnxttbl =
                itbl + 1 < ntable ? fabs(px[ix] - ptable[itbl + 1]) : R_PosInf;

            if (diff <= ptolerance[ix]) {
                /* valid match, add + 1 to convert between R/C index */
                pout[ix] = itbl + 1;
                if (itbl == itbllastused &&
                        (diffnxtx < diffnxttbl || diff < diffnxttbl))
                    pout[ixlastused] = inomatch;
                ixlastused = ix;
                itbllastused = itbl;
            } else
                pout[ix] = inomatch;

            if (diffnxtx < diff || diffnxttbl < diff) {
                /* increment the index with the smaller distance */
                if (diffnxtx < diffnxttbl)
                    ++ix;
                else
                    ++itbl;
            } else {
                /* neither next x nor next table item offer a better match */
                ++ix;
                ++itbl;
            }
        } else
            pout[ix++] = inomatch;
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

    SEXP out = PROTECT(Rf_allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    const unsigned int inomatch = Rf_asInteger(nomatch);
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
                    pout[i] = inomatch;
                    pout[i - 1] = inomatch;
                } else {
                    pout[i] = j;
                }
            } else {
                /* match on the right */
                if (lastj == j + 1) {
                    pout[i] = inomatch;
                    pout[i - 1] = inomatch;
                } else {
                    pout[i] = ++j;
                }
            }
        } else
            pout[i] = inomatch;
        lastj = j;
    }

    UNPROTECT(1);
    return out;
}
