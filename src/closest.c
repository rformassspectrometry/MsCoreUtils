/* Sebastian Gibb <mail@sebastiangibb.de>
 */

#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>

/**
 * Find leftmost interval.
 *
 * This function is a simplified version of R's findInterval.
 *
 * \param x key value to look for.
 * \param ptable table/haystack where to look for.
 * \param low lowest/leftmost index to start the search.
 * \param n highest/rightmost index.
 * \return index of the leftmost element
 *
 * \note For easier implementation we ignore some corner cases here. Index `0`
 * and `n - 1` have to be handled outside this function. Also if it is called
 * multiple times (e.g. if x is an array in the calling function) x has to
 * sorted increasingly (we never lower `low`).
 *
 * \sa r-source/src/appl/interv.c
 */
int leftmost(double x, double* ptable, int low, int n) {
    int high = low + 1, mid = high;

    /* same interval as the last one */
    if (x < ptable[high] && ptable[low] <= x)
        return low;

    /* exponentional search */
    for (int step = 1;; step *= 2) {
        /* x is still > ptable[high] so we can update low as well to keep search
         * space small
         */
        low = high;
        high = low + step;
        if (high >= n || x < ptable[high])
            break;
    }
    high = (high >= n) ? n - 1 : high;

    /* binary search */
    while(1) {
        mid = (low + high) / 2;
        if (mid == low)
            break;
        if (x > ptable[mid])
            low = mid;
        else
            high = mid;
    }
    return low;
}

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
    int nx = LENGTH(x);
    double* px = REAL(x);

    int ntable = LENGTH(table);
    double* ptable = REAL(table);

    double* ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    int low = 0, cur = 0;

    for (int i = 0; i < nx; ++i) {
        if (px[i] < ptable[0])
            cur = 0;
        else if (px[i] >= ptable[ntable - 1])
            cur = ntable - 1;
        else {
            low = leftmost(px[i], ptable, low, ntable);
            cur = (px[i] - ptable[low] <= ptable[low + 1] - px[i]) ?
                low : low + 1;
        }

        /* cur + 1 is needed here to translate between R's and C's indices */
        pout[i] = fabs(px[i] - ptable[cur]) <= ptolerance[i] ?
            cur + 1 : asInteger(nomatch);
  }

  UNPROTECT(1);
  return(out);
}

/**
 * Find closest value to table, keep just closest duplicate.
 * This function should be reused in the outer join C implementation.
 *
 * \param pout pointer to output array.
 * \param px pointer to arrays with key values to look for.
 * \param nx number of elements in `px`.
 * \param ptable table/haystack where to look for.
 * \param ntable number of elements in `ptable`.
 * \param ptolerance pointer to tolerance array that stores the tolerance
 * to be accepted as match, has to be of length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
void closest_dup_closest(int *pout, double *px, int nx,
                         double *ptable, int ntable,
                         double *ptolerance, int nomatch) {

    int low = 0, cur = 0;
    int last = R_NegInf;
    double absdiff, lastdiff = R_PosInf;

    for (int i = 0; i < nx; ++i) {
        if (px[i] < ptable[0])
            cur = 0;
        else if (px[i] >= ptable[ntable - 1])
            cur = ntable - 1;
        else {
            low = leftmost(px[i], ptable, low, ntable);
            cur = (px[i] - ptable[low] <= ptable[low + 1] - px[i]) ?
                low : low + 1;
        }

        absdiff = fabs(px[i] - ptable[cur]);
        if (absdiff <= ptolerance[i]) {
            if (last == cur) {
                if (absdiff < lastdiff && last == cur) {
                    pout[i] = cur + 1;
                    pout[i - 1] = nomatch;
                    lastdiff = absdiff;
                } else
                    pout[i] = nomatch;
            } else {
                pout[i] = cur + 1;
                last = cur;
                lastdiff = absdiff;
            }
        } else
            pout[i] = nomatch;
    }
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
    int nx = LENGTH(x);
    SEXP out = PROTECT(allocVector(INTSXP, nx));

    closest_dup_closest(
        INTEGER(out),
        REAL(x), nx,
        REAL(table), LENGTH(table),
        REAL(tolerance), asInteger(nomatch)
    );

    UNPROTECT(1);
    return(out);
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
    int nx = LENGTH(x);
    double* px = REAL(x);

    int ntable = LENGTH(table);
    double* ptable = REAL(table);

    double* ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(INTSXP, nx));
    int* pout = INTEGER(out);

    int low = 0, cur = 0;
    int last = R_NegInf;

    for (int i = 0; i < nx; ++i) {
        if (px[i] < ptable[0])
            cur = 0;
        else if (px[i] >= ptable[ntable - 1])
            cur = ntable - 1;
        else {
            low = leftmost(px[i], ptable, low, ntable);
            cur = (px[i] - ptable[low] <= ptable[low + 1] - px[i]) ?
                low : low + 1;
        }

        if (fabs(px[i] - ptable[cur] <= ptolerance[i])) {
            if (last != cur)
                pout[i] = cur + 1;
            else
                pout[i - 1] = pout[i] = asInteger(nomatch);
            last = cur;
        }
  }

  UNPROTECT(1);
  return(out);
}
