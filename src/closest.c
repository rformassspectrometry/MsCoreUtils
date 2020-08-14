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
R_xlen_t leftmost(double x, double* ptable, R_xlen_t low, R_xlen_t n) {
  R_xlen_t high = low + 1, mid = high;

  /* same interval as the last one */
  if (x < ptable[high] && ptable[low] <= x)
    return low ;

  /* exponentional search */
  for (R_xlen_t step = 1;; step *= 2) {
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
  while(low != mid) {
    mid = (low + high) / 2;
    if (x >= ptable[mid])
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
 * length == length(tolerance).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_keep(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    R_xlen_t nx = XLENGTH(x);
    double* px = REAL(x);

    R_xlen_t ntable = XLENGTH(table);
    double* ptable = REAL(table);

    double* ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(REALSXP, nx));
    double* pout = REAL(out);

    R_xlen_t low = 0, cur = 0;

    for (R_xlen_t i = 0; i < nx; ++i) {
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
        pout[i] = fabs(px[i] - ptable[cur]) <= ptolerance[cur] ?
            cur + 1 : asReal(nomatch);
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
 * to be accepted as match, has to be of length == length(tolerance).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
void closest_dup_closest(double *pout, double *px, R_xlen_t nx,
                         double *ptable, R_xlen_t ntable,
                         double *ptolerance, double nomatch) {

    R_xlen_t low = 0, cur = 0;
    R_xlen_t last = R_NegInf;
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
        if (absdiff <= ptolerance[cur]) {
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
 * length == length(tolerance).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_closest(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    R_xlen_t nx = XLENGTH(x);
    SEXP out = PROTECT(allocVector(REALSXP, nx));

    closest_dup_closest(
        REAL(out),
        REAL(x), nx,
        REAL(table), XLENGTH(table),
        REAL(tolerance), asReal(nomatch)
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
 * length == length(tolerance).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \return index the closest element
 *
 * \note x and table have to be sorted increasingly and not containing any NA.
 */
SEXP C_closest_dup_remove(SEXP x, SEXP table, SEXP tolerance, SEXP nomatch) {
    R_xlen_t nx = XLENGTH(x);
    double* px = REAL(x);

    R_xlen_t ntable = XLENGTH(table);
    double* ptable = REAL(table);

    double* ptolerance = REAL(tolerance);

    SEXP out = PROTECT(allocVector(REALSXP, nx));
    double* pout = REAL(out);

    R_xlen_t low = 0, cur = 0;
    R_xlen_t last = R_NegInf;

    for (int i = 0; i < nx; ++i) {
        if (px[i] < ptable[0])
            cur = 0;
        else if (px[i] >= ptable[ntable - 1])
            cur = ntable - 1;
        else {
            low = leftmost(px[i], ptable, low, ntable);
            cur = (px[i] - ptable[low] <= ptable[low + 1] - px[i]) ? low : low + 1;
        }

        if (fabs(px[i] - ptable[cur] <= ptolerance[cur])) {
            if (last != cur)
                pout[i] = cur + 1;
            else
                pout[i - 1] = pout[i] = asReal(nomatch);
            last = cur;
        }
  }

  UNPROTECT(1);
  return(out);
}
