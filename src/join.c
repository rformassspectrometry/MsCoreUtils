#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <Rmath.h>

/**
 * Left join of two increasingly sorted arrays.
 *
 * \param x array, has to be sorted increasingly and not contain any NA.
 * \param y array, has to be sorted increasingly and not contain any NA.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \author Sebastian Gibb
 */
SEXP C_join_left(SEXP x, SEXP y, SEXP tolerance, SEXP nomatch) {
    SEXP ry = PROTECT(C_closest_dup_closest(x, y, tolerance, nomatch));
    const unsigned int ny = LENGTH(ry);

    SEXP rx = PROTECT(allocVector(INTSXP, ny));
    int* px = INTEGER(rx);

    for (unsigned int i = 0; i < ny; ++i)
        px[i] = i + 1;

    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(out, 0, rx);
    SET_VECTOR_ELT(out, 1, ry);
    SET_STRING_ELT(nms, 0, mkChar("x"));
    SET_STRING_ELT(nms, 1, mkChar("y"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(4);

    return out;
}

/**
 * Right join of two increasingly sorted arrays.
 *
 * \param x array, has to be sorted increasingly and not contain any NA.
 * \param y array, has to be sorted increasingly and not contain any NA.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \author Sebastian Gibb
 */
SEXP C_join_right(SEXP x, SEXP y, SEXP tolerance, SEXP nomatch) {
    SEXP c = PROTECT(C_closest_dup_closest(x, y, tolerance, nomatch));
    int* pc = INTEGER(c);
    const unsigned int nc = LENGTH(c);

    const int inomatch = asInteger(nomatch);

    const unsigned int ny = LENGTH(y);
    SEXP rx = PROTECT(allocVector(INTSXP, ny));
    int* px = INTEGER(rx);
    SEXP ry = PROTECT(allocVector(INTSXP, ny));
    int* py = INTEGER(ry);

    for (unsigned int i = 0; i < ny; ++i) {
        px[i] = inomatch;
        py[i] = i + 1;
    }
    for (unsigned int i = 0; i < nc; ++i) {
        if (pc[i] != inomatch)
            px[pc[i] - 1] = i + 1;
    }

    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(out, 0, rx);
    SET_VECTOR_ELT(out, 1, ry);
    SET_STRING_ELT(nms, 0, mkChar("x"));
    SET_STRING_ELT(nms, 1, mkChar("y"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(5);

    return out;
}

/**
 * Inner join of two increasingly sorted arrays.
 *
 * \param x array, has to be sorted increasingly and not contain any NA.
 * \param y array, has to be sorted increasingly and not contain any NA.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \author Sebastian Gibb
 */
SEXP C_join_inner(SEXP x, SEXP y, SEXP tolerance, SEXP nomatch) {
    SEXP ry = PROTECT(C_closest_dup_closest(x, y, tolerance, nomatch));
    int* py = INTEGER(ry);
    const unsigned int ny = LENGTH(ry);

    SEXP rx = PROTECT(allocVector(INTSXP, ny));
    int* px = INTEGER(rx);

    const int inomatch = asInteger(nomatch);
    unsigned int j = 0;

    for (unsigned int i = 0; i < ny; ++i) {
        if (py[i] != inomatch) {
            px[j] = i + 1;
            py[j] = py[i];
            ++j;
        }
    }
    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(out, 0, lengthgets(rx, j));
    SET_VECTOR_ELT(out, 1, lengthgets(ry, j));
    SET_STRING_ELT(nms, 0, mkChar("x"));
    SET_STRING_ELT(nms, 1, mkChar("y"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(4);

    return out;
}

/**
 * Outer join of two increasingly sorted arrays.
 *
 * \param x array, has to be sorted increasingly and not contain any NA.
 * \param y array, has to be sorted increasingly and not contain any NA.
 * \param tolerance allowed tolerance to be accepted as match, has to be of
 * length == length(x).
 * \param nomatch value that should be returned if a key couldn't be matched.
 * \author Johannes Rainer, Sebastian Gibb
 */
SEXP C_join_outer(SEXP x, SEXP y, SEXP tolerance, SEXP nomatch) {
    double *pix = REAL(x);
    const unsigned int nx = LENGTH(x);
    double *piy = REAL(y);
    const unsigned int ny = LENGTH(y);

    double *ptolerance = REAL(tolerance);

    const unsigned int inomatch = asInteger(nomatch);

    SEXP rx = PROTECT(allocVector(INTSXP, nx + ny));
    SEXP ry = PROTECT(allocVector(INTSXP, nx + ny));

    int* prx = INTEGER(rx);
    int* pry = INTEGER(ry);

    unsigned int i = 0, ix = 0, iy = 0;
    double diff = R_PosInf, diffnxtx = R_PosInf, diffnxty = R_PosInf, diffnxtxy = R_PosInf;

    while (ix < nx || iy < ny) {
        if (ix >= nx) {
            prx[i] = inomatch;
            pry[i] = ++iy;
        } else if (iy >= ny) {
            prx[i] = ++ix;
            pry[i] = inomatch;
        } else {
            /* difference for current pair */
            diff = fabs(pix[ix] - piy[iy]);

            if (diff <= ptolerance[ix]) {
                /* difference for next pairs */
                diffnxtx =
                    ix + 1 < nx ? fabs(pix[ix + 1] - piy[iy]) : R_PosInf;
                diffnxty =
                    iy + 1 < ny ? fabs(pix[ix] - piy[iy + 1]) : R_PosInf;
                diffnxtxy =
                    (ix + 1 < nx && iy + 1 < ny) ? fabs(pix[ix + 1] - piy[iy + 1]) : R_PosInf;

                if ((diffnxtx < diff && diffnxtx < diffnxtxy) ||
                        (diffnxty < diff && diffnxty < diffnxtxy)) {
                    if (diffnxtx < diffnxty) {
                        prx[i] = ++ix;
                        pry[i] = inomatch;
                    } else {
                        prx[i] = inomatch;
                        pry[i] = ++iy;
                    }
                } else {
                    prx[i] = ++ix;
                    pry[i] = ++iy;
                }
            } else {
                if (pix[ix] < piy[iy]) {
                    prx[i] = ++ix;
                    pry[i] = inomatch;
                } else {
                    prx[i] = inomatch;
                    pry[i] = ++iy;
                }
            }
        }
        ++i;
    }

    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(out, 0, lengthgets(rx, i));
    SET_VECTOR_ELT(out, 1, lengthgets(ry, i));
    SET_STRING_ELT(nms, 0, mkChar("x"));
    SET_STRING_ELT(nms, 1, mkChar("y"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(4);

    return out;
}
