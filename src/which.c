/* Sebastian Gibb <mail@sebastiangibb.de>
 */

#include "MsCoreUtils.h"

#include <R.h>

/**
 * Find first TRUE value.
 *
 * \param x logical vector.
 * \return integer, index of first TRUE value
 *
 * \note Modified from src/main/summary.c:do_which_min()
 */
SEXP C_which_first(SEXP x) {
    if (!isLogical(x))
        error("argument to 'which.first' is not logical");

    const R_len_t n = XLENGTH(x);
    R_len_t i = 0, j = -1;
    int *xp = LOGICAL(x);

    for (; i < n; ++i) {
        if (xp[i] == TRUE) {
            j = i; break;
        }
    }

    i = (j != -1);
    SEXP r = PROTECT(allocVector(INTSXP, i ? 1 : 0));
    if (i) {
        INTEGER(r)[0] = j + 1;
        if (getAttrib(x, R_NamesSymbol) != R_NilValue) { /* preserve names */
	        SEXP name;
	        PROTECT(
                name = ScalarString(
                    STRING_ELT(getAttrib(x, R_NamesSymbol), j)));
	        setAttrib(r, R_NamesSymbol, name);
	        UNPROTECT(1);
        }
	}
    UNPROTECT(1);

    return r;
}

/**
 * Find last TRUE value.
 *
 * \param x logical vector.
 * \return integer, index of last TRUE value
 *
 * \note Modified from src/main/summary.c:do_which_min()
 */
SEXP C_which_last(SEXP x) {
    if (!isLogical(x))
        error("argument to 'which.last' is not logical");

    const R_len_t n = XLENGTH(x);
    R_len_t i = n - 1, j = -1;
    int *xp = LOGICAL(x);

    for (; i >= 0; --i) {
        if (xp[i] == TRUE) {
            j = i; break;
        }
    }

    i = (j != -1);
    SEXP r = PROTECT(allocVector(INTSXP, i ? 1 : 0));
    if (i) {
        INTEGER(r)[0] = j + 1;
        if (getAttrib(x, R_NamesSymbol) != R_NilValue) { /* preserve names */
	        SEXP name;
	        PROTECT(
                name = ScalarString(
                    STRING_ELT(getAttrib(x, R_NamesSymbol), j)));
	        setAttrib(r, R_NamesSymbol, name);
	        UNPROTECT(1);
        }
    }
    UNPROTECT(1);

    return r;
}
