#include "MsCoreUtils.h"

#include <R.h>
#include <Rinternals.h>

/* x = matrix
 * k = lowest value
 */
SEXP C_impNeighbourAvg(SEXP x, SEXP k) {
    SEXP output;
    PROTECT(output = duplicate(x));

    double *po = REAL(output);
    double dk = asReal(k);
    R_xlen_t nr = nrows(x), nc = ncols(x);

    int cur = 0;
    for(int i = 0; i < nr; ++i) {
        /* first and last values are set to k if NA */
        if (R_IsNA(po[i]))
            po[i] = dk;
        if (R_IsNA(po[i + nr * (nc - 1)]))
            po[i + nr * (nc - 1)] = dk;

        for(int j = 1; j < (nc - 1); ++j) {
            cur = i + j * nr;
            if (R_IsNA(po[cur])) {
	            /* if the next value is NA and
                 * all previous values are k then we set to k */
	            if (R_IsNA(po[cur + nr]) && po[cur - nr] == dk)
                    po[cur] = dk;
	            else /* next is not NA, set to mean of neighbours */
                    po[cur] = (po[cur - nr] + po[cur + nr]) / 2;
            }
        }
    }

    UNPROTECT(1);

    return output;
}
