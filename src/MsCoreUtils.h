#ifndef MSCOREUTILS_H
#define MSCOREUTILS_H

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP C_closest_dup_keep(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_closest_dup_closest(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_closest_dup_remove(SEXP, SEXP, SEXP, SEXP);

extern SEXP C_impNeighbourAvg(SEXP, SEXP);

extern SEXP C_join_left(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_join_right(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_join_inner(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_join_outer(SEXP, SEXP, SEXP, SEXP);

extern SEXP C_which_first(SEXP);
extern SEXP C_which_last(SEXP);
extern SEXP C_between(SEXP, SEXP);

extern SEXP C_localMaxima(SEXP, SEXP);

extern SEXP _MsCoreUtils_imp_neighbour_avg(SEXP, SEXP);

extern SEXP C_sumi(SEXP);

extern SEXP C_maxi(SEXP);

/* Originally in MALDIquant */
extern SEXP C_dilation(SEXP y, SEXP s);
extern SEXP C_erosion(SEXP y, SEXP s);
extern SEXP C_snip(SEXP y, SEXP iterations, SEXP decreasing);
extern SEXP C_lowerConvexHull(SEXP x, SEXP y);

#endif /* end of MSCOREUTILS_H */
