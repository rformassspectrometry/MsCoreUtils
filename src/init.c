#include "MsCoreUtils.h"

static const R_CallMethodDef CallEntries[] = {
    {"C_closest_dup_keep", (DL_FUNC) &C_closest_dup_keep, 4},
    {"C_closest_dup_closest", (DL_FUNC) &C_closest_dup_closest, 4},
    {"C_closest_dup_remove", (DL_FUNC) &C_closest_dup_remove, 4},
    {"C_impNeighbourAvg", (DL_FUNC) &C_impNeighbourAvg, 2},
    {"C_join_left", (DL_FUNC) &C_join_left, 4},
    {"C_join_right", (DL_FUNC) &C_join_right, 4},
    {"C_join_inner", (DL_FUNC) &C_join_inner, 4},
    {"C_join_outer", (DL_FUNC) &C_join_outer, 4},
    {"C_localMaxima", (DL_FUNC) &C_localMaxima, 2},
    {"C_which_first", (DL_FUNC) &C_which_first, 1},
    {"C_which_last", (DL_FUNC) &C_which_last, 1},
    {"C_between", (DL_FUNC) &C_between, 2},
    {"C_sumi", (DL_FUNC) &C_sumi, 1},
    {"C_maxi", (DL_FUNC) &C_maxi, 1},
    {NULL, NULL, 0}
};

void R_init_MsCoreUtils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
