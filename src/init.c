#include "MsCoreUtils.h"

static const R_CallMethodDef CallEntries[] = {
    {"C_localMaxima", (DL_FUNC) &C_localMaxima, 2},
    {"C_impNeighbourAvg", (DL_FUNC) &C_impNeighbourAvg, 2},
    {"C_joinOuter", (DL_FUNC) &C_joinOuter, 4},
    {NULL, NULL, 0}
};

void R_init_MsCoreUtils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
