#ifndef MSCOREUTILS_H
#define MSCOREUTILS_H

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP C_localMaxima(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_localMaxima", (DL_FUNC) &C_localMaxima, 2},
    {NULL, NULL, 0}
};

void R_init_MsCoreUtils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
#endif /* end of MSCOREUTILS_H */
