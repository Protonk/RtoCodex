// Registration of native routines
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern void C_add1(int *x, int *n);

static const R_CMethodDef CEntries[] = {
  {"C_add1", (DL_FUNC) &C_add1, 2},
  {NULL, NULL, 0}
};

void R_init_RtoCodex(DllInfo *dll) {
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
