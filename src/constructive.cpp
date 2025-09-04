#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include <stdlib.h> // for NULL, strtoull
#include <stdint.h> // for uintptr_t
#include <R_ext/Error.h> // for Rf_error
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif

/* .Call calls */
SEXP external_pointer_address(SEXP);
SEXP objectFromAddress(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"external_pointer_address", (DL_FUNC) &external_pointer_address, 1},
  {"objectFromAddress",        (DL_FUNC) &objectFromAddress,        1},
  {NULL, NULL, 0}
};

void R_init_constructive(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

SEXP external_pointer_address(SEXP s) {
  if (TYPEOF(s) != EXTPTRSXP) {
    Rf_error("external_pointer_address() expects an input of type 'externalptr'");
  }
  char buf[32];                       /* <-- fix: was char* buf[20] */
  snprintf(buf, sizeof buf, "%p", R_ExternalPtrAddr(s));
  return Rf_mkString(buf);
}

// Used by .env() to retrieve environments by address (session-local, unsafe across sessions)
SEXP objectFromAddress(SEXP a) {
  if (TYPEOF(a) != STRSXP || XLENGTH(a) != 1) {
    Rf_error("'a' must be a length-1 character vector");
  }
  const char* s = CHAR(STRING_ELT(a, 0));
  char* end = NULL;
  unsigned long long u = strtoull(s, &end, 0); // base 0 handles 0x...
  if (end == s || (end && *end != '\0')) {
    Rf_error("'a' is not a formatted unsigned integer address");
  }
  uintptr_t p = (uintptr_t) u;
  SEXP result = (SEXP) p;
  if (TYPEOF(result) != ENVSXP) return R_NilValue;
  return result;
}
