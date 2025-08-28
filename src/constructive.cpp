#include <Rinternals.h>
#include <Rdefines.h>
#include <stdint.h> // for uintptr_t
#include <stdio.h>
#include <inttypes.h> // for SCNxPTR
#include <stdlib.h> // for NULL
#include <R_ext/Error.h> // for error
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif

/* .Call calls */
SEXP external_pointer(SEXP);
SEXP external_pointer_address(SEXP);
SEXP objectFromAddress(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"external_pointer",         (DL_FUNC) &external_pointer,         1},
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

// Thanks to Randy Lai: https://github.com/randy3k/xptr/
SEXP external_pointer(SEXP p) {
  if (TYPEOF(p) != STRSXP || LENGTH(p) < 1) {
    error("Input must be a character vector of at least length 1");
  }
  const char* str = CHAR(STRING_ELT(p, 0));

  /* robust parse for 64-bit pointers */
  uintptr_t u = (uintptr_t) strtoull(str, NULL, 0);
  void* ptr = (void*) u;

  return R_MakeExternalPtr(ptr, R_NilValue, R_NilValue);
}

SEXP external_pointer_address(SEXP s) {
  if (TYPEOF(s) != EXTPTRSXP) {
    error("external_pointer_address() expects an input of type 'externalptr'");
  }
  char buf[32];                       /* <-- fix: was char* buf[20] */
  snprintf(buf, sizeof buf, "%p", R_ExternalPtrAddr(s));
  return Rf_mkString(buf);
}

// Thanks to Mikael Lagan: https://stackoverflow.com/questions/75874717
SEXP objectFromAddress(SEXP a) {
  uintptr_t p = 0;

  if (TYPEOF(a) != STRSXP || XLENGTH(a) != 1 ||
      (a = STRING_ELT(a, 0)) == NA_STRING ||
      (sscanf(CHAR(a), "%" SCNxPTR, &p) != 1))
    error("'a' is not a formatted unsigned hexadecimal integer");

  SEXP result = (SEXP) p;
  if (TYPEOF(result) != ENVSXP) return R_NilValue;
  return result;
}


