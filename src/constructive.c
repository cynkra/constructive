#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP external_pointer(SEXP);
extern SEXP external_pointer_address(SEXP);
extern SEXP objectFromAddress(SEXP);

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

// Thanks to Randi Lai: https://github.com/randy3k/xptr/
SEXP external_pointer(SEXP p) {
  if (TYPEOF(p) != STRSXP || LENGTH(p) < 1) {
    error("Input must be a character vector of at least length 1");
  }
  const char* str = CHAR(STRING_ELT(p, 0));
  void* ptr = (void*) strtol(str, NULL, 0);
  return R_MakeExternalPtr(ptr, R_NilValue, R_NilValue);
}

SEXP external_pointer_address(SEXP s) {
  if (TYPEOF(s) != EXTPTRSXP) {
    error("external_pointer_address() expects an input of type 'externalptr'");
  }
  char* buf[20];
  snprintf((char*) buf, 20, "%p", R_ExternalPtrAddr(s));
  return Rf_mkString((char*) buf);
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


