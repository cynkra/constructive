#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include <inttypes.h>

// Thanks to Randi Lai: https://github.com/randy3k/xptr/
SEXP external_pointer(SEXP p) {
  return R_MakeExternalPtr((void*) strtol(CHAR(STRING_PTR(p)[0]), NULL, 0), NULL, NULL);
}

SEXP external_pointer_address(SEXP s) {
  if (TYPEOF(s) != EXTPTRSXP) {
    error("external_pointer_address() expects an input of type 'externalptr'");
  }
  char* buf[20];
  sprintf((char*) buf, "%p", R_ExternalPtrAddr(s));
  return Rf_mkString((char*) buf);
}

// Thanks to Mikael Lagan: https://stackoverflow.com/questions/75874717
SEXP objectFromAddress(SEXP a) {
  uintptr_t p = 0;

  if (TYPEOF(a) != STRSXP || XLENGTH(a) != 1 ||
      (a = STRING_ELT(a, 0)) == NA_STRING ||
      (sscanf(CHAR(a), "%" SCNxPTR, &p) != 1))
    error("'a' is not a formatted unsigned hexadecimal integer");

  return (SEXP) p;
}
