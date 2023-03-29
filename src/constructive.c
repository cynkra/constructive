#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>


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
