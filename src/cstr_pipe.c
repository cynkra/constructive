#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

/* Helper: call an R function (by name, no args) in base and return the result */
static SEXP call0(const char* fname) {
  SEXP fun = Rf_findFun(Rf_install(fname), R_BaseEnv);
  if (fun == R_UnboundValue)
    error("Internal error: base::%s() not found", fname);
  return Rf_eval(Rf_lang1(fun), R_BaseEnv);
}

/* Compare two numeric_version objects: returns TRUE if v1 >= v2 */
static int version_ge(SEXP v1, const char* v2_str) {
  /* v2_str is e.g. "4.2.0" */
  SEXP cmp_call = PROTECT(Rf_lang3(
    Rf_install(">="),
    v1,
    Rf_ScalarString(Rf_mkChar(v2_str))
  ));
  SEXP res = PROTECT(Rf_eval(cmp_call, R_BaseEnv));
  int ok = Rf_asLogical(res);
  UNPROTECT(2);
  return ok == TRUE;
}

/* Map a pipe name to its symbol, error on anything else */
static const char* get_pipe_symbol(const char* pipe) {
  if (strcmp(pipe, "base") == 0)      return "|>";
  if (strcmp(pipe, "magrittr") == 0)  return "%>%";
  if (strcmp(pipe, "plus") == 0)      return "+";
  error("`.cstr_pipe`: unknown pipe '%s' (allowed: base, magrittr, plus)", pipe);
  return NULL;  /* not reached */
}

SEXP cstr_pipeC(SEXP x, SEXP y, SEXP pipeSEXP, SEXP one_linerSEXP, SEXP indentSEXP) {
  PROTECT(x = Rf_coerceVector(x, STRSXP));
  PROTECT(y = Rf_coerceVector(y, STRSXP));
  int nx = Rf_length(x), ny = Rf_length(y);

  /* 1) Determine pipe name at runtime */
  const char* pipe_name;
  if (pipeSEXP == R_NilValue) {
    /* call base::getRversion() */
    SEXP rver = PROTECT(call0("getRversion"));
    pipe_name = version_ge(rver, "4.2.0") ? "base" : "magrittr";
    UNPROTECT(1);
  } else {
    SEXP pstr = PROTECT(Rf_coerceVector(pipeSEXP, STRSXP));
    pipe_name = CHAR(STRING_ELT(pstr, 0));
    if (strcmp(pipe_name, "plus") != 0 &&
        strcmp(pipe_name, "base") != 0 &&
        strcmp(pipe_name, "magrittr") != 0)
      error("`.cstr_pipe`: `pipe` must be NULL or one of 'base', 'magrittr', 'plus'");
    UNPROTECT(1);
  }
  const char* symbol = get_pipe_symbol(pipe_name);

  /* 2) one-liner path */
  if (LOGICAL(one_linerSEXP)[0]) {
    const char *sx = CHAR(STRING_ELT(x, nx - 1));
    const char *sy = CHAR(STRING_ELT(y, 0));
    size_t buflen = strlen(sx) + 1 + strlen(symbol) + 1 + strlen(sy) + 1;
    char *buf = (char*) R_alloc(buflen, 1);
    snprintf(buf, buflen, "%s %s %s", sx, symbol, sy);

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(out, 0, Rf_mkChar(buf));
    UNPROTECT(3); /* x, y, out */
  return out;
  }

  /* 3) General case: build output of length nx+ny */
  SEXP ans = PROTECT(Rf_allocVector(STRSXP, nx + ny));

  /* 3a) copy all but last of x */
  for (int i = 0; i < nx - 1; i++)
    SET_STRING_ELT(ans, i, STRING_ELT(x, i));

  /* 3b) last x with pipe appended */
    {
      const char *sx = CHAR(STRING_ELT(x, nx - 1));
      size_t bl = strlen(sx) + 1 + strlen(symbol) + 1;
      char *b = (char*) R_alloc(bl, 1);
      snprintf(b, bl, "%s %s", sx, symbol);
      SET_STRING_ELT(ans, nx - 1, Rf_mkChar(b));
    }

    /* 3c) copy or indent y */
    int do_indent = LOGICAL(indentSEXP)[0];
    for (int j = 0; j < ny; j++) {
      const char *sy = CHAR(STRING_ELT(y, j));
      if (do_indent) {
        size_t bl = 2 + strlen(sy) + 1;
        char *b = (char*) R_alloc(bl, 1);
        snprintf(b, bl, "  %s", sy);
        SET_STRING_ELT(ans, nx + j, Rf_mkChar(b));
      } else {
        SET_STRING_ELT(ans, nx + j, STRING_ELT(y, j));
      }
    }

    UNPROTECT(3); /* x, y, ans */
    return ans;
}

