#include <R.h>
#include <Rinternals.h>
#include <string.h>

/* Forward-declare the pipe function from your previous work */
SEXP cstr_pipeC(SEXP x, SEXP y, SEXP pipeSEXP, SEXP one_linerSEXP, SEXP indentSEXP);

/* repair_encoding(code, string_is_ascii, encoding) */
SEXP repair_encodingC(SEXP codeSEXP, SEXP string_is_asciiSEXP, SEXP encodingSEXP) {
  /* 1) Coerce inputs */
  PROTECT(codeSEXP            = coerceVector(codeSEXP,    STRSXP));  /* code as character */
  PROTECT(string_is_asciiSEXP = coerceVector(string_is_asciiSEXP, LGLSXP));
  PROTECT(encodingSEXP        = coerceVector(encodingSEXP, STRSXP));
  const char *encoding = CHAR(STRING_ELT(encodingSEXP, 0));
  int string_is_ascii = LOGICAL(string_is_asciiSEXP)[0] == TRUE;

  /* 2) locale_is_like_encoding: call l10n_info() */
  SEXP l10n = PROTECT(Rf_eval(Rf_lang1(Rf_install("l10n_info")), R_BaseEnv));
  /* extract $`UTF-8` */
  SEXP utf8_flag = PROTECT(Rf_eval(Rf_lang3(Rf_install("$"), l10n, Rf_mkString("UTF-8")), R_BaseEnv));
  int locale_utf8 = LOGICAL(utf8_flag)[0] == TRUE;
  /* extract $`Latin-1` */
  SEXP l1_flag = PROTECT(Rf_eval(Rf_lang3(Rf_install("$"), l10n, Rf_mkString("Latin-1")), R_BaseEnv));
  int locale_l1 = LOGICAL(l1_flag)[0] == TRUE;
  UNPROTECT(3);  /* l10n, utf8_flag, l1_flag */

  int locale_is_like_encoding =
    (strcmp(encoding, "UTF-8") == 0 && locale_utf8) ||
    (strcmp(encoding, "latin1") == 0 && locale_l1);

  /* 3) globals$pedantic_encoding %||% FALSE */
  int pedantic = 0;
  SEXP globals = Rf_findVar(Rf_install("globals"), R_GlobalEnv);
  if (globals != R_UnboundValue && globals != R_NilValue) {
    SEXP ped_call = PROTECT(Rf_lang3(Rf_install("$"), globals, Rf_mkString("pedantic_encoding")));
    SEXP ped_val  = PROTECT(Rf_eval(ped_call, R_GlobalEnv));
    if (ped_val != R_NilValue && Rf_length(ped_val) >= 1) {
      pedantic = Rf_asLogical(ped_val) == TRUE;
    }
    UNPROTECT(2);
  }

  /* 4) decide whether to skip repair */
  int no_repair_needed =
  string_is_ascii ||
  locale_is_like_encoding ||
  ((!pedantic) && strcmp(encoding, "unknown") == 0);
  if (no_repair_needed) {
    UNPROTECT(3);  /* codeSEXP, string_is_asciiSEXP, encodingSEXP */
  return codeSEXP;
  }

  /* 5) build the one-liner y = sprintf("(`Encoding<-`)(\"%s\")", encoding) */
  const char *fmt = "(`Encoding<-`)(\"%s\")";
  size_t buf_len = strlen(fmt) - 2 + strlen(encoding) + 1;
  char *buf = (char*) R_alloc(buf_len, 1);
  snprintf(buf, buf_len, fmt, encoding);

  SEXP yArg = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(yArg, 0, Rf_mkChar(buf));

  /* call cstr_pipeC(code, yArg, pipe=NULL, one_liner=TRUE, indent=TRUE) */
  SEXP one = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP indent = PROTECT(Rf_ScalarLogical(TRUE));
  SEXP out = PROTECT(cstr_pipeC(codeSEXP, yArg, R_NilValue, one, indent));

  UNPROTECT(7);  /* codeSEXP, string_is_asciiSEXP, encodingSEXP,
   yArg, one, indent, out */
  return out;
}

