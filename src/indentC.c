#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <string.h>

// C implementation of indent(x, depth)
SEXP indentC(SEXP x, SEXP depth) {
  // If x has length 0, return it unchanged
  if (LENGTH(x) == 0) {
    return x;
  }
  // Coerce depth to integer (truncates if non-integer)
  int d = Rf_asInteger(depth);
  // Compute total number of spaces: depth * 2, but not negative
  int prefixLen = d > 0 ? d * 2 : 0;
  // Allocate prefix buffer in R's memory (auto-freed)
  char *prefix = (char *) R_alloc(prefixLen + 1, sizeof(char));
  if (prefixLen > 0) {
    memset(prefix, ' ', prefixLen);
  }
  prefix[prefixLen] = '\0';

  // Coerce x to a character vector
  SEXP x_chr = PROTECT(Rf_coerceVector(x, STRSXP));
  int n = LENGTH(x_chr);

  // Allocate result character vector of same length
  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));

  for (int i = 0; i < n; ++i) {
    const char *s = CHAR(STRING_ELT(x_chr, i));
    int s_len = strlen(s);
    // Allocate buffer for prefix + original string
    char *buf = (char *) R_alloc(prefixLen + s_len + 1, sizeof(char));
    memcpy(buf, prefix, prefixLen);
    memcpy(buf + prefixLen, s, s_len);
    buf[prefixLen + s_len] = '\0';
    // Set the i-th element of the result
    SET_STRING_ELT(res, i, Rf_mkCharCE(buf, CE_UTF8));
  }
  UNPROTECT(2); // x_chr and res
  return res;
}


#include <R.h>
#include <Rinternals.h>

/* Escape a single C string, writing into buf (which must be at least newlen+1 bytes).
 Returns pointer just past last written char. */
static char* escape_one(const char* src, char* buf) {
  const unsigned char *s = (const unsigned char*) src;
  char *d = buf;
  for (; *s; s++) {
    switch (*s) {
    case '\"':  *d++ = '\\'; *d++ = '\"'; break;
    case '\\':  *d++ = '\\'; *d++ = '\\'; break;
    case '\n':  *d++ = '\\'; *d++ = 'n';  break;
    case '\t':  *d++ = '\\'; *d++ = 't';  break;
    case '\r':  *d++ = '\\'; *d++ = 'r';  break;
    case '\b':  *d++ = '\\'; *d++ = 'b';  break;
    case '\f':  *d++ = '\\'; *d++ = 'f';  break;
    case '\v':  *d++ = '\\'; *d++ = 'v';  break;
    default:
      if (*s < 32) {
        /* fallback for other control chars: use \\xHH */
        static const char hexdig[] = "0123456789ABCDEF";
        *d++ = '\\'; *d++ = 'x';
        *d++ = hexdig[*s >> 4];
        *d++ = hexdig[*s & 0xF];
      } else {
        *d++ = *s;
      }
    }
  }
  *d = '\0';
  return d;
}

/* .Call entry point: takes a character vector, returns a character vector. */
SEXP deparse_no_quoteC(SEXP x) {
  if (!isString(x))
    error("Input must be a character vector.");

  R_xlen_t n = xlength(x);
  SEXP out = PROTECT(allocVector(STRSXP, n));

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP s_elt = STRING_ELT(x, i);
    if (s_elt == NA_STRING) {
      SET_STRING_ELT(out, i, NA_STRING);
      continue;
    }
    const char* s = CHAR(s_elt);

    /* First pass: compute needed length */
    size_t newlen = 0;
    for (const unsigned char *p = (const unsigned char*) s; *p; p++) {
      switch (*p) {
      case '\"': case '\\':
        newlen += 2; break;
      case '\n': case '\t': case '\r':
      case '\b': case '\f': case '\v':
        newlen += 2; break;
      default:
        newlen += (*p < 32) ? 4 : 1;
      }
    }

    /* Allocate and do the escape */
    char *buf = (char*) R_alloc(newlen + 1, 1);
    escape_one(s, buf);

    SET_STRING_ELT(out, i, mkChar(buf));
  }

  UNPROTECT(1);
  return out;
}


