
compress_character <- function(x, ...) {
  if (length(x) > 2 && isTRUE(all(x == ""))) return(sprintf("character(%s)", length(x)))
  format_rep(x, ...)
}

# construct a vector of code that can be parsed to strings
construct_strings <- function(x, unicode_representation.chr = "ascii", escape.chr = FALSE, mode = "string", ...) {
  out <- sapply(
    x,
    construct_string,
    unicode_representation = unicode_representation.chr,
    escape = escape.chr,
    mode,
    USE.NAMES = FALSE
  )
  names(out) <- names(x)
  out
}


# construct a string with relevant format, incl quotes, so it can be parsed to a string
# We need modes "string", "name" and "symbol"
# * "string" always produces strings
# * names should not have double quotes unless backslash in the name, and backquotes if not syntatic
# * symbols can't use \U but can use \x (but then are dependent on native encoding),
#   if `protect` is FALSE we don't use backquotes
construct_string <- function(x, unicode_representation, escape, mode = "string", protect = TRUE) {
  # deal with NA early
  if (is_na(x)) return("NA_character_")

  encoding <- Encoding(x)
  locale_is_utf8 <- l10n_info()$`UTF-8`
  if (!locale_is_utf8) {
    return(construct_string_from_byte_value(x, encoding, mode, protect))
  }

  # If the encoding was not UTF-8 we impose it so we can use utf-8 construction
  # The encoding will be repaired if necessary
  out <- construct_utf8_string(
    x,
    encoding,
    unicode_representation,
    escape,
    mode,
    protect
  )

  out
}

construct_utf8_string <- function(x, encoding, unicode_representation, escape, mode = "string", protect = TRUE) {
  # Deal with corrupted strings
  x_utf8 <- iconv(x, to = "UTF-8") # NA on corrupted
  x_is_corrupted <- is_na(x_utf8)
  if (x_is_corrupted) {
    out <- construct_string_from_byte_value(x, encoding, mode, protect)
    return(out)
  }

  # Split the string into chars, fetch codepoints, and deparse without surrounding quotes
  chars <- suppressWarnings(strsplit(x_utf8, "")[[1]])
  codepoints <- sapply(chars, utf8ToInt) # NAs stay NA
  codepoints[is.na(chars)] <- -1
  deparsed_chars <- deparse_no_quotes(chars)
  # repair the corrupted items
  deparsed_chars[is.na(chars)] <- sapply(chars[is.na(chars)], deparse)

  # Use the "\U{}" notation where relevant
  limit <- switch(
    unicode_representation,
    ascii = 128,
    latin = 256,
    character = 0x1F000,
    unicode = Inf
  )
  uses_u <- any(codepoints >= limit)
  if (uses_u && mode == "symbol") {
    out <- construct_string_from_byte_value(x, encoding, mode, protect)
    return(out)
  }

  deparsed_chars <- ifelse(
    codepoints >= limit,
    sprintf("\\U{%X}", codepoints),
    deparsed_chars
  )

  # gather info about usage of special characters in string, necessary to
  # know what simplification can be applied
  uses_special_backlashes <- any(
    grepl("\\", deparsed_chars, fixed = TRUE) &
      chars != "\\" &
      chars != "\""
  )
  uses_regular_backslashes <- "\\" %in% chars
  uses_sq <-  "'" %in% chars
  uses_dbq <-  "\"" %in% chars

  # define conditions
  surround_with_single_quotes <- !escape && uses_dbq && !uses_sq
  use_raw_strings <- !escape && !uses_special_backlashes &&
    (uses_regular_backslashes | (uses_sq && uses_dbq))

  # finalize the code with the right format and surrounding quotes
  if (use_raw_strings) {
    out <- sprintf('r"[%s]"', paste(chars, collapse = ""))
  } else if (surround_with_single_quotes) {
    # unescape double quotes
    deparsed_chars[deparsed_chars == "\\\""] <- "\""
    out <- sprintf("'%s'", paste(deparsed_chars, collapse = ""))
  } else if (protect && (mode == "symbol" || (mode == "name" && !uses_u))) {
    out <- protect(paste(deparsed_chars, collapse = ""))
  } else if (protect) {
    out <- paste(c('"', deparsed_chars, '"'), collapse = "")
  } else {
    out <- paste(deparsed_chars, collapse = "")
  }

  # repair encoding with |> (`Encoding<-`)() when relevant
  string_is_ascii <- !x_is_corrupted && Encoding(x_utf8) == "unknown"
  repair_encoding(out, string_is_ascii, encoding)
}

construct_string_from_byte_value <- function(x, encoding, mode = "string", protect = TRUE) {
  bytes <- charToRaw(x)
  string_is_ascii <- all(bytes < 128)
  chars <- ifelse(
    bytes < 128,
    deparse_no_quotes(sapply(bytes, rawToChar)),
    sprintf("\\x%s", as.character(bytes))
  )
  out <- paste(chars, collapse = "")
  if (mode == "string") {
    out <- paste0('"', out, '"')
  } else if (protect) {
    out <- protect(out)
  }

  repair_encoding(out, string_is_ascii, encoding)
}

repair_encoding <- function(code, string_is_ascii, encoding) {
  locale_is_like_encoding <-
    (encoding == "UTF-8" && l10n_info()$`UTF-8`) ||
    (encoding == "latin1" && l10n_info()$`Latin-1`)

  no_repair_needed <-
    string_is_ascii ||
    locale_is_like_encoding ||
    (!(globals$pedantic_encoding %||% FALSE) && encoding == "unknown")
  if (no_repair_needed) return(code)
  .cstr_pipe(
    code,
    sprintf("(`Encoding<-`)(\"%s\")", encoding),
    one_liner = TRUE
  )
}

# a vectorized deparse() that trims the surrounding double quotes
deparse_no_quotes <- function(x) {
  sub("^.(.*).$", "\\1", sapply(x, deparse))
}
