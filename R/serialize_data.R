serialize_data <- function(x, i) {
  # This is the main dispatcher function.
  # It reads an object's header and calls the appropriate sub-function.
  # The layout follows `WriteItem()` in R's src/main/serialize.c
  if (length(x) < 4) {
    abort(sprintf("Internal error: serialized stream ended unexpectedly at byte %s", i))
  }
  header_info <- serialize_packed_header(x, i)
  type <- header_info$type
  flags <- header_info$flags

  # Dispatch based on type
  res <- switch(
    as.character(type),
    "255" = serialize_refsxp(header_info$x, header_info$i, header_info$ref_index),  # 0xFF REFSXP (reference)
    "254" = ,  # 0xFE NILVALUE_SXP (NULL)
    "253" = ,  # 0xFD GLOBALENV_SXP (global env)
    "252" = ,  # 0xFC UNBOUNDVALUE_SXP (unbound value)
    "251" = ,  # 0xFB MISSINGARG_SXP (missing arg)
    "250" = ,  # 0xFA BASENAMESPACE_SXP (base namespace)
    "242" = ,  # 0xF2 EMPTYENV_SXP (empty env)
    "241" = serialize_singleton_sxp(header_info$x, header_info$i),  # 0xF1 BASEENV_SXP (base env)
    "249" = serialize_persistent_names(header_info$x, header_info$i, "NAMESPACESXP", "<namespace>"),  # 0xF9 NAMESPACESXP
    "248" = serialize_persistent_names(header_info$x, header_info$i, "PACKAGESXP", "<package env>"),  # 0xF8 PACKAGESXP
    "247" = serialize_persistent_names(header_info$x, header_info$i, "PERSISTSXP", "<persistent>"),  # 0xF7 PERSISTSXP
    "238" = serialize_altrep_sxp(header_info$x, header_info$i),  # 0xEE ALTREP_SXP (alt-rep)
    "25" = serialize_s4sxp(header_info$x, header_info$i),    # 0x19 S4SXP (S4 object, only attributes)
    "24" = serialize_rawsxp(header_info$x, header_info$i),   # 0x18 RAWSXP (raw vector)
    "23" = serialize_weakrefsxp(header_info$x, header_info$i),  # 0x17 WEAKREFSXP (weak reference)
    "22" = serialize_extptrsxp(header_info$x, header_info$i),  # 0x16 EXTPTRSXP (external pointer)
    "20" = serialize_exprsxp(header_info$x, header_info$i),  # 0x14 EXPRSXP (expression vector)
    "19" = serialize_vecsxp(header_info$x, header_info$i),   # 0x13 VECSXP (generic list)
    "16" = serialize_strsxp(header_info$x, header_info$i),   # 0x10 STRSXP (character vector)
    "15" = serialize_cplxsxp(header_info$x, header_info$i),  # 0x0F CPLXSXP (complex vector)
    "14" = serialize_realsxp(header_info$x, header_info$i),  # 0x0E REALSXP (numeric vector)
    "13" = serialize_intsxp(header_info$x, header_info$i),   # 0x0D INTSXP (integer vector)
    "10" = serialize_lglsxp(header_info$x, header_info$i),   # 0x0A LGLSXP (logical vector)
    "9"  = serialize_chrsxp(header_info$x, header_info$i, flags),   # 0x09 CHARSXP (a single string)
    "8"  = serialize_builtinsxp(header_info$x, header_info$i),  # 0x08 BUILTINSXP (builtin function)
    "7"  = serialize_specialsxp(header_info$x, header_info$i),  # 0x07 SPECIALSXP (special function)
    "17" = serialize_listsxp(header_info$x, header_info$i, flags, "DOTSXP"),  # 0x11 DOTSXP (dots)
    "6"  = serialize_listsxp(header_info$x, header_info$i, flags, "LANGSXP"),  # 0x06 LANGSXP (language/call)
    "5"  = serialize_listsxp(header_info$x, header_info$i, flags, "PROMSXP"),  # 0x05 PROMSXP (promise)
    "4"  = serialize_envsxp(header_info$x, header_info$i),   # 0x04 ENVSXP (environment)
    "3"  = serialize_closxp(header_info$x, header_info$i, flags),   # 0x03 CLOSXP (function)
    "2"  = serialize_listsxp(header_info$x, header_info$i, flags, "LISTSXP"),  # 0x02 LISTSXP (pairlist)
    "1"  = serialize_symsxp(header_info$x, header_info$i),   # 0x01 SYMSXP (symbol)
    "21" = serialize_bcodesxp(header_info$x, header_info$i),  # 0x15 BCODESXP (byte code)
    abort(sprintf(
      "Internal error: unknown or unsupported SEXP type %s (0x%02x) at byte %s",
      type, type, i
    ))
  )

  # Prepend the header code that we already processed
  res$code <- c(header_info$code, res$code)

  # Most types write their attributes last if HAS_ATTR (bit 1 = 0x02) is set.
  # Exceptions:
  # * pairlist-like types and closures write them first (handled in their own functions)
  # * environments and ALTREP objects always write them (handled in their own functions)
  # * for references the flag bits are part of the reference index
  # * CHARSXP, symbols and the singleton/persistent types have none
  attr_last_types <- c(7, 8, 10, 13, 14, 15, 16, 19, 20, 21, 22, 23, 24, 25)
  has_attr <- type %in% attr_last_types && bitwAnd(flags, 0x02) > 0
  if (has_attr) {
    attr_comment <- sprintf("# %s: Object attributes", res$i)
    res$code <- c(res$code, attr_comment)

    # Recursively parse attributes (pairlist)
    attr_res <- serialize_data(res$x, res$i)
    res$code <- c(res$code, attr_res$code)
    res$x <- attr_res$x
    res$i <- attr_res$i
  }

  res
}

serialize_packed_header <- function(x, i) {
  # Packed header is 4 bytes:
  # - Bytes 1-2 and high half of byte 3: GP bits ("levels", e.g. the S4 bit is 0x01 in byte 2)
  # - Byte 3: Flags (IS_OBJECT=0x01, HAS_ATTR=0x02, HAS_TAG=0x04)
  # - Byte 4: Type code identifying the SEXP type
  # For REFSXP bytes 1-3 are not flags but the index of the referenced object
  header_bytes <- x[1:4]
  x <- x[-(1:4)]
  type <- as.integer(header_bytes[[4]])
  flags <- as.integer(header_bytes[[3]])

  # Map type codes to SEXP names
  type_names <- c(
    "0" = "NILSXP (NULL)",
    "1" = "SYMSXP (symbol)",
    "2" = "LISTSXP (pairlist)",
    "3" = "CLOSXP (function closure)",
    "4" = "ENVSXP (environment)",
    "5" = "PROMSXP (promise)",
    "6" = "LANGSXP (language/call)",
    "7" = "SPECIALSXP (special function)",
    "8" = "BUILTINSXP (builtin function)",
    "9" = "CHARSXP (string)",
    "10" = "LGLSXP (logical vector)",
    "13" = "INTSXP (integer vector)",
    "14" = "REALSXP (numeric vector)",
    "15" = "CPLXSXP (complex vector)",
    "16" = "STRSXP (character vector)",
    "17" = "DOTSXP (dots)",
    "19" = "VECSXP (list)",
    "20" = "EXPRSXP (expression vector)",
    "21" = "BCODESXP (byte code)",
    "22" = "EXTPTRSXP (external pointer)",
    "23" = "WEAKREFSXP (weak reference)",
    "24" = "RAWSXP (raw vector)",
    "25" = "S4SXP (S4 object)",
    "238" = "ALTREP_SXP (alternative representation)",
    "241" = "BASEENV_SXP (base environment)",
    "242" = "EMPTYENV_SXP (empty environment)",
    "247" = "PERSISTSXP (persistent reference)",
    "248" = "PACKAGESXP (package environment)",
    "249" = "NAMESPACESXP (namespace)",
    "250" = "BASENAMESPACE_SXP (base namespace)",
    "251" = "MISSINGARG_SXP (missing argument)",
    "252" = "UNBOUNDVALUE_SXP (unbound value)",
    "253" = "GLOBALENV_SXP (global environment)",
    "254" = "NILVALUE_SXP (NULL value)",
    "255" = "REFSXP (reference)"
  )

  type_name <- type_names[as.character(type)]
  if (is.na(type_name)) {
    type_name <- sprintf("Unknown type (0x%x)", type)
  }

  # Decode flags
  flag_descriptions <- character(0)
  ref_index <- NULL
  if (type == 255) {
    # REFSXP: the index is packed in the first 3 bytes, if it is 0 it didn't fit
    # and follows as a separate integer
    ref_index <- sum(as.integer(header_bytes[1:3]) * 256^c(2, 1, 0))
  } else {
    if (bitwAnd(flags, 0x01) > 0) {
      flag_descriptions <- c(flag_descriptions, "IS_OBJECT")
    }
    if (bitwAnd(flags, 0x02) > 0) {
      flag_descriptions <- c(flag_descriptions, "HAS_ATTR")
    }
    if (bitwAnd(flags, 0x04) > 0) {
      flag_descriptions <- c(flag_descriptions, "HAS_TAG")
    }
    if (type == 9) {
      # CHARSXP: the GP bits contain the encoding
      gp2 <- as.integer(header_bytes[[2]])
      if (bitwAnd(flags, 0x20) > 0) flag_descriptions <- c(flag_descriptions, "BYTES")
      if (bitwAnd(flags, 0x40) > 0) flag_descriptions <- c(flag_descriptions, "LATIN1")
      if (bitwAnd(flags, 0x80) > 0) flag_descriptions <- c(flag_descriptions, "UTF8")
      if (bitwAnd(gp2, 0x04) > 0) flag_descriptions <- c(flag_descriptions, "ASCII")
    } else if (bitwAnd(as.integer(header_bytes[[2]]), 0x01) > 0) {
      flag_descriptions <- c(flag_descriptions, "IS_S4")
    }
  }

  # Build comment
  if (length(flag_descriptions) > 0) {
    flag_str <- sprintf(" | flags: %s", paste(flag_descriptions, collapse = ", "))
  } else {
    flag_str <- ""
  }

  comment <- sprintf("# %s-%s: %s%s", i, i + 3, type_name, flag_str)
  code <- paste(sprintf("0x%s,", as.character(header_bytes)), collapse = " ")
  list(
    code = c(comment, code),
    x = x,
    i = i + 4,
    type = type,
    flags = flags,
    ref_index = ref_index
  )
}

serialize_strsxp <- function(x, i) {
  # Handles a STRSXP (character vector)
  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Loop and process each element (which will be a CHARSXP)
  if (len > 0) {
    for (j in 1:len) {
      element_res <- serialize_data(x, i)
      # Trim the comma from the code block of the element
      trimmed_code <- trim_last_comma(element_res$code)
      # Always add comma after closing paren - trim_last_comma will remove if needed
      suffix <- if (j < len) ")," else "),"
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), suffix)
      x <- element_res$x
      i <- element_res$i
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_rawsxp <- function(x, i) {
  # Handles a RAWSXP (raw vector)
  # Raw vectors are sequences of bytes (1 byte each)

  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read raw bytes
  if (len > 0) {
    raw_bytes <- x[1:len]
    x <- x[-(1:len)]

    raw_comment <- sprintf("# %s-%s: raw bytes", i, i + len - 1)
    raw_code <- paste(sprintf("0x%s,", as.character(raw_bytes)), collapse = " ")
    all_code <- c(all_code, raw_comment, raw_code)
    i <- i + len
  }

  list(code = all_code, x = x, i = i)
}

serialize_cplxsxp <- function(x, i) {
  # Handles a CPLXSXP (complex vector)
  # Each complex value is 16 bytes: 8 bytes real + 8 bytes imaginary
  # Both parts are IEEE 754 doubles

  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read each complex value (16 bytes each)
  if (len > 0) {
    for (j in 1:len) {
      # Read real part (8 bytes)
      real_bytes <- x[1:8]
      x <- x[-(1:8)]
      real_label <- identify_double(real_bytes)

      # Read imaginary part (8 bytes)
      imag_bytes <- x[1:8]
      x <- x[-(1:8)]
      imag_label <- identify_double(imag_bytes)

      # Determine overall label
      if (real_label == "NA_real_" && imag_label == "NA_real_") {
        cplx_label <- "NA_complex_"
      } else {
        cplx_label <- sprintf("complex (real: %s, imag: %s)", real_label, imag_label)
      }

      real_comment <- sprintf("# %s-%s: %s", i, i + 15, cplx_label)
      real_code <- paste(sprintf("0x%s,", as.character(real_bytes)), collapse = " ")
      imag_code <- paste(sprintf("0x%s,", as.character(imag_bytes)), collapse = " ")
      # Wrap in c() with proper indentation
      element_code <- c(real_comment, real_code, imag_code)
      trimmed_code <- trim_last_comma(element_code)
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
      i <- i + 16
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_realsxp <- function(x, i) {
  # Handles a REALSXP (numeric/double vector)
  # Values are stored as 8-byte IEEE 754 double-precision floats (big-endian)

  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read each double value (8 bytes each)
  if (len > 0) {
    for (j in 1:len) {
      val_bytes <- x[1:8]
      x <- x[-(1:8)]

      # Get detailed breakdown with formula and byte-level comments
      breakdown <- explain_double(val_bytes, i)
      # Trim the comma from the code block of the element
      trimmed_code <- trim_last_comma(breakdown$code)
      # Wrap in c() with proper indentation
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")

      i <- i + 8
    }
  }

  list(code = all_code, x = x, i = i)
}

explain_double <- function(val_bytes, i) {
  # Generate detailed breakdown showing IEEE 754 formula and byte-level comments
  # Returns list(code = character vector with formula line + byte lines)

  byte1 <- as.integer(val_bytes[1])
  byte2 <- as.integer(val_bytes[2])

  # Extract IEEE 754 components
  sign_bit <- byte1 %/% 128
  exp_upper <- byte1 %% 128  # Lower 7 bits of byte1
  exp_lower <- byte2 %/% 16  # Upper 4 bits of byte2
  exp_biased <- exp_upper * 16 + exp_lower
  mantissa_upper <- byte2 %% 16  # Lower 4 bits of byte2
  mantissa_bytes <- as.integer(val_bytes[3:8])
  mantissa_hex <- sprintf("0x%x%s", mantissa_upper,
                          paste(sprintf("%02x", mantissa_bytes), collapse = ""))

  # Convert to actual value
  val <- readBin(val_bytes, "double", n = 1, size = 8, endian = "big")

  # Check for special cases
  if (identical(val_bytes[1:2], as.raw(c(0x7f, 0xf0))) &&
      identical(val_bytes[7:8], as.raw(c(0x07, 0xa2)))) {
    # NA_real_
    formula <- sprintf("# %d-%d: NA_real_", i, i + 7)
    byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
    return(list(code = c(formula, byte_code)))
  }

  if (identical(val_bytes, as.raw(c(0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    formula <- sprintf("# %d-%d: Inf", i, i + 7)
    byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
    return(list(code = c(formula, byte_code)))
  }

  if (identical(val_bytes, as.raw(c(0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    formula <- sprintf("# %d-%d: -Inf", i, i + 7)
    byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
    return(list(code = c(formula, byte_code)))
  }

  if (identical(val_bytes, as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    formula <- sprintf("# %d-%d: 0 (all bits zero)", i, i + 7)
    byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
    return(list(code = c(formula, byte_code)))
  }

  if (identical(val_bytes, as.raw(c(0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    formula <- sprintf("# %d-%d: -0 (sign=1, all other bits zero)", i, i + 7)
    byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
    return(list(code = c(formula, byte_code)))
  }

  # Check for NaN (exponent all 1s, mantissa non-zero)
  exp_all_ones <- ((byte1 %% 128) == 127) && ((byte2 %/% 16) == 15)
  if (exp_all_ones) {
    mantissa_bits <- c(byte2 %% 16, mantissa_bytes)
    if (any(mantissa_bits != 0)) {
      if (identical(val_bytes[1:2], as.raw(c(0x7f, 0xf8)))) {
        formula <- sprintf("# %d-%d: NaN (standard pattern)", i, i + 7)
      } else {
        formula <- sprintf("# %d-%d: NaN (non-standard payload)", i, i + 7)
      }
      byte_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
      return(list(code = c(formula, byte_code)))
    }
  }

  # Regular number - show full breakdown
  formula <- sprintf(
    "# %d-%d: (-1)^0x%02x * 2^(16 * 0x%02x + 0x%02x - 1023) * (1 + %s/2^52) == %.15g",
    i, i + 7, sign_bit, exp_upper, exp_lower, mantissa_hex, val
  )

  # Byte 1: sign and upper exponent
  byte1_bits <- as.integer(intToBits(byte1)[8:1])
  byte1_bin <- sprintf("%d %s", byte1_bits[1], paste(byte1_bits[2:8], collapse = ""))
  byte1_comment <- sprintf("0x%02x, # binary: %s => sign==%d, exponent_upper==0x%02x",
                           byte1, byte1_bin, sign_bit, exp_upper)

  # Byte 2: lower exponent and upper mantissa
  byte2_bits <- as.integer(intToBits(byte2)[8:1])
  byte2_bin <- sprintf("%s %s",
                       paste(byte2_bits[1:4], collapse = ""),
                       paste(byte2_bits[5:8], collapse = ""))
  byte2_comment <- sprintf("0x%02x, # binary: %s => exponent_lower==0x%02x, mantissa_upper==0x%x",
                           byte2, byte2_bin, exp_lower, mantissa_upper)

  # Bytes 3-8: rest of mantissa
  mantissa_rest_hex <- paste(sprintf("%02x", mantissa_bytes), collapse = "")
  bytes_3_8 <- sprintf("%s # mantissa_lower==0x%s",
                       paste(sprintf("0x%02x,", mantissa_bytes), collapse = " "),
                       mantissa_rest_hex)

  list(code = c(formula, byte1_comment, byte2_comment, bytes_3_8))
}

identify_double <- function(val_bytes) {
  # Identify special double values by their IEEE 754 byte patterns
  # Input: 8 raw bytes in big-endian format
  # Returns: descriptive string with value

  # Check for R's NA_real_ first (specific NaN payload)
  if (identical(val_bytes[1:2], as.raw(c(0x7f, 0xf0))) &&
      identical(val_bytes[7:8], as.raw(c(0x07, 0xa2)))) {
    return("NA_real_")
  }

  # Check for positive infinity
  if (identical(val_bytes, as.raw(c(0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    return("Inf")
  }

  # Check for negative infinity
  if (identical(val_bytes, as.raw(c(0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    return("-Inf")
  }

  # Check for positive zero
  if (identical(val_bytes, as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    return("0 (sign=0 exp=0 mantissa=0 == 0)")
  }

  # Check for negative zero
  if (identical(val_bytes, as.raw(c(0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    return("-0 (sign=1 exp=0 mantissa=0 == -0)")
  }

  # Check for NaN (exponent all 1s, mantissa non-zero)
  # Byte 1: 0x7f (positive) or 0xff (negative) means sign + top 7 bits of exp
  # Byte 2: 0xfX means bottom 4 bits of exp are 1s
  byte1 <- as.integer(val_bytes[1])
  byte2 <- as.integer(val_bytes[2])

  # Exponent is all 1s if (byte1 & 0x7f) == 0x7f and (byte2 & 0xf0) == 0xf0
  exp_all_ones <- ((byte1 %% 128) == 127) && ((byte2 %/% 16) == 15)

  if (exp_all_ones) {
    # Check if mantissa is non-zero (making it NaN, not Inf)
    # Mantissa is in bottom 4 bits of byte2 and all of bytes 3-8
    mantissa_bits <- c(byte2 %% 16, as.integer(val_bytes[3:8]))
    if (any(mantissa_bits != 0)) {
      # It's some kind of NaN (not Inf, not NA_real_)
      # Check if it's the standard NaN pattern
      if (identical(val_bytes[1:2], as.raw(c(0x7f, 0xf8)))) {
        return("NaN")
      } else {
        # Non-standard NaN with specific payload
        return("NaN (non-standard)")
      }
    }
  }

  # Regular numeric value - decode it and show IEEE 754 breakdown
  # Convert bytes back to double to get the actual value
  val <- readBin(val_bytes, "double", n = 1, size = 8, endian = "big")

  # IEEE 754 double precision format (8 bytes = 64 bits):
  # +---------------+---------------+---------------------------------+
  # | Byte 1        | Byte 2        | Bytes 3-8 (6 bytes)             |
  # | [S][Exp 7bit] | [Exp 4b][Man] | [Mantissa 48 bits]              |
  # +---------------+---------------+---------------------------------+
  #   1bit  7bits      4bits  4bits    48 bits
  # Total: 1 sign + 11 exp + 52 mantissa = 64 bits
  #
  # Example for pi (0x40, 0x09, 0x21, ...):
  # 0x40 = 0100 0000 -> sign=0, exp upper 7 bits = 100 0000 = 64
  # 0x09 = 0000 1001 -> exp lower 4 bits = 0000, mantissa starts with 1001

  sign <- byte1 %/% 128  # Extract bit 0 (sign bit)
  # Exponent: 11 bits total
  #   - Upper 7 bits from byte1 (bits 1-7)
  #   - Lower 4 bits from byte2 (bits 0-3)
  exp_raw <- ((byte1 %% 128) * 16) + (byte2 %/% 16)  # Biased by 1023
  exp <- exp_raw - 1023  # Actual exponent

  # Mantissa (fraction): 52 bits total
  #   - Upper 4 bits from byte2 (bits 4-7)
  #   - Remaining 48 bits from bytes 3-8
  mantissa_bytes <- c(byte2 %% 16, as.integer(val_bytes[3:8]))
  mantissa_hex <- paste(sprintf("%02x", mantissa_bytes), collapse = "")

  # IEEE 754 formula: value = (-1)^sign * 2^(exp_raw - 1023) * (1 + mantissa/2^52)
  # The "1 +" is the implicit leading bit (always 1 for normalized numbers)
  # The mantissa/2^52 is the fractional part
  sprintf("numeric (sign=%d exp=%d mantissa=0x%s: (-1)^%d * 2^%d * (1 + 0x%s/2^52) == %.15g)",
          sign, exp_raw, mantissa_hex, sign, exp, mantissa_hex, val)
}

serialize_intsxp <- function(x, i) {
  # Handles an INTSXP (integer vector)
  # Integer values are stored as 4-byte signed integers, NA_integer_ = -2147483648

  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read each integer value (4 bytes each)
  if (len > 0) {
    for (j in 1:len) {
      val_bytes <- x[1:4]
      x <- x[-(1:4)]

      # Convert to signed integer
      val <- sum(as.integer(val_bytes) * 256^c(3,2,1,0))
      # Handle 2's complement for negative values
      if (val >= 2^31) val <- val - 2^32

      # Determine what the value represents
      val_label <- if (val == -2147483648) {
        "NA_integer_"
      } else {
        sprintf("%dL", val)
      }

      val_comment <- sprintf("# %s-%s: %s", i, i + 3, val_label)
      val_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
      # Wrap in c() with proper indentation
      element_code <- c(val_comment, val_code)
      trimmed_code <- trim_last_comma(element_code)
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
      i <- i + 4
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_lglsxp <- function(x, i) {
  # Handles a LGLSXP (logical vector)
  # Logical values are stored as 4-byte integers: TRUE=1, FALSE=0, NA=-2147483648

  # 1. Read vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read each logical value (4 bytes each)
  if (len > 0) {
    for (j in 1:len) {
      val_bytes <- x[1:4]
      x <- x[-(1:4)]

      # Convert to signed integer
      val <- sum(as.integer(val_bytes) * 256^c(3,2,1,0))
      # Handle 2's complement for negative values
      if (val >= 2^31) val <- val - 2^32

      # Determine what the value represents
      val_label <- if (val == 1) {
        "TRUE"
      } else if (val == 0) {
        "FALSE"
      } else if (val == -2147483648) {
        "NA"
      } else {
        sprintf("value=%d", val)
      }

      val_comment <- sprintf("# %s-%s: %s", i, i + 3, val_label)
      val_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
      # Wrap in c() with proper indentation
      element_code <- c(val_comment, val_code)
      trimmed_code <- trim_last_comma(element_code)
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
      i <- i + 4
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_chrsxp <- function(x, i, flags) {
  # Handles a CHARSXP (a single string)
  # 1. Read string length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))

  # Check for NA_character_ (length = -1, which is 0xffffffff)
  if (identical(len_bytes, as.raw(c(0xff, 0xff, 0xff, 0xff)))) {
    len_comment <- sprintf("# %s-%s: NA_character_ (length = -1)", i, i + 3)
    len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
    return(list(
      code = c(len_comment, len_code),
      x = x,
      i = i + 4
    ))
  }

  len_comment <- sprintf("# %s-%s: length of string in bytes: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  # 2. Read string data
  str_bytes <- x[seq_len(len)]
  x <- x[seq_along(x) > len]

  # Convert to characters, respecting multi-byte sequences
  full_string <- rawToChar(str_bytes)
  is_latin1 <- bitwAnd(flags, 0x40) > 0
  if (is_latin1) {
    # one byte per character, we display the translated characters
    chars <- strsplit(iconv(full_string, "latin1", "UTF-8"), "")[[1]]
    bytes_per_char <- rep(1L, len)
    chars <- vapply(chars, serialize_escape, character(1), USE.NAMES = FALSE)
  } else if (!validUTF8(full_string)) {
    # "bytes" encoding or native non UTF-8 strings, we display the bytes
    chars <- sprintf("\\x%s", as.character(str_bytes))
    is_ascii <- str_bytes < as.raw(0x80)
    chars[is_ascii] <- vapply(str_bytes[is_ascii], function(b) serialize_escape(rawToChar(b)), character(1))
    bytes_per_char <- rep(1L, len)
  } else {
    chars <- strsplit(full_string, "")[[1]]
    bytes_per_char <- vapply(chars, function(ch) length(charToRaw(ch)), integer(1), USE.NAMES = FALSE)
    # escape characters like \n
    chars <- vapply(chars, serialize_escape, character(1), USE.NAMES = FALSE)
  }

  # Split into chunks of max 12 bytes per line
  all_code <- c(len_comment, len_code)

  if (len > 0) {
    byte_idx <- 1
    char_idx <- 1

    while (byte_idx <= len) {
      # Determine how many bytes fit in this line (max 12)
      line_bytes_count <- 0
      line_chars <- character(0)
      line_bytes_per_char <- integer(0)

      while (char_idx <= length(chars) && line_bytes_count + bytes_per_char[char_idx] <= 12) {
        line_chars <- c(line_chars, chars[char_idx])
        line_bytes_per_char <- c(line_bytes_per_char, bytes_per_char[char_idx])
        line_bytes_count <- line_bytes_count + bytes_per_char[char_idx]
        char_idx <- char_idx + 1
      }

      # Get the bytes for this line
      line_bytes <- str_bytes[byte_idx:(byte_idx + line_bytes_count - 1)]

      # Create character comment with proper alignment
      # Pattern: "#  " prefix, then chars spaced to align with bytes below
      # Single-byte chars: left-aligned like UTF-8 header
      # Multi-byte chars: centered with adjusted spacing
      padded_chars <- character(0)
      for (j in seq_along(line_chars)) {
        char <- line_chars[j]
        byte_len <- line_bytes_per_char[j]

        # Calculate total width for this character's bytes
        is_last_char_in_line <- (j == length(line_chars))
        is_last_line <- (byte_idx + line_bytes_count > len)
        is_last_byte_of_string <- is_last_char_in_line && is_last_line

        if (is_last_byte_of_string) {
          # Last byte has no comma
          total_width <- (byte_len - 1) * 6 + 4
        } else {
          total_width <- byte_len * 6
        }

        char_escaped <- char
        char_width <- nchar(char_escaped, type = "width")

        if (byte_len == 1) {
          # Single-byte character: left-align (like UTF-8 header)
          padded_char <- paste0(char_escaped, strrep(" ", max(total_width - char_width, 0)))
        } else {
          # Multi-byte character: center in its byte span
          left_padding <- max(floor((total_width - char_width) / 2), 0)
          right_padding <- max(total_width - char_width - left_padding, 0)
          padded_char <- paste0(
            strrep(" ", left_padding),
            char_escaped,
            strrep(" ", right_padding)
          )
        }
        padded_chars <- c(padded_chars, padded_char)
      }

      char_comment <- paste0("#  ", paste(padded_chars, collapse = ""))

      # Create byte code for this line
      # Always add commas - trim_last_comma will remove the final one if needed
      line_code <- sprintf("0x%s,", as.character(line_bytes))
      line_code <- paste(line_code, collapse = " ")

      all_code <- c(all_code, char_comment, line_code)
      byte_idx <- byte_idx + line_bytes_count
    }
  }

  i <- i + len

  list(
    code = all_code,
    x = x,
    i = i
  )
}

serialize_singleton_sxp <- function(x, i) {
  # Handles the types that stand for a unique object: NILVALUE_SXP (NULL),
  # GLOBALENV_SXP, EMPTYENV_SXP, BASEENV_SXP, BASENAMESPACE_SXP,
  # MISSINGARG_SXP and UNBOUNDVALUE_SXP
  # They have no data after the packed header, just return empty code
  list(code = character(0), x = x, i = i)
}

serialize_escape <- function(chr) {
  # Escape characters like \n so they can be displayed in a comment
  # non ASCII characters are displayed as is
  if (grepl("[^ -~]", gsub("[[:cntrl:]]", "", chr), useBytes = TRUE)) return(chr)
  escaped <- construct_string(chr, unicode_representation = "latin", escape = FALSE)
  # Remove leading and trailing double quotes
  substr(escaped, 2, nchar(escaped) - 1)
}

serialize_add_ref <- function(label) {
  # Symbols, environments, namespaces, external pointers and weak references
  # are stored in a reference table when first met, later occurrences are
  # REFSXP items pointing to the (1-based) index in this table.
  # We keep track of the table to document what references point to.
  globals[["serialize_refs"]] <- c(globals[["serialize_refs"]], label)
}

serialize_symsxp <- function(x, i) {
  # Handles a SYMSXP (symbol)
  # A symbol contains a CHARSXP that holds the symbol name
  # We recursively process the CHARSXP

  charsxp_res <- serialize_data(x, i)
  name <- rawToChar(x[-(1:8)][seq_len(charsxp_res$i - i - 8)])
  serialize_add_ref(sprintf("symbol `%s`", serialize_escape(name)))

  # Return the CHARSXP code
  list(code = charsxp_res$code, x = charsxp_res$x, i = charsxp_res$i)
}

serialize_vecsxp <- function(x, i) {
  # Handles a VECSXP (generic list)
  # 1. Read list length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of list: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Loop through each element and recursively serialize
  if (len > 0) {
    for (j in 1:len) {
      element_res <- serialize_data(x, i)
      # Trim the comma from the code block of the element
      trimmed_code <- trim_last_comma(element_res$code)
      # Wrap in c() with proper indentation
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
      x <- element_res$x
      i <- element_res$i
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_listsxp <- function(x, i, flags, type = "LISTSXP") {
  # Handles a LISTSXP (pairlist node) and the types sharing its layout:
  # LANGSXP (call), PROMSXP (promise) and DOTSXP (dots)
  # Structure depends on flags:
  #   HAS_ATTR (0x02): if set, attributes follow
  #   HAS_TAG (0x04): if set, TAG (name) follows
  # Then CAR (value) and CDR (next node or NULL) always follow

  all_code <- character(0)
  labels <- switch(
    type,
    LISTSXP = c(tag = "TAG (name)", car = "CAR (value)", cdr = "CDR (next node or NULL)"),
    LANGSXP = c(tag = "TAG (name)", car = "CAR (function)", cdr = "CDR (arguments)"),
    PROMSXP = c(tag = "environment", car = "value", cdr = "expression"),
    DOTSXP = c(tag = "TAG (name)", car = "CAR (value)", cdr = "CDR (next node or NULL)")
  )

  # Check if this node has attributes (HAS_ATTR = bit 1 = 0x02)
  # Unlike for other types they come first
  has_attr <- bitwAnd(flags, 0x02) > 0
  if (has_attr) {
    attr_comment <- sprintf("# %s: %s attributes", i, type)
    all_code <- c(all_code, attr_comment)

    # Recursively parse attributes
    attr_res <- serialize_data(x, i)
    all_code <- c(all_code, attr_res$code)
    x <- attr_res$x
    i <- attr_res$i
  }

  # Check if this node has a TAG/name (HAS_TAG = bit 2 = 0x04)
  has_tag <- bitwAnd(flags, 0x04) > 0
  if (has_tag) {
    tag_comment <- sprintf("# %s: %s %s", i, type, labels[["tag"]])
    all_code <- c(all_code, tag_comment)

    # Recursively parse TAG (usually a SYMSXP)
    tag_res <- serialize_data(x, i)
    all_code <- c(all_code, tag_res$code)
    x <- tag_res$x
    i <- tag_res$i
  }

  # CAR: the value of this pairlist element (always present)
  car_comment <- sprintf("# %s: %s %s", i, type, labels[["car"]])
  all_code <- c(all_code, car_comment)

  car_res <- serialize_data(x, i)
  all_code <- c(all_code, car_res$code)
  x <- car_res$x
  i <- car_res$i

  # CDR: next pairlist node or NULL (always present)
  cdr_comment <- sprintf("# %s: %s %s", i, type, labels[["cdr"]])
  all_code <- c(all_code, cdr_comment)

  cdr_res <- serialize_data(x, i)
  all_code <- c(all_code, cdr_res$code)
  x <- cdr_res$x
  i <- cdr_res$i

  list(code = all_code, x = x, i = i)
}

serialize_exprsxp <- function(x, i) {
  # Handles an EXPRSXP (expression vector)
  # Structure: length (4 bytes) + N recursively serialized expressions
  # Similar to VECSXP but for expressions

  # 1. Read expression vector length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  len_comment <- sprintf("# %s-%s: length of expression vector: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Loop through each expression and recursively serialize
  if (len > 0) {
    for (j in 1:len) {
      expr_res <- serialize_data(x, i)
      # Trim the comma from the code block of the expression
      trimmed_code <- trim_last_comma(expr_res$code)
      # Wrap in c() with proper indentation
      all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
      x <- expr_res$x
      i <- expr_res$i
    }
  }

  list(code = all_code, x = x, i = i)
}

serialize_envsxp <- function(x, i) {
  # Handles an ENVSXP (environment)
  # Structure:
  #   Locked flag (4 bytes): 0 = not locked, 1 = locked
  #   Enclosing environment (ENVSXP or reference)
  #   Frame (LISTSXP pairlist of bindings, or NULL)
  #   Hashtab (VECSXP hash table, or NULL)
  #   Attributes (always present, NULL if no attributes, the HAS_ATTR flag is not used)

  serialize_add_ref("environment")
  all_code <- character(0)

  # Locked flag
  locked_bytes <- x[1:4]
  x <- x[-(1:4)]
  locked <- sum(as.integer(locked_bytes) * 256^c(3,2,1,0))
  locked_comment <- sprintf("# %s-%s: ENVSXP locked flag: %d", i, i + 3, locked)
  locked_code <- paste(sprintf("0x%s,", as.character(locked_bytes)), collapse = " ")
  i <- i + 4
  all_code <- c(all_code, locked_comment, locked_code)

  # Enclosing environment
  enclos_comment <- sprintf("# %s: ENVSXP enclosing environment", i)
  all_code <- c(all_code, enclos_comment)

  enclos_res <- serialize_data(x, i)
  all_code <- c(all_code, enclos_res$code)
  x <- enclos_res$x
  i <- enclos_res$i

  # Frame (bindings)
  frame_comment <- sprintf("# %s: ENVSXP frame (bindings)", i)
  all_code <- c(all_code, frame_comment)

  frame_res <- serialize_data(x, i)
  all_code <- c(all_code, frame_res$code)
  x <- frame_res$x
  i <- frame_res$i

  # Hashtab
  hashtab_comment <- sprintf("# %s: ENVSXP hashtab", i)
  all_code <- c(all_code, hashtab_comment)

  hashtab_res <- serialize_data(x, i)
  all_code <- c(all_code, hashtab_res$code)
  x <- hashtab_res$x
  i <- hashtab_res$i

  # Attributes
  attr_comment <- sprintf("# %s: ENVSXP attributes (always present)", i)
  all_code <- c(all_code, attr_comment)

  attr_res <- serialize_data(x, i)
  all_code <- c(all_code, attr_res$code)
  x <- attr_res$x
  i <- attr_res$i

  list(code = all_code, x = x, i = i)
}

serialize_closxp <- function(x, i, flags) {
  # Handles a CLOSXP (function/closure)
  # Same layout as a pairlist node:
  #   Attributes: only if the HAS_ATTR flag is set (e.g. srcref), they come first
  #   Environment: function's environment (ENVSXP, reference, or special type)
  #   Formals: parameter list (LISTSXP pairlist or NILVALUE_SXP)
  #   Body: function body (usually LANGSXP or { } block)

  all_code <- character(0)

  # Attributes
  has_attr <- bitwAnd(flags, 0x02) > 0
  if (has_attr) {
    attr_comment <- sprintf("# %s: CLOSXP attributes", i)
    all_code <- c(all_code, attr_comment)

    attr_res <- serialize_data(x, i)
    all_code <- c(all_code, attr_res$code)
    x <- attr_res$x
    i <- attr_res$i
  }

  # Environment
  env_comment <- sprintf("# %s: CLOSXP environment", i)
  all_code <- c(all_code, env_comment)

  env_res <- serialize_data(x, i)
  all_code <- c(all_code, env_res$code)
  x <- env_res$x
  i <- env_res$i

  # Formals (parameters)
  formals_comment <- sprintf("# %s: CLOSXP formals (parameters)", i)
  all_code <- c(all_code, formals_comment)

  formals_res <- serialize_data(x, i)
  all_code <- c(all_code, formals_res$code)
  x <- formals_res$x
  i <- formals_res$i

  # Body
  body_comment <- sprintf("# %s: CLOSXP body", i)
  all_code <- c(all_code, body_comment)

  body_res <- serialize_data(x, i)
  all_code <- c(all_code, body_res$code)
  x <- body_res$x
  i <- body_res$i

  list(code = all_code, x = x, i = i)
}

serialize_refsxp <- function(x, i, ref_index) {
  # Handles REFSXP (type 0xFF, 255)
  # This represents a reference to a previously serialized symbol, environment,
  # namespace, external pointer or weak reference.
  # The reference index is packed in the first 3 bytes of the header, unless it
  # doesn't fit, then these are 0 and the index follows as a 4-byte integer

  all_code <- character(0)
  if (ref_index == 0) {
    ref_bytes <- x[1:4]
    x <- x[-(1:4)]
    ref_index <- sum(as.integer(ref_bytes) * 256^c(3,2,1,0))
    all_code <- paste(sprintf("0x%s,", as.character(ref_bytes)), collapse = " ")
    i <- i + 4
  }

  refs <- globals[["serialize_refs"]]
  target <- if (ref_index <= length(refs)) sprintf(" (%s)", refs[[ref_index]]) else ""
  ref_comment <- sprintf("# REFSXP reference index: %d%s", ref_index, target)

  list(code = c(ref_comment, all_code), x = x, i = i)
}

serialize_persistent_names <- function(x, i, type, ref_label) {
  # Handles NAMESPACESXP, PACKAGESXP and PERSISTSXP
  # These are not serialized by value but by name, as a character vector without
  # its own packed header:
  #   4 bytes: always 0
  #   4 bytes: length of character vector
  #   N CHARSXP items (for a namespace: its name and version)

  serialize_add_ref(ref_label)

  zero_bytes <- x[1:4]
  len_bytes <- x[5:8]
  x <- x[-(1:8)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))
  all_code <- c(
    sprintf("# %s-%s: %s padding (always 0)", i, i + 3, type),
    paste(sprintf("0x%s,", as.character(zero_bytes)), collapse = " "),
    sprintf("# %s-%s: %s length of info vector: %d", i + 4, i + 7, type, len),
    paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  )
  i <- i + 8

  for (j in seq_len(len)) {
    element_res <- serialize_data(x, i)
    all_code <- c(all_code, element_res$code)
    x <- element_res$x
    i <- element_res$i
  }

  list(code = all_code, x = x, i = i)
}

serialize_extptrsxp <- function(x, i) {
  # Handles EXTPTRSXP (external pointer)
  # The address itself is not serialized (it is NULL after unserializing)
  # Structure: protected value, tag, then attributes if HAS_ATTR (handled by caller)

  serialize_add_ref("external pointer")

  prot_comment <- sprintf("# %s: EXTPTRSXP protected value", i)
  prot_res <- serialize_data(x, i)

  tag_comment <- sprintf("# %s: EXTPTRSXP tag", prot_res$i)
  tag_res <- serialize_data(prot_res$x, prot_res$i)

  list(
    code = c(prot_comment, prot_res$code, tag_comment, tag_res$code),
    x = tag_res$x,
    i = tag_res$i
  )
}

serialize_weakrefsxp <- function(x, i) {
  # Handles WEAKREFSXP (weak reference)
  # Key and value are not serialized, only attributes if HAS_ATTR (handled by caller)
  serialize_add_ref("weak reference")
  list(code = character(0), x = x, i = i)
}

serialize_s4sxp <- function(x, i) {
  # Handles S4SXP (S4 object that is not of a basic type)
  # No data of its own, slots are attributes (handled by caller)
  list(code = character(0), x = x, i = i)
}

serialize_altrep_sxp <- function(x, i) {
  # Handles ALTREP_SXP (type 0xEE, 238)
  # Alt-rep (alternative representation) is used for compact storage of sequences
  # Structure (from empirical observation):
  #   Class info pairlist: contains class names ("compact_intseq", "base", etc.)
  #   Data: implementation-specific, usually REALSXP with (length, start, step)
  #   Attributes: always present (usually NULL if no attributes)

  all_code <- character(0)

  # Class info (pairlist)
  class_comment <- sprintf("# %s: ALTREP class info (pairlist)", i)
  all_code <- c(all_code, class_comment)

  class_res <- serialize_data(x, i)
  all_code <- c(all_code, class_res$code)
  x <- class_res$x
  i <- class_res$i

  # Data (implementation-specific, often REALSXP with parameters)
  data_comment <- sprintf("# %s: ALTREP data", i)
  all_code <- c(all_code, data_comment)

  data_res <- serialize_data(x, i)
  all_code <- c(all_code, data_res$code)
  x <- data_res$x
  i <- data_res$i

  # ALTREP always has a 3rd component (attributes), even if it's NULL
  # This is NOT controlled by HAS_ATTR flag - it's always present
  attr_comment <- sprintf("# %s: ALTREP attributes (always present)", i)
  all_code <- c(all_code, attr_comment)

  attr_res <- serialize_data(x, i)
  all_code <- c(all_code, attr_res$code)
  x <- attr_res$x
  i <- attr_res$i

  list(code = all_code, x = x, i = i)
}
serialize_builtinsxp <- function(x, i) {
  # Handles BUILTINSXP (type 0x08, 8)
  # Builtin functions like sum, mean, length, etc.
  # Structure: 4-byte length + N bytes of function name (as raw bytes)

  # 1. Read name length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))

  len_comment <- sprintf("# %s-%s: BUILTINSXP name length: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read name bytes
  if (len > 0) {
    name_bytes <- x[1:len]
    x <- x[-(1:len)]
    name <- rawToChar(name_bytes)

    name_escaped <- serialize_escape(name)
    name_comment <- sprintf("# %s-%s: BUILTINSXP name: \"%s\"", i, i + len - 1, name_escaped)
    all_code <- c(all_code, name_comment)

    # Format name bytes in rows of 8
    byte_code <- sprintf("0x%s,", as.character(name_bytes))
    for (row_start in seq(1, len, by = 8)) {
      row_end <- min(row_start + 7, len)
      row <- paste(byte_code[row_start:row_end], collapse = " ")
      all_code <- c(all_code, row)
    }

    i <- i + len
  }

  list(code = all_code, x = x, i = i)
}

serialize_specialsxp <- function(x, i) {
  # Handles SPECIALSXP (type 0x07, 7)
  # Special functions like if, for, while, function, etc.
  # Structure: 4-byte length + N bytes of function name (as raw bytes)

  # 1. Read name length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))

  len_comment <- sprintf("# %s-%s: SPECIALSXP name length: %d", i, i + 3, len)
  len_code <- paste(sprintf("0x%s,", as.character(len_bytes)), collapse = " ")
  i <- i + 4

  all_code <- c(len_comment, len_code)

  # 2. Read name bytes
  if (len > 0) {
    name_bytes <- x[1:len]
    x <- x[-(1:len)]
    name <- rawToChar(name_bytes)

    name_escaped <- serialize_escape(name)
    name_comment <- sprintf("# %s-%s: SPECIALSXP name: \"%s\"", i, i + len - 1, name_escaped)
    all_code <- c(all_code, name_comment)

    # Format name bytes in rows of 8
    byte_code <- sprintf("0x%s,", as.character(name_bytes))
    for (row_start in seq(1, len, by = 8)) {
      row_end <- min(row_start + 7, len)
      row <- paste(byte_code[row_start:row_end], collapse = " ")
      all_code <- c(all_code, row)
    }

    i <- i + len
  }

  list(code = all_code, x = x, i = i)
}

serialize_int <- function(x, i, label) {
  # Reads a 4-byte integer that is not part of a packed header
  if (length(x) < 4) {
    abort(sprintf("Internal error: serialized stream ended unexpectedly at byte %s", i))
  }
  bytes <- x[1:4]
  value <- sum(as.integer(bytes) * 256^c(3,2,1,0))
  comment <- sprintf("# %s-%s: %s: %d", i, i + 3, label, value)
  code <- paste(sprintf("0x%s,", as.character(bytes)), collapse = " ")
  list(code = c(comment, code), x = x[-(1:4)], i = i + 4, value = value)
}

serialize_bcodesxp <- function(x, i) {
  # Handles BCODESXP (byte code), e.g. the body of a byte-compiled function
  # Structure (see `WriteBC()` in R's src/main/serialize.c):
  #   4 bytes: size of the table of language objects found several times in the
  #     constants (they're written once and referenced afterwards)
  #   the byte code itself
  reps_res <- serialize_int(x, i, "BCODESXP size of table of repeated language objects")
  bc_res <- serialize_bc1(reps_res$x, reps_res$i)
  list(code = c(reps_res$code, bc_res$code), x = bc_res$x, i = bc_res$i)
}

serialize_bc1 <- function(x, i) {
  # Structure:
  #   instructions: INTSXP, the first value is the byte code version
  #   4 bytes: number of constants
  #   constants, each of them is preceded by a 4-byte type, and is either:
  #     * nested byte code (type BCODESXP)
  #     * a language object or pairlist in the special byte code layout
  #     * a regular item
  code_comment <- sprintf("# %s: BCODESXP instructions", i)
  code_res <- serialize_data(x, i)
  n_res <- serialize_int(code_res$x, code_res$i, "BCODESXP number of constants")
  all_code <- c(code_comment, code_res$code, n_res$code)
  x <- n_res$x
  i <- n_res$i

  for (j in seq_len(n_res$value)) {
    const_res <- serialize_bc_item(x, i, sprintf("BCODESXP constant %s", j))
    trimmed_code <- trim_last_comma(const_res$code)
    all_code <- c(all_code, "c(", paste0("  ", trimmed_code), "),")
    x <- const_res$x
    i <- const_res$i
  }

  list(code = all_code, x = x, i = i)
}

serialize_bc_item <- function(x, i, label) {
  # Handles constants of byte code and the CAR and CDR of their language objects
  # (see `WriteBCLang()` in R's src/main/serialize.c)
  # Each item starts with a 4-byte type, without flags:
  #   BCODESXP (21): nested byte code
  #   LANGSXP (6), LISTSXP (2): tag, CAR, CDR
  #   ATTRLANGSXP (240), ATTRLISTSXP (239): the same preceded by attributes
  #   BCREPDEF (244): 4-byte index, then the type and the object as above, which
  #     is registered in the table of repeated language objects
  #   BCREPREF (243): 4-byte index, points to an object of the above table
  #   anything else: a regular item with its own packed header, the type
  #     is 0 for the CAR and CDR of language objects
  type_names <- c(
    "2" = "LISTSXP (pairlist)",
    "6" = "LANGSXP (language/call)",
    "21" = "BCODESXP (byte code)",
    "239" = "ATTRLISTSXP (pairlist with attributes)",
    "240" = "ATTRLANGSXP (language/call with attributes)",
    "243" = "BCREPREF (reference to repeated language object)",
    "244" = "BCREPDEF (definition of repeated language object)"
  )
  type_label <- function(type) {
    if (as.character(type) %in% names(type_names)) {
      return(sprintf("%s type: %s", label, type_names[[as.character(type)]]))
    }
    sprintf("%s type, ignored for regular items", label)
  }

  if (length(x) < 4) {
    abort(sprintf("Internal error: serialized stream ended unexpectedly at byte %s", i))
  }
  type <- as.integer(x[[4]])
  type_res <- serialize_int(x, i, type_label(type))
  all_code <- type_res$code
  x <- type_res$x
  i <- type_res$i

  if (type == 243) {
    index_res <- serialize_int(x, i, "BCREPREF index")
    return(list(code = c(all_code, index_res$code), x = index_res$x, i = index_res$i))
  }

  if (type == 244) {
    index_res <- serialize_int(x, i, "BCREPDEF index")
    type <- as.integer(index_res$x[[4]])
    type_res <- serialize_int(index_res$x, index_res$i, type_label(type))
    all_code <- c(all_code, index_res$code, type_res$code)
    x <- type_res$x
    i <- type_res$i
  }

  if (type == 21) {
    res <- serialize_bc1(x, i)
    return(list(code = c(all_code, res$code), x = res$x, i = res$i))
  }

  if (!type %in% c(2, 6, 239, 240)) {
    res <- serialize_data(x, i)
    return(list(code = c(all_code, res$code), x = res$x, i = res$i))
  }

  if (type %in% c(239, 240)) {
    all_code <- c(all_code, sprintf("# %s: %s attributes", i, label))
    attr_res <- serialize_data(x, i)
    all_code <- c(all_code, attr_res$code)
    x <- attr_res$x
    i <- attr_res$i
  }

  # contrary to regular pairlists and calls the tag is always there
  all_code <- c(all_code, sprintf("# %s: %s TAG (name)", i, label))
  tag_res <- serialize_data(x, i)
  car_res <- serialize_bc_item(tag_res$x, tag_res$i, "CAR")
  cdr_res <- serialize_bc_item(car_res$x, car_res$i, "CDR")

  list(
    code = c(all_code, tag_res$code, car_res$code, cdr_res$code),
    x = cdr_res$x,
    i = cdr_res$i
  )
}
