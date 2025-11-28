serialize_data <- function(x, i) {
  # This is the main dispatcher function.
  # It reads an object's header and calls the appropriate sub-function.
  header_info <- serialize_packed_header(x, i)

  # Dispatch based on type
  res <- switch(
    as.character(header_info$type),
    "255" = serialize_refsxp(header_info$x, header_info$i),  # 0xFF REFSXP (reference)
    "254" = serialize_nilvalue_sxp(header_info$x, header_info$i),  # 0xFE NILVALUE_SXP (NULL)
    "253" = serialize_globalenv_sxp(header_info$x, header_info$i),  # 0xFD GLOBALENV_SXP (global env)
    "251" = serialize_missingarg_sxp(header_info$x, header_info$i),  # 0xFB MISSINGARG_SXP (missing arg)
    "238" = serialize_altrep_sxp(header_info$x, header_info$i),  # 0xEE ALTREP_SXP (alt-rep)
    "24" = serialize_rawsxp(header_info$x, header_info$i),   # 0x18 RAWSXP (raw vector)
    "20" = serialize_exprsxp(header_info$x, header_info$i),  # 0x14 EXPRSXP (expression vector)
    "19" = serialize_vecsxp(header_info$x, header_info$i),   # 0x13 VECSXP (generic list)
    "16" = serialize_strsxp(header_info$x, header_info$i),   # 0x10 STRSXP (character vector)
    "15" = serialize_cplxsxp(header_info$x, header_info$i),  # 0x0F CPLXSXP (complex vector)
    "14" = serialize_realsxp(header_info$x, header_info$i),  # 0x0E REALSXP (numeric vector)
    "13" = serialize_intsxp(header_info$x, header_info$i),   # 0x0D INTSXP (integer vector)
    "10" = serialize_lglsxp(header_info$x, header_info$i),   # 0x0A LGLSXP (logical vector)
    "9"  = serialize_chrsxp(header_info$x, header_info$i),   # 0x09 CHARSXP (a single string)
    "8"  = serialize_builtinsxp(header_info$x, header_info$i),  # 0x08 BUILTINSXP (builtin function)
    "7"  = serialize_specialsxp(header_info$x, header_info$i),  # 0x07 SPECIALSXP (special function)
    "6"  = serialize_langsxp(header_info$x, header_info$i),  # 0x06 LANGSXP (language/call)
    "4"  = serialize_envsxp(header_info$x, header_info$i),   # 0x04 ENVSXP (environment)
    "3"  = serialize_closxp(header_info$x, header_info$i),   # 0x03 CLOSXP (function)
    "2"  = serialize_listsxp(header_info$x, header_info$i, header_info$flags),  # 0x02 LISTSXP (pairlist)
    "1"  = serialize_symsxp(header_info$x, header_info$i),   # 0x01 SYMSXP (symbol)
    # Fallback for unknown types
    list(code = "# UNKNOWN OR UNIMPLEMENTED DATA TYPE", x = header_info$x, i = header_info$i)
  )

  # Prepend the header code that we already processed
  res$code <- c(header_info$code, res$code)

  # Check if this object has attributes (HAS_ATTR = bit 1 = 0x02)
  has_attr <- bitwAnd(header_info$flags, 0x02) > 0
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
  # - Bytes 1-2: GP bits and levs (not commonly used)
  # - Byte 3: Flags (HAS_ATTR=0x02, HAS_TAG=0x04, IS_S4=0x08, etc.)
  # - Byte 4: Type code identifying the SEXP type
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
    "6" = "LANGSXP (language/call)",
    "7" = "SPECIALSXP (special function)",
    "8" = "BUILTINSXP (builtin function)",
    "9" = "CHARSXP (string)",
    "10" = "LGLSXP (logical vector)",
    "13" = "INTSXP (integer vector)",
    "14" = "REALSXP (numeric vector)",
    "15" = "CPLXSXP (complex vector)",
    "16" = "STRSXP (character vector)",
    "19" = "VECSXP (list)",
    "20" = "EXPRSXP (expression vector)",
    "24" = "RAWSXP (raw vector)",
    "238" = "ALTREP_SXP (alternative representation)",
    "251" = "MISSINGARG_SXP (missing argument)",
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
  if (bitwAnd(flags, 0x02) > 0) {
    flag_descriptions <- c(flag_descriptions, "HAS_ATTR")
  }
  if (bitwAnd(flags, 0x04) > 0) {
    flag_descriptions <- c(flag_descriptions, "HAS_TAG")
  }
  if (bitwAnd(flags, 0x08) > 0) {
    flag_descriptions <- c(flag_descriptions, "IS_S4")
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
    flags = flags
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
  # ┌─────────────────────────────────────────────────────────────────┐
  # │ Byte 1        │ Byte 2        │ Bytes 3-8 (6 bytes)             │
  # │ [S][Exp 7bit] │ [Exp 4b][Man] │ [Mantissa 48 bits]              │
  # └─────────────────────────────────────────────────────────────────┘
  #   1bit  7bits      4bits  4bits    48 bits
  # Total: 1 sign + 11 exp + 52 mantissa = 64 bits
  #
  # Example for pi (0x40, 0x09, 0x21, ...):
  # 0x40 = 0100 0000₂ → sign=0, exp upper 7 bits = 100 0000₂ = 64
  # 0x09 = 0000 1001₂ → exp lower 4 bits = 0000₂, mantissa starts with 1001₂

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

  # IEEE 754 formula: value = (-1)^sign × 2^(exp_raw - 1023) × (1 + mantissa/2^52)
  # The "1 +" is the implicit leading bit (always 1 for normalized numbers)
  # The mantissa/2^52 is the fractional part
  sprintf("numeric (sign=%d exp=%d mantissa=0x%s: (-1)^%d × 2^%d × (1 + 0x%s/2^52) == %.15g)",
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

serialize_chrsxp <- function(x, i) {
  # Handles a CHARSXP (a single string)
  # 1. Read string length
  len_bytes <- x[1:4]
  x <- x[-(1:4)]
  len <- sum(as.integer(len_bytes) * 256^c(3,2,1,0))

  # Check for NA_character_ (length = -1, which is 0xffffffff)
  if (identical(len_bytes, as.raw(c(0xff, 0xff, 0xff, 0xff)))) {
    len_comment <- sprintf("# %s-%s: NA_character_ (length = -1)", i, i + 3)
    len_code <- paste(sprintf("0x%s", as.character(len_bytes)), collapse = ", ")
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
  str_bytes <- x[1:len]
  x <- x[-(1:len)]

  # Convert to characters, respecting multi-byte sequences
  full_string <- rawToChar(str_bytes)
  chars <- strsplit(full_string, "")[[1]]
  bytes_per_char <- sapply(chars, function(ch) length(charToRaw(ch)))

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

        char_width <- nchar(char, type = "width")

        if (byte_len == 1) {
          # Single-byte character: left-align (like UTF-8 header)
          padded_char <- paste0(char, strrep(" ", total_width - char_width))
        } else {
          # Multi-byte character: center in its byte span
          left_padding <- floor((total_width - char_width) / 2)
          right_padding <- total_width - char_width - left_padding
          padded_char <- paste0(
            strrep(" ", left_padding),
            char,
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

serialize_nilvalue_sxp <- function(x, i) {
  # Handles NILVALUE_SXP (NULL)
  # NULL has no data after the packed header, just return empty code
  list(code = character(0), x = x, i = i)
}

serialize_symsxp <- function(x, i) {
  # Handles a SYMSXP (symbol)
  # A symbol contains a CHARSXP that holds the symbol name
  # We recursively process the CHARSXP

  charsxp_res <- serialize_data(x, i)

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

serialize_listsxp <- function(x, i, flags) {
  # Handles a LISTSXP (pairlist node)
  # Structure depends on flags:
  #   HAS_ATTR (0x02): if set, attributes follow
  #   HAS_TAG (0x04): if set, TAG (name) follows
  # Then CAR (value) and CDR (next node or NULL) always follow

  all_code <- character(0)

  # Check if this node has attributes (HAS_ATTR = bit 1 = 0x02)
  has_attr <- bitwAnd(flags, 0x02) > 0
  if (has_attr) {
    attr_comment <- sprintf("# %s: LISTSXP node attributes", i)
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
    tag_comment <- sprintf("# %s: LISTSXP TAG (name)", i)
    all_code <- c(all_code, tag_comment)

    # Recursively parse TAG (usually a SYMSXP)
    tag_res <- serialize_data(x, i)
    all_code <- c(all_code, tag_res$code)
    x <- tag_res$x
    i <- tag_res$i
  }

  # CAR: the value of this pairlist element (always present)
  car_comment <- sprintf("# %s: LISTSXP CAR (value)", i)
  all_code <- c(all_code, car_comment)

  car_res <- serialize_data(x, i)
  all_code <- c(all_code, car_res$code)
  x <- car_res$x
  i <- car_res$i

  # CDR: next pairlist node or NULL (always present)
  cdr_comment <- sprintf("# %s: LISTSXP CDR (next node or NULL)", i)
  all_code <- c(all_code, cdr_comment)

  cdr_res <- serialize_data(x, i)
  all_code <- c(all_code, cdr_res$code)
  x <- cdr_res$x
  i <- cdr_res$i

  list(code = all_code, x = x, i = i)
}

serialize_langsxp <- function(x, i) {
  # Handles a LANGSXP (language object / function call)
  # Structure:
  #   CAR: the function to call (usually a SYMSXP)
  #   CDR: the arguments (LISTSXP or NILVALUE_SXP)
  # Similar to LISTSXP but without TAG support

  all_code <- character(0)

  # CAR: the function to call
  car_comment <- sprintf("# %s: LANGSXP CAR (function)", i)
  all_code <- c(all_code, car_comment)

  car_res <- serialize_data(x, i)
  all_code <- c(all_code, car_res$code)
  x <- car_res$x
  i <- car_res$i

  # CDR: arguments pairlist or NULL
  cdr_comment <- sprintf("# %s: LANGSXP CDR (arguments)", i)
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

serialize_globalenv_sxp <- function(x, i) {
  # Handles GLOBALENV_SXP (type 0xFD, 253)
  # This is a special reference to the global environment
  # It has no data beyond the header
  list(code = character(0), x = x, i = i)
}

serialize_envsxp <- function(x, i) {
  # Handles an ENVSXP (environment)
  # Structure:
  #   Locked flag (4 bytes): 0 = not locked, 1 = locked
  #   Enclosing environment (ENVSXP or reference)
  #   Frame (LISTSXP pairlist of bindings, or NULL)
  #   Hashtab (VECSXP hash table, or NULL)
  #   Attributes (if HAS_ATTR flag set)

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

  list(code = all_code, x = x, i = i)
}

serialize_closxp <- function(x, i) {
  # Handles a CLOSXP (function/closure)
  # Structure (from empirical observation):
  #   Environment: reference to function's environment (ENVSXP or special reference)
  #   Formals: parameter list (LISTSXP pairlist or NILVALUE_SXP)
  #   Body: function body (usually LANGSXP or { } block)
  #   Attributes: always present (usually NULL if no attributes)

  all_code <- character(0)

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

  # CLOSXP always has a 4th component (attributes), even if it's NULL
  # This is NOT controlled by HAS_ATTR flag - it's always present
  attr_comment <- sprintf("# %s: CLOSXP attributes (always present)", i)
  all_code <- c(all_code, attr_comment)

  attr_res <- serialize_data(x, i)
  all_code <- c(all_code, attr_res$code)
  x <- attr_res$x
  i <- attr_res$i

  list(code = all_code, x = x, i = i)
}

serialize_missingarg_sxp <- function(x, i) {
  # Handles MISSINGARG_SXP (type 0xFB, 251)
  # This represents a missing argument in a function's formals
  # (parameters without default values)
  # It has no data beyond the header
  list(code = character(0), x = x, i = i)
}

serialize_refsxp <- function(x, i) {
  # Handles REFSXP (type 0xFF, 255)
  # This represents a reference to a previously serialized object
  # Structure: 4-byte reference index

  # Read reference index
  ref_bytes <- x[1:4]
  x <- x[-(1:4)]
  ref_idx <- sum(as.integer(ref_bytes) * 256^c(3,2,1,0))

  ref_comment <- sprintf("# %s-%s: REFSXP reference index: %d", i, i + 3, ref_idx)
  ref_code <- paste(sprintf("0x%s,", as.character(ref_bytes)), collapse = " ")
  i <- i + 4

  list(code = c(ref_comment, ref_code), x = x, i = i)
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

    name_comment <- sprintf("# %s-%s: BUILTINSXP name: \"%s\"", i, i + len - 1, name)
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

    name_comment <- sprintf("# %s-%s: SPECIALSXP name: \"%s\"", i, i + len - 1, name)
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
