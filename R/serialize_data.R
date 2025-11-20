serialize_data <- function(x, i) {
  # This is the main dispatcher function.
  # It reads an object's header and calls the appropriate sub-function.
  header_info <- serialize_packed_header(x, i)

  # Dispatch based on type
  res <- switch(
    as.character(header_info$type),
    "24" = serialize_rawsxp(header_info$x, header_info$i),   # 0x18 RAWSXP (raw vector)
    "16" = serialize_strsxp(header_info$x, header_info$i),   # 0x10 STRSXP (character vector)
    "15" = serialize_cplxsxp(header_info$x, header_info$i),  # 0x0F CPLXSXP (complex vector)
    "14" = serialize_realsxp(header_info$x, header_info$i),  # 0x0E REALSXP (numeric vector)
    "13" = serialize_intsxp(header_info$x, header_info$i),   # 0x0D INTSXP (integer vector)
    "10" = serialize_lglsxp(header_info$x, header_info$i),   # 0x0A LGLSXP (logical vector)
    "9"  = serialize_chrsxp(header_info$x, header_info$i),   # 0x09 CHARSXP (a single string)
    # Fallback for unknown types
    list(code = "# UNKNOWN OR UNIMPLEMENTED DATA TYPE", x = header_info$x, i = header_info$i)
  )

  # Prepend the header code that we already processed
  res$code <- c(header_info$code, res$code)
  res
}

serialize_packed_header <- function(x, i) {
  header_bytes <- x[1:4]
  x <- x[-(1:4)]
  type <- as.integer(header_bytes[[4]])
  # For now, a generic comment. This could be enhanced to show flags.
  comment <- sprintf("# %s-%s: Packed Header (type 0x%x)", i, i + 3, type)
  code <- paste(sprintf("0x%s,", as.character(header_bytes)), collapse = " ")
  list(
    code = c(comment, code),
    x = x,
    i = i + 4,
    type = type
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
      # Add a comma only if it's not the last element of the vector
      suffix <- if (j < len) ")," else ")"
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
      all_code <- c(all_code, real_comment, real_code, imag_code)
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

      val_label <- identify_double(val_bytes)

      val_comment <- sprintf("# %s-%s: %s", i, i + 7, val_label)
      val_code <- paste(sprintf("0x%s,", as.character(val_bytes)), collapse = " ")
      all_code <- c(all_code, val_comment, val_code)
      i <- i + 8
    }
  }

  list(code = all_code, x = x, i = i)
}

identify_double <- function(val_bytes) {
  # Identify special double values by their IEEE 754 byte patterns
  # Input: 8 raw bytes in big-endian format

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

  # Check for negative zero
  if (identical(val_bytes, as.raw(c(0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)))) {
    return("-0")
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

  # Regular numeric value
  "numeric"
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
      all_code <- c(all_code, val_comment, val_code)
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
      all_code <- c(all_code, val_comment, val_code)
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

  # Create a comment string that aligns each character over its corresponding bytes
  padded_chars <- mapply(function(char, byte_len) {
    # Each hex code is formatted as "0xXX, ", roughly 6 characters wide
    total_width <- byte_len * 6 - 1
    # We center the character in the allocated space
    padding <- floor((total_width - nchar(char)) / 2)
    paste0(strrep(" ", padding), char, strrep(" ", total_width - nchar(char) - padding))
  }, chars, bytes_per_char)
  char_comment <- paste0("#  ", paste(padded_chars, collapse = " "))

  str_code <- paste(sprintf("0x%s,", as.character(str_bytes)), collapse = " ")
  i <- i + len

  list(
    code = c(len_comment, len_code, char_comment, str_code),
    x = x,
    i = i
  )
}

serialize_lstsxp <- function(x, i) {
  # Placeholder for pairlists (attributes)
}

serialize_vecsxp <- function(x, i) {
  # Placeholder for generic lists
}