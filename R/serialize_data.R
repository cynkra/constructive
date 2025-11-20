serialize_data <- function(x, i) {
  # This is the main dispatcher function.
  # It reads an object's header and calls the appropriate sub-function.
  header_info <- serialize_packed_header(x, i)

  # Dispatch based on type
  res <- switch(
    as.character(header_info$type),
    "16" = serialize_strsxp(header_info$x, header_info$i),   # 0x10 STRSXP (character vector)
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