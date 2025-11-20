serialize_header <- function(x, i = 1) {
  serialization_format <- serialize_serialization_format(x, i)
  header_separator <- serialize_header_separator(
    serialization_format$x,
    serialization_format$i
  )
  workspace_format <- serialize_workspace_format(
    header_separator$x,
    header_separator$i
  )
  r_version <- serialize_r_version(
    workspace_format$x,
    workspace_format$i
  )
  minimum_version <- serialize_minimum_version(
    r_version$x,
    r_version$i
  )
  character_set <- serialize_character_set(
    minimum_version$x,
    minimum_version$i
  )
  code <- c(
    serialization_format$code,
    header_separator$code,
    workspace_format$code,
    r_version$code,
    minimum_version$code,
    character_set$code
  )
  list(
    code = code,
    x = character_set$x,
    i = character_set$i
  )
}

serialize_serialization_format <- function(x, i) {
  serialization_format <- x[[1]]
  x <- x[-1]
  comment <- sprintf(
    "# %s: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x%s)) == '%s'",
    i,
    as.character(serialization_format),
    rawToChar(serialization_format)
  )
  code <- sprintf("0x%s,", as.character(serialization_format))
  out <- list(
    code = c(comment, code),
    x = x,
    i = i + 1
  )
  out
}

serialize_header_separator <- function(x, i) {
  header_separator <- x[[1]]
  stopifnot(identical(header_separator, as.raw(0x0a)))
  x <- x[-1]
  comment <- sprintf(
    '# %s: always 0x0a, the bytecode for "\\n", just a separator',
    i
  )
  code <- sprintf("0x%s,", as.character(header_separator))
  out <- list(
    code = c(comment, code),
    x = x,
    i = i + 1
  )
  out
}

serialize_workspace_format <- function(x, i) {
  workspace_format_bytes <- x[1:4]
  x <- x[-(1:4)]
  version_val <- as.integer(workspace_format_bytes[[4]])
  comment <- sprintf(
    "# %s-%s: The workspace format version, %s since R 3.5.0, always 2 before that",
    i,
    i + 3,
    version_val
  )
  code <- paste(sprintf("0x%s,", as.character(workspace_format_bytes)), collapse = " ")
  out <- list(
    code = c(comment, code),
    x = x,
    i = i + 4
  )
  out
}

serialize_r_version <- function(x, i) {
  r_version_bytes <- x[1:4]
  x <- x[-(1:4)]
  version_string <- paste(
    as.integer(r_version_bytes[[2]]),
    as.integer(r_version_bytes[[3]]),
    as.integer(r_version_bytes[[4]]),
    sep = "."
  )
  comment <- sprintf(
    "# %s-%s: The R version used to serialize, here %s",
    i,
    i + 3,
    version_string
  )
  code <- paste(sprintf("0x%s,", as.character(r_version_bytes)), collapse = " ")
  out <- list(
    code = c(comment, code),
    x = x,
    i = i + 4
  )
  out
}

serialize_minimum_version <- function(x, i) {
  min_version_bytes <- x[1:4]
  x <- x[-(1:4)]
  version_string <- paste(
    as.integer(min_version_bytes[[2]]),
    as.integer(min_version_bytes[[3]]),
    as.integer(min_version_bytes[[4]]),
    sep = "."
  )
  comment <- sprintf(
    "# %s-%s: The minimal R version that can unserialize this, e.g. %s",
    i,
    i + 3,
    version_string
  )
  code <- paste(sprintf("0x%s,", as.character(min_version_bytes)), collapse = " ")
  out <- list(
    code = c(comment, code),
    x = x,
    i = i + 4
  )
  out
}

serialize_character_set <- function(x, i) {
  # Part 1: Length of the character set name
  length_bytes <- x[1:4]
  x <- x[-(1:4)]
  charset_len <- as.integer(length_bytes[[4]])
  length_comment <- sprintf(
    "# %s-%s: The length of the character set name in bytes: %s",
    i,
    i + 3,
    charset_len
  )
  length_code <- paste(sprintf("0x%s,", as.character(length_bytes)), collapse = " ")

  # Part 2: The character set name itself
  i_name_start <- i + 4
  charset_bytes <- x[1:charset_len]
  x <- x[-(1:charset_len)]
  charset_name <- rawToChar(charset_bytes)
  name_comment <- sprintf(
    "# %s-%s: Which character set is used for the encoding, e.g. '%s'",
    i_name_start,
    i_name_start + charset_len - 1,
    charset_name
  )

  # The new character-by-character comment
  chars <- sapply(charset_bytes, rawToChar)
  char_comment <- trimws(paste0("#  ", paste(sprintf("%-6s", chars), collapse = "")))

  name_code <- paste(sprintf("0x%s,", as.character(charset_bytes)), collapse = " ")

  # Combine and return
  out <- list(
    code = c(length_comment, length_code, name_comment, char_comment, name_code),
    x = x,
    i = i_name_start + charset_len
  )
  out
}