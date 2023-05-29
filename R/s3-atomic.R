#' Constructive options for atomic types
#'
#' These options will be used on atomic types ("logical", "integer", "numeric", "complex", "character" and "raw")
#'
#' @param ... Should not be used. Forces passing arguments by name.
#' @param trim `NULL` or integerish. Maximum of elements showed before it's trimmed.
#' Note that it will necessarily produce code that doesn't reproduce the input.
#' This code will parse without failure but its evaluation might fail.
#' @param fill String. Method to use to represent the trimmed elements.
#' @param compress Boolean. It `TRUE` instead of `c()` Use `seq()`, `rep()`, or atomic constructors `logical()`, `integer()`,
#'   `numeric()`, `complex()`, `raw()` when relevant to simplify the output.
#' @param unicode_representation By default "ascii", which means only ASCII characters
#'   (code point < 128) will be used to construct a string. This makes sure that
#'   homoglyphs (different spaces and other identically displayed unicode characters)
#'   are printed differently, and avoid possible unfortunate copy and paste
#'   auto conversion issues. "latin" is more lax and uses all latin characters
#'   (code point < 256). "character" shows all characters, but not emojis. Finally
#'   "unicode" displays all characters and emojis, which is what `dput()` does.
#' @param escape Whether to escape double quotes and backslashes. If `FALSE` we use
#'   single quotes to suround strings containing double quotes, and raw strings
#'   for strings that contain backslashes and/or a combination of single and
#'   double quotes. Depending on `unicode_representation` `escape = FALSE` cannot be applied
#'   on all strings.
#'
#' @details
#'
#' If `trim` is provided, depending on `fill` we will present trimmed elements as followed:
#' * `"default"` : Use default atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", character(2))`.
#' * `"rlang"` : Use rlang atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", rlang::new_character(2))`,
#'   these `rlang` constructors create vectors of `NAs`, so it's different from the default option.
#' * `"+"`: Use unary `+`, so for instance `c("a", "b", "c")` might become `c("a", +2)`.
#' * `"..."`: Use `...`, so for instance `c("a", "b", "c")` might become `c("a", ...)`
#' * `"none"`: Don't represent trimmed elements.
#'
#' Depending on the case some or all of the choices above might generate code that
#' cannot be executed. The 2 former options above are the most likely to suceed
#' and produce an output of the same type and dimensions recursively. This would
#' at least be the case for data frame.
#'
#' @return An object of class <constructive_options/constructive_options_atomic>
#' @export
#' @examples
#' construct(iris, opts_atomic(trim = 2), check = FALSE) # fill = "default"
#' construct(iris, opts_atomic(trim = 2, fill = "rlang"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "+"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "..."), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#' x <- c("a a", "a\U000000A0a", "a\U00002002a", "\U430 \U430")
#' construct(x, opts_atomic(unicode_representation = "unicode"))
#' construct(x, opts_atomic(unicode_representation = "character"))
#' construct(x, opts_atomic(unicode_representation = "latin"))
#' construct(x, opts_atomic(unicode_representation = "ascii"))
opts_atomic <- function(
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE
) {
  .cstr_combine_errors(
    ellipsis::check_dots_empty(),
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill),
    abort_not_boolean(compress),
    unicode_representation <- rlang::arg_match(unicode_representation)
  )
  .cstr_options("atomic", trim = trim, fill = fill, compress = compress, unicode_representation = unicode_representation, escape = escape)
}

#' @export
.cstr_construct.atomic <- function(x, ...) {
  code <- construct_atomic(x, ...)
  .cstr_repair_attributes(x, code, ...)
}

construct_atomic <- function(x, ..., one_liner = FALSE) {
  opts <- .cstr_fetch_opts("atomic", ...)
  trim <- opts$trim
  fill <- opts$fill

  nms <- names(x)
  attributes(x) <- NULL
  # if all names are "" we let `repair_attributes_impl()` deal with it
  names(x) <- if (!anyNA(nms) && !all(nms == "")) nms

  code <- if (opts$compress && is.null(names(x))) simplify_atomic(x, ..., one_liner = one_liner)
  if (!is.null(code)) return(code)

  l <- length(x)
  if (!is.null(trim) && trim < l) {
    opts$trim <- NULL
    code <- construct_atomic(x[seq_len(trim)], opts, ..., one_liner = one_liner)
    if (fill == "none" || trim == 0) return(code)
    if (trim == 1) code <- sprintf("c(%s)", code)
    replacement <- switch(
      fill,
      default = sprintf("%s(%s)", mode(x), l - trim),
      rlang = sprintf("rlang::new_%s(%s)", typeof(x), l - trim),
      "+" = paste0("+", l - trim),
      "..." = "..."
    )
    replacement <- sprintf(", %s)", replacement)
    code[[length(code)]] <- sub(")$", replacement, code[[length(code)]])
    return(code)
  }

  if (is.character(x)) {
    return(construct_chr(x, opts$unicode_representation, opts$escape, one_liner = one_liner, ...))
  }

  if (!is.double(x)) {
    code <- deparse(x)
    if (one_liner) code <- paste(code, collapse = " ")
    return(code)
  }

  # numeric

  # doubles need special treatment because deparse doesn't produce faithful code
  if (l == 0) return("numeric(0)")
  # unnamed scalars don't need c()
  if (l == 1 && is.null(names(x))) return(format_flex(x, all_na = TRUE))

  args <- vapply(x, format_flex, character(1), all_na = all(is.na(x)))
  code <- .cstr_apply(args, "c", ..., new_line = FALSE, recurse = FALSE)
  if (one_liner) code <- paste(code, collapse = " ")
  code
}

simplify_atomic <- function(x, ...) {
  l <- length(x)
  if (l) {
    rle_ <- rle2(x)
    # default vectors ----------------------------------------------------------
    if (l > 2) {
      if (is.logical(x) && isTRUE(all(!x))) return(sprintf("logical(%s)", l))
      if (is.integer(x) && isTRUE(all(x == 0L))) return(sprintf("integer(%s)", l))
      if (is.double(x) && isTRUE(all(x == 0L))) return(sprintf("numeric(%s)", l))
      if (is.complex(x) && isTRUE(all(x == 0i))) return(sprintf("complex(%s)", l))
      if (is.raw(x) && isTRUE(all(x == raw(1)))) return(sprintf("raw(%s)", l))
    }

    # rep ----------------------------------------------------------------------
    # each
    if (length(rle_[[2]]) > 1 && length(unique(rle_[[2]])) == 1 && length(rle_[[1]]) + 1 < length(x)) {
      return(.cstr_apply(list(rle_[[1]], each = rle_[[2]][[1]]), "rep", ...))
    }
    if (length(rle_[[1]]) * 2 < length(x)) {
      # this also supports rep(x, n) with scalar x and n, but not scalar n and vector x
      return(.cstr_apply(rle_, "rep", ...))
    }

    # scalar n and vector x
    for (d in divisors(l)) {
      if (identical(x, rep(.subset(x, 1:d), l / d))) return(.cstr_apply(list(.subset(x, 1:d), l / d), "rep", ...))
    }

    # seq ----------------------------------------------------------------------
    if (is.numeric(x) && l > 3 && !anyNA(x)) {
      d <- diff(x)
      if (length(unique(d)) == 1) {
        if (is.integer(x) && abs(d[[1]]) == 1) return(sprintf("%s:%s", x[[1]], x[[l]]))
        return(.cstr_apply(list(x[[1]], x[[l]], by = d[[1]]), "seq", ...))
      }
    }
  }
  NULL
}

# divisors except self and 1
divisors <- function(x) {
  y <- setdiff(seq_len(x / 2), 1)
  y[x %% y == 0]
}

# A rle without checks that treats NAs like a regular values and return an unnamed list
# with value first
rle2 <- function (x) {
  n <- length(x)
  t <- x[-1L]
  h <- x[-n]
  y <- t != h
  y <- ifelse(is.na(y), !(is.na(t) & is.na(h)), y)
  i <- c(which(y), n)
  list(x[i], diff(c(0L, i)))
}


# Special treatmnent of doubles is necessary because `dput()`, used for default
# the method, sometimes cuts values too short,
# however this gives ugly values in the general case
# in the 2 following cases we want the shortest "equal" output
# format(5.1, digits = 22) # "5.099999999999999644729"
# format(1e24, digits = 22) # 999999999999999983222784
# => so we use `digits = 16` for the default since it seems to simplify those values,
#    and we fall back on `digits = 22` for other cases
# if it still doesn't fit it we use `sprintf("%a", x)` which provides a representation
# that is not very readable but always works


format_flex <- function(x, all_na) {
  formatted <- format(x, digits = 15)
  if (formatted == "NA") {
    if (all_na) return("NA_real_") else return("NA")
  }
  if (formatted == "NaN") {
    return("NaN")
  }
  if (as.numeric(formatted) == x) return(formatted)
  formatted <- format(x, digits = 22)
  if (as.numeric(formatted) == x) return(formatted)
  # remove from coverage since system dependent
  sprintf("%a", x) # nocov
}

construct_chr <- function(x, unicode_representation, escape, one_liner, ...) {
  if (!length(x)) return("character(0)")
  strings0 <- strings <- sapply(x, deparse, USE.NAMES = FALSE)
  strings <- sapply(strings0, format_unicode, unicode_representation, USE.NAMES = FALSE)
  if (!escape) strings <- unescape_relevant_strings(strings, strings0)
  if (length(strings) == 1) return(strings)
  nas <- strings == "NA_character_"
  if (any(nas) && !all(nas)) strings[nas] <- "NA"
  .cstr_apply(strings, "c", one_liner = one_liner, ..., recurse = FALSE)
}


format_unicode <- function(x, type = c("ascii", "latin", "character", "unicode")) {
  type <- match.arg(type)
  if (type == "unicode") return(x)
  int <- utf8ToInt(x)
  limit <- switch(
    type,
    ascii = 128,
    latin = 256,
    character = 0x1F000
  )
  special_chars <- int[int >= limit]
  for (chr in special_chars) {
    x <- gsub(intToUtf8(chr), sprintf("\\U{%X}", chr), x, fixed = TRUE)
  }
  x
}

unescape_relevant_strings <- function(strings, strings0) {
  # https://stackoverflow.com/questions/42598040/
  odd_consecutive_backslashes_pattern <-
    r"[(?<!\\)\\(?:\\{2})*(?!\\)]"
  has_odd_consecutive_backlashes <-
    grepl(odd_consecutive_backslashes_pattern, strings, perl = TRUE)

  # escape if it wasn't altered by previous and if deparsed code doesn't contain single backslashes
  strings <- ifelse(
    strings == strings0 & !has_odd_consecutive_backlashes,
    unescape_strings(strings),
    strings)
}

unescape_strings <- function(x) {
  single_q <- grepl("'", x, fixed = TRUE)
  double_q <- grepl('\\"', x, fixed = TRUE)
  backslash <- grepl("\\\\", x, fixed = TRUE)

  as_raw_string <- function(x) {
    # remove external dbquotes
    x <- gsub("^.", "", x)
    x <- gsub(".$", "", x)
    # unescape double quotes\
    x <- gsub("\\\"", "\"", x, fixed = TRUE)
    # unescape backslashes
    x <- gsub("\\\\", "\\", x, fixed = TRUE)
    # build raw string
    x <- sprintf('r"[%s]"', x)
    x
  }

  ifelse(
    backslash | (single_q & double_q),
    as_raw_string(x),
    x
  )
}
