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
#' @param unicode_representation,escape Deprecated, kept for compatibility with older versions.
#' Overrides the arguments of `construct()`
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
    escape = NULL
) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill),
    abort_not_boolean(compress),
    unicode_representation <- rlang::arg_match(unicode_representation)
  )
  .cstr_options("atomic", ..., trim = trim, fill = fill, compress = compress, unicode_representation = unicode_representation, escape = escape)
}

#' @export
.cstr_construct.atomic <- function(x, ...) {
  code <- construct_atomic(x, ...)
  .cstr_repair_attributes(x, code, ...)
}

construct_atomic <- function(x, ..., unicode_representation = c("ascii", "latin", "character", "unicode"), escape = FALSE) {
  if(is.null(x)) return("NULL")
  nms <- names(x)
  attributes(x) <- NULL
  l <- length(x)
  if (l == 0) return(deparse(x))

  # We might deprecate this feature, `unicode_representation` and `escape` were
  # provided through opts_atomic() but are now provided at the top level so they
  # apply on names too. The behavior set by opts_atomic() now overrided the
  # top level behavior for compatibility
  opts <- list(...)$opts$atomic %||% opts_atomic()
  unicode_representation <- if (opts[["unicode_representation"]] == "default") {
    match.arg(unicode_representation)
  } else {
    opts[["unicode_representation"]]
  }
  escape <- opts[["escape"]] %||% escape

  trim <- opts[["trim"]]
  fill <- opts[["fill"]]


  # if all names are "" we let `repair_attributes_impl()` deal with it
  names(x) <- if (!anyNA(nms) && !all(nms == "")) nms

  code <- if (opts[["compress"]] && is.null(names(x))) simplify_atomic(x, opts = opts, ...)
  if (!is.null(code)) return(code)


  if (!is.null(trim) && trim < l) {
    opts$atomic$trim <- NULL
    code <- construct_atomic(x[seq_len(trim)], opts, ...)
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
    return(construct_chr(x, unicode_representation = unicode_representation, escape = escape, ...))
  }

  if (is.complex(x)) {
    return(construct_complex(x, unicode_representation = unicode_representation, escape = escape, ...))
  }

  if (!is.double(x)) {
    code <- sapply(x, deparse)
    if (!all(is.na(x))) {
      code[is.na(x)] <- "NA"
    }
    if (l == 1 && is.null(names(x))) return(code)
    code <- .cstr_apply(
      code,
      "c",
      ...,
      recurse = FALSE,
      unicode_representation = unicode_representation,
      escape = escape
    )
    if (list(...)$one_liner) code <- paste(code, collapse = " ")
    return(code)
  }

  # numeric

  # doubles need special treatment because deparse doesn't produce faithful code
  # unnamed scalars don't need c()
  if (l == 1 && is.null(names(x))) return(format_flex(x, all_na = TRUE))

  args <- vapply(x, format_flex, character(1), all_na = all(is.na(x)))
  code <- .cstr_apply(
    args,
    "c",
    ...,
    recurse = FALSE,
    unicode_representation = unicode_representation,
    escape = escape
  )
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
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
      if (is.complex(x) && isTRUE(all(x == 0i))) return(sprintf("complex(%s)", l))
      if (is.raw(x) && isTRUE(all(x == raw(1)))) return(sprintf("raw(%s)", l))
      if (is.double(x) && isTRUE(all(x == 0L))) {
        signs <- sign(1/x)
        if (all(signs == 1)) return(sprintf("numeric(%s)", l))
        if (all(signs == -1)) return(sprintf("-numeric(%s)", l))
      }
    }

    # don't compress if x contains both positive and negative zeroes
    zeros_ind <- which(x == 0)
    if (length(zeros_ind)) {
      contains_pos_and_neg_zeroes <- length(unique(1/x[zeros_ind])) != 1
      if (contains_pos_and_neg_zeroes) return(NULL)
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
    if (is.integer(x) && l >=2 && !anyNA(x)) {
      # diff returns NA when span of difference exceeds .Machine$integer.max
      d <- suppressWarnings(diff(x))
      if (!anyNA(d) && length(unique(d)) == 1) {
        if (abs(d[[1]]) == 1) return(sprintf("%s:%s", x[[1]], x[[l]]))
        if (l > 3) return(.cstr_apply(list(x[[1]], x[[l]], by = d[[1]]), "seq", ...))
        return(NULL)
      }
    }

    if (is.numeric(x) && l > 3 && !anyNA(x)) {
      # diff returns NA when span of difference exceeds .Machine$integer.max
      d <- suppressWarnings(diff(x))
      if (!anyNA(d) && length(unique(d)) == 1) {
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
  # negative zeroes
  if (identical(x, 0) && sign(1/x) == -1) return("-0")
  # negative NAs, commented for now as might be overkill, and inconsistent
  # if(is.na(x) && serialize(x, NULL)[[32]] == as.raw(0xff)) {
  #   if (is.nan(x)) return("-NaN")
  #   return("-NA_real_")
  # }
  formatted <- format(x, digits = 15)
  if (formatted == "NA") {
    if (all_na) return("NA_real_") else return("NA")
  }
  if (formatted == "NaN") {
    return("NaN")
  }
  if (as.numeric(formatted) == x) return(formatted)
  # FIXME: Increase digits only for those array elements that don't match
  for (digits in 16:22) {
    formatted <- format(x, digits = digits)
    if (as.numeric(formatted) == x) return(formatted)
  }
  # remove from coverage since system dependent
  # (similarly to .deparseOpts("hexNumeric"))
  sprintf("%a", x) # nocov
}

construct_complex <- function(x, ...) {
  re <- sapply(Re(x), construct_atomic, ...)
  im <- sapply(Im(x), construct_atomic, ...)
  code <- ifelse(
    is.na(x),
    "NA",
    ifelse (
      re == "0",
      paste0(im, "i"),
      ifelse(
        im == "0" & !all(im == "0"),
        re,
        paste0(re, "+", im, "i")
      )
    )
  )
  if (all(is.na(x) | im == "0")) {
    code[is.na(x)] <- "NA_complex_"
  }
  if (length(x) == 1 && is.null(names(x))) return(code)
  code <- .cstr_apply(
    code,
    "c",
    ...,
    recurse = FALSE
  )
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  code
}
