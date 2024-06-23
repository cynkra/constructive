
format_rep <- function(x, ..., double = FALSE) {
  rle_x <- rle2(x, double = double)
  values <- rle_x[[1]]
  lengths <- rle_x[[2]]
  l <- length(x)

  # use rep(x, each=)
  rep_each_is_applicable <-
    # value is not unique
    length(lengths) != 1 &&
    # values are repeated the same amount
    length(unique(lengths)) == 1 &&
    # length is at least 2 more than unique values
    # FIXME: not clear if this is necessary
    length(values) + 1 < length(x)
  if (rep_each_is_applicable) {
    code <- .cstr_apply(list(values, each = lengths[[1]]), "rep", ...)
    return(code)
  }

  # use rep(x, times=) with `times` and `x` of same length
  rep_times_reduces_verbosity <- length(values) * 2 < l
  if (rep_times_reduces_verbosity) {
    code <- .cstr_apply(list(values, lengths), "rep", ...)
    return(code)
  }

  # use rep(x, times =) with scalar `times`
  # we test all subdivisions
  for (d in divisors(l)) {
    sequence <- x[1:d]
    times <- l / d
    the_repeated_sequence_matches <- identical(x, rep(sequence, times))
    if (the_repeated_sequence_matches) {
      code <- .cstr_apply(list(sequence, times), "rep", ...)
      return(code)
    }
  }

}

format_seq <- function(x, ...) {
  # for diff()
  attributes(x) <- NULL
  l <- length(x)
  # seq ----------------------------------------------------------------------
  if (is.integer(x) && l >= 2 && !anyNA(x)) {
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

trim_atomic <- function(x, trim, fill, ...) {
  l <- length(x)
  if (trim >= l) return(NULL)
  if (trim == 0) return(.cstr_construct(x[0], ...))
  x_short <- x[seq_len(trim)]
  strings <- vapply(
    x_short,
    function(x, ...) .cstr_construct(x, ...),
    character(1),
    ...
  )
  nms <- names(x_short)
  if (fill == "none" && trim == 1 && is.null(nms)) return(strings)
  names(strings) <- names(x_short)
  replacement <- switch(
    fill,
    none = NULL,
    default =  sprintf(
      "%s(%s)",
      if (is.double(x)) "numeric" else typeof(x),
      l - trim
    ),
    rlang = sprintf("rlang::new_%s(%s)", typeof(x), l - trim),
    "+" = paste0("+", l - trim),
    "..." = "..."
  )
  code <- .cstr_apply(c(strings, replacement), "c", ..., recurse = FALSE)
  return(code)
}
