construct_double <- function(x, ...) {
  # numeric

  # doubles need special treatment because deparse doesn't produce faithful code
  # unnamed scalars don't need c()
  if (length(x) == 1 && is.null(names(x))) return(format_flex(x, all_na = TRUE))

  args <- vapply(x, format_flex, character(1), all_na = all(is.na(x)))
  code <- .cstr_apply(
    args,
    "c",
    ...,
    recurse = FALSE
  )
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  code
}

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
