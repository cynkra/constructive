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
