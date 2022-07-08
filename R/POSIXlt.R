#' @export
construct_idiomatic.POSIXlt <- function(x, ...) {
  gmtoff <- .subset2(x, "gmtoff")
  from_posixct <- !is.null(gmtoff) && !all(is.na(gmtoff))
  if (from_posixct) {
    code_posixct <- construct_raw(as.POSIXct(x), ...)
    code <- wrap(code_posixct, "as.POSIXlt", new_line = FALSE)
    return(code)
  }

  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (!is.null(tzone) && length(tzone) == 1) {
    args <- c(args, list(tz = tzone))
  }
  construct_apply(args, "as.POSIXlt", new_line = TRUE)
}

#' @export
repair_attributes.POSIXlt <- function(x, code, pipe ="base", ...) {
  code <- repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = c("POSIXlt", "POSIXt"),
    #ignore = if (length(attr(x, "tzone")) > 1) "names" else c("names", "tzone"),
    ignore =  "tzone",
    remove = NULL,
    ...
  )
  code
}
