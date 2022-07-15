#' @export
construct_idiomatic.POSIXct <- function(x, max_atomic = NULL, ...) {
  browser()
  if (length(x) == 0 || (!is.null(max_atomic) && max_atomic == 0)) {
    # should I arrive at this point with length(x) already 0?
    # I don't think so because here we have to retrieve other attributes,
    # it is a more complex case compared with numeric(0)

    # do something with x...
    x <- x[0]
  }

  tzone <- attr(x, "tzone")
  x_chr <- format(x)
  split_s <- as.numeric(x) %% 1
  dec_lgl <- split_s != 0
  x_chr[dec_lgl] <- paste0(x_chr[dec_lgl], sub("^0", "", format(split_s[dec_lgl], digits = 5)))
  args <- list(x_chr)
  if (!is.null(tzone) && tzone != "") {
    args <- c(args, list(tz = tzone))
  }
  construct_apply(args, "as.POSIXct", new_line = TRUE)
}

#' @export
repair_attributes.POSIXct <- function(x, code, pipe ="base", ...) {
  browser()
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = c("POSIXct", "POSIXt"),
    ignore = "tzone",
    remove = if (is.null(attr(x, "tzone"))) "tzone" else NULL,
    ...
  )
}
