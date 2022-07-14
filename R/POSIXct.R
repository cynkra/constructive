#' @export
construct_idiomatic.POSIXct <- function(x, ...) {
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
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = c("POSIXct", "POSIXt"),
    ignore = "tzone",
    remove = if (is.null(attr(x, "tzone"))) "tzone" else NULL,
    ...
  )
}
