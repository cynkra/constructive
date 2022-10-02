# aes
#' @export
construct_idiomatic.uneval <- function(x, ...) {
  args <- lapply(x, function(x) rlang::expr_deparse(rlang::quo_squash(x)))
  nm1 <- names(args)[1]
  # omit `x` and `y` if provided in this order
  if (nm1 == "x") {
    names(args)[1] <- ""
    nm2 <- names(args)[2]
    if (!is.na(nm2) && nm2 == "y") {
      names(args)[2] <- ""
    }
  }
  construct_apply(args, fun = "ggplot2::aes", language = TRUE, new_line = FALSE)
}

#' @export
repair_attributes.uneval <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = c("uneval"),
    ...
  )
}
