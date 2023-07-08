# This reproduces the aes() call, note that due to NSE we cannot guarantee that
# the variables will be found in caller env, and it would be costly and unsafe to
# eval the expressions or components

#' @export
.cstr_construct.uneval <- function(x, ...) {
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
  code <- .cstr_apply(args, fun = "ggplot2::aes", recurse = FALSE, new_line = FALSE)
  repair_attributes_uneval(x, code, ...)
}

repair_attributes_uneval <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "uneval",
    ...
  )
}
