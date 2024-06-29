#' @export
#' @rdname other-opts
opts_expression <- function(
    constructor = c("default"),
    ...) {
  .cstr_options(
    "expression",
    constructor = constructor,
    ...
  )
}

#' @export
#' @method .cstr_construct expression
.cstr_construct.expression <- function(x, ...) {
  opts <- list(...)$opts$expression %||% opts_expression()
  if (is_corrupted_expression(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.expression", structure(NA, class = opts$constructor))
}

is_corrupted_expression <- function(x) {
  typeof(x) != "expression"
}

#' @export
#' @method .cstr_construct.expression default
.cstr_construct.expression.default <- function(x, ...) {
  x_bkp <- x
  # return length 0 object early
  if (!length(x)) return("expression()")

  # non standard names
  nms <- names(x)
  repair_names <- names_need_repair(nms, c_names = FALSE)
  if (repair_names) names(x) <- NULL

  # items with attributes need to be repaired,
  with_attr <-
    which(sapply(x, function(x) is.expression(x) || !is.null(attributes(x))))
  if (length(with_attr)) {
    x[with_attr] <- replicate(length(with_attr), NULL)
  }

  # construct items, without quote
  code <- lapply(x, deparse_call0, ...)

  # wrap with expression()
  code <- .cstr_apply(code, "expression", ..., recurse = FALSE)

  # repair attributes of items
  for (ind in with_attr) {
    bracket_call <- .cstr_apply(list(ind, value = x_bkp[[ind]]), "(`[[<-`)")
    code <- .cstr_pipe(code, bracket_call, ...)
  }

  .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
}
