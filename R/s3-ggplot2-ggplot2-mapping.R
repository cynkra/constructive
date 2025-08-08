# This reproduces the aes() call, note that due to NSE we cannot guarantee that
# the variables will be found in caller env, and it would be costly and unsafe to
# eval the expressions or components

#' @export
#' @rdname other-opts
opts_ggplot2_mapping <- function(constructor = c("aes", "next", "list"), ...) {
  .cstr_options("uneval", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggplot2::mapping
`.cstr_construct.ggplot2::mapping` <- function(x, ...) {
  opts <- list(...)$opts$uneval %||% opts_ggplot2_mapping()
  if (`is_corrupted_ggplot2::mapping`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::mapping", structure(NA, class = opts$constructor))
}

#' @export
#' @method `.cstr_construct.ggplot2::mapping` list
`.cstr_construct.ggplot2::mapping.list` <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method `.cstr_construct.ggplot2::mapping` aes
`.cstr_construct.ggplot2::mapping.aes` <- function(x, ...) {
  if (!length(x)) {
    return(
      `repair_attributes_ggplot2::mapping`(x, "ggplot2::aes()", ...)
    )
  }
  env <- list(...)$env
  args <- lapply(
    x,
    function(x) {
      if (!inherits(x, "quosure")) return(.cstr_construct(x, ...))
      if (identical(environment(x), env)) return(rlang::expr_deparse(rlang::quo_squash(x)))
      code <- .cstr_construct(x, ...)
      code[[1]] <- paste0("!!", code[[1]])
      code
    }
  )

  nm1 <- names(args)[1]
  # omit `x` and `y` if provided in this order
  if (nm1 == "x") {
    names(args)[1] <- ""
    nm2 <- names(args)[2]
    if (!is.na(nm2) && nm2 == "y") {
      names(args)[2] <- ""
    }
  }
  code <- .cstr_apply(args, fun = "ggplot2::aes", recurse = FALSE)
  `repair_attributes_ggplot2::mapping`(x, code, ...)
}

`is_corrupted_ggplot2::mapping` <- function(x) {
  # TODO
  FALSE
}

`repair_attributes_ggplot2::mapping` <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe = pipe,
    idiomatic_class = c("ggplot2::mapping", "uneval", "gg", "S7_object"),
    ignore = "S7_class",
    ...
  )
}
