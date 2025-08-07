#' @export
#' @rdname other-opts
opts_ggproto <- function(constructor = c("default", "ggproto", "next", "environment"), ...) {
  .cstr_options("ggproto", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct ggproto
.cstr_construct.ggproto <- function(x, ...) {
  opts <- list(...)$opts$ggproto %||% opts_ggproto()
  if (is_corrupted_ggproto(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggproto", structure(NA, class = opts$constructor))
}

is_corrupted_ggproto <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.ggproto environment
.cstr_construct.ggproto.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.ggproto default
.cstr_construct.ggproto.default <- function(x, ..., ggproto.ignore_draw_key = FALSE) {
  pkg_protos <- Filter(function(x) inherits(x, "ggproto"), as.list(asNamespace("ggplot2")))
  ind <- sapply(pkg_protos, identical, x)
  if (any(ind)) {
    code <- paste0("ggplot2::", names(pkg_protos[ind])[[1]])
    return(code)
  }
  .cstr_construct.ggproto.ggproto(x, ...)
}

#' @export
#' @method .cstr_construct.ggproto ggproto
.cstr_construct.ggproto.ggproto <- function(x, ..., ggproto.ignore_draw_key = FALSE) {
  args <- env2list(x)

  # FIXME: we could build ggproto objects using the super in theory but
  # in practice this doesn't quite work because of NSE, and ggproto doesn't handle !!.
  # We could use inject, but we should reserve !! for cases where it's simple and easy
  # otherwise we'll get issues with nested !!
  if (FALSE) { # if ("super" %in% names(args)) {
    super <- GeomSegment$super()
    super_args <- env2list(super)
    for (nm in names(args)) {
      if (identical(args[[nm]], super_args[[nm]])) {
        args[[nm]] <- NULL
      }
    }
    cl <- setdiff(class(x),  class(super))
    args <- c(list(cl), super, env2list(x))
  } else {
    cl <- setdiff(class(x), c("ggproto", "gg"))
    args <- c(list(cl), env2list(x))
  }

  code <- .cstr_apply(args, "ggplot2::ggproto", ...)
  code
}

repair_attributes_ggproto <- function(x, code, pipe = NULL, ...) {
  code
}
