# FIXME: would be nice to construct using the ggproto() function

constructors$ggproto <- new.env()

#' @export
opts_ggproto <- function(constructor = c("default", "next", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("ggproto", constructor = constructor)
}

#' @export
.cstr_construct.ggproto <- function(x, ...) {
  opts <- .cstr_fetch_opts("ggproto", ...)
  if (is_corrupted_ggproto(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$ggproto[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_ggproto <- function(x) {
  # TODO
  FALSE
}

#' @export
constructors$ggproto$environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
constructors$ggproto$default <- function(x, ..., ggproto.ignore_draw_key = FALSE) {
  if (ggproto.ignore_draw_key) {
    x <- as.list(x)
    x$draw_key <- NULL
  }
  code <- find_in_package_protos(x, ggproto.ignore_draw_key)
  if (is.null(code)) return(.cstr_construct.environment(x, ...))
  repair_attributes_ggproto(x, code, ...)
}

repair_attributes_ggproto <- function(x, code, pipe = NULL, ...) {
  code
}

find_in_package_protos <- function(x, ggproto.ignore_draw_key) {
  for (pkg in globals$ggpackages) {
    pkg_protos <- Filter(function(x) inherits(x, "ggproto"), as.list(asNamespace(pkg)))
    for (nm in names(pkg_protos)) {
      proto <- pkg_protos[[nm]]
      if (ggproto.ignore_draw_key) {
        proto <- as.list(proto)
        proto$draw_key <- NULL
      }
      if (identical(x, proto, ignore.environment = TRUE)) {
        if (pkg == "ggplot2") {
          # FIXME: not good enough GeomContourFixed -> countour_fixed ?
          return(sprintf('"%s"', sub("^[^_]+_", "", snakeize(nm))))
        }
        return(paste0(pkg, ":::", nm))
      }
    }
  }
}
