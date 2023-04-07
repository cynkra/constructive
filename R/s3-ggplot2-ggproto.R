# ggproto() is probably to messy to be able to use it as a reverse constructor,
# so we just construct if we find the

#' @export
construct_idiomatic.ggproto <- function(x, ggproto.ignore_draw_key = FALSE, ..., data) {
  if (ggproto.ignore_draw_key) {
    x <- as.list(x)
    x$draw_key <- NULL
  }
  res <- find_in_package_protos(x, ggproto.ignore_draw_key)
  if(!is.null(res)) return(res) else construct_idiomatic.environment(x)
}

#' @export
repair_attributes.ggproto <- function(x, code, pipe = "base", ...) {
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
      if(identical(x, proto, ignore.environment = TRUE)) {
        if (pkg == "ggplot2") {
          # FIXME: not good enough GeomContourFixed -> countour_fixed ?
          return(sprintf('"%s"', sub( "^[^_]+_", "", snakeize(nm))))
        }
        return(paste0(pkg, ":::", nm))
      }
    }
  }
}
