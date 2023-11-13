opts_funs <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  opts_funs_chr <- ls(ns, all.names = TRUE, pattern = "^opts_")
  opts_funs <<- mget(opts_funs_chr, ns)
}
