all_opts_funs <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  all_opts_funs_chr <- ls(ns, all.names = TRUE, pattern = "^opts_")
  all_opts_funs <<- mget(all_opts_funs_chr, ns)
}
