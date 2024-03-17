all_opts_funs <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  all_opts_funs_chr <- ls(ns, all.names = TRUE, pattern = "^opts_")
  all_opts_funs <<- mget(all_opts_funs_chr, ns)
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_enumerateOptFunctions")
  s3_register("roxygen2::roxy_tag_rd", "roxy_tag_enumerateOptFunctions")
}
