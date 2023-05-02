#' @useDynLib constructive
NULL

#' Build a pointer from a memory address
#'
#' Base R doesn't provide utilities to build or manipulate external pointers
#' (objects of type "externalptr"), so we provide our own. Be warned that
#' objects defined with `external_pointer()` are not stable across sessions,
#' however this is the best we can
#' @param address Memory address
#' @export
external_pointer <- function(address) {
  .Call("external_pointer", PACKAGE = "constructive", address)
}

external_pointer_address <- function(s) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return("0x000000000")
  .Call("external_pointer_address", PACKAGE = "constructive", s)
}

env_impl <- function(address) {
  .Call("objectFromAddress", PACKAGE = "constructive", address)
}
