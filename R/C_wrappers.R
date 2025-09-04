#' @useDynLib constructive
NULL

#' Build a pointer from a memory address
#'
#' Base R doesn't provide utilities to build or manipulate external pointers
#' (objects of type "externalptr"), so we provide our own.
#' Objects defined with `.xptr()` are not stable across sessions,
#'
#' @param address Memory address
#' @return The external pointer (type "externalptr") that the memory address points to.
#' @export
.xptr <- function(address) {
  if (!missing(address) && is.character(address) && length(address) == 1L) {
    ptr <- globals[["external_pointers"]][[address]]
    if (!is.null(ptr)) return(ptr)
  }
  abort(paste0("No external pointer registered for key ", address))
}

external_pointer_address <- function(s) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return("0x123456789")
  .Call("external_pointer_address", PACKAGE = "constructive", s)
}

env_impl <- function(address) {
  .Call("objectFromAddress", PACKAGE = "constructive", address)
}

promise_env <- function(name, env = parent.frame()) {
  enquo_call <- substitute(rlang::enquo(X), list(X = rlang::sym(name)))
  quo <- eval(enquo_call, env)
  rlang::quo_get_env(quo)
}
