

# redesign ---------------------------------------------------------------------

#' @export
construct_idiomatic.list <- function(x, ...) {
  construct_apply(x, ...)
}

#' @export
construct_idiomatic.vctrs_list_of <- function(x, ...) {
  construct_apply(
    args = c(as.list(x), list(.ptype= attr(x, "ptype"))),
    fun = "vctrs::list_of",
    ...
  )
}

#' @export
repair_attributes.vctrs_list_of <- function(x, code, pipe = "base") {
  repair_attributes_impl(
    x, code, pipe,
    ignore = "ptype",
    idiomatic_class = c("vctrs_list_of", "vctrs_vctr", "list")
  )
}
