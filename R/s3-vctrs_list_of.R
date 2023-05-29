constructors$vctrs_list_of <- new.env()

#' Constructive options for class 'data.table'
#'
#' These options will be used on objects of class 'data.table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"list_of"` (default): Wrap the column definitions in a `list_of()` call.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.table>
#' @export
opts_vctrs_list_of <- function(constructor = c("list_of", "list"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "vctrs_list_of"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("vctrs_list_of", constructor = constructor)
}

#' @export
.cstr_construct.vctrs_list_of <- function(x, ...) {
  opts <- .cstr_fetch_opts("vctrs_list_of", ...)
  if (is_corrupted_vctrs_list_of(x) || opts$constructor == "next") return(NextMethod())
  constructors <- constructors$vctrs_list_of[[opts$constructor]]
  constructors(x, ...)
}

is_corrupted_vctrs_list_of <- function(x) {
  # TODO
  FALSE
}

constructors$vctrs_list_of$list_of <- function(x, ...) {
  code <- .cstr_apply(
    args = c(as.list(x), list(.ptype = attr(x, "ptype"))),
    fun = "vctrs::list_of",
    ...
  )
  repair_attributes_vctrs_list_of(x, code, ...)
}

constructors$vctrs_list_of$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_vctrs_list_of <- function(x, code, ..., pipe = "base") {
  opts <- .cstr_fetch_opts("vctrs_list_of", ...)
  if (opts$constructor == "list") {
    return(repair_attributes_list(x, code, ..., pipe = pipe))
  }
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = "ptype",
    idiomatic_class = c("vctrs_list_of", "vctrs_vctr", "list")
  )
}
