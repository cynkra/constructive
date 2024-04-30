#' @export
#' @rdname other-opts
opts_person <- function(constructor = c("person", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    check_dots_empty()
  )
  .cstr_options("person", constructor = constructor)
}

#' @export
.cstr_construct.person <- function(x, ...) {
  opts <- .cstr_fetch_opts("person", ...)
  if (is_corrupted_person(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$person[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_person <- function(x) {
  x <- unclass(x)
  !(
    is.list(x) &&
      length(x) == 1 &&
      is.list(x[[1]]) &&
      identical(names(x[[1]]), c("given", "family", "role", "email", "comment"))
  )
}

#' @export
constructors$person$person <- function(x, ...) {
  args <- unclass(unclass(x)[[1]])
  args <- Filter(Negate(is.null), args)
  code <- .cstr_apply(args, "person", ...)
  repair_attributes_person(x, code, ...)
}

repair_attributes_person <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "person",
    ...
  )
}
