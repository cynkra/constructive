#' @export
#' @rdname other-opts
opts_person <- function(constructor = c("person", "next"), ...) {
  .cstr_options("person", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct person
.cstr_construct.person <- function(x, ...) {
  opts <- list(...)$opts$person %||% opts_person()
  if (is_corrupted_person(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.person", structure(NA, class = opts$constructor))
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
#' @method .cstr_construct.person person
.cstr_construct.person.person <- function(x, ...) {
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
