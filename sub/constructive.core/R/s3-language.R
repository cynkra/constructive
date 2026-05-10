#' Constructive options for type 'language'
#'
#' These options will be used on objects of type 'language'. By default this
#' function is useless as nothing can be set, this is provided in case users want
#' to extend the method with other constructors.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` : We use constructive's deparsing algorithm on attributeless calls,
#'   and use `as.call()` on other language elements when attributes need to be constructed.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_language>
#' @export
opts_language  <- function(constructor = c("default"), ...) {
  .cstr_options("language", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct language
.cstr_construct.language <- function(x, ...) {
  opts <- list(...)$opts$language %||% opts_language()
  if (is_corrupted_language(x)) return(NextMethod())
  UseMethod(".cstr_construct.language", structure(NA, class = opts$constructor))
}

is_corrupted_language <- function(x) {
  !typeof(x) %in% c("language", "symbol", "expression")
}

#' @export
#' @method .cstr_construct.language default
.cstr_construct.language.default <- function(x, ...) {
  if (identical(x, quote(expr=))) return("quote(expr = )")
  x_stripped <- x
  attributes(x_stripped) <- NULL

  if (is_expression2(x_stripped)) {
    code <- deparse_call0(x_stripped, ...)
    code <- .cstr_wrap(code, "quote", new_line = FALSE)
  } else {
    list_call <- .cstr_apply(as.list(x_stripped), "list", ...)
    code <- .cstr_wrap(list_call, "as.call", new_line = FALSE)
  }
  repair_attributes_language(x, code, ...)
}

repair_attributes_language <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("srcref", "srcfile", "wholeSrcref")
  )
}

# adapted from rlang::is_syntactic_literal
is_syntactic_literal2 <- function(x) {
  if (!is.null(attributes(x))) return(FALSE)
  switch(
    typeof(x),
    "NULL" = TRUE,
    integer = ,
    # handles, 0, positive and NA
    double = length(x) == 1 && !isTRUE(sign(x) == -1),
    logical = ,
    character = length(x) == 1,
    complex = length(x) == 1 && isTRUE(Re(x) == 0) && !isTRUE(Im(x) < 0),
    FALSE
  )
}

is_expression2 <- function(x) {
  non_srcref_attr_nms <-  setdiff(
    names(attributes(x)),
    c("srcref", "srcfile", "wholeSrcref")
  )
  if (length(non_srcref_attr_nms)) return(FALSE)
  if (is_syntactic_literal2(x) || rlang::is_symbol(x)) return(TRUE)
  if (!rlang::is_call(x)) return(FALSE)
  # if the caller itself is empty the call can't be syntactic
  if (identical(x[[1]], quote(expr=))) return(FALSE)
  if (is_regular_function_definition(x)) return(TRUE)
  if (!is_regular_bracket_call(x)) {
    # if the only arg is missing then we can't use lisp notation to represent
    # missing args.
    if (length(x) == 2 && identical(x[[2]], quote(expr=))) return(FALSE)
  }
  if (is_regular_function_definition(x[[1]])) return(FALSE)
  all(vapply(x, is_expression2, logical(1)))
}

is_regular_function_definition <- function(x) {
  is.call(x) &&
  identical(x[[1]], as.symbol("function")) &&
    length(x) %in% c(3,4) &&
    (
      is.null(x[[2]]) ||
        (is.pairlist(x[[2]]) && all(vapply(x[[2]], is_expression2, logical(1))))
    ) &&
    is_expression2(x[[3]])
}
