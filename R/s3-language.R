# FIXME: we should use the names used by .class2 and have different constructors for language, symbol (could be quote, as.name, as.symbol, and expression (not yet supported it seems)
constructors$language <- new.env()

#' Constructive options for type 'language'
#'
#' These options will be used on objects of type 'language'. By default this
#' function is useless as nothing can be set, this is provided in case users want
#' to extend the method with other constructors.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"default"` : We use constructive's deparsing algorithm on attributeless calls,
#'   and use `as.call()` on other language elements when attributes need to be constructed.
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_language  <- function(constructor = c("default"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "language"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("language", constructor = constructor)
}

#' @export
.cstr_construct.language <- function(x, ...) {
  opts <- .cstr_fetch_opts("language", ...)
  if (is_corrupted_language(x)) return(NextMethod())
  constructors$language[[opts$constructor]](x, ...)
}

is_corrupted_language <- function(x) {
  ! typeof(x) %in% c("language", "symbol", "expression")
}

constructors$language$default <- function(x, ..., one_liner = FALSE) {
  if (identical(x, quote(expr=))) return("quote(expr=)")
  x_stripped <- x
  attributes(x_stripped) <- NULL

  if (is_expression2(x_stripped)) {
    code <- deparse_call(x_stripped, one_liner = one_liner, style = FALSE)
    code <- .cstr_wrap(code, "quote", new_line = FALSE)
  } else {
    list_call <- .cstr_apply(as.list(x_stripped), "list", ...)
    code <- .cstr_wrap(list_call, "as.call", new_line = FALSE)
  }
  repair_attributes_language(x, code, ...)
}

repair_attributes_language <- function(x, code, ..., pipe = "base") {
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = c("srcref", "srcfile", "wholeSrcref")
  )
}

is_expression2 <- function(x) {
  non_srcref_attr_nms <-  setdiff(
    names(attributes(x)),
    c("srcref", "srcfile", "wholeSrcref")
  )
  if (length(non_srcref_attr_nms)) return(FALSE)
  if (rlang::is_syntactic_literal(x) || rlang::is_symbol(x)) return(TRUE)
  if (!rlang::is_call(x)) return(FALSE)
  if (is_regular_function_definition(x)) return(TRUE)
  all(vapply(x, is_expression2, logical(1)))
}

is_regular_function_definition <- function(x) {
  identical(x[[1]], as.symbol("function")) &&
    length(x) == 4 &&
    (is.null(x[[2]]) || is.pairlist(x[[2]])) &&
    is_expression2(x[[3]])
}
