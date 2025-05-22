#' @export
.cstr_construct.default <- function(x, ...) {
  if (is.matrix(x)) return(.cstr_construct.matrix(x, ...))
  if (is.array(x)) return(.cstr_construct.array(x, ...))
  switch(
    typeof(x),
    environment = .cstr_construct.environment(x, ...),
    list = .cstr_construct.list(x, ...),
    special = ,
    builtin = ,
    closure = .cstr_construct.function(x, ...),
    symbol = ,
    language = .cstr_construct.language(x, ...),
    `...` = .cstr_construct.dots(x, ...),
    externalptr = .cstr_construct.externalptr(x, ...),
    S4 = .cstr_construct.S4(x, ...),
    object = .cstr_construct.object(x, ...),
    character = .cstr_construct.character(x, ...),
    integer = .cstr_construct.integer(x, ...),
    double = .cstr_construct.double(x, ...),
    complex = .cstr_construct.complex(x, ...),
    logical = .cstr_construct.logical(x, ...),
    raw = .cstr_construct.raw(x, ...),
    `NULL` = .cstr_construct.NULL(x, ...)
  )
}

#' .cstr_apply
#'
#' Exported for custom constructor design. If `recurse` is `TRUE` (default), we
#' recurse to construct `args` and insert their construction code in a `fun(...)` call returned
#' as a character vector. If `args` already contains code rather than object to
#' construct one should set `recurse` to `FALSE`.
#'
#' @param args A list of arguments to construct recursively, or code if `recurse = FALSE`.
#'   If elements are named, the arguments will be named in the generated code.
#' @param fun The function name to use to build code of the form "fun(...)"
#' @param ... Options passed recursively to the further methods
#' @param trailing_comma Boolean. Whether to leave a trailing comma after the last argument if
#'   the code is multiline, some constructors allow it (e.g. `tibble::tibble()`) and it makes for nicer
#'   diffs in version control.
#' @param recurse Boolean. Whether to recursively generate the code to construct `args`. If `FALSE` arguments
#' are expected to contain code.
#' @param implicit_names When data is provided, compress calls of the form `f(a = a)` to `f(a)`
#' @param new_line Boolean. Forwarded to `wrap()` to add a line between "fun(" and ")", forced to
#'   `FALSE` if `one_liner` is `TRUE`
#' @param one_liner Boolean. Whether to return a one line call.
#' @inheritParams construct
#'
#' @export
#' @return A character vector of code
#'
#' @examples
#' a <- 1
#' .cstr_apply(list(a=a), "foo")
#' .cstr_apply(list(a=a), "foo", data = list(a=1))
#' .cstr_apply(list(a=a), "foo", data = list(a=1), implicit_names = TRUE)
#' .cstr_apply(list(b=a), "foo", data = list(a=1), implicit_names = TRUE)
#' .cstr_apply(list(a="c(1,2)"), "foo")
#' .cstr_apply(list(a="c(1,2)"), "foo", recurse = FALSE)
.cstr_apply <- function(
    args,
    fun = "list",
    ...,
    trailing_comma = FALSE,
    recurse = TRUE,
    implicit_names = FALSE,
    new_line = TRUE,
    one_liner = FALSE,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE) {
  new_line <- new_line && !one_liner
  trailing_comma <- trailing_comma && !one_liner
  unicode_representation <- match.arg(unicode_representation)
  # so we make sure we use the right methods for length, [, [[
  # and lapply iterates properly at the low level
  args <- unclass(args)
  if (!length(args)) return(sprintf("%s()", fun))
  if (recurse) args <- lapply(
    args,
    # for some reason, using simply .cstr_construct on the next line doesn't
    # dispatch to the right method
    function(x, ...) .cstr_construct(x, ...),
    ...,
    one_liner = one_liner,
    unicode_representation = unicode_representation,
    escape = escape
  )
  args_chr <- Map(
    name_and_append_comma,
    unname(args),
    names2(args),
    MoreArgs = list(
      implicit_names = implicit_names,
      unicode_representation = unicode_representation,
      escape = escape
    )
  )
  args_chr <- unlist(args_chr)
  # if line is short enough stick all in one line
  # FIXME : chunk unnamed lists of single line items by lines of 80 chars ?
  nchrs <- nchar(args_chr)

  one_liner <- one_liner || (sum(nchrs) < 80 && all(endsWith(args_chr, ",")))
  if (one_liner) {
    args_chr <- paste(args_chr, collapse = " ")
    new_line <- FALSE
    trailing_comma <- FALSE
  } else if (all(rlang::names2(args) == "") && all(endsWith(args_chr, ","))) {
    lines <- character()
    while (length(args_chr)) {
      ind <- union(1, which(cumsum(nchar(args_chr) + 1) < 80))
      lines[[length(lines) + 1]] <- paste(args_chr[ind], collapse = " ")
      args_chr <- args_chr[-ind]
    }
    args_chr <- lines
  }
  if (!trailing_comma) {
    args_chr[[length(args_chr)]] <- sub(",$", "", args_chr[[length(args_chr)]])
  }

  .cstr_wrap(args_chr, fun, new_line)
}
