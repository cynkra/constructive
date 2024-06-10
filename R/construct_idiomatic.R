#' @export
.cstr_construct.default <- function(x, ..., one_liner = FALSE) {
  if (is.environment(x)) return(.cstr_construct.environment(x, ..., one_liner = one_liner))
  if (is.list(x))  return(.cstr_construct.list(x, ..., one_liner = one_liner))
  if (is.function(x))  return(.cstr_construct.function(x, ..., one_liner = one_liner))
  if (is.language(x) && !is.expression(x))  return(.cstr_construct.language(x, ..., one_liner = one_liner))
  if (typeof(x) == "...")  return(.cstr_construct.dots(x, ..., one_liner = one_liner))
  # for some reason the S3 method is not always caught the first time
  if (typeof(x) == "externalptr")  return(.cstr_construct.externalptr(x, ..., one_liner = one_liner))
  if (typeof(x) == "S4")  return(.cstr_construct.S4(x, ..., one_liner = one_liner))
  if (typeof(x) == "character")  return(.cstr_construct.character(x, ..., one_liner = one_liner))
  if (typeof(x) == "integer")  return(.cstr_construct.integer(x, ..., one_liner = one_liner))
  if (typeof(x) == "double")  return(.cstr_construct.double(x, ..., one_liner = one_liner))
  if (typeof(x) == "complex")  return(.cstr_construct.complex(x, ..., one_liner = one_liner))
  if (typeof(x) == "logical")  return(.cstr_construct.logical(x, ..., one_liner = one_liner))
  if (typeof(x) == "raw")  return(.cstr_construct.raw(x, ..., one_liner = one_liner))
  if (typeof(x) == "NULL")  return(.cstr_construct.NULL(x, ..., one_liner = one_liner))
  .cstr_construct.atomic(x, ..., one_liner = one_liner)
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
#' @param ... options passed recursively to the further methods
#' @param trailing_comma leave a trailing comma after the last argument if
#'   the code is multiline, some constructors allow it (e.g. `tibble::tibble()`) and it makes for nicer
#'   diffs in version control.
#' @param recurse Whether to recursively generate the code to construct `args`. If `FALSE` arguments
#' are expected to contain code.
#' @param implicit_names When data is provided, compress calls of the form `f(a = a)` to `f(a)`
#' @param new_line passed to wrap to remove add a line after "fun(" and before ")", forced to
#'   `FALSE` if `one_liner` is `TRUE`
#' @param one_liner Whether to return a one line call.
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
