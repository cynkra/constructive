#' @export
construct_raw.default <- function(x, ..., one_liner = FALSE) {
  if (is.environment(x)) return(construct_raw.environment(x, ..., one_liner = one_liner))
  if (is.list(x))  return(construct_raw.list(x, ..., one_liner = one_liner))
  if (is.function(x))  return(construct_raw.function(x, ..., one_liner = one_liner))
  if (is.language(x) && !is.expression(x))  return(construct_raw.language(x, ..., one_liner = one_liner))
  if (typeof(x) == "...")  return(construct_raw.dots(x, ..., one_liner = one_liner))
  # for some reason the S3 method is not always caught the first time
  if (typeof(x) == "externalptr")  return(construct_raw.externalptr(x, ..., one_liner = one_liner))
  construct_raw.atomic(x, ..., one_liner = one_liner)
}

#' construct_apply
#'
#' @param args Arguments to construct recursively, or code if `language = TRUE`
#' @param fun The function name to use to build code of the form "fun(...)"
#' @param ... options passed recursively to the further methods
#' @param keep_trailing_comma leave a trailing comma after the last argument if
#'   the code is multiline, some constructors allow it (e.g. `tibble::tibble()`) and it makes for nicer
#'   diffs in version control.
#' @param language Whether to use the args as they are or to recurse, should be renamed to `recurse` (and negated)
#' @param implicit_names When data is provided, compress calls of the form `f(a = a)` to `f(a)`
#' @param new_line passed to wrap to remove add a line after "fun(" and before ")", forced to
#'   `FALSE` if `one_liner` is `TRUE`
#' @param one_liner Whether to return a one line call.
#'
#' @return A character vector of code
#'
#' @examples
#' construct_apply(list(a=a), "foo", data = list(a=1), template = NULL, implicit_names = TRUE)
.cstr_apply <- function(args, fun = "list", ..., keep_trailing_comma = FALSE, language = FALSE, implicit_names = FALSE, new_line = TRUE, one_liner = FALSE) {
  new_line <- new_line && !one_liner
  keep_trailing_comma <- keep_trailing_comma && !one_liner
  if (!length(args)) return(sprintf("%s()", fun))
  if (!language) args <- lapply(unclass(args), construct_raw, ..., one_liner = one_liner)
  args_chr <- Map(name_and_append_comma, args, names2(args), implicit_names = implicit_names)
  args_chr <- unlist(args_chr)
  # if line is short enough stick all in one line
  # FIXME : chunk unnamed lists of single line items by lines of 80 chars ?
  nchrs <- nchar(args_chr)

  one_liner <- one_liner || (sum(nchrs) < 80 && all(endsWith(args_chr, ",")))
  if (one_liner) {
    args_chr <- paste(args_chr, collapse = " ")
    new_line <- FALSE
    keep_trailing_comma <- FALSE
  } else if (all(rlang::names2(args) == "") && all(endsWith(args_chr, ","))) {
    lines <- character()
    while(length(args_chr)) {
      ind <- union(1, which(cumsum(nchar(args_chr) + 1) < 80))
      lines[[length(lines) + 1]] <- paste(args_chr[ind], collapse = " ")
      args_chr <- args_chr[-ind]
    }
    args_chr <- lines
  }
  if (!keep_trailing_comma) {
    args_chr[[length(args_chr)]] <- sub(",$", "", args_chr[[length(args_chr)]])
  }

  .cstr_wrap(args_chr, fun, new_line)
}
