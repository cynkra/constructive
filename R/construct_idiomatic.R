construct_idiomatic <- function(x, ...) {
  UseMethod("construct_idiomatic")
}

# the default case handles all atomic modes through dput except for numeric
# ("logical", "integer", "complex", "character" and "raw")
#' @export
construct_idiomatic.default <- function(x, ..., one_liner = FALSE) {
  if (is.environment(x)) return(construct_idiomatic.environment(x, ..., one_liner = one_liner))
  if (is.list(x))  return(construct_idiomatic.list(x, ..., one_liner = one_liner))
  if (is.function(x))  return(construct_idiomatic.function(x, ..., one_liner = one_liner))
  if (rlang::is_formula(x))  return(construct_idiomatic.formula(x, ..., one_liner = one_liner))
  if (is.language(x) && !is.expression(x))  return(construct_idiomatic.language(x, ..., one_liner = one_liner))
  if (typeof(x) == "...")  return(construct_idiomatic.dots(x, ..., one_liner = one_liner))
  # for some reason the S3 method is not always caught the first time
  if (typeof(x) == "externalptr")  return(construct_idiomatic.externalptr(x, ..., one_liner = one_liner))
  construct_idiomatic.atomic(x, ..., one_liner = one_liner)
}

construct_apply <- function(args, fun = "list", ..., keep_trailing_comma = FALSE, language = FALSE, implicit_names = FALSE, new_line = TRUE, one_liner = FALSE) {
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

  wrap(args_chr, fun, new_line)
}
