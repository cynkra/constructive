construct_raw <- function(x, data = NULL, ...) {
  idiomatic_code <- data_match(x, data) %||% construct_idiomatic(x, data = data, ...)
  repaired_code <- repair_attributes(x, idiomatic_code, data = data, ...)
  repaired_code
}

# kept generic for flexibility
data_match <- function(x, data) {
  UseMethod("data_match")
}

#' @export
data_match.default <- function(x, data) {
  if (is.null(data)) return(NULL)
  # match doesn't look at attributes, which is a feature here
  m <- match(list(x), data)
  if (is.na(m)) return(NULL)
  names(data)[m]
}

construct_idiomatic <- function(x, ...) {
  UseMethod("construct_idiomatic")
}

#' @export
construct_idiomatic.default <- function(x, ...) {
  if (is.list(x))  return(construct_idiomatic.list(x, ...))
  if (rlang::is_formula(x))  return(construct_idiomatic.formula(x, code, pipe, ...))
  if (is.language(x))  return(construct_idiomatic.language(x, ...))
  attributes(x) <- NULL
  capture.output(dput(x))
}

repair_attributes <- function(x, code, pipe = "base", ...) {
  UseMethod("repair_attributes")
}

#' @export
repair_attributes.default <- function(x, code, pipe = "base", ...) {
  if (rlang::is_formula(x))  return(repair_attributes.formula(x, code, pipe, ...))
  if (is.language(x))  return(repair_attributes.language(x, code, pipe, ...))
  repair_attributes_impl(x, code, pipe, ...)
}

repair_attributes_impl <- function(x, code, pipe = "base", ignore = NULL, idiomatic_class = NULL, ...) {
  # fetch non idiomatic args and class
  attrs <- attributes(x)
  attrs$names <- NULL # names are already provided by construct_idiomatic
  attrs[ignore] <- NULL
  if (identical(attrs$class, idiomatic_class)) {
    attrs$class <- NULL
  } else if(is.null(attrs$class)) {
    # to be able to remove the idiomatic class explicitly, mainly (only ?) useful for classless formulas
    attrs["class"] <- list(NULL)
  }
  if (!length(attrs)) return(code)
  # append structure() code to repair object
  attrs_code <- construct_apply(attrs, fun = "structure", pipe = pipe, ...)
  pipe(code, attrs_code, pipe)
}

construct_apply <- function(args, fun = "list", keep_trailing_comma = FALSE, language = FALSE, implicit_names = FALSE, new_line = TRUE, ...) {
  if (!length(args)) return("list()")
  if (!language) args <- lapply(args, construct_raw, ...)
  args <- Map(name_and_append_comma, args, names2(args), implicit_names = implicit_names)
  args <- unlist(args)
  # if line is short enough stick all in one line
  # FIXME : chunk unnamed lists of single line items by lines of 80 chars ?
  if(sum(nchar(args)) < 80 && all(endsWith(args, ","))) {
    args <- paste(args, collapse = " ")
    new_line <- FALSE
    keep_trailing_comma <- FALSE
  }
  if (!keep_trailing_comma) {
    args[[length(args)]] <- sub(",$", "", args[[length(args)]])
  }

  wrap(args, fun, new_line)
}






