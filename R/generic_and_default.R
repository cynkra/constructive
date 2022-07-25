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

# the default case handles all atomic modes through dput except for numeric
# ("logical", "integer", "complex", "character" and "raw")
#' @export
construct_idiomatic.default <- function(x, max_atomic = NULL, ...) {
  if (is.environment(x)) return(construct_idiomatic.environment(x, max_atomic = max_atomic, ...))
  if (is.list(x))  return(construct_idiomatic.list(x, max_atomic = max_atomic, ...))
  if (rlang::is_formula(x))  return(construct_idiomatic.formula(x, max_atomic = max_atomic, ...))
  if (is.language(x) && !is.expression(x))  return(construct_idiomatic.language(x, max_atomic = max_atomic, ...))
  attributes(x) <- NULL
  if (!is.null(max_atomic)) trim_atomic(x, max_atomic) else capture.output(dput(x))
}

trim_atomic <- function(x, max_atomic) {
  if (max_atomic == 0) x <- x[0]
  l <- length(x)
  if (l <= max_atomic) return(capture.output(dput(x)))
  code <- capture.output(dput(head(x, max_atomic)))
  code[[length(code)]] <- sub(")$", ", ...)", code[[length(code)]])
  code
}

repair_attributes <- function(x, code, pipe = "base", ...) {
  UseMethod("repair_attributes")
}

#' @export
repair_attributes.default <- function(x, code, pipe = "base", ...) {
  if (is.environment(x)) return(repair_attributes.environment(x, code, pipe, ...))
  if (rlang::is_formula(x))  return(repair_attributes.formula(x, code, pipe, ...))
  if (is.language(x) && !is.expression(x))  return(repair_attributes.language(x, code, pipe, ...))
  repair_attributes_impl(x, code, pipe, ...)
}

repair_attributes_impl <- function(x, code, pipe = "base", ignore = NULL, idiomatic_class = NULL, remove = NULL, ...) {
  # fetch non idiomatic args and class
  attrs <- attributes(x)
  attrs[ignore] <- NULL
  # names are already provided by construct_idiomatic except if they're ""
  if (is.null(attrs$names) || !all(attrs$names == "")) attrs$names <- NULL
  # The `noquote` class is added at the end of the class vector so method `.noquote`
  # wouldn't be triggered
  if (inherits(x, "noquote")) {
    attrs$class <- setdiff(attrs$class, "noquote")
    if (!length(attrs$class)) attrs$class <- NULL
    code <- wrap(code, "noquote", new_line = FALSE)
  }
  if (identical(attrs$class, idiomatic_class)) {
    attrs$class <- NULL
  } else if (is.null(attrs$class)) {
    # to be able to remove the idiomatic class explicitly, mainly (only ?) useful for classless formulas
    attrs["class"] <- list(NULL)
  }
  if (length(remove)) attrs <- c(attrs, setNames(replicate(length(remove), NULL), remove))
  if (!length(attrs)) return(code)
  # append structure() code to repair object
  attrs_code <- construct_apply(attrs, fun = "structure", pipe = pipe, ...)
  pipe(code, attrs_code, pipe)
}

construct_apply <- function(args, fun = "list", keep_trailing_comma = FALSE, language = FALSE, implicit_names = FALSE, new_line = TRUE, ...) {
  if (!length(args)) return("list()")
  if (!language) args <- lapply(args, construct_raw, ...)
  args_chr <- Map(name_and_append_comma, args, names2(args), implicit_names = implicit_names)
  args_chr <- unlist(args_chr)
  # if line is short enough stick all in one line
  # FIXME : chunk unnamed lists of single line items by lines of 80 chars ?
  nchrs <- nchar(args_chr)
  if (sum(nchrs) < 80 && all(endsWith(args_chr, ","))) {
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






