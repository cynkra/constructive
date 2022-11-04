construct_raw <- function(x, ..., data = NULL) {
  idiomatic_code <- data_match(x, data) %||% construct_idiomatic(x, ..., data = data)
  repaired_code <- repair_attributes(x, idiomatic_code, ..., data = data)
  repaired_code
}

data_match <- function(x, data) {
  if (is.null(data)) return(NULL)
  m <- match2(x, data)
  if (!length(m)) return(NULL)
  names(data)[m]
}

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
  construct_idiomatic.atomic(x, ..., one_liner = one_liner)
}

repair_attributes <- function(x, code, ..., pipe = "base") {
  UseMethod("repair_attributes")
}

#' @export
repair_attributes.default <- function(x, code, ..., pipe = "base") {
  if (is.environment(x)) return(repair_attributes.environment(x, code, ..., pipe = pipe))
  if (rlang::is_formula(x))  return(repair_attributes.formula(x, code, ..., pipe = pipe))
  if (is.language(x) && !is.expression(x))  return(repair_attributes.language(x, code, ..., pipe = pipe))
  repair_attributes_impl(x, code, ..., pipe = pipe)
}

repair_attributes_impl <- function(x, code, ..., pipe = "base", ignore = NULL, idiomatic_class = NULL, remove = NULL, one_liner = FALSE) {
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
  attrs_code <- construct_apply(attrs, fun = "structure", ..., pipe = pipe, one_liner = one_liner)
  pipe(code, attrs_code, pipe, one_liner)
}

construct_apply <- function(args, fun = "list", ..., keep_trailing_comma = FALSE, language = FALSE, implicit_names = FALSE, new_line = TRUE, one_liner = FALSE) {
  new_line <- new_line && !one_liner
  keep_trailing_comma <- keep_trailing_comma && !one_liner
  if (!length(args)) return(sprintf("%s()", fun))
  if (!language) args <- lapply(args, construct_raw, ..., one_liner = one_liner)
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






