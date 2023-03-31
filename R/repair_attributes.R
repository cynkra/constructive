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
