# FIXME: find a better place for those
# FIXME: repair_attributes should be dispatched on a prototype and attributes already present should
# be ignored by default

#' constructors
#'
#' A nested environment containing constructor functions for the package {constructive}
#' @export
constructors <- new.env()

repair_attributes <- function(x, code, ..., pipe = "base") {
  UseMethod("repair_attributes")
}

#' Repair attributes after idiomatic construction
#'
#' Exported for custom constructor design. In the general case an object might have more attributes than given by the idiomatic
#' construction. `.cstr_repair_attributes()` sets some of those attributes and ignores
#' others.
#'
#' @param x The object to construct
#' @param code The code constructing the object before attribute reparation
#' @param ... Forwarded to `.construct_apply()` when relevant
#' @param ignore The attributes that shouldn't be repaired, i.e. we expect them
#'   to be set by the constructor already in `code`
#' @param idiomatic_class The class of the objects that the constructor produces,
#'   if `x` is of class `idiomatic_class` there is no need to repair the class.
#' @param remove Attributes that should be removed, should rarely be useful.
#' @inheritParams construct
#'
#' @return A character vector
#' @export
.cstr_repair_attributes <- function(x, code, ..., pipe = "base", ignore = NULL, idiomatic_class = NULL, remove = NULL, one_liner = FALSE) {
  # fetch non idiomatic args and class
  attrs <- attributes(x)
  attrs[ignore] <- NULL
  # names are already provided by construct_raw except if they're ""
  if (is.null(attrs$names) || anyNA(attrs$names) || !all(attrs$names == "")) attrs$names <- NULL
  # The `noquote` class is added at the end of the class vector so method `.noquote`
  # wouldn't be triggered
  if (inherits(x, "noquote")) {
    attrs$class <- setdiff(attrs$class, "noquote")
    if (!length(attrs$class)) attrs$class <- NULL
    code <- .cstr_wrap(code, "noquote", new_line = FALSE)
  }
  if (identical(attrs$class, idiomatic_class)) {
    attrs$class <- NULL
  } else if (is.null(attrs$class)) {
    # to be able to remove the idiomatic class explicitly, mainly (only ?) useful for classless formulas
    attrs["class"] <- list(NULL)
  }
  if (length(remove)) attrs <- c(attrs, setNames(replicate(length(remove), NULL), remove))
  if (!length(attrs)) return(code)
  # See ?structure, when those arguments are provided to structure() differently named attributes are created
  special_structure_args <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
  special_attr_nms <- intersect(names(attrs), special_structure_args)
  special_attrs <- attrs[special_attr_nms]
  attrs[special_attr_nms] <- NULL
  # append structure() code to repair object
  attrs_code <- .cstr_apply(attrs, fun = "structure", ..., pipe = pipe, one_liner = one_liner)
  code <- .cstr_pipe(code, attrs_code, pipe, one_liner)
  for (attr_nm in special_attr_nms) {
    attr_code <- .cstr_apply(
      list(attr_nm, special_attrs[[attr_nm]]),
      "(`attr<-`)",
      ...,
      pipe = pipe,
      one_liner = one_liner)
    code <- .cstr_pipe(code, attr_code, pipe, one_liner)
  }
  code
}
