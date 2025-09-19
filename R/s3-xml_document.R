#' Constructive options for class 'xml_document'
#'
#' These options will be used on objects of class 'xml_document'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` (default): We build the object using `xml2::read_xml()` or `xml2::read_html()`
#'   on a string.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the constructor, often the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @param simplify Whether to remove the "meta http-equiv" and "!DOCTYPE" tags from the
#' input if they're the default ones.
#' @return An object of class <constructive_options/constructive_options_xml_document>
#' @export
opts_xml_document <- function(constructor = c("default", "next"), ..., simplify = TRUE) {
  constructive::.cstr_options("xml_document", constructor = constructor[[1]], ..., simplify = simplify)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.xml_document <- function(x, ...) {
  opts <- list(...)$opts$xml_document %||% opts_xml_document()
  if (is_corrupted_xml_document(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.xml_document", structure(NA, class = opts$constructor))
}

is_corrupted_xml_document <- function(x) {
  FALSE
}

#' @export
.cstr_construct.xml_document.default <- function(x, ...) {
  # doc_type() is an internal function so there is a small
  # chance for it to be renamed or removed
  if (!rlang::is_installed("xml2") || !exists("doc_type", asNamespace("xml2"))) {
    out <- .cstr_construct.list(x, ...)
    return(out)
  }
  opts <- list(...)$opts$xml_document %||% opts_xml_document()
  arg <- as.character(x)
  type <- getFromNamespace("doc_type", "xml2")(x)
  if (type == "xml") {
    fun <- "xml2::read_xml"
    if (opts$simplify) {
      arg <- sub(
        '<?xml version="1.0" encoding="UTF-8"?>',
        "", 
        arg, 
        fixed = TRUE
      )
    }
  } else {
    fun <- "xml2::read_html"
    if (opts$simplify) {
      # remove default DOCTYPE header
      arg <- sub(
        '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">\n', 
        "", 
        arg, 
        fixed = TRUE
      )
      # remove default meta tag
      arg <- sub(
        '<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n', 
        "", 
        arg, 
        fixed = TRUE
      )
      # remove trailing new lines
      arg <- sub("\n+$", "", arg)
    }
  }
  code <- constructive::.cstr_apply(list(arg), fun = fun, ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("xml_document", "xml_node")
  )
}