#' Constructive options for class 'html_dependency'
#'
#' These options will be used on objects of class 'html_dependency'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"htmlDependency"` (default): We build the object using
#'   `htmltools::htmlDependency()`, arguments left to their default values are omitted.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_html_dependency>
#' @export
opts_html_dependency <- function(constructor = c("htmlDependency", "next"), ...) {
  .cstr_options("html_dependency", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct html_dependency
.cstr_construct.html_dependency <- function(x, ...) {
  opts <- list(...)$opts$html_dependency %||% opts_html_dependency()
  if (is_corrupted_html_dependency(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.html_dependency", structure(NA, class = opts$constructor))
}

is_corrupted_html_dependency <- function(x) {
  expected <- c(
    "name", "version", "src", "meta", "script", "stylesheet", "head",
    "attachment", "package", "all_files"
  )
  if (!is.list(x) || !identical(names(x), expected)) return(TRUE)
  # as validated by `htmltools::htmlDependency()`
  is_valid_name <- function(nm) {
    rlang::is_string(nm) && nzchar(nm) && !grepl("[/\\]", nm) && is.null(attributes(nm))
  }
  if (!is_valid_name(x$name) || !is_valid_name(x$version)) return(TRUE)
  src <- x$src
  !is.list(src) ||
    !length(src) ||
    !identical(names(attributes(src)), "names") ||
    anyNA(names(src)) ||
    !all(nzchar(names(src)))
}

#' @export
#' @method .cstr_construct.html_dependency htmlDependency
.cstr_construct.html_dependency.htmlDependency <- function(x, ...) {
  src <- x$src
  src_is_chr <- all(vapply(
    src,
    function(elt) rlang::is_string(elt) && is.null(attributes(elt)),
    logical(1)
  ))
  if (src_is_chr) {
    src <- unlist(src)
    if (identical(names(src), "file")) src <- unname(src)
  }
  args <- list(x$name, x$version, src = src)
  optional <- x[c("meta", "script", "stylesheet", "head", "attachment", "package")]
  args <- c(args, Filter(Negate(is.null), optional))
  if (!isTRUE(x$all_files)) args$all_files <- x$all_files
  code <- .cstr_apply(args, fun = "htmltools::htmlDependency", ...)
  repair_attributes_html_dependency(x, code, ...)
}

repair_attributes_html_dependency <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "html_dependency"
  )
}
