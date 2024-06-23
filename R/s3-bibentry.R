#' @export
#' @rdname other-opts
opts_bibentry <- function(constructor = c("bibentry", "next"), ...) {
  .cstr_options("bibentry", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct bibentry
.cstr_construct.bibentry <- function(x, ...) {
  opts <- list(...)$opts$bibentry %||% opts_bibentry()
  if (is_corrupted_bibentry(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.bibentry", structure(NA, class = opts$constructor))
}

is_corrupted_bibentry <- function(x) {
  x <- unclass(x)
  # the class was removed in "4.3.1"
  if (!is.list(x)) return(FALSE)
  is_footer_header <-
    with_versions(R > "4.3.0") &&
    length(x) == 0 &&
    (is.character(attr(x, "mfooter")) || is.character(attr(x, "mheader")))

  is_regular_bibentry <-
    length(x) == 1 &&
    is.list(x[[1]]) &&
    identical(names(attributes(unname(x[[1]]))), "bibtype") &&
    is.character(attr(x[[1]], "bibtype")) &&
    length(attr(x[[1]], "bibtype")) == 1
  !is_footer_header && !is_regular_bibentry
}

#' @export
#' @method .cstr_construct.bibentry bibentry
.cstr_construct.bibentry.bibentry <- function(x, ...) {
  is_footer_header <- length(x) == 0
  if (is_footer_header) {
    if (!is.null(attr(x, "mfooter"))) {
      code <- .cstr_apply(list(attr(x, "mfooter")), "citFooter", ...)
    } else {
      code <- .cstr_apply(list(attr(x, "mheader")), "citHeader", ...)
    }
  } else {
    stripped <- strip(x)
    args <- strip(stripped[[1]])
    args <- c(list(bibtype = attr(stripped[[1]], "bibtype")), args)
    code <- .cstr_apply(args, "bibentry", ...)
  }
  repair_attributes_bibentry(x, code, ...)
}

repair_attributes_bibentry <- function(x, code, pipe = NULL, ...) {
  .cstr_repair_attributes(
    x, code, pipe,
    idiomatic_class = "bibentry",
    ignore = c("mfooter", "mheader"),
    ...
  )
}
