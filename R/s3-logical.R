#' @export
#' @rdname other-opts
opts_logical <- function(
    constructor = c("default"),
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    { fill <- rlang::arg_match(fill) },
    abort_not_boolean(compress)
  )
  .cstr_options(
    "logical",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct logical
.cstr_construct.logical <- function(x, ...) {
  opts <- list(...)$opts$logical %||% opts_logical()
  if (is_corrupted_logical(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.logical", structure(NA, class = opts$constructor))
}

is_corrupted_logical <- function(x) {
  typeof(x) != "logical"
}

#' @export
#' @method .cstr_construct.logical default
.cstr_construct.logical.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return(.cstr_repair_attributes(x, "logical(0)", ...))

  # we apply in priority the logical opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$logical %||% opts_logical()
  x_bkp <- x

  # non standard names
  nms <- names(x)
  names_need_repair <- !is.null(nms) && (anyNA(nms) || all(nms == ""))
  if (names_need_repair) names(x) <- NULL

  # trim
  # FIXME: the name reparation is affected by trim
  if (!is.null(opts$trim)) {
    code <- trim_atomic(x, opts$trim, opts$fill, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  # compression
  if (opts$compress && is.null(names(x))) {
    code <- compress_logical(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  code <- sapply(x, deparse)
  if (!all(is.na(x))) {
    code[is.na(x)] <- "NA"
  }
  if (length(x) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, code, ...)
    return(code)
  }

  # wrap with c()
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

compress_logical <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(!x))) return(sprintf("logical(%s)", l))
  format_rep(x, ...)
}
