#' @export
#' @rdname other-opts
opts_raw <- function(
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
    "raw",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct raw
.cstr_construct.raw <- function(x, ...) {
  opts <- list(...)$opts$raw %||% opts_raw()
  if (is_corrupted_raw(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.raw", structure(NA, class = opts$constructor))
}

is_corrupted_raw <- function(x) {
  typeof(x) != "raw"
}

#' @export
#' @method .cstr_construct.raw default
.cstr_construct.raw.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return("raw(0)")

  # we apply in priority the raw opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$raw %||% opts_raw()
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
    code <- compress_raw(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  code <- sapply(x, deparse)
  if (length(x) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, code, ...)
    return(code)
  }

  # wrap with c()
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

compress_raw <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0))) return(sprintf("raw(%s)", l))
  format_rep(x, ...)
}
