#' @export
#' @rdname other-opts
opts_integer <- function(
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
    "integer",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct integer
.cstr_construct.integer <- function(x, ...) {
  opts <- list(...)$opts$integer %||% opts_integer()
  if (is_corrupted_integer(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.integer", structure(NA, class = opts$constructor))
}

is_corrupted_integer <- function(x) {
  typeof(x) != "integer"
}

#' @export
#' @method .cstr_construct.integer default
.cstr_construct.integer.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return("integer(0)")

  # we apply in priority the integer opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$integer %||% opts_integer()
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
    code <- compress_integer(x, ...)
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

compress_integer <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0L))) return(sprintf("integer(%s)", l))
  format_rep(x, ...) %||% format_seq(x, ...)
}
