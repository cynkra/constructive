#' @export
#' @rdname other-opts
opts_raw <- function(
    constructor = c("as.raw", "charToRaw"),
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE,
    representation = c("hexadecimal", "integer", "character")
    ) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    { fill <- rlang::arg_match(fill) },
    abort_not_boolean(compress),
    { representation <- rlang::arg_match(representation) }
  )
  .cstr_options(
    "raw",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress,
    representation = representation
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
#' @method .cstr_construct.raw as.raw
.cstr_construct.raw.as.raw <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return(.cstr_repair_attributes(x, "raw(0)", ...))

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

  if (length(x) == 1 && is.null(names(x))) {
    code <- switch(
      opts$representation,
      hexadecimal = sprintf("as.raw(0x%02x)", as.integer(x)),
      integer = sprintf("as.raw(%s)", as.integer(x)),
      character = sprintf('as.raw("%02x")', as.integer(x))
    )
    code <- .cstr_repair_attributes(x_bkp, code, ...)
    return(code)
  }

  # wrap with c()
  code <- switch(
    opts$representation,
    hexadecimal = sprintf("0x%02x", as.integer(x)),
    integer = sprintf("%s", as.integer(x)),
    character = sprintf('"%02x"', as.integer(x))
  )
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  code <- .cstr_wrap(code, "as.raw")
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

#' @export
#' @method .cstr_construct.raw charToRaw
.cstr_construct.raw.charToRaw <- function(x, ...) {
  # Fall back when it cannot be represented by a string
  if (!length(x) || raw(1) %in% x) return(.cstr_construct.raw.as.raw(x, ...))

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

  code <- .cstr_wrap(.cstr_construct(rawToChar(x), ...), "charToRaw")
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

compress_raw <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0))) return(sprintf("raw(%s)", l))
  format_rep(x, ...)
}
