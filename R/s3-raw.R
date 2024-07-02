#' Constructive options for type 'raw'
#'
#' @description
#'
#' These options will be used on objects of type 'raw'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.raw"` (default): Use `as.raw()`, or `raw()` when relevant
#' * `"charToRaw"` : Use `charToRaw()` on a string, if the a raw vector contains
#'   a zero we fall back to the "as.raw" constructor.
#'
#' To set options on all atomic types at once see \link{opts_atomic}().
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param representation For "as.raw" constructor. Respectively generate output
#'   in the formats `as.raw(0x10)` or `as.raw(16)`
#' @inheritParams opts_atomic
#' @inheritParams other-opts
#' @param fill String. Method to use to represent the trimmed elements. See `?opts_atomic`
#' @return An object of class <constructive_options/constructive_options_raw>
#' @export
opts_raw <- function(
    constructor = c("as.raw", "charToRaw"),
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE,
    representation = c("hexadecimal", "decimal")
    ) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    { fill <- rlang::arg_match(fill) },
    abort_not_boolean(compress),
    { representation <- rlang::arg_match(representation) }
  )
  .cstr_options(
    "raw",
    constructor = constructor[[1]],
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
  repair_names <- names_need_repair(nms)
  if (repair_names) names(x) <- NULL

  # trim
  # FIXME: the name repair is affected by trim
  if (!is.null(opts$trim)) {
    code <- trim_atomic(x, opts$trim, opts$fill, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
      return(code)
    }
  }

  # compression
  if (opts$compress && is.null(names(x))) {
    code <- compress_raw(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
      return(code)
    }
  }

  if (length(x) == 1 && is.null(names(x))) {
    code <- switch(
      opts$representation,
      hexadecimal = sprintf("as.raw(0x%02x)", as.integer(x)),
      decimal = sprintf("as.raw(%s)", as.integer(x))
    )
    code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
    return(code)
  }

  # wrap with c()
  code <- switch(
    opts$representation,
    hexadecimal = sprintf("0x%02x", as.integer(x)),
    decimal = sprintf("%s", as.integer(x)),
  )
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  code <- .cstr_wrap(code, "as.raw")
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
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
  repair_names <- names_need_repair(nms)
  if (repair_names) names(x) <- NULL

  # trim
  # FIXME: the name repair is affected by trim
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
  .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
}

compress_raw <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0))) return(sprintf("raw(%s)", l))
  format_rep(x, ...)
}
