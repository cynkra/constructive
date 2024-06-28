#' @export
#' @rdname other-opts
opts_character <- function(
    constructor = c("default"),
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    { fill <- rlang::arg_match(fill) },
    abort_not_boolean(compress),
    { unicode_representation <- rlang::arg_match(unicode_representation) },
    abort_not_boolean(escape)
  )
  .cstr_options(
    "character",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress,
    unicode_representation = unicode_representation,
    escape = escape
  )
}

#' @export
#' @method .cstr_construct character
.cstr_construct.character <- function(x, ...) {
  opts <- list(...)$opts$character %||% opts_character()
  if (is_corrupted_character(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.character", structure(NA, class = opts$constructor))
}

is_corrupted_character <- function(x) {
  typeof(x) != "character"
}

#' @export
#' @method .cstr_construct.character default
.cstr_construct.character.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return(.cstr_repair_attributes(x, "character(0)", ...))

  # we apply in priority the character opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$character %||% opts_character()
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
    code <- compress_character(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  # build code for strings with relevant format (a better sapply(x, deparse))
  strings <- construct_strings(x, ...)

  # return length 1 object early, no need for c() or NA compaction
  if (length(strings) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, strings, ...)
    return(code)
  }

  # use NA rather than NA_character when relevant
  nas <- strings == "NA_character_"
  if (any(nas) && !all(nas)) strings[nas] <- "NA"

  # wrap with c()
  code <- .cstr_apply(strings, "c", ..., recurse = FALSE)
  .cstr_repair_attributes(x_bkp, code, ...)
}
