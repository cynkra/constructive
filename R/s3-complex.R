#' @export
#' @rdname other-opts
opts_complex <- function(
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
    "complex",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct complex
.cstr_construct.complex <- function(x, ...) {
  opts <- list(...)$opts$complex %||% opts_complex()
  if (is_corrupted_complex(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.complex", structure(NA, class = opts$constructor))
}

is_corrupted_complex <- function(x) {
  typeof(x) != "complex"
}

#' @export
#' @method .cstr_construct.complex default
.cstr_construct.complex.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return("complex(0)")

  # we apply in priority the complex opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$complex %||% opts_complex()
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
  if (opts$compress && is.null(nms)) {
    code <- compress_complex(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  re <- sapply(Re(x), function(x, ...) .cstr_construct.double(x, ...), ...)
  im <- sapply(Im(x), function(x, ...) .cstr_construct.double(x, ...), ...)
  code <- ifelse(
    is.na(x),
    "NA",
    ifelse (
      re == "0",
      paste0(im, "i"),
      ifelse(
        im == "0" & !all(im == "0"),
        re,
        paste0(re, "+", im, "i")
      )
    )
  )
  if (all(is.na(x) | im == "0")) {
    code[is.na(x)] <- "NA_complex_"
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

compress_complex <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0+0i))) return(sprintf("complex(%s)", l))
  format_rep(x, ...)
}

construct_complex <- function(x, ...) {
  re <- sapply(Re(x), construct_atomic, ...)
  im <- sapply(Im(x), construct_atomic, ...)
  code <- ifelse(
    is.na(x),
    "NA",
    ifelse (
      re == "0",
      paste0(im, "i"),
      ifelse(
        im == "0" & !all(im == "0"),
        re,
        paste0(re, "+", im, "i")
      )
    )
  )
  if (all(is.na(x) | im == "0")) {
    code[is.na(x)] <- "NA_complex_"
  }
  if (length(x) == 1 && is.null(names(x))) return(code)
  code <- .cstr_apply(
    code,
    "c",
    ...,
    recurse = FALSE
  )
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  code
}
