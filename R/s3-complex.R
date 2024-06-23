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

  re <- Re(x)
  im <- Im(x)
  re_code <- sapply(re, function(x, ...) .cstr_construct.double(x, ...), ...)
  im_code <- sapply(im, function(x, ...) .cstr_construct.double(x, ...), ...)

  # general case
  code <- sprintf("%s+%si", re_code, im_code)
  # zero real parts can be omitted
  zero_real <- re_code == "0"
  code[zero_real] <- paste0(im_code[zero_real], "i")
  # zero im parts can sometimes be omitted
  zero_im <- !zero_real & im_code == "0" & !all(im_code == "0")
  code[zero_im] <- re_code[zero_im]
  # if both parts are true NA we have a NA_complex
  complex_na <- is_na_real(re) & is_na_real(im)
  code[complex_na] <- "NA_complex_"
  other_na <- is.na(x) & !complex_na
  code[other_na] <- mapply(
    function(re, im) {
      .cstr_apply(list(real = re, imaginary = im), "complex", recurse = FALSE)
    },
    re = re[other_na],
    im = im[other_na]
  )

  if (length(x) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, code, ...)
    return(code)
  }

  # wrap with c()
  names(code) <- names(x)
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

compress_complex <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0+0i))) return(sprintf("complex(%s)", l))
  format_rep(x, ...)
}
