#' Constructive options for type 'complex'
#'
#' @description
#' These options will be used on objects of type 'complex'. This type has
#' a single native constructor, but some additional options can be set.
#'
#' To set options on all atomic types at once see \link{opts_atomic}().
#'
#' @inheritParams opts_atomic
#' @inheritParams other-opts
#' @param fill String. Method to use to represent the trimmed elements. See `?opts_atomic`
#' @return An object of class <constructive_options/constructive_options_complex>
#' @export
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
  if (!length(x)) return(.cstr_repair_attributes(x, "complex(0)", ...))

  # we apply in priority the complex opts, fall back on atomic opts otherwise
  all_opts <- list(...)$opts
  opts <- all_opts$complex %||% opts_complex()
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
  if (opts$compress && is.null(nms)) {
    code <- compress_complex(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
      return(code)
    }
  }

  re <- Re(x)
  im <- Im(x)
  op <- if (isTRUE(sign(1/im) == -1)) "-" else "+"
  im <- abs(im)
  # override double options so they don't affect complex numbers
  all_opts$double <- opts
  re_code <- sapply(re, function(x, ..., opts) .cstr_construct.double(x, ..., opts = all_opts), ...)
  im_code <- sapply(im, function(x, ..., opts) .cstr_construct.double(x, ..., opts = all_opts), ...)

  # general case

  code <- sprintf("%s%s%si", re_code, op, im_code)
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
  if (any(other_na)) {
    code[other_na] <- mapply(
      function(re, im) {
        .cstr_apply(list(real = re, imaginary = im), "complex", recurse = FALSE)
      },
      re = re[other_na],
      im = im[other_na]
    )
  }

  if (length(x) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
    return(code)
  }

  # wrap with c()
  names(code) <- names(x)
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
}

compress_complex <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0+0i))) return(sprintf("complex(%s)", l))
  format_rep(x, ...)
}
